# =============================================================================
#  Groundwater Bouquet Explorer — Shiny App
#  Depends on: shiny, bslib, bsicons, leaflet, plotly, sf, dplyr, rlang,
#              tidygeocoder, DBI, duckdb, bouquets
#
#  Install once:
#    install.packages(c("shiny", "bslib", "bsicons", "leaflet", "plotly",
#                       "sf", "dplyr", "rlang", "tidygeocoder",
#                       "DBI", "duckdb",
#                       "patchwork", "maps", "mapdata", "ggforce"))
#    remotes::install_github("MxNl/bouquets")
#
#  First run: execute create_db.R once to build groundwater.duckdb from your
#  CSV / RDS sources before launching the app.
# =============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(leaflet)
library(plotly)
library(sf)
library(dplyr)
library(rlang)
library(tidygeocoder)
library(DBI)
library(duckdb)
library(shinyWidgets)
library(shinyjs)
library(hues)
library(bouquets)

# ── Column name constants ─────────────────────────────────────────────────────
WELL_ID_COL <- "well_id"
LON_COL     <- "coords_x"
LAT_COL     <- "coords_y"
DATE_COL    <- "date"
VALUE_COL   <- "gwl"

# ── Coordinate reference system of coords_x / coords_y ───────────────────────
# Set this to the EPSG code matching your data. Common German options:
#   4326  — WGS84 geographic (decimal degrees)
#   25832 — ETRS89 / UTM zone 32N  ← typical for GEMS / BfG data
#   25833 — ETRS89 / UTM zone 33N
#   31467 — DHDN / Gauß-Krüger zone 3
#   3035  — ETRS89-LAEA Europe
COORDS_CRS <- 3035

# ── DuckDB path ───────────────────────────────────────────────────────────────
# TODO: adjust path to your database file created by create_db.R
DB_PATH <- "groundwater.duckdb"

# ── Demo: auto-create DB if the file doesn't exist yet ───────────────────────
# Remove this block once you have run create_db.R with your real data.
if (!file.exists(DB_PATH)) {
  message("groundwater.duckdb not found — creating demo database.")

  set.seed(42)
  n_steps <- 156L
  weeks   <- seq(as.Date("2022-01-03"), by = "week", length.out = n_steps)
  season  <- sin(seq(0, 2 * pi, length.out = n_steps))

  demo_wells <- tibble::tibble(
    well_id  = paste0("GW-", sprintf("%03d", 1:40)),
    coords_x = runif(40, 7.5, 14.5),
    coords_y = runif(40, 47.5, 55.0)
  )
  demo_ts <- purrr::map_dfr(demo_wells$well_id, function(id) {
    tibble::tibble(
      well_id = id,
      date    = weeks,
      gwl     = 5 + runif(1, -2, 2) +
        runif(1, 0.5, 1.5) * season +
        cumsum(rnorm(n_steps, 0, 0.12))
    )
  })

  setup_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = DB_PATH)
  DBI::dbWriteTable(setup_con, "metadata",   demo_wells, overwrite = TRUE)
  DBI::dbWriteTable(setup_con, "timeseries", demo_ts,    overwrite = TRUE)
  # Optimise for range queries on well_id + date
  DBI::dbExecute(setup_con,
    "CREATE INDEX IF NOT EXISTS idx_ts_well_date
     ON timeseries (well_id, date)")
  DBI::dbDisconnect(setup_con, shutdown = TRUE)
  message("  ✓ demo database created")
}

# ── Open a *read-only* shared connection at startup ───────────────────────────
# Must come AFTER the demo block above so the file is guaranteed to exist.
# metadata is loaded once here into R memory (it's small — one row per well).
# The time-series connection is opened per-session inside server() to avoid
# DuckDB's "invalid connection" error when a single connection object is shared
# across Shiny reactive contexts.
.meta_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = DB_PATH, read_only = TRUE)
meta_df   <- DBI::dbReadTable(.meta_con, "metadata")
DBI::dbDisconnect(.meta_con, shutdown = TRUE)
rm(.meta_con)

# ── Query the overall date range once for slider initialisation ───────────────
.range_con  <- DBI::dbConnect(duckdb::duckdb(), dbdir = DB_PATH, read_only = TRUE)
.yr         <- DBI::dbGetQuery(.range_con,
  "SELECT YEAR(MIN(date)) AS yr_min, YEAR(MAX(date)) AS yr_max FROM timeseries")
DB_YEAR_MIN <- as.integer(.yr$yr_min)
DB_YEAR_MAX <- as.integer(.yr$yr_max)
DBI::dbDisconnect(.range_con, shutdown = TRUE)
rm(.range_con, .yr)

# ── App-level geocode cache — persists across sessions for the lifetime of the
# process, so repeated searches by any user are served instantly without a
# network round-trip to Nominatim.
.geocode_cache <- new.env(parent = emptyenv())
# ─────────────────────────────────────────────────────────────────────────────


# ── Helper: query + resample time series entirely in DuckDB ──────────────────
# resolution: "week" | "month" | "quarter" | "year"  (used as DATE_TRUNC unit)
# year_from / year_to: integer years, inclusive
query_timeseries <- function(con, well_ids,
                             year_from  = NULL,
                             year_to    = NULL,
                             resolution = "week") {

  ids_sql <- paste0("'", well_ids, "'", collapse = ", ")

  # Date filter clause
  date_filter <- ""
  if (!is.null(year_from))
    date_filter <- paste0(date_filter,
      sprintf(" AND date >= '%d-01-01'::DATE", as.integer(year_from)))
  if (!is.null(year_to))
    date_filter <- paste0(date_filter,
      sprintf(" AND date <= '%d-12-31'::DATE", as.integer(year_to)))

  # For weekly resolution keep raw rows; for coarser resolutions aggregate
  # via DATE_TRUNC so we get one representative row per period per well.
  # AVG(gwl) is the summary statistic — change to FIRST/LAST if preferred.
  if (resolution == "week") {
    query <- sprintf(
      "SELECT well_id, date::DATE AS date, gwl
         FROM timeseries
        WHERE well_id IN (%s)%s
        ORDER BY well_id, date",
      ids_sql, date_filter
    )
  } else {
    trunc_unit <- switch(resolution,
      "month"   = "month",
      "quarter" = "quarter",
      "year"    = "year",
      "month"   # safe fallback
    )
    query <- sprintf(
      "SELECT well_id,
              DATE_TRUNC('%s', date)::DATE AS date,
              AVG(gwl) AS gwl
         FROM timeseries
        WHERE well_id IN (%s)%s
        GROUP BY well_id, DATE_TRUNC('%s', date)
        ORDER BY well_id, date",
      trunc_unit, ids_sql, date_filter, trunc_unit
    )
  }

  DBI::dbGetQuery(con, query)
}


# ── Helper: geocode address ───────────────────────────────────────────────────
geocode_address <- function(query) {
  result <- tidygeocoder::geocode(
    tibble::tibble(address = query),
    address      = address,
    method       = "osm",   # free, no API key; swap to "google" for higher accuracy
    quiet        = TRUE,
    custom_query = list(countrycodes = "de")  # restrict Nominatim results to Germany
  )
  list(lat = result$lat, lon = result$long)
}


# ── Helper: find n nearest wells ─────────────────────────────────────────────
find_nearest_wells <- function(target_lon, target_lat, meta, n) {
  # Build sf from the input CRS (may be projected, e.g. UTM32)
  wells_sf  <- sf::st_as_sf(meta, coords = c(LON_COL, LAT_COL), crs = COORDS_CRS)

  # Geocoder always returns WGS84 — transform target to match well CRS first,
  # then both go to ETRS89-LAEA (metric) for the distance calculation
  target_sf <- sf::st_sfc(sf::st_point(c(target_lon, target_lat)), crs = 4326) |>
    sf::st_transform(crs = COORDS_CRS)

  wells_m  <- sf::st_transform(wells_sf,  crs = 3035)
  target_m <- sf::st_transform(target_sf, crs = 3035)

  dists        <- sf::st_distance(wells_m, target_m)
  meta$dist_m  <- as.numeric(dists)
  meta$dist_km <- round(meta$dist_m / 1000, 2)

  nearest <- meta |>
    dplyr::arrange(dist_m) |>
    dplyr::slice_head(n = min(n, nrow(meta)))

  # Add WGS84 lon/lat columns for leaflet — reproject from input CRS if needed
  if (COORDS_CRS == 4326) {
    nearest$lon_wgs84 <- nearest[[LON_COL]]
    nearest$lat_wgs84 <- nearest[[LAT_COL]]
  } else {
    wgs84_coords <- nearest |>
      sf::st_as_sf(coords = c(LON_COL, LAT_COL), crs = COORDS_CRS) |>
      sf::st_transform(crs = 4326) |>
      sf::st_coordinates()
    nearest$lon_wgs84 <- wgs84_coords[, "X"]
    nearest$lat_wgs84 <- wgs84_coords[, "Y"]
  }

  nearest
}


# =============================================================================
#  UI
# =============================================================================
ui <- bslib::page_navbar(
  title = "💧 Groundwater Bouquet Explorer",
  theme = bslib::bs_theme(
    bootswatch  = "flatly",
    primary     = "#2C7BB6",
    base_font   = bslib::font_google("Inter")
  ),
  navbar_options = bslib::navbar_options(bg = "#2C7BB6"),
  header = tagList(
    shinyjs::useShinyjs(),
    # Intro modal — shown on first load
    tags$script(HTML("
      $(document).ready(function() {
        // Show intro on load
        setTimeout(function() {
          Shiny.setInputValue('show_intro', 1, {priority: 'event'});
        }, 300);

        // Enter key in address input fires the Shiny button click normally
        $(document).on('keydown', '#address', function(e) {
          if (e.key === 'Enter') {
            e.preventDefault();
            var btn = document.getElementById('geocode_btn');
            if (btn && !btn.disabled) { btn.click(); }
          }
        });

      });
    "))
  ),

  # ── Main tab ────────────────────────────────────────────────────────────────
  bslib::nav_panel(
    "Explorer",

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 310,
        open  = TRUE,

        # — Location input —
        bslib::card(
          bslib::card_header(
            bsicons::bs_icon("geo-alt-fill"), " Location"
          ),
          textInput(
            "address", NULL,
            placeholder = "ZIP code or address…",
            value = ""
          ),
          tags$p(
            style = "font-size:11px; color:#888; margin: -6px 0 6px 0;",
            "German ZIP code, city name, or full address"
          ),
          uiOutput("geocode_btn_ui"),
          uiOutput("geocode_status_ui")
        ),

        # — Settings —
        bslib::card(
          bslib::card_header(
            bsicons::bs_icon("sliders"), " Settings"
          ),
          sliderInput(
            "n_wells", "Number of nearest wells",
            min = 2, max = 50, value = 15, step = 1
          ),
          sliderInput(
            "year_range", "Time period",
            min   = DB_YEAR_MIN, max = DB_YEAR_MAX,
            value = c(DB_YEAR_MIN, DB_YEAR_MAX),
            step  = 1, sep = ""
          ),
          shinyWidgets::sliderTextInput(
            "resolution", "Temporal resolution",
            choices  = c("week", "month", "quarter", "year"),
            selected = "week",
            grid     = TRUE
          )
        ),

        # — Plot options —
        bslib::card(
          bslib::card_header(
            bsicons::bs_icon("palette"), " Plot options"
          ),
          uiOutput("marker_every_ui"),
          checkboxInput("show_labels",  "Show path labels",  value = FALSE),
          checkboxInput("show_rings",   "Show rings",        value = FALSE),
          checkboxInput("dark_mode",    "Dark mode",         value = FALSE),
          checkboxInput("show_cluster", "Colour by cluster", value = FALSE)
        )
      ),  # /sidebar

      # — Main content area —
      bslib::layout_columns(
        col_widths  = c(7, 5),
        row_heights = "calc(100vh - 140px)",

        # Bouquet plot card
        bslib::card(
          full_screen = TRUE,
          height      = "calc(100vh - 140px)",
          style       = "background-color: #f5f0e8;",
          bslib::card_header(
            style = "background-color: #ede8de;",
            bsicons::bs_icon("flower1"), " Bouquet Plot",
            div(
              style = "float:right; display:flex; gap:6px; align-items:center;",
              uiOutput("reset_btn_ui"),
              downloadButton(
                "download_bouquet", "Download",
                class = "btn btn-sm btn-outline-primary"
              )
            )
          ),
          bslib::card_body(
            padding = 0,
            style   = "background-color: #f5f0e8;",
            uiOutput("bouquet_ui")
          )
        ),

        # Map card
        bslib::card(
          full_screen = TRUE,
          height      = "calc(100vh - 140px)",
          bslib::card_header(
            bsicons::bs_icon("map"), " Well Locations"
          ),
          bslib::card_body(
            padding = 0,
            leaflet::leafletOutput("well_map", height = "100%")
          )
        )
      )
    )
  ),

  # ── About tab ───────────────────────────────────────────────────────────────
  bslib::nav_panel(
    "About",
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header(bsicons::bs_icon("flower1"), " The Bouquet Plot"),
        bslib::card_body(
          tags$p(
            "This app visualises groundwater level time series using the ",
            tags$a(tags$b("bouquets"), href = "https://github.com/MxNl/bouquets",
                   target = "_blank"),
            " R package, which implements ", tags$em("angular accumulation plots"),
            " — a technique structurally related to DNA walk visualisations",
            " (Gates 1986; Yau et al. 2003)."
          ),
          tags$p(
            "Each monitoring well is encoded as a path drawn by an imaginary turtle",
            " starting at the centre of the plot. At each time step the turtle:"
          ),
          tags$ul(
            tags$li(tags$span("↺ turns left", style="color:#2C7BB6;font-weight:600"),
                    " — when the groundwater level ", tags$b("rises")),
            tags$li(tags$span("↻ turns right", style="color:#c0392b;font-weight:600"),
                    " — when the groundwater level ", tags$b("falls")),
            tags$li("walks straight — when the level is unchanged")
          ),
          tags$p(
            "All paths share the same origin and the same turning angle θ, derived",
            " so that even the most volatile well in the selection never completes",
            " a full loop. This means:"
          ),
          tags$ul(
            tags$li(tags$b("Path direction"), " encodes directional dynamics, not absolute level."),
            tags$li(tags$b("Path length"), " reflects the number of observed time steps."),
            tags$li(tags$b("Curl direction"),
                    " — persistent ↺ counter-clockwise curl → ",
                    tags$b("rising long-term trend"), "; ↻ clockwise → ",
                    tags$b("declining trend"), "."),
            tags$li(tags$b("Path spread"),
                    " — compact paths = stable levels; elongated = strong seasonality."),
            tags$li(tags$b("Angular similarity"),
                    " — wells pointing in the same direction share similar dynamics,",
                    " regardless of geography or absolute depth.")
          ),
          tags$h6("How to use this app", style="margin-top:12px"),
          tags$ol(
            tags$li("Enter a ZIP code or address and click ", tags$b("Find nearest wells")),
            tags$li("Adjust ", tags$b("n"), " wells, time period and resolution with the sliders"),
            tags$li("Click a well marker on the map to ", tags$b("highlight"),
                    " its path in the bouquet (click map background to deselect)"),
            tags$li("Use the ", tags$b("Download"), " button to save the plot as PNG"),
            tags$li("Click ", tags$b("Help"), " in the navbar to reopen this guide")
          )
        )
      ),

      bslib::card(
        bslib::card_header(bsicons::bs_icon("database"), " The GEMS-GER Dataset"),
        bslib::card_body(
          tags$p(
            tags$b("GEMS-GER"), " (", tags$em("Groundwater Levels, Environment,",
            " Meteorology, Site Properties — Germany"), ") is the first benchmark",
            " dataset specifically designed for machine learning applications in",
            " long-term groundwater level modelling in Germany",
            " (Ohmer et al., 2025, ", tags$em("Earth System Science Data"), ")."
          ),
          tags$h6("Key facts", style="margin-top:4px"),
          tags$ul(
            tags$li(tags$b("3,207 monitoring wells"), " distributed across all 16 German",
                    " federal states, covering diverse hydrogeological settings and aquifer types."),
            tags$li(tags$b("32 years"), " of gapless weekly groundwater level observations",
                    " (1991–2022), preprocessed via harmonisation, outlier removal,",
                    " and iterative imputation."),
            tags$li(tags$b("50+ static site attributes"), " per well: topographic indices,",
                    " aquifer medium, filter depths, screen length, and administrative features."),
            tags$li(tags$b("Meteorological forcings"), " co-located per well: daily",
                    " temperature, precipitation and humidity (HYRAS/DWD), evapotranspiration,",
                    " soil moisture, snow water equivalent, and runoff (ERA5-Land)."),
            tags$li("Data sourced from the responsible environmental authorities of all",
                    " 16 German federal states (LUBW, LfU Bavaria, HLNUG, NLWKN, etc.).")
          ),
          tags$h6("Benchmark models", style="margin-top:8px"),
          tags$p(
            "The dataset ships with three initial benchmark models: a single-well CNN,",
            " a global LSTM using only dynamic inputs, and a global LSTM incorporating",
            " both dynamic and static features — enabling systematic comparison of",
            " local vs. global modelling strategies."
          ),
          tags$h6("Access", style="margin-top:8px"),
          tags$p(
            "Publicly available under an open-access licence via Zenodo",
            " (", tags$a("doi:10.5281/zenodo.15530171",
                         href = "https://doi.org/10.5281/zenodo.15530171",
                         target = "_blank"), ").",
            " Publication: ", tags$a("Ohmer et al. (2025), ESSD 18, 77.",
                                      href = "https://essd.copernicus.org/articles/18/77/2026/",
                                      target = "_blank")
          ),
          tags$hr(),
          tags$p(style = "font-size:11px; color:#888",
            "Distances are computed in ETRS89-LAEA (EPSG:3035) for metric accuracy.",
            " Geocoding uses OpenStreetMap / Nominatim (OSM).",
            " Coordinate reprojection via the sf package."
          )
        )
      )
    )   # close nav_panel("About", ...)
  ),
  bslib::nav_item(
    tags$a(
      href = "#",
      onclick = "Shiny.setInputValue('show_intro', Math.random(), {priority: 'event'}); return false;",
      bsicons::bs_icon("question-circle"), " Help"
    )
  )
)


# =============================================================================
#  Server
# =============================================================================
server <- function(input, output, session) {

  # ── Intro modal ──────────────────────────────────────────────────────────────
  observeEvent(input$show_intro, {
    showModal(modalDialog(
      title = tagList(bsicons::bs_icon("flower1"), " Welcome to the Groundwater Bouquet Explorer"),
      size  = "l",
      easyClose = TRUE,
      footer = modalButton("Got it — let's explore!"),

      tags$p(
        "This app explores groundwater level dynamics across Germany using ",
        tags$b("bouquet plots"), " — a novel angular accumulation visualisation."
      ),
      tags$hr(),
      tags$h6("How the bouquet plot works"),
      tags$p(
        "Each monitoring well is drawn as a path. An imaginary turtle starts at the",
        " centre and walks one step per time unit, turning:"
      ),
      tags$ul(
        tags$li(HTML("<span style='color:#2C7BB6;font-weight:600'>↺ left</span> when the water level <b>rises</b>")),
        tags$li(HTML("<span style='color:#c0392b;font-weight:600'>↻ right</span> when the water level <b>falls</b>"))
      ),
      tags$p(HTML(
        "All paths share the same origin, so wells with similar dynamics point the same way.<br>
         <b>↺ persistent curl</b> → rising long-term trend &nbsp;·&nbsp;
         <b>↻ persistent curl</b> → declining trend<br>
         Compact path → stable levels &nbsp;·&nbsp; Elongated path → strong seasonality"
      )),
      tags$p(
        style = "background:#f0f4f8; border-left:3px solid #2C7BB6; padding:8px 12px; border-radius:3px; margin-top:8px;",
        tags$b("Important: "),
        "Bouquet plots do ",
        tags$em("not"),
        " show absolute groundwater levels — they visualise only the ",
        tags$b("temporal dynamics"),
        " of table fluctuations (rises and falls)."
      ),
      tags$p(
        style = "background:#f0f4f8; border-left:3px solid #2C7BB6; padding:8px 12px; border-radius:3px; margin-top:4px;",
        tags$b("Resolution matters: "),
        "The shape of a path is strongly dependent on the chosen temporal resolution.",
        " At weekly resolution, short-term noise dominates.",
        " For long-term trends, choose ",
        tags$b("yearly resolution"),
        " — since groundwater tables typically fall during most weeks of the year",
        " and only rise during the few wet recharge weeks, a weekly bouquet will",
        " curl clockwise even for a stable or slowly rising well."
      ),
      tags$hr(),
      tags$h6("Getting started"),
      tags$ol(
        tags$li("Enter a ", tags$b("German ZIP code or address"), " in the sidebar and click ",
                tags$b("Find nearest wells")),
        tags$li("Adjust the number of wells, time period, and resolution"),
        tags$li("Click any well on the map to highlight its path in the bouquet"),
        tags$li("Use the ", tags$b("Download"), " button to save the plot as PNG")
      ),
      tags$p(style = "font-size:11px; color:#888; margin-top:8px;",
        "Data: GEMS-GER dataset (Ohmer et al., 2025) — 3,207 wells, weekly GWL 1991–2022.")
    ))
  })

  # ── Geocode button UI — shows spinner while loading ───────────────────────────
  output$geocode_btn_ui <- renderUI({
    actionButton(
      "geocode_btn", "Find nearest wells",
      class = "btn-primary w-100",
      icon  = icon("magnifying-glass")
    )
  })

  # ── Reset highlight button — only shown when a well is highlighted ────────────
  output$reset_btn_ui <- renderUI({
    req(highlighted_well())
    actionButton(
      "reset_highlight", "Reset highlight",
      class = "btn btn-sm btn-outline-secondary",
      icon  = icon("rotate-left")
    )
  })
  # Each session gets its own read-only connection. DuckDB allows multiple
  # concurrent read-only connections to the same file without locking issues.
  gw_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = DB_PATH, read_only = TRUE)

  # ── Reactive: geocoded location ─────────────────────────────────────────────
  target_loc         <- reactiveVal(NULL)
  confirmed_address  <- reactiveVal(NULL)   # set only on successful geocode
  # .geocode_cache lives at app level — shared across all sessions

  observeEvent(input$geocode_btn, {
    query <- trimws(input$address)
    req(nchar(query) > 0)

    # Show spinner on the button while geocoding runs; restore it when done
    # (on.exit fires on both success and error paths)
    shinyjs::html("geocode_btn",
      '<span class="spinner-border spinner-border-sm me-2" role="status" aria-hidden="true"></span>Searching…'
    )
    shinyjs::disable("geocode_btn")
    on.exit({
      shinyjs::html("geocode_btn",
        '<i class="fa fa-magnifying-glass" role="presentation" aria-label="magnifying-glass icon"></i> Find nearest wells'
      )
      shinyjs::enable("geocode_btn")
    })

    # Check cache first — avoids redundant API calls for repeated searches
    cache_key <- tolower(query)
    if (exists(cache_key, envir = .geocode_cache)) {
      loc <- .geocode_cache[[cache_key]]
    } else {
      tryCatch({
        loc <- geocode_address(query)
        if (!is.na(loc$lat) && !is.na(loc$lon)) {
          assign(cache_key, loc, envir = .geocode_cache)
        }
      }, error = function(e) {
        showNotification(
          paste("Geocoding error:", conditionMessage(e)),
          type = "error", duration = 8
        )
        loc <<- list(lat = NA, lon = NA)
      })
    }

    if (is.na(loc$lat) || is.na(loc$lon)) {
      showNotification(
        "Address not found. Try a more specific query or a different spelling.",
        type = "warning", duration = 6
      )
      target_loc(NULL)
      confirmed_address(NULL)
    } else {
      target_loc(loc)
      confirmed_address(query)
    }
  })

  # ── Reactive: n nearest wells (metadata) ────────────────────────────────────
  nearest_meta <- reactive({
    req(target_loc())
    find_nearest_wells(
      target_loc()$lon,
      target_loc()$lat,
      meta_df,
      input$n_wells
    )
  })

  # ── Reactive: filtered + enriched time series ───────────────────────────────
  selected_ts <- reactive({
    req(nearest_meta())
    ids <- nearest_meta()[[WELL_ID_COL]]

    yr_from <- input$year_range[1]
    yr_to   <- input$year_range[2]

    # All filtering and resampling happens in DuckDB — nothing loaded into R
    # beyond the rows actually needed for the plot.
    ts <- query_timeseries(
      gw_con,
      well_ids   = ids,
      year_from  = yr_from,
      year_to    = yr_to,
      resolution = input$resolution
    )

    ts |>
      dplyr::left_join(
        nearest_meta() |>
          dplyr::select(all_of(c(WELL_ID_COL, LON_COL, LAT_COL, "dist_km"))),
        by = WELL_ID_COL
      )
  })

  # ── Reactive: clustered time series (shared by plot + map) ──────────────────
  # Runs cluster_bouquet() only when show_cluster is TRUE, otherwise returns
  # the plain selected_ts(). Having this as a separate reactive avoids running
  # the clustering twice (once for the plot, once for the map).
  clustered_ts <- reactive({
    req(selected_ts())
    df <- selected_ts()
    if (!isTRUE(input$show_cluster)) return(df)
    bouquets::cluster_bouquet(
      df,
      time_col   = !!sym(DATE_COL),
      series_col = !!sym(WELL_ID_COL),
      value_col  = !!sym(VALUE_COL)
    )
  })

  # ── Reactive: per-well cluster colour map ────────────────────────────────────
  # Builds the bouquet plot once, extracts the actual colour assigned to each
  # well_id from the rendered ggplot layers — guaranteed to match the plot.
  cluster_colours <- reactive({
    if (!isTRUE(input$show_cluster)) return(NULL)
    req(clustered_ts())
    df <- clustered_ts()
    if (!"cluster" %in% names(df)) return(NULL)

    dark <- isTRUE(input$dark_mode)

    # Build the same plot the renderPlot will draw
    p <- bouquets::make_plot_bouquet(
      df,
      time_col      = !!sym(DATE_COL),
      series_col    = !!sym(WELL_ID_COL),
      value_col     = !!sym(VALUE_COL),
      stem_colors   = cluster,
      flower_colors = cluster,
      dark_mode     = dark,
      verbose       = FALSE
    )

    # Convert S7 bouquet_plot → patchwork → extract first ggplot panel
    pw <- patchwork::wrap_plots(p)
    gg <- pw[[1L]]

    # Build the ggplot to get resolved aesthetics per data point
    built <- ggplot2::ggplot_build(gg)

    # Find the first layer that has both 'colour' and the series column
    # (bouquets draws path segments; colour is mapped to the series/cluster)
    colour_df <- NULL
    series_col_name <- WELL_ID_COL
    for (ld in built$data) {
      if ("colour" %in% names(ld) && nrow(ld) > 0) {
        colour_df <- ld
        break
      }
    }
    if (is.null(colour_df)) return(NULL)

    # The layout maps group integers to the original factor levels
    # group in built$data corresponds to the order of unique series values
    well_ids <- unique(df[[WELL_ID_COL]])

    # Each well gets one group integer; extract one colour per group
    grp_colours <- colour_df |>
      dplyr::distinct(group, colour) |>
      dplyr::arrange(group)

    # Match group index to well_id by position (both ordered by appearance)
    n_match <- min(nrow(grp_colours), length(well_ids))
    tibble::tibble(
      !!WELL_ID_COL := well_ids[seq_len(n_match)],
      colour        = grp_colours$colour[seq_len(n_match)]
    )
  })
  highlighted_well <- reactiveVal(NULL)

  observeEvent(input$reset_highlight, {
    highlighted_well(NULL)
  })

  # ── Reactive: plot title with address, date range and resolution ──────────────
  plot_title <- reactive({
    addr <- confirmed_address()
    if (is.null(addr) || nchar(addr) == 0) {
      sprintf('Groundwater level dynamics · %d nearest wells', input$n_wells)
    } else {
      sprintf('Groundwater level dynamics · %d nearest wells to "%s"',
              input$n_wells, addr)
    }
  })

  # ── Render marker_every UI: slider or info text depending on resolution ───────
  output$marker_every_ui <- renderUI({
    RESOLUTION_ORDER <- c("year", "quarter", "month", "week", "day")
    res_idx <- match(input$resolution, RESOLUTION_ORDER)
    valid   <- if (res_idx > 1L) RESOLUTION_ORDER[seq_len(res_idx - 1L)] else character(0)

    if (length(valid) == 0L) {
      tagList(
        tags$label("Time step markers",
                   class = "control-label",
                   style = "font-weight:600; font-size:0.875rem;"),
        tags$p(
          style = "font-size:11px; color:#888; margin:2px 0 6px 0;",
          "Not available at yearly resolution."
        )
      )
    } else {
      choices <- c("off", valid)
      current <- isolate(input$marker_every)
      sel     <- if (!is.null(current) && current %in% choices) current else "off"
      shinyWidgets::sliderTextInput(
        "marker_every", "Time step markers",
        choices  = choices,
        selected = sel,
        grid     = FALSE
      )
    }
  })
  onStop(function() {
    DBI::dbDisconnect(gw_con, shutdown = TRUE)
  })

  # ── Download handler for static bouquet ──────────────────────────────────────
  output$download_bouquet <- downloadHandler(
    filename = function() {
      sprintf("bouquet_%s_%dwells_%s_%d-%d.png",
              gsub("[^a-zA-Z0-9]", "_", trimws(input$address)),
              input$n_wells,
              input$resolution,
              input$year_range[1],
              input$year_range[2])
    },
    content = function(file) {
      req(selected_ts())
      df  <- selected_ts()
      hw  <- highlighted_well()
      use_cluster <- isTRUE(input$show_cluster)

      if (use_cluster) {
        df <- bouquets::cluster_bouquet(
          df,
          time_col   = !!sym(DATE_COL),
          series_col = !!sym(WELL_ID_COL),
          value_col  = !!sym(VALUE_COL)
        )
      }

      valid_markers <- c("year", "quarter", "month", "week", "day")
      marker_arg <- if (input$marker_every == "off" || !input$marker_every %in% valid_markers) NULL else input$marker_every

      p <- if (use_cluster) {
        bouquets::make_plot_bouquet(
          df,
          time_col      = !!sym(DATE_COL),
          series_col    = !!sym(WELL_ID_COL),
          value_col     = !!sym(VALUE_COL),
          stem_colors   = cluster,
          flower_colors = cluster,
          highlight     = hw,
          show_labels   = isTRUE(input$show_labels),
          show_rings    = isTRUE(input$show_rings),
          marker_every  = marker_arg,
          dark_mode     = isTRUE(input$dark_mode),
          title         = plot_title(),
          verbose       = FALSE
        )
      } else {
        bouquets::make_plot_bouquet(
          df,
          time_col      = !!sym(DATE_COL),
          series_col    = !!sym(WELL_ID_COL),
          value_col     = !!sym(VALUE_COL),
          stem_colors   = "greens",
          flower_colors = "blossom",
          highlight     = hw,
          show_labels   = isTRUE(input$show_labels),
          show_rings    = isTRUE(input$show_rings),
          marker_every  = marker_arg,
          dark_mode     = isTRUE(input$dark_mode),
          title         = plot_title(),
          verbose       = FALSE
        )
      }

      if (!isTRUE(input$show_legend)) {
        p <- patchwork::wrap_plots(p) & ggplot2::theme(legend.position = "none")
      }

      bg_col <- if (isTRUE(input$dark_mode)) "#1a1a2e" else "#f5f0e8"
      ggplot2::ggsave(file, plot = p, width = 12, height = 10, dpi = 300, bg = bg_col)
    }
  )

  # ── Geocode status badge ─────────────────────────────────────────────────────
  output$geocode_status_ui <- renderUI({
    req(target_loc(), nearest_meta())
    loc  <- target_loc()
    meta <- nearest_meta()
    tagList(
      tags$hr(style = "margin: 6px 0"),
      tags$small(
        bsicons::bs_icon("check-circle-fill", class = "text-success"),
        sprintf(
          " %.4f°N, %.4f°E — %d wells loaded (max %.1f km)",
          loc$lat, loc$lon, nrow(meta), max(meta$dist_km)
        )
      )
    )
  })

  # ── Bouquet UI (switches between static and interactive) ────────────────────
  output$bouquet_ui <- renderUI({
    if (is.null(target_loc())) {
      # Empty state — no address entered yet
      div(
        style = paste(
          "height:100%; display:flex; flex-direction:column;",
          "align-items:center; justify-content:center;",
          "color:#aaa; border: 2px dashed #d8cfc0; border-radius:8px; margin:16px;"
        ),
        tags$span(style = "font-size:3rem;", "🌸"),
        tags$p(style = "margin-top:12px; font-size:14px; text-align:center;",
          "Enter a location in the sidebar", tags$br(),
          "to explore nearby groundwater wells"
        )
      )
    } else {
      tagList(
        # Spinner overlay while plot renders
        div(id = "bouquet_spinner",
          style = paste(
            "position:absolute; top:50%; left:50%; transform:translate(-50%,-50%);",
            "z-index:10; display:none;"
          ),
          tags$div(class = "spinner-border text-secondary", role = "status")
        ),
        plotOutput("bouquet_static", height = "100%",
                   click = "bouquet_static_click")
      )
    }
  })

  # ── Static bouquet ──────────────────────────────────────────────────────────
  output$bouquet_static <- renderPlot({
    req(clustered_ts())

    # Dark mode background — must be set inside the render expression
    bg_col <- if (isTRUE(input$dark_mode)) "#1a1a2e" else "white"
    par(bg = bg_col)

    # Use shared clustered_ts reactive — avoids running cluster_bouquet() twice
    df          <- clustered_ts()
    use_cluster <- isTRUE(input$show_cluster)

    valid_markers <- c("year", "quarter", "month", "week", "day")
    marker_arg <- if (input$marker_every == "off" || !input$marker_every %in% valid_markers) NULL else input$marker_every

    # make_plot_bouquet() accepts either a keyword string (e.g. "greens") or
    # a bare column symbol. We must use !! only when passing a sym(), and pass
    # strings directly — mixing them up is what caused the "column not found" error.
    hw <- highlighted_well()

    p <- if (use_cluster) {
      bouquets::make_plot_bouquet(
        df,
        time_col      = !!sym(DATE_COL),
        series_col    = !!sym(WELL_ID_COL),
        value_col     = !!sym(VALUE_COL),
        stem_colors   = cluster,
        flower_colors = cluster,
        highlight     = hw,
        show_labels   = isTRUE(input$show_labels),
        show_rings    = isTRUE(input$show_rings),
        marker_every  = marker_arg,
        dark_mode     = isTRUE(input$dark_mode),
        title         = plot_title(),
        verbose       = FALSE
      )
    } else {
      bouquets::make_plot_bouquet(
        df,
        time_col      = !!sym(DATE_COL),
        series_col    = !!sym(WELL_ID_COL),
        value_col     = !!sym(VALUE_COL),
        stem_colors   = "greens",
        flower_colors = "blossom",
        highlight     = hw,
        show_labels   = isTRUE(input$show_labels),
        show_rings    = isTRUE(input$show_rings),
        marker_every  = marker_arg,
        dark_mode     = isTRUE(input$dark_mode),
        title         = plot_title(),
        verbose       = FALSE
      )
    }

    # Apply legend: convert to patchwork first so & operator works
    if (!isTRUE(input$show_legend)) {
      p <- patchwork::wrap_plots(p) & ggplot2::theme(legend.position = "none")
    }
    p
  })

  # ── Leaflet map — default Germany view with all wells in grey ───────────────
  output$well_map <- leaflet::renderLeaflet({
    # Reproject all wells to WGS84 once at startup
    all_wgs84_init <- meta_df |>
      sf::st_as_sf(coords = c(LON_COL, LAT_COL), crs = COORDS_CRS) |>
      sf::st_transform(crs = 4326) |>
      sf::st_coordinates() |>
      as.data.frame() |>
      dplyr::rename(lon_wgs84 = X, lat_wgs84 = Y)

    leaflet::leaflet() |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addCircleMarkers(
        lng         = all_wgs84_init$lon_wgs84,
        lat         = all_wgs84_init$lat_wgs84,
        group       = "all_wells",
        radius      = 3,
        color       = "#888888",
        fillColor   = "#888888",
        fillOpacity = 0.3,
        weight      = 0,
        opacity     = 0.4
      ) |>
      leaflet::setView(lng = 10.5, lat = 51.2, zoom = 6)
  })

  # ── Update map with wells when data is ready ─────────────────────────────────
  observe({
    req(nearest_meta(), target_loc())
    meta <- nearest_meta()
    loc  <- target_loc()

    # Reproject all wells to WGS84 for the grey background layer
    all_wgs84 <- meta_df |>
      sf::st_as_sf(coords = c(LON_COL, LAT_COL), crs = COORDS_CRS) |>
      sf::st_transform(crs = 4326) |>
      sf::st_coordinates() |>
      as.data.frame() |>
      dplyr::rename(lon_wgs84 = X, lat_wgs84 = Y) |>
      dplyr::mutate(well_id = meta_df[[WELL_ID_COL]])

    # Mark which wells are selected (nearest n)
    selected_ids <- meta[[WELL_ID_COL]]
    all_wgs84$selected <- all_wgs84$well_id %in% selected_ids

    dist_pal <- leaflet::colorNumeric(
      palette = "YlOrRd",
      domain  = meta$dist_km,
      reverse = FALSE
    )

    # ── Cluster colours (if enabled) ─────────────────────────────────────────
    use_cluster <- isTRUE(input$show_cluster)
    clust_map   <- cluster_colours()   # NULL when show_cluster is FALSE

    # Build a plain character vector of hex colours, one per well in meta order.
    # Using a named lookup avoids any join/formula NA issues in leaflet.
    well_colours <- if (use_cluster && !is.null(clust_map) && nrow(clust_map) > 0) {
      colour_lookup <- setNames(clust_map$colour, clust_map[[WELL_ID_COL]])
      cols <- colour_lookup[meta[[WELL_ID_COL]]]
      unmatched <- is.na(cols)
      if (any(unmatched)) cols[unmatched] <- dist_pal(meta$dist_km[unmatched])
      unname(cols)
    } else {
      dist_pal(meta$dist_km)
    }

    # ── Build per-well popup HTML before the pipe chain ──────────────────────
    # sapply returns a plain character vector; leaflet's popup arg requires that.
    popup_html <- sapply(seq_len(nrow(meta)), function(i) {
      paste0(
        "<b>", meta[[WELL_ID_COL]][i], "</b>",
        "<hr style='margin:4px 0'>",
        "<table style='font-size:12px;line-height:1.6'>",
        "<tr><td style='color:#666;padding-right:8px'>Distance</td>",
            "<td><b>", meta$dist_km[i], " km</b></td></tr>",
        "<tr><td style='color:#666'>Depth</td>",
            "<td>", meta$depth[i], " m</td></tr>",
        "<tr><td style='color:#666'>Upper filter</td>",
            "<td>", meta$up_filter[i], " m</td></tr>",
        "<tr><td style='color:#666'>Lower filter</td>",
            "<td>", meta$lo_filter[i], " m</td></tr>",
        "<tr><td style='color:#666'>Screen length</td>",
            "<td>", meta$scr_length[i], " m</td></tr>",
        "<tr><td style='color:#666'>Aquifer medium</td>",
            "<td>", meta$aquifer_med[i], "</td></tr>",
        "<tr><td style='color:#666'>State</td>",
            "<td>", meta$pre_state[i], "</td></tr>",
        "</table>"
      )
    })

    # ── Use leafletProxy to update the existing base map ─────────────────────
    proxy <- leaflet::leafletProxy("well_map") |>
      leaflet::clearGroup("all_wells") |>
      leaflet::clearGroup("wells") |>
      leaflet::clearGroup("lines") |>
      leaflet::clearGroup("target") |>
      leaflet::clearControls() |>

      # ── All wells (grey background) ──────────────────────────────────────────
      leaflet::addCircleMarkers(
        data        = dplyr::filter(all_wgs84, !selected),
        lng         = ~lon_wgs84,
        lat         = ~lat_wgs84,
        group       = "all_wells",
        radius      = 5,
        color       = "#888888",
        fillColor   = "#888888",
        fillOpacity = 0.35,
        weight      = 1,
        opacity     = 0.5,
        label       = ~well_id
      ) |>

      # ── Target location pin ───────────────────────────────────────────────────
      leaflet::addAwesomeMarkers(
        lng   = loc$lon,
        lat   = loc$lat,
        icon  = leaflet::awesomeIcons(
          icon        = "home",
          library     = "fa",
          markerColor = "blue",
          iconColor   = "white"
        ),
        label = htmltools::HTML(paste0("<b>📍 ", confirmed_address(), "</b>")),
        group = "target"
      ) |>

      # ── Selected n wells (coloured) ──────────────────────────────────────────
      leaflet::addCircleMarkers(
        data        = meta,
        lng         = ~lon_wgs84,
        lat         = ~lat_wgs84,
        group       = "wells",
        layerId     = ~get(WELL_ID_COL),
        color       = well_colours,
        fillColor   = well_colours,
        fillOpacity = 0.85,
        radius      = 9,
        weight      = 1.5,
        label       = ~htmltools::HTML(sprintf(
          "<b>%s</b><br/>%.2f km from target",
          get(WELL_ID_COL), dist_km
        )),
        popup = popup_html
      ) |>
      leaflet::addLayersControl(
        overlayGroups = c("all_wells", "wells", "lines", "target"),
        options       = leaflet::layersControlOptions(collapsed = TRUE)
      ) |>
      leaflet::fitBounds(
        lng1 = min(c(meta$lon_wgs84, loc$lon)) - 0.1,
        lat1 = min(c(meta$lat_wgs84, loc$lat)) - 0.1,
        lng2 = max(c(meta$lon_wgs84, loc$lon)) + 0.1,
        lat2 = max(c(meta$lat_wgs84, loc$lat)) + 0.1
      )

    # ── Distance lines ────────────────────────────────────────────────────────
    proxy <- Reduce(
      f = function(map, i) {
        leaflet::addPolylines(
          map,
          lng     = c(loc$lon, meta$lon_wgs84[i]),
          lat     = c(loc$lat, meta$lat_wgs84[i]),
          color   = well_colours[i],
          weight  = 1,
          opacity = 0.4,
          group   = "lines"
        )
      },
      x    = seq_len(nrow(meta)),
      init = proxy
    )

    # Add distance legend only when not in cluster mode
    if (!use_cluster || is.null(clust_map)) {
      proxy |>
        leaflet::addLegend(
          position  = "bottomright",
          pal       = dist_pal,
          values    = meta$dist_km,
          title     = "Distance (km)",
          opacity   = 0.9,
          labFormat = leaflet::labelFormat(suffix = " km")
        )
    }
  })

  # ── Map background click → clear highlight ───────────────────────────────────
  # Fires when the user clicks anywhere on the map that is NOT a marker.
  # This is the "deselect" gesture — double-click also works naturally because
  # it fires two click events, the second of which lands on the background.
  observeEvent(input$well_map_click, {
    highlighted_well(NULL)
    leaflet::leafletProxy("well_map") |>
      leaflet::clearGroup("highlight")
  })

  # ── Map click → highlight bouquet trace ─────────────────────────────────────
  observeEvent(input$well_map_marker_click, {
    click <- input$well_map_marker_click
    req(click$id)  # layerId = well_id set above

    well_id <- click$id
    highlighted_well(well_id)

    # Pulse the clicked marker — guard against clicks on unlabelled grey markers
    meta <- nearest_meta()
    row  <- meta[meta[[WELL_ID_COL]] == well_id, ]
    req(nrow(row) > 0)

    leaflet::leafletProxy("well_map") |>
      leaflet::clearGroup("highlight") |>
      leaflet::addCircleMarkers(
        lng         = row$lon_wgs84,
        lat         = row$lat_wgs84,
        group       = "highlight",
        radius      = 17,
        color       = "#e74c3c",
        fill        = FALSE,
        weight      = 3,
        opacity     = 1,
        options     = leaflet::markerOptions(interactive = FALSE)
      )
  })

}  # /server


shinyApp(ui, server)