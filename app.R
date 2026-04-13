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
library(promises)

source("location_module.R")

# ── Column name constants ─────────────────────────────────────────────────────
WELL_ID_COL <- "proj_id"
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
# resolution:    "week" | "month" | "quarter" | "year"  (used as DATE_TRUNC unit)
# year_from / year_to: integer years, inclusive
# annual_mean:   logical — if TRUE, ignore year_from/year_to and return one
#                row per ISO-week number per well, averaged over all years.
#                Dates are anchored to a fixed reference year (2000) so that
#                the bouquet receives proper Date objects at weekly spacing.
query_timeseries <- function(con, well_ids,
                             year_from   = NULL,
                             year_to     = NULL,
                             resolution  = "week",
                             annual_mean = FALSE) {

  ids_sql <- paste0("'", well_ids, "'", collapse = ", ")

  if (annual_mean) {
    # Average GWL per ISO week number across ALL years in the database.
    # We reconstruct a real Date by using ISO year 2000 as the anchor:
    #   DATE '2000-01-03' is the Monday of ISO week 1 of 2000, so adding
    #   (iso_week - 1) * 7 days gives the Monday of that week in year 2000.
    # This produces exactly 52 (or 53) rows per well at weekly spacing.
    query <- sprintf(
      "SELECT m.proj_id AS proj_id,
              CAST('2000-01-03'::DATE + INTERVAL (ISODOW(MIN(t.date)) - 1 + (EXTRACT(WEEK FROM t.date)::INTEGER - 1) * 7) DAY AS DATE) AS date,
              AVG(t.gwl) AS gwl
         FROM timeseries t
         JOIN metadata   m ON m.well_id = t.well_id
        WHERE t.well_id IN (%s)
        GROUP BY m.proj_id, EXTRACT(WEEK FROM t.date)::INTEGER
        ORDER BY m.proj_id, EXTRACT(WEEK FROM t.date)::INTEGER",
      ids_sql
    )
    return(DBI::dbGetQuery(con, query))
  }

  # ── Standard (non-annual-mean) path ──────────────────────────────────────
  # Date filter clause
  date_filter <- ""
  if (!is.null(year_from))
    date_filter <- paste0(date_filter,
      sprintf(" AND t.date >= '%d-01-01'::DATE", as.integer(year_from)))
  if (!is.null(year_to))
    date_filter <- paste0(date_filter,
      sprintf(" AND t.date <= '%d-12-31'::DATE", as.integer(year_to)))

  # Join metadata to swap the internal well_id for proj_id in the output.
  # For weekly resolution keep raw rows; for coarser resolutions aggregate
  # via DATE_TRUNC so we get one representative row per period per well.
  # AVG(gwl) is the summary statistic — change to FIRST/LAST if preferred.
  if (resolution == "week") {
    query <- sprintf(
      "SELECT m.proj_id AS proj_id, t.date::DATE AS date, t.gwl
         FROM timeseries t
         JOIN metadata   m ON m.well_id = t.well_id
        WHERE t.well_id IN (%s)%s
        ORDER BY m.proj_id, t.date",
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
      "SELECT m.proj_id AS proj_id,
              DATE_TRUNC('%s', t.date)::DATE AS date,
              AVG(t.gwl) AS gwl
         FROM timeseries t
         JOIN metadata   m ON m.well_id = t.well_id
        WHERE t.well_id IN (%s)%s
        GROUP BY m.proj_id, DATE_TRUNC('%s', t.date)
        ORDER BY m.proj_id, DATE_TRUNC('%s', t.date)",
      trunc_unit, ids_sql, date_filter, trunc_unit, trunc_unit
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
    tags$style(HTML("
      /* Full-height layout for the bslib grid rows */
      #loc_a_panels_row, #loc_b_panels_row { width: 100%; }
      #loc_a_panels_row .bslib-grid,
      #loc_b_panels_row .bslib-grid {
        height: 100%;
      }
      /* Compact the accordion that lives inside the sidebar */
      .bslib-sidebar-layout .accordion {
        margin-top: 4px;
      }
      .bslib-sidebar-layout .accordion-button {
        padding-top: 7px;
        padding-bottom: 7px;
        font-size: 0.875rem;
        font-weight: 600;
      }
      .bslib-sidebar-layout .accordion-body {
        padding-top: 8px;
        padding-bottom: 4px;
      }
    ")),
    # Intro modal — shown on first load
    tags$script(HTML("
      $(document).ready(function() {
        // Show intro on load
        setTimeout(function() {
          Shiny.setInputValue('show_intro', 1, {priority: 'event'});
        }, 300);

        // Dynamic Enter-key binding: called once per module instance
        Shiny.addCustomMessageHandler('bindEnterKey', function(msg) {
          $(document).off('keydown.enterKey_' + msg.inputId);
          $(document).on('keydown.enterKey_' + msg.inputId, '#' + msg.inputId, function(e) {
            if (e.key === 'Enter') {
              e.preventDefault();
              var btn = document.getElementById(msg.btnId);
              if (btn && !btn.disabled) { btn.click(); }
            }
          });
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

        # — Location A input —
        location_ui("loc_a", "Location A"),

        # — Location B input (hidden until compare toggled) —
        shinyjs::hidden(
          div(id = "loc_b_card",
            location_ui("loc_b", "Location B")
          )
        ),

        # — Compare toggle buttons —
        div(
          id    = "add_compare_btn_wrap",
          style = "margin-bottom:6px;",
          actionButton(
            "add_compare", "+  Compare with another location",
            class = "btn btn-sm btn-success w-100"
          )
        ),
        shinyjs::hidden(
          div(
            id    = "remove_compare_btn_wrap",
            style = "margin-bottom:6px;",
            actionButton(
              "remove_compare", "\u2715 Remove comparison",
              class = "btn btn-sm btn-outline-danger w-100"
            )
          )
        ),

        # — Settings & Plot Options (collapsible) —
        bslib::accordion(
          id            = "sidebar_accordion",
          open          = FALSE,   # all panels collapsed by default
          multiple      = TRUE,

          # ── Settings panel ──────────────────────────────────────────────
          bslib::accordion_panel(
            title = tagList(bsicons::bs_icon("sliders"), " Settings"),
            value = "settings_panel",

            sliderInput(
              "n_wells", "Number of nearest wells",
              min = 2, max = 50, value = 15, step = 1
            ),

            # ── Mode selector ──────────────────────────────────────────────
            tags$div(
              style = "margin-bottom: 10px;",
              tags$label(
                "Visualisation mode",
                class = "control-label",
                style = "font-weight:600; font-size:0.875rem; display:block; margin-bottom:6px;"
              ),
              shinyWidgets::radioGroupButtons(
                inputId   = "annual_mean",
                label     = NULL,
                choices   = c(
                  `<span title='Show the full recorded time series for each well'>&#x1F4C8; Time series</span>` = "timeseries",
                  `<span title='Average each calendar week across all years to show the typical annual cycle'>&#x1F4C5; Annual cycle</span>` = "annual_mean"
                ),
                selected  = "timeseries",
                justified = TRUE,
                size      = "sm",
                status    = "primary"
              ),
              # Description box — updates based on selection
              uiOutput("mode_description_ui")
            ),

            uiOutput("year_range_ui"),
            uiOutput("resolution_ui")
          ),

          # ── Plot options panel ───────────────────────────────────────────
          bslib::accordion_panel(
            title = tagList(bsicons::bs_icon("palette"), " Plot options"),
            value = "plot_options_panel",

            uiOutput("marker_every_ui"),
            checkboxInput("show_labels",  "Show well labels",  value = FALSE),
            checkboxInput("show_rings",   "Show rings",        value = FALSE),
            checkboxInput("dark_mode",    "Dark mode",         value = FALSE),
            checkboxInput("show_cluster", "Colour by cluster", value = FALSE)
          )
        )
      ),  # /sidebar

      # — Main content area: both rows always in DOM; row B hidden via CSS —
      div(
        id    = "loc_a_panels_row",
        style = "height: calc(100vh - 140px);",
        do.call(bslib::layout_columns,
          c(list(col_widths = c(7, 5), row_heights = "calc(100vh - 140px)"),
            location_panels_ui("loc_a")))
      ),
      shinyjs::hidden(
        div(
          id = "loc_b_panels_row",
          do.call(bslib::layout_columns,
            c(list(col_widths = c(7, 5), row_heights = "calc(50vh - 90px)"),
              location_panels_ui("loc_b")))
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

  # ── Per-session DuckDB connection ────────────────────────────────────────────
  gw_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = DB_PATH, read_only = TRUE)

  # ── Compare toggle state ─────────────────────────────────────────────────────
  observeEvent(input$add_compare, {
    shinyjs::show("loc_b_card")
    shinyjs::show("remove_compare_btn_wrap")
    shinyjs::hide("add_compare_btn_wrap")
    shinyjs::show("loc_b_panels_row")
    shinyjs::runjs("
      var h = 'calc(50vh - 90px)';
      var a = document.getElementById('loc_a_panels_row');
      if (a) { a.style.height = h; a.querySelector('.bslib-grid').style.setProperty('--bslib-grid--row-heights', h); }
    ")
  })

  observeEvent(input$remove_compare, {
    shinyjs::hide("loc_b_card")
    shinyjs::hide("remove_compare_btn_wrap")
    shinyjs::show("add_compare_btn_wrap")
    shinyjs::hide("loc_b_panels_row")
    shinyjs::runjs("
      var h = 'calc(100vh - 140px)';
      var a = document.getElementById('loc_a_panels_row');
      if (a) { a.style.height = h; a.querySelector('.bslib-grid').style.setProperty('--bslib-grid--row-heights', h); }
    ")
  })

  # ── Shared inputs passed to both module instances ────────────────────────────
  shared <- list(
    n_wells      = reactive(input$n_wells),
    year_range   = reactive(input$year_range),
    resolution   = reactive(input$resolution),
    annual_mean  = reactive(identical(input$annual_mean, "annual_mean")),
    show_labels  = reactive(input$show_labels),
    show_rings   = reactive(input$show_rings),
    dark_mode    = reactive(input$dark_mode),
    show_cluster = reactive(input$show_cluster),
    marker_every = reactive(input$marker_every),
    show_legend  = reactive(FALSE),  # legend hidden by default
    # Column name / CRS constants — plain values, not reactives
    well_id_col  = WELL_ID_COL,
    lon_col      = LON_COL,
    lat_col      = LAT_COL,
    date_col     = DATE_COL,
    value_col    = VALUE_COL,
    coords_crs    = COORDS_CRS,
    geocode_cache    = .geocode_cache,
    geocode_address  = geocode_address,
    find_nearest     = find_nearest_wells,
    query_ts         = query_timeseries
  )

  # ── Instantiate location modules ─────────────────────────────────────────────
  location_server("loc_a", gw_con, shared, meta_df)
  location_server("loc_b", gw_con, shared, meta_df)

  # ── Mode description box ────────────────────────────────────────────────────
  output$mode_description_ui <- renderUI({
    am <- identical(input$annual_mean, "annual_mean")
    if (am) {
      tags$div(
        style = paste(
          "font-size:11px; color:#555; background:#eef4fb;",
          "border-left:3px solid #2C7BB6; padding:6px 8px;",
          "border-radius:3px; margin-top:6px;"
        ),
        bsicons::bs_icon("calendar-week"),
        tags$b(" Annual cycle mode: "),
        "Each ISO week is averaged across all years in the database,",
        " revealing the typical intra-year pattern independent of long-term trends.",
        tags$br(),
        tags$span(
          style = "color:#888;",
          bsicons::bs_icon("lock-fill"),
          " Time period and resolution controls are disabled."
        )
      )
    } else {
      tags$div(
        style = paste(
          "font-size:11px; color:#555; background:#f0faf2;",
          "border-left:3px solid #27ae60; padding:6px 8px;",
          "border-radius:3px; margin-top:6px;"
        ),
        bsicons::bs_icon("graph-up"),
        tags$b(" Time series mode: "),
        "Shows the full recorded time series for each well within the selected period.",
        " Use the sliders below to adjust the time window and temporal resolution."
      )
    }
  })

  # ── Year-range UI: disabled (greyed) when annual_mean is active ─────────────
  output$year_range_ui <- renderUI({
    am <- identical(input$annual_mean, "annual_mean")
    tagList(
      tags$div(
        style = if (am) "opacity:0.4; pointer-events:none;" else "",
        sliderInput(
          "year_range", "Time period",
          min   = DB_YEAR_MIN, max = DB_YEAR_MAX,
          value = c(DB_YEAR_MIN, DB_YEAR_MAX),
          step  = 1, sep = ""
        )
      ),
      if (am) tags$p(
        style = "font-size:11px; color:#888; margin:-10px 0 4px 0;",
        bsicons::bs_icon("lock-fill"), " Disabled in annual mean mode."
      )
    )
  })

  # ── Resolution UI: locked to "week" when annual_mean is active ───────────────
  output$resolution_ui <- renderUI({
    am <- identical(input$annual_mean, "annual_mean")
    tagList(
      tags$div(
        style = if (am) "opacity:0.4; pointer-events:none;" else "",
        shinyWidgets::sliderTextInput(
          "resolution", "Temporal resolution",
          choices  = c("week", "month", "quarter", "year"),
          selected = if (am) "week" else isolate({
            cur <- input$resolution
            if (!is.null(cur)) cur else "week"
          }),
          grid     = TRUE
        )
      ),
      if (am) tags$p(
        style = "font-size:11px; color:#888; margin:-10px 0 4px 0;",
        bsicons::bs_icon("lock-fill"), " Locked to week in annual mean mode."
      )
    )
  })

  # ── Render marker_every UI: slider or info text depending on resolution ───────
  output$marker_every_ui <- renderUI({
    am <- identical(input$annual_mean, "annual_mean")
    if (am) {
      tagList(
        tags$label("Time step markers",
                   class = "control-label",
                   style = "font-weight:600; font-size:0.875rem;"),
        tags$p(
          style = "font-size:11px; color:#888; margin:2px 0 6px 0;",
          bsicons::bs_icon("lock-fill"),
          " Not available in annual mean mode."
        )
      )
    } else {
      RESOLUTION_ORDER <- c("year", "quarter", "month", "week", "day")
      res_idx <- match(input$resolution, RESOLUTION_ORDER)
      valid   <- if (length(res_idx) == 1L && !is.na(res_idx) && res_idx > 1L)
                   RESOLUTION_ORDER[seq_len(res_idx - 1L)]
                 else character(0)

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
    }
  })

  onStop(function() {
    DBI::dbDisconnect(gw_con, shutdown = TRUE)
  })


}  # /server

shinyApp(ui, server)

