
# =============================================================================
#  location_module.R — Shiny module for a single location's UI and server
#  logic. Sourced by app.R.
#
#  Exports:
#    location_ui(id, label)        — sidebar address card
#    location_panels_ui(id)        — bouquet card + map card for main area
#    location_server(id, gw_con, shared, meta_df)
# =============================================================================


# ── UI: sidebar address card ──────────────────────────────────────────────────
location_ui <- function(id, label = "Location") {
  ns <- NS(id)
  bslib::card(
    bslib::card_header(
      bsicons::bs_icon("geo-alt-fill"),
      paste0(" ", label)
    ),
    textInput(
      ns("address"), NULL,
      placeholder = "ZIP code or address…",
      value = ""
    ),
    tags$p(
      style = "font-size:11px; color:#888; margin: -6px 0 6px 0;",
      "German ZIP code, city name, or full address"
    ),
    uiOutput(ns("geocode_btn_ui")),
    uiOutput(ns("geocode_status_ui"))
  )
}


# ── UI: main-area panels (bouquet card + map card) ────────────────────────────
location_panels_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Bouquet plot card
    bslib::card(
      full_screen = TRUE,
      style       = "background-color: #f5f0e8;",
      bslib::card_header(
        style = "background-color: #ede8de;",
        bsicons::bs_icon("flower1"), " Bouquet Plot",
        div(
          style = "float:right; display:flex; gap:6px; align-items:center;",
          uiOutput(ns("reset_btn_ui")),
          downloadButton(
            ns("download_bouquet"), "Download",
            class = "btn btn-sm btn-outline-primary"
          )
        )
      ),
      bslib::card_body(
        padding = 0,
        style   = "background-color: #f5f0e8;",
        uiOutput(ns("bouquet_ui"))
      )
    ),

    # Map card
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        bsicons::bs_icon("map"), " Well Locations"
      ),
      bslib::card_body(
        padding = 0,
        leaflet::leafletOutput(ns("well_map"), height = "100%")
      )
    )
  )
}


# ── Internal helper: build a bouquet ggplot ───────────────────────────────────
# Returns a patchwork / bouquet_plot object.
# df           — data frame (possibly with `cluster` column)
# use_cluster  — logical
# hw           — highlighted well id or NULL
# shared_input — the reactive `shared` list (already resolved via `()`)
.build_bouquet <- function(df, use_cluster, hw, inputs) {
  # Constants passed via inputs
  DATE_COL    <- inputs$date_col
  WELL_ID_COL <- inputs$well_id_col
  VALUE_COL   <- inputs$value_col

  valid_markers <- c("year", "quarter", "month", "week", "day")
  marker_arg <- if (
    is.null(inputs$marker_every) ||
    inputs$marker_every == "off" ||
    !inputs$marker_every %in% valid_markers
  ) NULL else inputs$marker_every

  # Use a tighter ceiling_pct in annual mean mode so the condensed
  # seasonal cycle fills the plot area better (52 steps vs 1000+ steps).
  ceiling_pct_val <- if (isTRUE(inputs$annual_mean)) 0.7 else 0.9

  if (use_cluster) {
    bouquets::make_plot_bouquet(
      df,
      time_col      = !!rlang::sym(DATE_COL),
      series_col    = !!rlang::sym(WELL_ID_COL),
      value_col     = !!rlang::sym(VALUE_COL),
      stem_colors   = cluster,
      flower_colors = cluster,
      highlight     = hw,
      show_labels   = isTRUE(inputs$show_labels),
      label_color   = "#999999",
      show_rings    = isTRUE(inputs$show_rings),
      marker_every  = marker_arg,
      dark_mode     = isTRUE(inputs$dark_mode),
      ceiling_pct   = ceiling_pct_val,
      title         = inputs$plot_title,
      verbose       = FALSE
    )
  } else {
    bouquets::make_plot_bouquet(
      df,
      time_col      = !!rlang::sym(DATE_COL),
      series_col    = !!rlang::sym(WELL_ID_COL),
      value_col     = !!rlang::sym(VALUE_COL),
      stem_colors   = "greens",
      flower_colors = "blossom",
      highlight     = hw,
      show_labels   = isTRUE(inputs$show_labels),
      label_color   = "#999999",
      show_rings    = isTRUE(inputs$show_rings),
      marker_every  = marker_arg,
      dark_mode     = isTRUE(inputs$dark_mode),
      ceiling_pct   = ceiling_pct_val,
      title         = inputs$plot_title,
      verbose       = FALSE
    )
  }
}


# ── Module server — Part A: geocode + nearest wells + time series ─────────────
# id      — module namespace string, e.g. "loc_a"
# gw_con  — open DuckDB connection (per-session, passed from outer server)
# shared  — named list of zero-arg functions returning shared input values:
#   shared$n_wells(), shared$year_range(), shared$resolution(),
#   shared$show_labels(), shared$show_rings(), shared$dark_mode(),
#   shared$show_cluster(), shared$marker_every()
# meta_df — wells metadata data frame (passed from app level)
location_server <- function(id, gw_con, shared, meta_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Local aliases for column/CRS constants (passed via shared) ────────────
    WELL_ID_COL <- shared$well_id_col
    LON_COL     <- shared$lon_col
    LAT_COL     <- shared$lat_col
    DATE_COL    <- shared$date_col
    VALUE_COL   <- shared$value_col
    COORDS_CRS  <- shared$coords_crs

    # ── Bind Enter key for this module's address input ──────────────────────
    observe({
      session$sendCustomMessage(
        "bindEnterKey",
        list(inputId = ns("address"), btnId = ns("geocode_btn"))
      )
    })

    # ── Geocode button UI ─────────────────────────────────────────────────
    output$geocode_btn_ui <- renderUI({
      actionButton(
        ns("geocode_btn"), "Find nearest wells",
        class = "btn-primary w-100",
        icon  = icon("magnifying-glass")
      )
    })

    # ── State ─────────────────────────────────────────────────────────────
    target_loc        <- reactiveVal(NULL)
    confirmed_address <- reactiveVal(NULL)

    # ── Geocode on button click ───────────────────────────────────────────
    observeEvent(input$geocode_btn, {
  query <- trimws(input$address)
  req(nchar(query) > 0)

  # Show spinner — these messages are queued but the browser won't paint
  # them until R yields. shinyjs::delay() defers the blocking geocode call
  # by one JS tick so the browser can render the spinner first.
  shinyjs::html(ns("geocode_btn"), '<span class="spinner-border spinner-border-sm me-2" role="status" aria-hidden="true"></span>Searching\u2026')
  shinyjs::disable(ns("geocode_btn"))

  shinyjs::delay(50, {
    cache_key <- tolower(query)
    geocode_cache <- shared$geocode_cache

    if (exists(cache_key, envir = geocode_cache)) {
      loc <- geocode_cache[[cache_key]]
    } else {
      tryCatch({
        loc <- shared$geocode_address(query)
        if (!is.na(loc$lat) && !is.na(loc$lon)) {
          assign(cache_key, loc, envir = geocode_cache)
        }
      }, error = function(e) {
        showNotification(paste("Geocoding error:", conditionMessage(e)),
                         type = "error", duration = 8)
        loc <<- list(lat = NA, lon = NA)
      })
    }

    if (is.na(loc$lat) || is.na(loc$lon)) {
      showNotification("Address not found. Try a more specific query or a different spelling.",
                       type = "warning", duration = 6)
      target_loc(NULL)
      confirmed_address(NULL)
    } else {
      target_loc(loc)
      confirmed_address(query)
    }

    # Restore button after geocoding completes
    shinyjs::html(ns("geocode_btn"),
      '<i class="fa fa-magnifying-glass" role="presentation" aria-label="magnifying-glass icon"></i> Find nearest wells')
    shinyjs::enable(ns("geocode_btn"))
  })
})

    # ── Nearest wells ─────────────────────────────────────────────────────
    nearest_meta <- reactive({
      req(target_loc())
      shared$find_nearest(
        target_loc()$lon,
        target_loc()$lat,
        meta_df,
        shared$n_wells()
      )
    })

    # ── Time series ───────────────────────────────────────────────────────
    selected_ts <- reactive({
      req(nearest_meta())
      yr          <- shared$year_range()
      am          <- isTRUE(shared$annual_mean())
      internal_ids <- nearest_meta()$well_id
      ts <- shared$query_ts(
        gw_con,
        well_ids    = internal_ids,
        year_from   = if (am) NULL else yr[1],
        year_to     = if (am) NULL else yr[2],
        resolution  = if (am) "week" else shared$resolution(),
        annual_mean = am
      )
      ts |>
        dplyr::left_join(
          nearest_meta() |>
            dplyr::select(dplyr::all_of(c(WELL_ID_COL, LON_COL, LAT_COL, "dist_km"))),
          by = WELL_ID_COL
        )
    })

    # ── Clustered time series ─────────────────────────────────────────────
    clustered_ts <- reactive({
      req(selected_ts())
      df <- selected_ts()
      if (!isTRUE(shared$show_cluster())) return(df)
      bouquets::cluster_bouquet(
        df,
        time_col   = !!rlang::sym(DATE_COL),
        series_col = !!rlang::sym(WELL_ID_COL),
        value_col  = !!rlang::sym(VALUE_COL)
      )
    })

    # ── Per-well cluster colour map ───────────────────────────────────────
    cluster_colours <- reactive({
      if (!isTRUE(shared$show_cluster())) return(NULL)
      req(clustered_ts())
      df <- clustered_ts()
      if (!"cluster" %in% names(df)) return(NULL)

      dark <- isTRUE(shared$dark_mode())
      p <- bouquets::make_plot_bouquet(
        df,
        time_col      = !!rlang::sym(DATE_COL),
        series_col    = !!rlang::sym(WELL_ID_COL),
        value_col     = !!rlang::sym(VALUE_COL),
        stem_colors   = cluster,
        flower_colors = cluster,
        dark_mode     = dark,
        verbose       = FALSE
      )

      pw      <- patchwork::wrap_plots(p)
      gg      <- pw[[1L]]
      built   <- ggplot2::ggplot_build(gg)

      colour_df <- NULL
      for (ld in built$data) {
        if ("colour" %in% names(ld) && nrow(ld) > 0) { colour_df <- ld; break }
      }
      if (is.null(colour_df)) return(NULL)

      well_ids    <- unique(df[[WELL_ID_COL]])
      grp_colours <- colour_df |>
        dplyr::distinct(group, colour) |>
        dplyr::arrange(group)
      n_match <- min(nrow(grp_colours), length(well_ids))
      tibble::tibble(
        !!WELL_ID_COL := well_ids[seq_len(n_match)],
        colour         = grp_colours$colour[seq_len(n_match)]
      )
    })

    # ── Highlight state ───────────────────────────────────────────────────
    highlighted_well <- reactiveVal(NULL)

    observeEvent(input$reset_highlight, {
      highlighted_well(NULL)
    })

    # ── Reset button UI ───────────────────────────────────────────────────
    output$reset_btn_ui <- renderUI({
      req(highlighted_well())
      actionButton(
        ns("reset_highlight"), "Reset highlight",
        class = "btn btn-sm btn-outline-secondary",
        icon  = icon("rotate-left")
      )
    })

    # ── Plot title ────────────────────────────────────────────────────────
    plot_title <- reactive({
      addr <- confirmed_address()
      am   <- isTRUE(shared$annual_mean())
      mode_label <- if (am) "Annual mean dynamics" else "Groundwater level dynamics"
      raw <- if (is.null(addr) || nchar(addr) == 0) {
        sprintf('%s · %d nearest wells', mode_label, shared$n_wells())
      } else {
        sprintf('%s · %d nearest wells to "%s"',
                mode_label, shared$n_wells(), addr)
      }
      stringr::str_wrap(raw, width = 55)
    })

    # ── Geocode status badge ──────────────────────────────────────────────
    output$geocode_status_ui <- renderUI({
      req(target_loc(), nearest_meta())
      loc  <- target_loc()
      meta <- nearest_meta()
      tagList(
        tags$hr(style = "margin: 6px 0"),
        tags$small(
          bsicons::bs_icon("check-circle-fill", class = "text-success"),
          sprintf(
            " %.4f\u00b0N, %.4f\u00b0E \u2014 %d wells loaded (max %.1f km)",
            loc$lat, loc$lon, nrow(meta), max(meta$dist_km)
          )
        )
      )
    })
    # ── Bouquet UI (empty state vs plot) ───────────────────────────────────
    output$bouquet_ui <- renderUI({
      if (is.null(target_loc())) {
        div(
          style = paste(
            "height:100%; display:flex; flex-direction:column;",
            "align-items:center; justify-content:center;",
            "color:#aaa; border: 2px dashed #d8cfc0; border-radius:8px; margin:16px;"
          ),
          tags$span(style = "font-size:3rem;", "\U0001F338"),
          tags$p(style = "margin-top:12px; font-size:14px; text-align:center;",
            "Enter a location in the sidebar", tags$br(),
            "to explore nearby groundwater wells"
          )
        )
      } else {
        tagList(
          div(id = ns("bouquet_spinner"),
            style = paste(
              "position:absolute; top:50%; left:50%; transform:translate(-50%,-50%);",
              "z-index:10; display:none;"
            ),
            tags$div(class = "spinner-border text-secondary", role = "status")
          ),
          plotOutput(ns("bouquet_static"), height = "100%",
                     click = ns("bouquet_static_click"))
        )
      }
    })

    # ── Static bouquet plot ──────────────────────────────────────────────
    output$bouquet_static <- renderPlot({
      req(clustered_ts())
      bg_col <- if (isTRUE(shared$dark_mode())) "#1a1a2e" else "white"
      par(bg = bg_col)

      df          <- clustered_ts()
      use_cluster <- isTRUE(shared$show_cluster())
      hw          <- highlighted_well()

      inputs <- list(
        marker_every  = shared$marker_every(),
        show_labels   = shared$show_labels(),
        show_rings    = shared$show_rings(),
        dark_mode     = shared$dark_mode(),
        annual_mean   = shared$annual_mean(),
        plot_title    = plot_title(),
        well_id_col   = WELL_ID_COL,
        date_col      = DATE_COL,
        value_col     = VALUE_COL
      )

      p <- .build_bouquet(df, use_cluster, hw, inputs)

      if (!isTRUE(shared$show_legend())) {
        p <- patchwork::wrap_plots(p) & ggplot2::theme(legend.position = "none")
      }
      p
    })

    # ── Download handler ─────────────────────────────────────────────────
    output$download_bouquet <- downloadHandler(
      filename = function() {
        am <- isTRUE(shared$annual_mean())
        if (am) {
          sprintf("bouquet_%s_%dwells_annual_mean.png",
                  gsub("[^a-zA-Z0-9]", "_", trimws(input$address)),
                  shared$n_wells())
        } else {
          sprintf("bouquet_%s_%dwells_%s_%d-%d.png",
                  gsub("[^a-zA-Z0-9]", "_", trimws(input$address)),
                  shared$n_wells(),
                  shared$resolution(),
                  shared$year_range()[1],
                  shared$year_range()[2])
        }
      },
      content = function(file) {
        req(selected_ts())
        df  <- selected_ts()
        hw  <- highlighted_well()
        use_cluster <- isTRUE(shared$show_cluster())

        if (use_cluster) {
          df <- bouquets::cluster_bouquet(
            df,
            time_col   = !!rlang::sym(DATE_COL),
            series_col = !!rlang::sym(WELL_ID_COL),
            value_col  = !!rlang::sym(VALUE_COL)
          )
        }

        inputs <- list(
          marker_every  = shared$marker_every(),
          show_labels   = shared$show_labels(),
          show_rings    = shared$show_rings(),
          dark_mode     = shared$dark_mode(),
          annual_mean   = shared$annual_mean(),
          plot_title    = plot_title(),
          well_id_col   = WELL_ID_COL,
          date_col      = DATE_COL,
          value_col     = VALUE_COL
        )

        p      <- .build_bouquet(df, use_cluster, hw, inputs)
        bg_col <- if (isTRUE(shared$dark_mode())) "#1a1a2e" else "#f8f8f5"
        gg <- patchwork::wrap_plots(p)[[1L]] +
          ggplot2::theme(
            plot.background  = ggplot2::element_rect(fill = bg_col, colour = NA),
            panel.background = ggplot2::element_rect(fill = bg_col, colour = NA)
          )
        ggplot2::ggsave(file, plot = gg, width = 12, height = 10,
                        dpi = 300, bg = bg_col)
      }
    )

    # ── Leaflet map ──────────────────────────────────────────────────────
    output$well_map <- leaflet::renderLeaflet({
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

    # ── Update map when data is ready ─────────────────────────────────────
    observe({
      req(nearest_meta(), target_loc())
      meta <- nearest_meta()
      loc  <- target_loc()

      all_wgs84 <- meta_df |>
        sf::st_as_sf(coords = c(LON_COL, LAT_COL), crs = COORDS_CRS) |>
        sf::st_transform(crs = 4326) |>
        sf::st_coordinates() |>
        as.data.frame() |>
        dplyr::rename(lon_wgs84 = X, lat_wgs84 = Y) |>
        dplyr::mutate(well_id = meta_df[[WELL_ID_COL]])

      selected_ids <- meta[[WELL_ID_COL]]
      all_wgs84$selected <- all_wgs84$well_id %in% selected_ids

      dist_pal <- leaflet::colorNumeric(
        palette = "YlOrRd",
        domain  = meta$dist_km,
        reverse = FALSE
      )

      use_cluster <- isTRUE(shared$show_cluster())
      clust_map   <- cluster_colours()

      well_colours <- if (use_cluster && !is.null(clust_map) && nrow(clust_map) > 0) {
        colour_lookup <- setNames(clust_map$colour, clust_map[[WELL_ID_COL]])
        cols <- colour_lookup[meta[[WELL_ID_COL]]]
        unmatched <- is.na(cols)
        if (any(unmatched)) cols[unmatched] <- dist_pal(meta$dist_km[unmatched])
        unname(cols)
      } else {
        dist_pal(meta$dist_km)
      }

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

      map_id <- ns("well_map")
      proxy <- leaflet::leafletProxy(map_id, session) |>
        leaflet::clearGroup("all_wells") |>
        leaflet::clearGroup("wells") |>
        leaflet::clearGroup("lines") |>
        leaflet::clearGroup("target") |>
        leaflet::clearControls() |>
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
          opacity     = 0.5
        ) |>
        leaflet::addAwesomeMarkers(
          lng   = loc$lon,
          lat   = loc$lat,
          icon  = leaflet::awesomeIcons(
            icon        = "home",
            library     = "fa",
            markerColor = "blue",
            iconColor   = "white"
          ),
          label = htmltools::HTML(paste0("<b>\U0001F4CD ", confirmed_address(), "</b>")),
          group = "target"
        ) |>
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
          label        = if (isTRUE(shared$show_labels())) ~as.character(get(WELL_ID_COL)) else NULL,
          labelOptions = leaflet::labelOptions(
            permanent  = TRUE,
            direction  = "right",
            offset     = c(10, 0),
            textOnly   = TRUE,
            style      = list(
              "font-size"   = "10px",
              "font-weight" = "600",
              "color"       = "#999999",
              "text-shadow" = "0 0 3px #ffffff, 0 0 3px #ffffff"
            )
          ),
          popup = popup_html
        ) |>
        leaflet::addLayersControl(
          overlayGroups = c("all_wells", "wells", "lines", "target"),
          options       = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex = TRUE)
        ) |>
        htmlwidgets::onRender("
          function(el, x) {
            var toggle = el.querySelector('.leaflet-control-layers-toggle');
            if (toggle) toggle.removeAttribute('title');
          }
        ") |>
        leaflet::fitBounds(
          lng1 = min(c(meta$lon_wgs84, loc$lon)) - 0.1,
          lat1 = min(c(meta$lat_wgs84, loc$lat)) - 0.1,
          lng2 = max(c(meta$lon_wgs84, loc$lon)) + 0.1,
          lat2 = max(c(meta$lat_wgs84, loc$lat)) + 0.1
        )

      # Distance lines
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

    # ── Map background click → clear highlight ────────────────────────────
    observeEvent(input$well_map_click, {
      highlighted_well(NULL)
      leaflet::leafletProxy(ns("well_map"), session) |>
        leaflet::clearGroup("highlight")
    })

    # ── Map marker click → highlight bouquet trace ────────────────────────
    observeEvent(input$well_map_marker_click, {
      click <- input$well_map_marker_click
      req(click$id)

      well_id <- click$id
      highlighted_well(well_id)

      meta <- nearest_meta()
      row  <- meta[meta[[WELL_ID_COL]] == well_id, ]
      req(nrow(row) > 0)

      leaflet::leafletProxy(ns("well_map"), session) |>
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

  }) # /moduleServer
} # /location_server
