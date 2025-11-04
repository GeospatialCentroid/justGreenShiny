# Census Tract Map Module UI
tractMapUI <- function(id) {
  ns <- NS(id)

  card(
    id = "tract_map_card",
    full_screen = TRUE,
    card_header("Census Tract Detail View"),
    leafletOutput(ns("tract_map"), height = "85vh"),
    tags$div(
      class = "footer-banner",
      tags$img(src = "CSU-Symbol-r-K.png", height = "80px"),
      tags$span(
        "Rojos Lab - Geospatial Centroid",
        tags$br(),
        "Colorado State University © 2025"
      ),
      tags$img(
        src = "centroid_white_gray_logo_CROPPED.png",
        height = "60px"
      )
    )
  )
}

# Census Tract Map Module Server
tractMapServer <- function(id, selected_city, cityDF, tract_metric) {
  moduleServer(id, function(input, output, session) {
    # Reactive to load tract data when city is selected
    tract_data <- reactive({
      req(selected_city(), selected_city() != "")

      # Get city info to construct file names
      city_info <- cityDF[cityDF$fullCity == selected_city(), ]

      req(nrow(city_info) > 0)

      # Construct file names based on city and state
      city_name <- city_info$city

      # File paths
      gpkg_file <- file.path(
        "data",
        "ctFiles",
        paste0(city_name, "_ct.gpkg")
      )
      csv_file <- file.path(
        "data",
        "ctFiles",
        paste0(city_name, "_ct.csv")
      )

      # Check if files exist
      if (!file.exists(gpkg_file) || !file.exists(csv_file)) {
        showNotification(
          paste("Data files not found for", selected_city()),
          type = "warning"
        )
        return(NULL)
      }

      # Load the files
      tryCatch(
        {
          tract_gpkg <- sf::st_read(gpkg_file, quiet = TRUE)
          tract_csv <- read.csv(csv_file) |>
            dplyr::select(
              GEOID,
              meanNDVI,
              popOver20_2023,
              popOver35_2023,
              popOver55_2023,
              ls_Stroke,
              ls_Mortality,
              ls_Dementia,
              RPL_THEMES
            ) |>
            dplyr::mutate(
              GEOID = as.character(GEOID)
            )

          # Join the data
          joined_data <- tract_gpkg |>
            dplyr::left_join(tract_csv, by = "GEOID")

          return(joined_data)
        },
        error = function(e) {
          showNotification(
            paste("Error loading data:", e$message),
            type = "error"
          )
          return(NULL)
        }
      )
    })

    # Initialize map
    output$tract_map <- renderLeaflet({
      leaflet() |>
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") |>
        addTiles(group = "OpenStreetMap") |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") |>
        setView(lng = -99.9018, lat = 39.3812, zoom = 5) |>
        addLayersControl(
          baseGroups = c("CartoDB", "OpenStreetMap", "Esri Imagery"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    # Update map based on selected metric
    observeEvent(list(tract_data(), tract_metric()), {
      req(tract_data(), tract_metric())

      tract_sf <- tract_data()
      bounds <- sf::st_bbox(tract_sf)

      # Configure palette and column based on selected metric
      metric_config <- switch(
        tract_metric(),
        "Greenness (NDVI)" = list(
          col = "meanNDVI",
          palette = "BuGn",
          title = "NDVI",
          popup_label = "NDVI",
          domain = NULL # Use data range
        ),
        "Stroke Cases Prevented" = list(
          col = "ls_Stroke",
          palette = "BuPu",
          title = "Stroke Cases Prevented",
          popup_label = "Stroke Cases",
          domain = NULL # Use data range
        ),
        "Social Vulnerability (RPL)" = list(
          col = "RPL_THEMES",
          palette = "OrRd",
          title = "RPL Themes",
          popup_label = "RPL",
          domain = c(0, 1)
        )
      )

      # Check if column exists
      if (metric_config$col %in% names(tract_sf)) {
        # Create color palette
        pal <- colorNumeric(
          palette = metric_config$palette,
          domain = tract_sf[[metric_config$col]],
          na.color = "transparent"
        )

        leafletProxy("tract_map") |>
          clearShapes() |>
          clearControls() |>
          addPolygons(
            data = tract_sf,
            fillColor = ~ pal(tract_sf[[metric_config$col]]),
            fillOpacity = 0.7,
            color = "#444444",
            weight = 1,
            layerId = ~GEOID,
            highlightOptions = highlightOptions(
              weight = 3,
              color = "#666",
              fillOpacity = 0.9,
              bringToFront = TRUE
            ),
            label = ~ paste("Tract:", GEOID),
            popup = ~ paste(
              "<b>Census Tract:</b>",
              GEOID,
              "<br>",
              paste0("<b>", metric_config$popup_label, ":</b> "),
              round(tract_sf[[metric_config$col]], 3)
            )
          ) |>
          addLegend(
            pal = pal,
            values = tract_sf[[metric_config$col]],
            title = metric_config$title,
            position = "bottomright"
          ) |>
          fitBounds(
            lng1 = bounds[["xmin"]],
            lat1 = bounds[["ymin"]],
            lng2 = bounds[["xmax"]],
            lat2 = bounds[["ymax"]]
          )
      } else {
        # If column doesn't exist, display basic tracts
        leafletProxy("tract_map") |>
          clearShapes() |>
          clearControls() |>
          addPolygons(
            data = tract_sf,
            fillColor = "#3388ff",
            fillOpacity = 0.5,
            color = "#444444",
            weight = 1,
            layerId = ~GEOID,
            label = ~ paste("Tract:", GEOID)
          ) |>
          fitBounds(
            lng1 = bounds[["xmin"]],
            lat1 = bounds[["ymin"]],
            lng2 = bounds[["xmax"]],
            lat2 = bounds[["ymax"]]
          )
      }
    })

    # Return clicked tract ID
    clicked_tract <- reactive({
      input$tract_map_shape_click$id
    })

    return(list(
      clicked_tract = clicked_tract,
      tract_data = tract_data
    ))
  })
}
