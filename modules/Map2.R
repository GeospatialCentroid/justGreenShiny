# Census Tract Map Module UI
tractMapUI <- function(id) {
  ns <- NS(id)

  card(
    id = "tract_map_card",
    full_screen = TRUE,
    card_header("City Review View"),
    leafletOutput(ns("tract_map"), height = "85vh"),
    tags$div(
      class = "footer-banner",
      tags$img(src = "rojosLogo.png", height = "80px"),
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
tractMapServer <- function(id, selected_city, cityGPKG, tractsDF, tract_metric, active_tab) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Load tract data
    tract_data <- reactive({
      req(selected_city(), selected_city() != "")
      req(active_tab() == "City Review")
      
      # Ideally, move readRDS outside the reactive if the file is large
      # to prevent re-reading it on every city change.
      allTracts <- readRDS("data/tractsGPKG.rds") 
      # test set 
      
      city_info <- cityGPKG[cityGPKG$fullCity == "Milwaukee, Wisconsin", ]
      city_info <- cityGPKG[cityGPKG$fullCity == selected_city(), ]
      geoid  <- city_info$GEOID
      # this is the spatial data object 
      tracts <- allTracts[[geoid]]
      
      # pull in the health data for the specific city 
      ct_health <- tractsDF |>
        dplyr::filter(GEOID %in% tracts$GEOID) |>
        dplyr::select("GEOID","meanNDVI")
      # join this to the tracts spatial object 
      tracts <- tracts |>
        dplyr::left_join(y = ct_health, by = "GEOID")
      
      return(tracts)
    })
    
    # 2. Initialize map (Base layers only)
    output$tract_map <- renderLeaflet({
      leaflet() |>
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") |>
        addTiles(group = "OpenStreetMap") |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") |>
        # Set a generic view initially, fitBounds will handle the rest later
        setView(lng = -98.57, lat = 39.82, zoom = 4) |> 
        addLayersControl(
          baseGroups = c("CartoDB", "OpenStreetMap", "Esri Imagery"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })
    
    # 3. Update map (Proxy)
    observe({
      req(active_tab() == "City Review")
      req(tract_data())
      req(tract_metric()) 
      
      tract_sf <- tract_data()
      bounds <- sf::st_bbox(tract_sf)
      
      # REMOVED: pal1 definition. We will define the string inside the switch instead.
      
      metric_config <- switch(
        tract_metric(),
        "Current Vegetation Levels" = list(
          palette = "BuGn",          # CHANGED: Pass the string name, not the function
          col = "meanNDVI",
          title = "Greenness level (NDVI)"
        ),
        "Social Vulnerability (RPL)" = list(
          col = "RPL_THEMES",
          palette = "OrRd",
          title = "RPL Themes",
          popup_label = "RPL",
          domain = c(0, 1)
        )
      )
      
      # Check if column exists in the data
      if (!is.null(metric_config) && metric_config$col %in% names(tract_sf)) {
        
        # Create color palette dynamically here
        pal <- colorNumeric(
          palette = metric_config$palette, # Now this will always find a string ("BuGn" or "OrRd")
          domain = if(is.null(metric_config$domain)) tract_sf[[metric_config$col]] else metric_config$domain,
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
            # FIX: Added check for popup_label to prevent error if it's missing in "Current Vegetation Levels"
            popup = ~ paste(
              "<b>Census Tract:</b>", GEOID, "<br>",
              paste0("<b>", ifelse(!is.null(metric_config$popup_label), metric_config$popup_label, metric_config$title), ":</b> "),
              round(tract_sf[[metric_config$col]], 3)
            )
          ) |>
          addLegend(
            pal = pal,
            values = if(is.null(metric_config$domain)) tract_sf[[metric_config$col]] else metric_config$domain,
            title = metric_config$title,
            position = "bottomright"
          ) |>
          fitBounds(
            lng1 = bounds[["xmin"]], lat1 = bounds[["ymin"]],
            lng2 = bounds[["xmax"]], lat2 = bounds[["ymax"]]
          )
      } else {
        # Fallback if metric not found
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
            lng1 = bounds[["xmin"]], lat1 = bounds[["ymin"]],
            lng2 = bounds[["xmax"]], lat2 = bounds[["ymax"]]
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