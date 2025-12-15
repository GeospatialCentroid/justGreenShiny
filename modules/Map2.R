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
    
    # Define consistent names for layers
    data_layer_group <- "Tract Metrics"
    boundary_group <- "City Boundary" # New Group Name
    legend_id <- "map_legend"
    
    # 3. NEW: Update City Boundary (Runs only when city changes)
    city_boundary <-  reactive({
      req(selected_city())
      req(cityGPKG)
      
      # Filter for the specific city boundary
      # assuming cityGPKG is your spatial object containing city polygons
      cityGPKG[cityGPKG$fullCity == selected_city(), ]
    })
    
    # 1. Load tract data
    tract_data <- reactive({
      req(selected_city(), selected_city() != "")
      req(active_tab() == "City Review")
      
      allTracts <- readRDS("data/tractsGPKG.rds") 
      
      city_info <- cityGPKG[cityGPKG$fullCity == selected_city(), ]
      geoid  <- city_info$GEOID
      tracts <- allTracts[[geoid]]
      
      ct_health <- tractsDF |>
        dplyr::filter(GEOID %in% tracts$GEOID)
      tracts <- tracts |>
        dplyr::left_join(y = ct_health, by = "GEOID")
      
      return(tracts)
    })
    
    # 2. Initialize map
    output$tract_map <- renderLeaflet({
      leaflet() |>
        # Create a custom pane with high zIndex so the boundary is always on top
        addMapPane("borders", zIndex = 410) |> 
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") |>
        addTiles(group = "OpenStreetMap") |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") |>
        setView(lng = -98.57, lat = 39.82, zoom = 4) |> 
        addLayersControl(
          baseGroups = c("CartoDB", "OpenStreetMap", "Esri Imagery"),
          # Add the new boundary group to the list
          overlayGroups = c(data_layer_group, boundary_group), 
          options = layersControlOptions(collapsed = FALSE)
        )|>
        addPolygons(
          data = city_boundary(),
          group = boundary_group,
          fill = FALSE,         # No fill
          color = "#52525295",      # Dark outline
          weight = 3,           # Slightly thicker than tracts
          opacity = 1,
          options = pathOptions(pane = "borders", clickable = FALSE) # Use custom pane & disable clicks
        )
    })
    
 
    
    # 4. Update Tracts (Proxy)
    observe({
      req(active_tab() == "City Review")
      req(tract_data())
      req(tract_metric()) 
      
      tract_sf <- tract_data()
      bounds <- sf::st_bbox(tract_sf)
      
      metric_config <- switch(
        tract_metric(),
        "Current Vegetation Levels" = list(
          palette = "BuGn",          
          col = "meanNDVI",
          title = "Greenness level (NDVI)"
        ),
        "Lives Saved" = list(
          palette = "PuBuGn",
          col = "ls_Mortality_Rate",
          title = "Lives Save per 100,000"
        ),
        "Stroke Cases Prevented" = list(
          palette = "BuPu",
          col = "ls_Stroke_Rate",
          title = "Stroke Cases Prevented per 100,000"
        ),
        "Dementia Cases Prevented" = list(
          palette = "OrRd",
          col = "ls_Dementia_Rate",
          title = "Dementia Cases Prevented per 100,000"
        ),
        "Social Vulnerability (RPL)" = list(
          col = "RPL_THEMES",
          palette = "YlGnBu",
          title = "RPL Themes",
          popup_label = "RPL",
          domain = c(0, 1)
        )
      )
      
      if (!is.null(metric_config) && metric_config$col %in% names(tract_sf)) {
        
        pal <- colorNumeric(
          palette = metric_config$palette,
          domain = if(is.null(metric_config$domain)) tract_sf[[metric_config$col]] else metric_config$domain,
          na.color = "transparent"
        )
        
        leafletProxy("tract_map") |>
          clearGroup(data_layer_group) |>
          removeControl(layerId = legend_id) |> 
          addPolygons(
            data = tract_sf,
            group = data_layer_group, 
            fillColor = ~ pal(tract_sf[[metric_config$col]]),
            fillOpacity = 0.7,
            color = "#444444",
            weight = 1,
            layerId = ~GEOID,
            highlightOptions = highlightOptions(
              weight = 3,
              color = "#666",
              fillOpacity = 0.9,
              bringToFront = FALSE # Changed to FALSE so it doesn't cover the city boundary
            ),
            label = ~ paste("Tract:", GEOID),
            popup = ~ paste(
              "<b>Census Tract:</b>", GEOID, "<br>",
              paste0("<b>", ifelse(!is.null(metric_config$popup_label), metric_config$popup_label, metric_config$title), ":</b> "),
              round(tract_sf[[metric_config$col]], 3)
            )
          ) |>
          addLegend(
            layerId = legend_id, 
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
        # Fallback
        leafletProxy("tract_map") |>
          clearGroup(data_layer_group) |>
          removeControl(layerId = legend_id) |>
          addPolygons(
            data = tract_sf,
            group = data_layer_group, 
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