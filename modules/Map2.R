# Census Tract Map Module UI
tractMapUI <- function(id) {
  ns <- NS(id)

  card(
    id = "tract_map_card",
    full_screen = TRUE,
    card_header("City Health & Vegetation Map",
                tags$br(),
                tags$span(
                  style = "font-size: 0.8em; font-weight: normal;", 
                  "Evaluation of current health benefits of vegetation on the census tracts within your selected city"
                )),
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
    boundary_group <- "City Boundary"
    legend_id <- "map_legend"
    
    # 3. Update City Boundary (Runs only when city changes)
    city_boundary <-  reactive({
      req(selected_city())
      req(cityGPKG)
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
        addMapPane("borders", zIndex = 410) |> 
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") |>
        addTiles(group = "OpenStreetMap") |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") |>
        setView(lng = -98.57, lat = 39.82, zoom = 4) |> 
        addLayersControl(
          baseGroups = c("CartoDB", "OpenStreetMap", "Esri Imagery"),
          overlayGroups = c(data_layer_group, boundary_group), 
          options = layersControlOptions(collapsed = FALSE)
        )|>
        addPolygons(
          data = city_boundary(),
          group = boundary_group,
          fill = FALSE,         
          color = "#52525295",  
          weight = 3,           
          opacity = 1,
          options = pathOptions(pane = "borders", clickable = FALSE) 
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
          title = "Greenness level<br>(NDVI)",
          legend_type = "numeric",
          decimals = 1  # Fixes 0.60000001 -> 0.6
        ),
        "Lives Saved" = list(
          palette = "PuBuGn",
          col = "ls_Mortality_Rate",
          title = "Lives Saved<br>per 100,000",
          legend_type = "numeric",
          decimals = 0  # Whole numbers (e.g., 500)
        ),
        "Stroke Cases Prevented" = list(
          palette = "BuPu",
          col = "ls_Stroke_Rate",
          title = "Stroke Cases Prevented<br>per 100,000",
          legend_type = "numeric",
          decimals = 0
        ),
        "Dementia Cases Prevented" = list(
          palette = "OrRd",
          col = "ls_Dementia_Rate",
          title = "Dementia Cases Prevented<br>per 100,000",
          legend_type = "numeric",
          decimals = 0
        ),
        "Social Vulnerability (RPL)" = list(
          col = "RPL_THEMES",
          palette = "YlGnBu",
          title = "Social Vulnerability<br>Index",
          popup_label = "SVI",
          domain = c(0, 1),
          legend_type = "qualitative"
        )
      )
      
      if (!is.null(metric_config) && metric_config$col %in% names(tract_sf)) {
        
        # Calculate domain explicitly for clamping
        data_vals <- tract_sf[[metric_config$col]]
        val_rng <- range(data_vals, na.rm = TRUE)
        
        pal <- colorNumeric(
          palette = metric_config$palette,
          domain = if(is.null(metric_config$domain)) val_rng else metric_config$domain,
          na.color = "transparent"
        )
        
        proxy <- leafletProxy("tract_map") |>
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
              bringToFront = FALSE 
            ),
            label = ~ paste("Tract:", GEOID),
            popup = ~ paste(
              "<b>Census Tract:</b>", GEOID, "<br>",
              paste0("<b>", ifelse(!is.null(metric_config$popup_label), metric_config$popup_label, metric_config$title), ":</b> "),
              round(tract_sf[[metric_config$col]], 3)
            )
          )
        
        # --- LEGEND LOGIC ---
        
        if (metric_config$legend_type == "qualitative") {
          # SVI: Qualitative (High -> Low)
          svi_colors <- RColorBrewer::brewer.pal(5, "YlGnBu")
          
          proxy |> addLegend(
            layerId = legend_id,
            colors = rev(svi_colors), 
            labels = c("High", "", "Medium", "", "Low"),
            title = metric_config$title,
            position = "bottomright",
            opacity = 0.7
          )
          
        } else {
          # Numeric Metrics
          vals <- data_vals[!is.na(data_vals)]
          
          if(length(vals) > 0) {
            # 1. Create nice break points
            breaks <- pretty(vals, n = 5)
            
            # 2. CLAMP breaks for COLORS
            clamped_breaks <- pmax(val_rng[1], pmin(breaks, val_rng[2]))
            
            # 3. Get colors using clamped values
            break_colors <- pal(clamped_breaks)
            
            # 4. Clean Labels using the 'decimals' setting
            dec <- if(!is.null(metric_config$decimals)) metric_config$decimals else 2
            clean_labels <- round(breaks, dec)
            
            # 5. Reverse both so High Value is at top
            legend_colors <- rev(break_colors)
            legend_labels <- rev(clean_labels)
            
            proxy |> addLegend(
              layerId = legend_id, 
              colors = legend_colors, 
              labels = legend_labels,
              title = metric_config$title,
              position = "bottomright",
              opacity = 0.7
            )
          }
        }
        
        proxy |> fitBounds(
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