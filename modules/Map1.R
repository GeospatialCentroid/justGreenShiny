# Map Module UI
mapUI <- function(id) {
  ns <- NS(id)

  card(
    id = "map_card",
    full_screen = TRUE,
    card_header("City Health & Vegetation Map",
                tags$br(),
                tags$span(
                  style = "font-size: 0.8em; font-weight: normal;", 
                  "Evaluation of current health benefits of vegetation on the 200 most populated cities in the United States"
                )),
    leafletOutput(ns("map"), height = "70vh"),
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

# Map Module Server
mapServer <- function(id, cityGPKG, cityCentroid, selected_city, map_selector) {
  moduleServer(id, function(input, output, session) {
    
    # Map palettes
    pal1 <- colorNumeric(palette = "BuGn", domain = as.numeric(cityGPKG$meanNDVI))
    pal2 <- colorNumeric(palette = "PuBuGn", domain = as.numeric(cityGPKG$ls_Mortality_Rate))
    pal3 <- colorNumeric(palette = "BuPu", domain = as.numeric(cityGPKG$ls_Stroke_Rate))
    pal4 <- colorNumeric(palette = "OrRd", domain = as.numeric(cityGPKG$ls_Dementia_Rate))
    
    # Helper function to generate legend parameters
    # This ensures the Initial Map and the Proxy Update use identical logic
    get_legend_params <- function(data_col, palette_fun, decimals) {
      vals <- data_col[!is.na(data_col)]
      val_rng <- range(vals, na.rm = TRUE)
      
      if(length(vals) > 0) {
        # 1. Create nice breaks
        breaks <- pretty(vals, n = 5)
        
        # 2. Clamp breaks to data range (fixes white/NA top color)
        clamped_breaks <- pmax(val_rng[1], pmin(breaks, val_rng[2]))
        
        # 3. Get colors
        break_colors <- palette_fun(clamped_breaks)
        
        # 4. Clean labels
        clean_labels <- round(breaks, decimals)
        
        # 5. Reverse for display (High value at top)
        return(list(
          colors = rev(break_colors),
          labels = rev(clean_labels)
        ))
      }
      return(NULL)
    }
    
    # Render initial map
    output$map <- renderLeaflet({
      
      # Generate default legend (NDVI)
      leg_params <- get_legend_params(cityGPKG$meanNDVI, pal1, decimals = 1)
      
      leaflet() |>
        addProviderTiles(providers$CartoDB.Positron, group = "Simple Map") |>
        addTiles(group = "Street Map") |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
        setView(lng = -99.9018, lat = 39.3812, zoom = 3) |>
        addCircleMarkers(
          data = cityCentroid,
          group = "cityPoints",
          radius = 10,
          stroke = FALSE,
          layerId = ~fullCity,
          fillColor = ~ pal1(meanNDVI),
          fillOpacity = 0.8,
          popup = ~popup,
          label = ~fullCity
        ) |>
        addPolygons(
          data = cityGPKG,
          group = "cityPoly",
          fillColor = ~ pal1(meanNDVI),
          color = "black",
          weight = 0.5,
          layerId = ~fullCity,
          fillOpacity = 0.8,
          popup = ~popup,
          label = ~fullCity
        ) |>
        addLayersControl(
          baseGroups = c("Simple Map", "Street Map", "Satellite"),
          options = layersControlOptions(collapsed = FALSE)
        ) |>
        addLegend(
          colors = leg_params$colors,
          labels = leg_params$labels,
          title = "Greenness level<br>(NDVI)",
          layerId = "legend",
          position = "bottomright",
          opacity = 0.7
        )
    })
    
    # Update map colors based on selector
    observeEvent(
      map_selector(),
      {
        proxy <- leafletProxy("map")
        proxy |> clearGroup("cityPoly") |> clearGroup("cityPoints")
        
        # Configuration for each metric
        metric_config <- switch(
          map_selector(),
          "Current Vegetation Levels" = list(
            pal = pal1,
            col = "meanNDVI",
            title = "Greenness level<br>(NDVI)",
            decimals = 1
          ),
          "Lives Saved" = list(
            pal = pal2,
            col = "ls_Mortality_Rate",
            title = "Lives Saved<br>per 100,000",
            decimals = 0
          ),
          "Stroke Cases Prevented" = list(
            pal = pal3,
            col = "ls_Stroke_Rate",
            title = "Stroke Cases Prevented<br>per 100,000",
            decimals = 0
          ),
          "Dementia Cases Prevented" = list(
            pal = pal4,
            col = "ls_Dementia_Rate",
            title = "Dementia Cases Prevented<br>per 100,000",
            decimals = 0
          )
        )
        
        # Get data column
        data_col <- cityGPKG[[metric_config$col]]
        
        # Generate custom legend params
        leg_params <- get_legend_params(
          data_col, 
          metric_config$pal, 
          metric_config$decimals
        )
        
        proxy |>
          removeControl(layerId = "legend") |>
          addCircleMarkers(
            data = cityCentroid,
            group = "cityPoints",
            layerId = ~fullCity,
            radius = 10,
            stroke = FALSE,
            fillColor = ~ metric_config$pal(get(metric_config$col)),
            fillOpacity = 0.8,
            popup = ~popup,
            label = ~fullCity
          ) |>
          addPolygons(
            data = cityGPKG,
            group = "cityPoly",
            layerId = ~fullCity,
            fillColor = ~ metric_config$pal(get(metric_config$col)),
            color = "black",
            weight = 0.5,
            fillOpacity = 0.8,
            popup = ~popup,
            label = ~fullCity
          ) |>
          addLegend(
            colors = leg_params$colors,
            labels = leg_params$labels,
            title = metric_config$title,
            layerId = "legend",
            position = "bottomright",
            opacity = 0.7
          )
      },
      ignoreInit = TRUE
    )
    
    # Zoom behavior
    observe({
      req(input$map_zoom)
      proxy <- leafletProxy("map")
      
      if (input$map_zoom >= 9) {
        proxy |> showGroup("cityPoly") |> hideGroup("cityPoints")
      } else {
        proxy |> showGroup("cityPoints") |> hideGroup("cityPoly")
      }
    })
    
    # Highlight selected city
    observeEvent(
      selected_city(),
      {
        req(selected_city())
        proxy <- leafletProxy("map")
        proxy |> removeShape("highlight_shape")
        
        if (selected_city() != "") {
          selected_city_coords <- cityCentroid[
            cityCentroid$fullCity == selected_city(),
          ] |>
            sf::st_coordinates()
          
          longitude <- as.numeric(selected_city_coords[1, "X"])
          latitude <- as.numeric(selected_city_coords[1, "Y"])
          
          proxy |> setView(lng = longitude, lat = latitude, zoom = 11)
          
          selected_polygon <- cityGPKG[cityGPKG$fullCity == selected_city(), ]
          
          proxy |>
            addPolygons(
              data = selected_polygon,
              layerId = "highlight_shape",
              color = "#FCAF62",
              weight = 3,
              group = "highlight",
              fillOpacity = 0
            )
        }
      },
      ignoreInit = TRUE
    )
    
    # Return clicked city (either shape or marker)
    clicked_city <- reactive({
      if (!is.null(input$map_shape_click)) {
        return(input$map_shape_click$id)
      } else if (!is.null(input$map_marker_click)) {
        return(input$map_marker_click$id)
      }
      return(NULL)
    })
    
    # IMPORTANT: Return the reactive
    return(clicked_city)
  })
}