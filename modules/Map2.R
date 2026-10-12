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
    withSpinner(leafletOutput(ns("tract_map"), height = "55vh"), type = 6, color = "#1E4D2B"),
    tags$div(
      class = "footer-banner",
      tags$img(src = "rojosLogo.png", height = "80px"),
      tags$span(
        "Rojas Lab - Geospatial Centroid",
        tags$br(),
        "Colorado State University © 2026"
      ),
      tags$img(
        src = "centroid_white_gray_logo_CROPPED.png",
        height = "60px"
      )
    )
  )
}

# Census Tract Map Module Server
tractMapServer <- function(id, selected_city, cityGPKG, tractsGPKG, tractsDF, tract_metric, active_tab) {
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
      
      # Minimal delay for spinner visibility
      Sys.sleep(0.2) 
      
      city_info <- cityGPKG[cityGPKG$fullCity == selected_city(), ]
      geoid  <- city_info$GEOID
      tracts <- tractsGPKG[[geoid]]
      
      # Ensure data exists for this city
      if (is.null(tracts)) return(NULL)
      
      # Join with health data
      ct_health <- tractsDF |>
        dplyr::filter(GEOID %in% tracts$GEOID) |>
        dplyr::distinct(GEOID, .keep_all = TRUE) 
      
      tracts <- tracts |>
        dplyr::left_join(y = ct_health, by = "GEOID")
      
      return(tracts)
    })
    
    # Draw the tract layer and legend for one metric.
    # `map` is either a fresh leaflet() object (initial render) or a leafletProxy()
    # (metric change), so the same code serves both and the first view of the
    # City Review tab always carries tracts without waiting for a proxy update.
    draw_tracts <- function(map, tract_sf, metric) {
      bounds <- sf::st_bbox(tract_sf)
      
      metric_config <- switch(
        metric,
        "Current Vegetation Levels" = list(
          palette = "BuGn",            
          col = "meanNDVI_500m",
          title = "Greenness level<br>(NDVI)",
          legend_type = "numeric",
          decimals = 1
        ),
        "Lives Saved" = list(
          palette = "PuBuGn",
          col = "ls_Mortality_Rate",
          title = "Lives Saved<br>per 100,000",
          legend_type = "numeric",
          decimals = 0 
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
          domain = c(0, 1),
          legend_type = "qualitative",
          decimals = 2
        )
      )
      
      map <- map |>
        clearGroup(data_layer_group) |>
        removeControl(layerId = legend_id)
      
      if (!is.null(metric_config) && metric_config$col %in% names(tract_sf)) {
        
        # Calculate domain explicitly for clamping
        data_vals <- tract_sf[[metric_config$col]]
        val_rng <- range(data_vals, na.rm = TRUE)
        
        pal <- colorNumeric(
          palette = metric_config$palette,
          domain = if(is.null(metric_config$domain)) val_rng else metric_config$domain,
          na.color = "transparent"
        )
        # Clean up the title for the label (remove <br> tags)
        clean_title <- gsub("<br>", " ", metric_config$title)
        
        map <- map |>
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
            
            # Hover label: metric value and population
            label = ~ lapply(seq_len(nrow(tract_sf)), function(i) {
              val <- tract_sf[[metric_config$col]][i]
              pop_val <- tract_sf$over20[i]
              
              formatted_val <- if(metric_config$decimals == 0) {
                format(round(val, 0), big.mark = ",")
              } else {
                round(val, metric_config$decimals)
              }
              
              HTML(paste0(
                "<div style='font-family: Poppins, sans-serif;'>",
                "<b>", clean_title, ": </b>", formatted_val, "<br/>",
                "<span style='font-size: 0.9em; color: #666;'>Population (20+): ", format(pop_val, big.mark = ","), "</span>",
                "</div>"
              ))
            }),
            
            popup = ~ paste0(
              "<b>Census Tract:</b> ", GEOID, "<br>",
              "<hr style='margin: 5px 0;'>",
              "<b>Population (20+):</b>", format(over20, big.mark = ","), "<br>",
              "<b>Greenness (NDVI):</b> ", round(meanNDVI_500m, 3), "<br>",
              "<b>Lives Saved:</b> ", round(ls_Mortality_Rate, 0), " <small>(per 100k)</small><br>",
              "<b>Strokes Prevented:</b> ", round(ls_Stroke_Rate, 0), " <small>(per 100k)</small><br>",
              "<b>Dementia Prevented:</b> ", round(ls_Dementia_Rate, 0), " <small>(per 100k)</small><br>",
              "<b>Social Vulnerability:</b> ", round(RPL_THEMES, 2)
            )
          )
        
        # --- LEGEND ---
        if (metric_config$legend_type == "qualitative") {
          svi_colors <- RColorBrewer::brewer.pal(5, "YlGnBu")
          
          map <- map |> addLegend(
            layerId = legend_id,
            colors = rev(svi_colors), 
            labels = c("High", "", "Medium", "", "Low"),
            title = metric_config$title,
            position = "bottomright",
            opacity = 0.7
          )
          
        } else {
          vals <- data_vals[!is.na(data_vals)]
          
          if(length(vals) > 0) {
            breaks <- pretty(vals, n = 5)
            clamped_breaks <- pmax(val_rng[1], pmin(breaks, val_rng[2]))
            break_colors <- pal(clamped_breaks)
            
            dec <- if(!is.null(metric_config$decimals)) metric_config$decimals else 2
            clean_labels <- round(breaks, dec)
            
            map <- map |> addLegend(
              layerId = legend_id, 
              colors = rev(break_colors), 
              labels = rev(clean_labels),
              title = metric_config$title,
              position = "bottomright",
              opacity = 0.7
            )
          }
        }
      } else {
        # Fallback: plain tracts
        map <- map |>
          addPolygons(
            data = tract_sf,
            group = data_layer_group, 
            fillColor = "#3388ff",
            fillOpacity = 0.5,
            color = "#444444",
            weight = 1,
            layerId = ~GEOID,
            label = ~ paste("Tract:", GEOID)
          )
      }
      
      map |> fitBounds(
        lng1 = bounds[["xmin"]], lat1 = bounds[["ymin"]],
        lng2 = bounds[["xmax"]], lat2 = bounds[["ymax"]]
      )
    }
    
    # 2. Initialize map, with the tracts for the current metric drawn in the
    #    same render. Re-runs when the city changes; the metric is isolated so
    #    metric changes go through the proxy below instead of a full re-render.
    output$tract_map <- renderLeaflet({
      validate(need(selected_city() != "", "Please select a city from the sidebar to view tract details."))
      map <- leaflet() |>
        addMapPane("borders", zIndex = 410) |> 
        addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Simple Map") |>
        addProviderTiles(providers$Esri.WorldStreetMap, group = "Street Map") |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
        setView(lng = -98.57, lat = 39.82, zoom = 4) |> 
        addLayersControl(
          baseGroups = c("Simple Map", "Street Map", "Satellite"),
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
      
      tract_sf <- tract_data()
      if (!is.null(tract_sf)) {
        map <- draw_tracts(map, tract_sf, isolate(tract_metric()))
      }
      map
    })
    
    # 4. Update tracts when the metric changes (proxy, no full re-render)
    observeEvent(tract_metric(), {
      req(tract_data())
      draw_tracts(leafletProxy("tract_map"), tract_data(), tract_metric())
    }, ignoreInit = TRUE)
    
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
