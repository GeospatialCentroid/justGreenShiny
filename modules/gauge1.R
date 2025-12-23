# Gauge Module UI
gaugeUI <- function(id) {
  ns <- NS(id)
  
  card(
    id = "gauge_card",
    style = "max-height: 350px; overflow-y: auto;", # Increased slightly for text space
    
    # 1. Chart
    card_body(
      plotlyOutput(ns("gauge_chart"), height = "120px"),
      uiOutput(ns("gaugeText"))
    ),
    
    # 2. Dynamic Description (Replaces the old title)
    div(
      id = "gaugeHeader",
      style = "padding: 0px 15px 15px 15px; text-align: center; font-size: 0.9rem; color: #444;",
      uiOutput(ns("metric_desc")) 
    )
  )
}

# Gauge Module Server
gaugeServer <- function(id, cityDF, cityGPKG, selected_city, map_selector) {
  moduleServer(id, function(input, output, session) {
    
    selectedData <- reactive({
      req(selected_city())
      cityGPKG |> as.data.frame() |> dplyr::filter(fullCity == selected_city())
    })
    
    # Renamed this to be more generic since it holds full sentences now
    metric_desc_reactive <- reactiveVal("This gauge shows the average vegetation density.")
    
    output$metric_desc <- renderUI({
      metric_desc_reactive()
    })
    
    output$gauge_chart <- renderPlotly({
      if (is.null(selected_city()) || selected_city() == "") {
        # Default / National Average View
        output$gaugeText <- renderUI({
          p(style = "text-align: center;", "Low greenness → High greenness")
        })
        
        # Update description for the default view
        metric_desc_reactive("This compares the selected area against the National Average for Greenness (NDVI).")
        
        selectedRate <- round(mean(cityGPKG$meanNDVI, na.rm = TRUE), 2)
        valueRange <- round(range(cityGPKG$meanNDVI), 2)
        current_pal <- RColorBrewer::brewer.pal(n = 8, name = "BuGn")
        
        gaugePlot(
          selectedRate = selectedRate,
          valueRange = valueRange,
          colorPalette = current_pal,
          title = "National Average",
          height = 120
        )
      } else {
        # Dynamic View based on Map Selector
        palette_config <- switch(
          map_selector(),
          "Current Vegetation Levels" = list(
            pal = "BuGn",
            col = "meanNDVI",
            text = "Low greenness → High greenness",
            title = "Greenness level (NDVI)",
            # Added description field
            desc = "This measure (NDVI) indicates the density and health of vegetation. Higher values represent greener, more vegetated urban areas."
          ),
          "Lives Saved" = list(
            pal = "PuBuGn",
            col = "ls_Mortality_Rate",
            text = "Fewer lives → More lives saved",
            title = "Lives Saved per 100,000",
            desc = "Estimated annual reduction in all-cause mortality per 100,000 residents attributed to the presence of urban green spaces."
          ),
          "Stroke Cases Prevented" = list(
            pal = "BuPu",
            col = "ls_Stroke_Rate",
            text = "Fewer cases → More cases prevented",
            title = "Stroke Cases Prevented per 100,000",
            desc = "The estimated number of stroke cases prevented annually per 100,000 people due to current greenness levels."
          ),
          "Dementia Cases Prevented" = list(
            pal = "OrRd",
            col = "ls_Dementia_Rate",
            text = "Fewer cases → More cases prevented",
            title = "Dementia Cases Prevented per 100,000",
            desc = "The estimated reduction in new dementia cases per 100,000 people linked to exposure to green spaces."
          )
        )
        
        # Update the description text
        metric_desc_reactive(palette_config$desc)
        
        output$gaugeText <- renderUI({
          p(style = "text-align: center; font-size: 0.8em; color: #666;", palette_config$text)
        })
        
        current_pal <- RColorBrewer::brewer.pal(
          n = 8,
          name = palette_config$pal
        )
        selectedRate <- selectedData()[[palette_config$col]]
        valueRange <- round(
          range(as.data.frame(cityGPKG)[[palette_config$col]], na.rm = TRUE),
          0
        )
        
        gaugePlot(
          selectedRate = selectedRate,
          valueRange = valueRange,
          colorPalette = current_pal,
          title = selectedData()$fullCity,
          height = 100
        )
      }
    })
  })
}