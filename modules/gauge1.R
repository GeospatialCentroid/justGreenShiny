# Gauge Module UI
gaugeUI <- function(id) {
  ns <- NS(id)
  
  # Use 'info-box' to match the background color
  div(
    class = "info-box",
    
    # 1. The Chart Container (Centered)
    div(
      style = "display: flex; justify-content: center; align-items: center; min-height: 120px;",
      plotlyOutput(ns("gauge_chart"), height = "100px", width = "95%")
    ),
    
    # 2. The Description Text (Styled like the city box footnotes)
    div(
      style = "border-top: 1px solid #ccc; padding-top: 10px; margin-top: 10px; font-size: 0.8em; color: #666; font-style: italic; line-height: 1.3;",
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
    
    # Define text descriptions for each metric
    # (We re-define these here to pass them to the UI)
    get_desc <- function(metric) {
      switch(
        metric,
        "Current Vegetation Levels" = "This measure (NDVI) indicates the density and health of vegetation. Higher values represent greener, more vegetated urban areas.",
        "Lives Saved" = "Estimated annual reduction in all-cause mortality per 100,000 residents attributed to the presence of urban green spaces.",
        "Stroke Cases Prevented" = "The estimated number of stroke cases prevented annually per 100,000 people due to current greenness levels.",
        "Dementia Cases Prevented" = "The estimated reduction in new dementia cases per 100,000 people linked to exposure to green spaces.",
        "This compares the selected area against the National Average." # Fallback
      )
    }
    
    # Render the Description
    output$metric_desc <- renderUI({
      req(map_selector())
      if (is.null(selected_city()) || selected_city() == "") {
        "This compares the selected area against the National Average for Greenness (NDVI)."
      } else {
        get_desc(map_selector())
      }
    })
    
    output$gauge_chart <- renderPlotly({
      if (is.null(selected_city()) || selected_city() == "") {
        # Default / National Average View
        selectedRate <- round(mean(cityGPKG$meanNDVI, na.rm = TRUE), 2)
        valueRange <- round(range(cityGPKG$meanNDVI), 2)
        current_pal <- RColorBrewer::brewer.pal(n = 8, name = "BuGn")
        
        gaugePlot(
          selectedRate = selectedRate,
          valueRange = valueRange,
          colorPalette = current_pal,
          title = "National Average",
          height = 100
        )
      } else {
        # Dynamic View based on Map Selector
        palette_config <- switch(
          map_selector(),
          "Current Vegetation Levels" = list(pal = "BuGn", col = "meanNDVI"),
          "Lives Saved" = list(pal = "PuBuGn", col = "ls_Mortality_Rate"),
          "Stroke Cases Prevented" = list(pal = "BuPu", col = "ls_Stroke_Rate"),
          "Dementia Cases Prevented" = list(pal = "OrRd", col = "ls_Dementia_Rate")
        )
        
        current_pal <- RColorBrewer::brewer.pal(n = 8, name = palette_config$pal)
        selectedRate <- selectedData()[[palette_config$col]]
        valueRange <- round(range(as.data.frame(cityGPKG)[[palette_config$col]], na.rm = TRUE), 0)
        
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