# Gauge Module UI
gaugeUI <- function(id) {
  ns <- NS(id)

  card(
    id = "gauge_card",
    div(
      id = "gaugeHeader",
      tags$b("City Comparison: "),
      uiOutput(ns("metric_name"))
    ),
    br(),
    br(),
    style = "max-height: 250px; overflow-y: auto;",
    card_body(
      plotlyOutput(ns("gauge_chart")),
      uiOutput(ns("gaugeText"))
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

    metric_name_reactive <- reactiveVal("Greenness level (NDVI)")

    output$metric_name <- renderUI({
      metric_name_reactive()
    })

    output$gauge_chart <- renderPlotly({
      if (is.null(selected_city()) || selected_city() == "") {
        output$gaugeText <- renderUI({
          p(style = "text-align: center;", "Low greenness → High greenness")
        })

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
        palette_config <- switch(
          map_selector(),
          "Current Vegetation Levels" = list(
            pal = "BuGn",
            col = "meanNDVI",
            text = "Low greenness → High greenness",
            title = "Greenness level (NDVI)"
          ),
          "Lives Saved" = list(
            pal = "PuBuGn",
            col = "ls_Mortality_Rate",
            text = "Fewer lives → More lives saved",
            title = "Lives Save per 100,000"
          ),
          "Stroke Cases Prevented" = list(
            pal = "BuPu",
            col = "ls_Stroke_Rate",
            text = "Fewer cases → More cases prevented",
            title = "Stroke Cases Prevented per 100,000"
          ),
          "Dementia Cases Prevented" = list(
            pal = "OrRd",
            col = "ls_Dementia_Rate",
            text = "Fewer cases → More cases prevented",
            title = "Dementia Cases Prevented per 100,000"
          )
        )

        metric_name_reactive(palette_config$title)

        output$gaugeText <- renderUI({
          p(style = "text-align: center;", palette_config$text)
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
