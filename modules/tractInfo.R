# Tract Info Module UI
tractInfoUI <- function(id) {
  ns <- NS(id)

  div(
    class = "info-box",
    h5(
      "Census Tract Information",
      style = "text-align: center; color: #2c3e50; border-bottom: 1px solid #ccc; padding-bottom: 5px; margin-top: 0;"
    ),
    div(
      tags$b("Selected Tract:"),
      uiOutput(ns("tract_name"))
    ),
    div(
      tags$b("Population:"),
      uiOutput(ns("tract_pop"))
    ),
    div(
      tags$b("NDVI (Greenness):"),
      uiOutput(ns("tract_ndvi"))
    ),
    div(
      tags$b("Health Metrics:"),
      uiOutput(ns("tract_health"))
    )
  )
}

# Tract Info Module Server
tractInfoServer <- function(id, selected_city, selected_tract) {
  moduleServer(id, function(input, output, session) {
    output$tract_name <- renderUI({
      if (is.null(selected_tract()) || selected_tract() == "") {
        HTML("No tract selected")
      } else {
        HTML(paste("Tract ID:", selected_tract()))
      }
    })

    output$tract_pop <- renderUI({
      if (is.null(selected_tract()) || selected_tract() == "") {
        HTML("--")
      } else {
        # TODO: Get actual population data
        HTML("Data pending")
      }
    })

    output$tract_ndvi <- renderUI({
      if (is.null(selected_tract()) || selected_tract() == "") {
        HTML("--")
      } else {
        # TODO: Get actual NDVI data
        HTML("Data pending")
      }
    })

    output$tract_health <- renderUI({
      if (is.null(selected_tract()) || selected_tract() == "") {
        HTML("--")
      } else {
        # TODO: Get actual health metrics
        HTML("Data pending")
      }
    })
  })
}
