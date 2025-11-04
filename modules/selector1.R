# City Info Module UI
cityInfoUI <- function(id) {
  ns <- NS(id)

  div(
    class = "info-box",
    h5(
      "City Report",
      style = "text-align: center; color: #2c3e50; border-bottom: 1px solid #ccc; padding-bottom: 5px; margin-top: 0;"
    ),
    div(
      tags$b("Annual Health Benefits (Current Green Spaces)"),
      style = "text-align: center; color: #2c3e50; border-bottom: 1px solid #ccc; padding-bottom: 5px; margin-top: 0;"
    ),
    div(
      id = "ndvi_info",
      tags$b("Greenness Level:"),
      uiOutput(ns("ndvi_comp"))
    ),
    div(
      id = "ls_info",
      tags$b("Lives Saved:"),
      uiOutput(ns("ls_comp"))
    ),
    div(
      id = "stroke_info",
      tags$b("Stroke Cases Prevented:"),
      uiOutput(ns("scp_comp"))
    ),
    div(
      id = "deme_info",
      tags$b("Dementia Cases Prevented:"),
      uiOutput(ns("dcp_comp"))
    )
  )
}

# City Info Module Server
cityInfoServer <- function(id, cityDF, selected_city) {
  moduleServer(id, function(input, output, session) {
    selectedData <- reactive({
      req(selected_city())
      cityDF |> dplyr::filter(fullCity == selected_city())
    })

    natData <- reactive({
      cityDF |> dplyr::filter(fullCity == "all cities")
    })

    output$ndvi_comp <- renderUI({
      nat_avg_str <- paste0("US Average: ", round(natData()$meanNDVI, 2))
      if (selected_city() != "") {
        city_val <- round(selectedData()$meanNDVI, 2)
        HTML(paste(selectedData()$city, " ", city_val, "|", nat_avg_str))
      } else {
        HTML(nat_avg_str)
      }
    })

    output$ls_comp <- renderUI({
      nat_avg_str <- paste0(
        "US Average: ",
        round(natData()$ls_Mortality_Rate, 0)
      )
      if (selected_city() != "") {
        city_val <- abs(round(selectedData()$ls_Mortality_Rate, 0))
        HTML(paste(selectedData()$city, " ", city_val, "|", nat_avg_str))
      } else {
        HTML(nat_avg_str)
      }
    })

    output$scp_comp <- renderUI({
      nat_avg_str <- paste0("US Average: ", round(natData()$ls_Stroke_Rate, 0))
      if (selected_city() != "") {
        city_val <- abs(round(selectedData()$ls_Stroke_Rate, 0))
        HTML(paste(selectedData()$city, " ", city_val, "|", nat_avg_str))
      } else {
        HTML(nat_avg_str)
      }
    })

    output$dcp_comp <- renderUI({
      nat_avg_str <- paste0(
        "US Average: ",
        round(natData()$ls_Dementia_Rate, 0)
      )
      if (selected_city() != "") {
        city_val <- abs(round(selectedData()$ls_Dementia_Rate, 0))
        HTML(paste(selectedData()$city, " ", city_val, "|", nat_avg_str))
      } else {
        HTML(nat_avg_str)
      }
    })
  })
}
