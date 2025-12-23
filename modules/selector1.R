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
    ),
    br(), # Add a little space before the notes
    div(
      style = "border-top: 1px solid #ccc; padding-top: 10px; font-size: 0.8em; color: #666; font-style: italic; line-height: 1.3;",
      p("* All health metrics are reported as rates per 100,000 population.", style = "margin-bottom: 5px;"),
      p("* Greenness level measured using satellite-derived Normalized Difference Vegetation Index (NDVI).", style = "margin-bottom: 0;")
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
    
    # Calculate National Averages dynamically
    nat_stats <- list(
      meanNDVI = abs(round(mean(cityDF$meanNDVI, na.rm = TRUE),2)),
      ls_Mortality_Rate = abs(round(mean(cityDF$ls_Mortality_Rate, na.rm = TRUE),0)),
      ls_Stroke_Rate = abs(round(mean(cityDF$ls_Stroke_Rate, na.rm = TRUE),0)),
      ls_Dementia_Rate = abs(round(mean(cityDF$ls_Dementia_Rate, na.rm = TRUE), 0))
    )
    
    # Helper function to format the two lines consistentluy
    render_comparison <- function(city_name, city_val, nat_val, suffix = "") {
      HTML(paste0(
        # Line 1: The Selected City
        "<div style='margin-top: 4px; font-weight: 500; color: #1E4D2B;'>", 
        city_name, ": ", city_val, suffix,
        "</div>",
        
        # Line 2: The 200 Cities Average (Slightly lighter text)
        "<div style='margin-bottom: 12px; font-size: 0.9em; color: #666;'>",
        "200 Cities Average: ", nat_val, suffix,
        "</div>"
      ))
    }
    
    output$ndvi_comp <- renderUI({
      nat_val <- round(nat_stats$meanNDVI, 2)
      
      if (selected_city() != "") {
        city_val <- round(selectedData()$meanNDVI, 2)
        render_comparison(selectedData()$fullCity, city_val, nat_val)
      } else {
        # Fallback if no city selected
        HTML(paste0("<div style='margin-bottom: 12px; color: #666;'>200 Cities Average: ", nat_val, "</div>"))
      }
    })
    
    output$ls_comp <- renderUI({
      nat_val <- round(nat_stats$ls_Mortality_Rate, 0)
      
      if (selected_city() != "") {
        city_val <- abs(round(selectedData()$ls_Mortality_Rate, 0))
        render_comparison(selectedData()$fullCity, city_val, nat_val)
      } else {
        HTML(paste0("<div style='margin-bottom: 12px; color: #666;'>200 Cities Average: ", nat_val, "</div>"))
      }
    })
    
    output$scp_comp <- renderUI({
      nat_val <- round(nat_stats$ls_Stroke_Rate, 0)
      
      if (selected_city() != "") {
        city_val <- abs(round(selectedData()$ls_Stroke_Rate, 0))
        render_comparison(selectedData()$fullCity, city_val, nat_val)
      } else {
        HTML(paste0("<div style='margin-bottom: 12px; color: #666;'>200 Cities Average: ", nat_val, "</div>"))
      }
    })
    
    output$dcp_comp <- renderUI({
      nat_val <- round(nat_stats$ls_Dementia_Rate, 0)
      
      if (selected_city() != "") {
        city_val <- abs(round(selectedData()$ls_Dementia_Rate, 0))
        render_comparison(selectedData()$fullCity, city_val, nat_val)
      } else {
        HTML(paste0("<div style='margin-bottom: 12px; color: #666;'>200 Cities Average: ", nat_val, "</div>"))
      }
    })
  })
}