# City Info Module UI
cityInfoUI <- function(id) {
  ns <- NS(id)
  
  div(
    class = "info-box",
    
    # --- CONSOLIDATED HEADER SECTION ---
    div(
      # Apply the border and centering to this wrapper
      style = "text-align: center; color: #2c3e50; border-bottom: 1px solid #ccc; padding-bottom: 10px; margin-top: 0; margin-bottom: 15px;",
      
      # Line 1: Static Title
      h5("City Report", style = "margin: 0; font-weight: bold; color: inherit;"),
      
      # Line 2: Dynamic City Name (Rendered in Server)
      uiOutput(ns("city_header_lbl"))
    ),
    # -----------------------------------
    
    div(
      id = "ndvi_info",
      tags$b("Greenness Level:"),
      uiOutput(ns("ndvi_comp"), container = tags$span)
    ),
    div(
      id = "ls_info",
      tags$b("Lives Saved:"),
      uiOutput(ns("ls_comp"), container = tags$span)
    ),
    div(
      id = "stroke_info",
      tags$b("Stroke Cases Prevented:"),
      uiOutput(ns("scp_comp"), container = tags$span)
    ),
    div(
      id = "deme_info",
      tags$b("Dementia Cases Prevented:"),
      uiOutput(ns("dcp_comp"), container = tags$span)
    ),
    br(), 
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
    
    # Header Render
    output$city_header_lbl <- renderUI({
      req(selected_city())
      h5(selected_city(), style = "margin: 5px 0 0 0; font-weight: bold; color: inherit;")
    })
    
    # Calculate National Averages dynamically
    nat_stats <- list(
      meanNDVI = abs(round(mean(cityDF$meanNDVI, na.rm = TRUE),2)),
      ls_Mortality_Rate = abs(round(mean(cityDF$ls_Mortality_Rate, na.rm = TRUE),0)),
      ls_Stroke_Rate = abs(round(mean(cityDF$ls_Stroke_Rate, na.rm = TRUE),0)),
      ls_Dementia_Rate = abs(round(mean(cityDF$ls_Dementia_Rate, na.rm = TRUE), 0))
    )
    
    # --- UPDATED HELPER FUNCTION ---
    # Removed all inline color/font sizes. 
    # It now relies entirely on the CSS to match the title style.
    render_comparison <- function(city_val, nat_val, suffix = "") {
      HTML(paste0(
        # Line 1: City Value 
        # (CSS class .info-box span.shiny-html-output handles the bold/color)
        "<span>", 
        city_val, suffix,
        "</span>",
        
        # Line 2: Average
        "<div style='margin-top: 2px; margin-bottom: 12px; font-size: 0.9em; color: #666;'>",
        "200 Cities Average: ", nat_val, suffix,
        "</div>"
      ))
    }
    # -------------------------------
    
    output$ndvi_comp <- renderUI({
      nat_val <- round(nat_stats$meanNDVI, 2)
      
      if (selected_city() != "") {
        city_val <- round(selectedData()$meanNDVI, 2)
        render_comparison(city_val, nat_val)
      } else {
        HTML(paste0("<span>--</span><div style='margin-bottom: 12px; color: #666;'>200 Cities Average: ", nat_val, "</div>"))
      }
    })
    
    output$ls_comp <- renderUI({
      nat_val <- round(nat_stats$ls_Mortality_Rate, 0)
      
      if (selected_city() != "") {
        city_val <- abs(round(selectedData()$ls_Mortality_Rate, 0))
        render_comparison(city_val, nat_val)
      } else {
        HTML(paste0("<span>--</span><div style='margin-bottom: 12px; color: #666;'>200 Cities Average: ", nat_val, "</div>"))
      }
    })
    
    output$scp_comp <- renderUI({
      nat_val <- round(nat_stats$ls_Stroke_Rate, 0)
      
      if (selected_city() != "") {
        city_val <- abs(round(selectedData()$ls_Stroke_Rate, 0))
        render_comparison(city_val, nat_val)
      } else {
        HTML(paste0("<span>--</span><div style='margin-bottom: 12px; color: #666;'>200 Cities Average: ", nat_val, "</div>"))
      }
    })
    
    output$dcp_comp <- renderUI({
      nat_val <- round(nat_stats$ls_Dementia_Rate, 0)
      
      if (selected_city() != "") {
        city_val <- abs(round(selectedData()$ls_Dementia_Rate, 0))
        render_comparison(city_val, nat_val)
      } else {
        HTML(paste0("<span>--</span><div style='margin-bottom: 12px; color: #666;'>200 Cities Average: ", nat_val, "</div>"))
      }
    })
  })
}