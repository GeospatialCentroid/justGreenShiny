# Tract Info Module UI
tractInfoUI <- function(id) {
  ns <- NS(id)
  
  div(
    class = "info-box",
    
    # --- HEADER ---
    div(
      style = "text-align: center; color: #2c3e50; border-bottom: 1px solid #ccc; padding-bottom: 10px; margin-top: 0; margin-bottom: 15px;",
      h5("Census Tract Information", style = "margin: 0; font-weight: bold; color: inherit;")
    ),
    
    # --- 1. Tract ID ---
    div(
      tags$span("Selected Tract:", style = "color: #555; font-weight: 500;"),
      uiOutput(ns("tract_name"), container = tags$span, style = "font-weight: bold; margin-left: 5px;")
    ),
    
    # --- 2. Population ---
    div(
      tags$span("Population Over 20:", style = "color: #555; font-weight: 500;"),
      uiOutput(ns("tract_pop"), container = tags$span, style = "font-weight: bold; margin-left: 5px;")
    ),
    
    # --- 3. Greenness (NDVI) ---
    div(
      tags$span("Greenness Level (NDVI):", style = "color: #555; font-weight: 500;"),
      uiOutput(ns("tract_ndvi"), container = tags$span, style = "font-weight: bold; margin-left: 5px;")
    ),
    
    # --- 4. Health Metrics ---
    # The server renders these as a block, so we just output the block here.
    # We add a header for clarity since the server outputs the individual lines.
    div(
      style = "margin-top: 10px;",
      tags$span("Health Metrics per 100,000:", style = "color: #2c3e50; font-weight: bold; display: block; margin-bottom: 5px;"),
      uiOutput(ns("tract_health")) 
    ),
    
    # --- 5. Social Vulnerability ---
    div(
      style = "margin-top: 10px;",
      tags$span("CDC Social Vulnerability:", style = "color: #555; font-weight: 500;"),
      uiOutput(ns("tract_svi"), container = tags$span, style = "font-weight: bold; margin-left: 5px;")
    ),
    
    # --- Footer Notes ---
    br(), 
    div(
      style = "border-top: 1px solid #ccc; padding-top: 10px; font-size: 0.8em; color: #666; font-style: italic; line-height: 1.3;",
      p("* All health metrics are reported as rates per 100,000 population.", style = "margin-bottom: 5px;"),
      p("* Greenness level measured using satellite-derived Normalized Difference Vegetation Index (NDVI).", style = "margin-bottom: 0;")
    )
  )
}

# Tract Info Module Server
tractInfoServer <- function(id, selected_city, selected_tract, tract_data) {
  moduleServer(id, function(input, output, session) {
    
    current_tract_data <- reactive({
      req(tract_data())
      req(selected_tract())
      data <- tract_data()
      target <- data[data$GEOID == selected_tract(), ]
      if (nrow(target) == 0) return(NULL)
      return(target)
    })
    
    output$tract_name <- renderUI({
      req(selected_tract())
      HTML(paste(selected_tract()))
    })
    
    output$tract_pop <- renderUI({
      info <- current_tract_data()
      if (is.null(info)) return(HTML("--"))
      if ("over20" %in% names(info)) {
        HTML(format(info$over20, big.mark = ","))
      } else {
        HTML("--")
      }
    })
    
    output$tract_ndvi <- renderUI({
      info <- current_tract_data()
      if (is.null(info)) return(HTML("--"))
      if ("meanNDVI" %in% names(info)) {
        ndvi_val <- round(info$meanNDVI, 3) 
        HTML(as.character(ndvi_val))
      } else {
        HTML("--")
      }
    })
    
    # This renders the combined block of 3 health metrics
    output$tract_health <- renderUI({
      info <- current_tract_data()
      if (is.null(info)) return(HTML("--"))
      
      items <- c()
      
      # Define consistent styling for the health metric labels
      lbl_style <- "color: #555; font-weight: 500; margin-right: 5px;"
      
      if ("ls_Mortality_Rate" %in% names(info)) {
        val <- round(info$ls_Mortality_Rate, 2)
        items <- c(items, paste0("<div><span style='", lbl_style, "'>Lives Saved:</span><b>", val, "</b></div>"))
      }
      
      if ("ls_Stroke_Rate" %in% names(info)) {
        val <- round(info$ls_Stroke_Rate, 2)
        items <- c(items, paste0("<div><span style='", lbl_style, "'>Stroke Cases Prevented:</span><b>", val, "</b></div>"))
      }
      
      if ("ls_Dementia_Rate" %in% names(info)) {
        val <- round(info$ls_Dementia_Rate, 2)
        items <- c(items, paste0("<div><span style='", lbl_style, "'>Dementia Cases Prevented:</span><b>", val, "</b></div>"))
      }
      
      if (length(items) > 0) {
        HTML(paste(items, collapse = ""))
      } else {
        HTML("Health data pending")
      }
    })
    
    output$tract_svi <- renderUI({
      info <- current_tract_data()
      if (is.null(info)) return(HTML("--"))
      if ("RPL_THEMES" %in% names(info)) {
        val <- round(info$RPL_THEMES, 2)
        HTML(as.character(val))
      } else {
        HTML("--")
      }
    })
  })
}