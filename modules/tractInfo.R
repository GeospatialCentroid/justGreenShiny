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
      tags$b("Population Over 20:"),
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
tractInfoServer <- function(id, selected_city, selected_tract, tract_data) {
  moduleServer(id, function(input, output, session) {
    
    # Helper: Find the specific row for the selected tract
    current_tract_data <- reactive({
      req(tract_data()) # Ensure data exists
      req(selected_tract()) # Ensure a tract is clicked
      
      # Filter the reactive data for the specific GEOID
      data <- tract_data()
      target <- data[data$GEOID == selected_tract(), ]
      
      if (nrow(target) == 0) return(NULL)
      return(target)
    })
    
    output$tract_name <- renderUI({
      req(selected_tract())
      HTML(paste("Tract ID:", selected_tract()))
    })
    
    output$tract_pop <- renderUI({
      info <- current_tract_data()
      if (is.null(info)) return(HTML("--"))
      
      # Check that 'total_pop' matches your actual column name in tractsDF
      if ("over20" %in% names(info)) {
        pop_val <- info$over20  
        HTML(format(pop_val, big.mark = ","))
      } else {
        HTML("Column 'over20' not found")
      }
    })
    
    output$tract_ndvi <- renderUI({
      info <- current_tract_data()
      if (is.null(info)) return(HTML("--"))
      
      if ("meanNDVI" %in% names(info)) {
        # Round to 3 decimal places
        ndvi_val <- round(info$meanNDVI, 3) 
        HTML(as.character(ndvi_val))
      } else {
        HTML("--")
      }
    })
    
    output$tract_health <- renderUI({
      info <- current_tract_data()
      if (is.null(info)) return(HTML("--"))
      
      # Example placeholder for health metrics
      HTML("Health data pending")
    })
  })
}