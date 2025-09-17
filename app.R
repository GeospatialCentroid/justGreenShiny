# Load libraries
library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
library(DT)
library(shinyBS)
library(RColorBrewer)
library(bslib)

# parameters and datasets -------------------------------------------------
zoom_switch <- 9
circleRadius <- 10 
# read in city object 
cityGPKG <- sf::st_read("data/top200_simple.gpkg")
cityDF <- read.csv("data/top200.csv")
cityCentroid <- sf::st_read("data/top200_centroid.gpkg")
# source functions 
source("functions/gaugeChart.R")


ui <- fluidPage(
  # --- Custom CSS for banners and layout ---
  includeCSS("www/styles.css"),
  # page 1  -----------------------------------------------------------------
  conditionalPanel(
    condition = "output.page_tracker == 'page1'",
    page_sidebar(
      sidebar = sidebar(
        title = "National Level Trends",
        width = "25%",
        p("Use the selectors below to evaluate the average vegetation levels and related health impacts of the 200 most populated cities in the United States."),
        selectInput(
          "city_selector",
          "Select City:",
          choices = c("Select a City" = "", 
                      sort(cityGPKG$fullCity)),
          selected = ""
        ),
        selectInput("map1_selector", 
                    "Health Benefit Selector:", 
                    choices = c("Current Vegetation Levels",
                                "Lives Saved",
                                "Stroke Cases Prevented",
                                "Dementia Cases Prevented")),
        div(class = "info-box",
            # Replace textOutput with uiOutput for dynamic content
            div(id = "ndvi_info", tags$b("Current Vegetation Levels:"), uiOutput("ndvi_comp_output")),
            div(id = "ls_info", tags$b("Lives Saved:"), uiOutput("ls_comp_output")),
            div(id = "stroke_info", tags$b("Stroke Cases Prevented:"), uiOutput("scp_comp_output")),
            div(id = "deme_info", tags$b("Dementia Cases Prevented:"), uiOutput("dcp_comp_output"))
        ),
        card(
          card_header("Relative position of city"),
          style = "max-height: 280px; overflow-y: auto;",
          card_body(
            plotlyOutput("gauge_chart"),
            uiOutput("gaugeText")
          )
        ),
        actionButton("to_page2", "View Census Tract Details"),
        actionButton("to_page3", "About Page")
        ),
      # Main content for Page 1
      card(
        full_screen = TRUE,
        card_header("City Health & Vegetation Map"),
        leafletOutput("map1", height = "85vh"),
        # 2. Add the banner div with the class defined in your CSS
        tags$div(
          class = "footer-banner",
          tags$img(src = "CSU-Symbol-r-K.png", height = "80px"), # Image on the right
          tags$span("Rojos Lab - Geospatial Centroid", tags$br(), "Colorado State University © 2025"), # Text on the left
          tags$img(src = "centroid_white_gray_logo_CROPPED.png", height = "60px") # Image on the right
        )
      )
    )
  ),

  
  # --- UI defined with conditionalPanel instead of renderUI ---
 
  conditionalPanel(
    condition = "output.page_tracker == 'page2'",
    # UI for Page 2
    fluidRow(
        br(),
        column(12, h2("Page 2: Census Tract Summary"), 
               actionButton("to_page1", "Back to City Summary"),
               actionButton("to_page3", "About Page")
      )
    )
  ),
  conditionalPanel(
    condition = "output.page_tracker == 'page3'",
    # UI for Page 3
    fluidRow(
      column(12, h2("Page 3: About Page"),
             actionButton("to_page1", "Back to City Summary"),
             actionButton("to_page2", "View Census Tract Details"))
      )
    ),
  # 2. Add the banner div with the class defined in your CSS
  # tags$div(
  #   class = "footer-banner",
  #   "This is a fixed banner at the bottom of the page. Current time: ",
  #   format(Sys.time(), "%Y-%m-%d %H:%M:%S") # Example dynamic content
  # )
  )


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # Reactive value to store the current page
  current_page <- reactiveVal("page1")
  
  # Observers to switch pages
  observeEvent(input$to_page1, { current_page("page1") })
  observeEvent(input$to_page2, { current_page("page2") })
  observeEvent(input$to_page3, { current_page("page3") })
  
  # --- Page Tracker for conditionalPanel ---
  # This output's value is what the conditionalPanels in the UI check against.
  output$page_tracker <- reactive({
    current_page()
  })
  # This is crucial! It allows the conditionalPanel to check the output's value
  # even when the output itself is not displayed.
  outputOptions(output, "page_tracker", suspendWhenHidden = FALSE)
  

  # map palettes ------------------------------------------------------------ 
  # Palette for mean NDVI 
  pal1 <- colorNumeric(palette = "BuGn", domain = as.numeric(cityGPKG$meanNDVI))
  # palette for lives saved 
  pal2 <- colorNumeric(palette = "PuBuGn", domain = as.numeric(cityGPKG$ls_Mortality_Rate))
  # palette for stroke cases reduced 
  pal3 <- colorNumeric(palette = "BuPu", domain = as.numeric(cityGPKG$ls_Stroke_Rate))
  # palette for demetia cases reduced 
  pal4 <- colorNumeric(palette = "OrRd", domain = as.numeric(cityGPKG$ls_Dementia_Rate))
  

  # render initial map ------------------------------------------------------
  output$map1 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") %>%
      setView(lng = -99.9018, lat = 39.3812, zoom = 3) %>%
      addCircleMarkers(
        data = cityCentroid,
        group = "cityPoints",
        radius = circleRadius,
        stroke = FALSE,
        layerId = ~fullCity, # Assign layerId for click events
        fillColor = ~pal1(meanNDVI),
        fillOpacity = 0.8,
        popup = ~popup,
        label = ~fullCity 
      ) %>%
      addPolygons(
        data = cityGPKG,
        group = "cityPoly",
        fillColor = ~pal1(meanNDVI),
        color = "black",
        weight = 0.5,
        layerId = ~fullCity, # Assign layerId for click events
        fillOpacity = 0.8,
        popup = ~popup,
        label = ~fullCity 
      )|>
      addLayersControl(
        # Specify the layers to be used as base maps
        baseGroups = c("CartoDB", "OpenStreetMap", "Esri Imagery"),
        # Set the position of the control box on the map
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addLegend(pal = pal1, 
                values = cityGPKG$meanNDVI,
                title = "NDVI",
                layerId = "legend")
  })
  # Observer to update map colors based on NDVI selector ---
  observeEvent(input$map1_selector, {
    req(current_page() == "page1")
    
    proxy <- leafletProxy("map1")
    
    # Clear existing shapes and markers
    proxy %>% clearGroup("cityPoly") %>% clearGroup("cityPoints")
    
    # Determine which palette and data to use
    if (input$map1_selector == "Current Vegetation Levels") {
      current_pal <- pal1
      data_col <- "meanNDVI"
      title <- "NDVI"
    } else if (input$map1_selector == "Lives Saved") {
      current_pal <- pal2
      data_col <- "ls_Mortality_Rate"
      title <- "Lives Save per 100,000"
    } else if (input$map1_selector == "Stroke Cases Prevented") {
      current_pal <- pal3
      data_col <- "ls_Stroke_Rate"
      title <- "Stroke Cases Pervented per 100,000"
    } else { # Default case: "Measured NDVI"
      current_pal <- pal4
      data_col <- "ls_Dementia_Rate"
      title <- "Dementia Cases Pervented per 100,000"
    }
    
    # Redraw the map layers with the selected data and palette
    proxy %>%
      removeControl(layerId = "legend")|> 
      addCircleMarkers(
        data = cityCentroid,
        group = "cityPoints",
        layerId = ~fullCity,
        radius = circleRadius,
        stroke = FALSE,
        fillColor = ~current_pal(get(data_col)), # Use the selected palette and data
        fillOpacity = 0.8,
        popup = ~popup,
        label = ~fullCity
      ) %>%
      addPolygons(
        data = cityGPKG,
        group = "cityPoly",
        layerId = ~fullCity,
        fillColor = ~current_pal(get(data_col)), # Use the selected palette and data
        color = "black",
        weight = 0.5,
        fillOpacity = 0.8,
        popup = ~popup,
        label = ~fullCity
      )|>
      addLegend(
        pal = current_pal, 
        values = cityGPKG[[data_col]],
        title = title,
        layerId = "legend"
      )
  }, ignoreInit = TRUE)
    
    
  # --- Leaflet proxy for zoom switch ---
  # This observer now checks if the current page is "page1" before trying to update the map.
  observe({
    req(input$map1_zoom, current_page() == "page1")
    
    current_zoom <- input$map1_zoom
    proxy <- leafletProxy("map1")
    
    if (current_zoom >= zoom_switch) {
      proxy %>% 
        showGroup("cityPoly") %>% 
        hideGroup("cityPoints")
    } else {
      proxy %>% 
        showGroup("cityPoints") %>% 
        hideGroup("cityPoly")
        # hideGroup("highlight")
    }
  })
  
  # --- Observer for City Selection ---
  # This observer also checks if the current page is "page1" and uses req() for safety.
  observeEvent(input$city_selector, {
    req(input$city_selector, current_page() == "page1")
    # set proxy object 
    proxy <- leafletProxy("map1")
    
    # First, clear any existing highlight
    proxy %>% removeShape("highlight_shape")
    
    # Only proceed if a city is actually selected
    if (input$city_selector != "") {
      selected_city_coords <- cityCentroid[cityCentroid$fullCity == input$city_selector, ] %>%
        sf::st_coordinates()
      
      longitude <- as.numeric(selected_city_coords[1, "X"])
      latitude <- as.numeric(selected_city_coords[1, "Y"])
      
      # Zoom to the city
      proxy %>% setView(lng = longitude, lat = latitude, zoom = 11)
      
      # Add the highlight polygon
      selected_polygon <- cityGPKG[cityGPKG$fullCity == input$city_selector, ]
      
      proxy %>% addPolygons(
        data = selected_polygon,
        layerId = "highlight_shape", # Give the highlight a unique ID
        color = "#FCAF62",
        weight = 3,
        group = "highlight",
        fillOpacity = 0 # Make it transparent so the original color shows through
      )
    }
  }, ignoreInit = TRUE)
  
  
  # map click changes the side bar selector
  observeEvent(input$map1_shape_click, {
    # Ensure we are on page 1 and a shape was actually clicked
    req(input$map1_shape_click, current_page() == "page1")
    
    # Get the layerId of the clicked shape, which we set to be the city name.
    clicked_city <- input$map1_shape_click$id
    
    # Update the city selector dropdown to match the clicked city.
    updateSelectInput(session, "city_selector", selected = clicked_city)
  })
  
  # This observer handles clicks on the city markers (when zoomed out)
  observeEvent(input$map1_marker_click, {
    # Ensure we are on page 1 and a marker was actually clicked
    req(input$map1_marker_click, current_page() == "page1")
    
    # Get the layerId of the clicked marker, which we set to be the city name.
    clicked_city <- input$map1_marker_click$id
    
    # Update the city selector dropdown to match the clicked city.
    updateSelectInput(session, "city_selector", selected = clicked_city)
  })
  
  
  
  # --- Reactive data and text outputs ---

  # Selected City data ------------------------------------------------------
  selectedData <- reactive({
    req(input$city_selector)
    cityDF %>% dplyr::filter(fullCity == input$city_selector)
  })
  natData <- reactive({
    cityDF |> 
      dplyr::filter(fullCity == "all cities")
  })
  
  
  # --- NEW: UI Outputs for Comparison Text ---
  output$ndvi_comp_output <- renderUI({
    nat_avg_str <- paste0("National Avg: ", round(natData()$meanNDVI,2))
    if (input$city_selector != "") {
      city_val <- round(selectedData()$meanNDVI, 2)
      HTML(paste(nat_avg_str, "|", "Selected City:", city_val))
    } else {
      HTML(nat_avg_str)
    }
  })
  
  output$ls_comp_output <- renderUI({
    nat_avg_str <- paste0("National Avg: ", round(natData()$ls_Mortality_Rate,0))
    if (input$city_selector != "") {
      city_val <- abs(round(selectedData()$ls_Mortality_Rate,0))
      HTML(paste(nat_avg_str, "|", "Selected City:", city_val))
    } else {
      HTML(nat_avg_str)
    }
  })
  
  output$scp_comp_output <- renderUI({
    nat_avg_str <- paste0("National Avg: ", round(natData()$ls_Stroke_Rate,0))
    if (input$city_selector != "") {
      city_val <- abs(round(selectedData()$ls_Stroke_Rate,0))
      HTML(paste(nat_avg_str, "|", "Selected City:", city_val))
    } else {
      HTML(nat_avg_str)
    }
  })
  
  output$dcp_comp_output <- renderUI({
    nat_avg_str <- paste0("National Avg: ", round(natData()$ls_Dementia_Rate,0))
    if (input$city_selector != "") {
      city_val <- abs(round(selectedData()$ls_Dementia_Rate,0))
      HTML(paste(nat_avg_str, "|", "Selected City:", city_val))
    } else {
      HTML(nat_avg_str)
    }
  })

  

  # Gauge chart  ------------------------------------------------------------
  
  
  output$gauge_chart <- renderPlotly({
    req(input$city_selector)
    
    # intitalizing the chart 
    if(input$city_selector == ""){
      selectedRate <- round(mean(cityDF$meanNDVI, na.rm=TRUE), 2)
      valueRange <- round(range(cityDF$meanNDVI), 2)
      current_pal <- RColorBrewer::brewer.pal( n = 8, name = "BuGn")
      gaugePlot(selectedRate = selectedRate,
                valueRange = valueRange,
                colorPalette = current_pal,
                title =  "National Average",
                height = 100,
                # width = "25%"
      )
      
      output$gaugeText <- renderUI({
        p("Can add text here")
      })
      
    }else{
      # Determine which palette and data to use
      if (input$map1_selector == "Current Vegetation Levels") {
        current_pal <- RColorBrewer::brewer.pal( n = 8, name = "BuGn")
        data_col <- "meanNDVI"
        output$gaugeText <- renderUI({
          p("Specific information for NDVI ")
        })
      } else if (input$map1_selector == "Lives Saved") {
        current_pal <- RColorBrewer::brewer.pal( n = 8, name = "PuBuGn")
        data_col <- "ls_Mortality_Rate"
        output$gaugeText <- renderUI({
          p("Specific information for Mortality ")
        })
      } else if (input$map1_selector == "Stroke Cases Prevented") {
        current_pal <- RColorBrewer::brewer.pal( n = 8, name = "BuPu")
        data_col <- "ls_Stroke_Rate"
        output$gaugeText <- renderUI({
          p("Specific information for Stroke ")
        })
      } else { # Default case: "Measured NDVI"
        current_pal <- RColorBrewer::brewer.pal( n = 8, name = "OrRd")
        data_col <- "ls_Dementia_Rate"
        output$gaugeText <- renderUI({
          p("Specific information for Dementia ")
        })
      }
      selectedRate <- selectedData()[[data_col]]
      valueRange <-  round(range(as.data.frame(cityGPKG)[[data_col]],  na.rm = TRUE),0)
      gaugePlot(selectedRate = selectedRate,
                valueRange = valueRange,
                colorPalette = current_pal,
                title =  selectedData()$fullCity,
                height = 100,
                # width = "25%"
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

