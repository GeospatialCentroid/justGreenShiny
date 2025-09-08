# Load libraries
library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
library(DT)
library(shinyBS)
library(RColorBrewer)

# parameters and datasets -------------------------------------------------
zoom_switch <- 9
circleRadius <- 10 
# read in city object 
cityGPKG <- sf::st_read("data/top200_simple.gpkg")
cityDF <- as.data.frame(cityGPKG)
cityCentroid <- sf::st_read("data/top200_centroid.gpkg")
# source functions 
source("functions/gaugeChart.R")


ui <- fluidPage(
  # --- Custom CSS for banners and layout ---
  includeCSS("www/styles.css"),
  
  # --- UI defined with conditionalPanel instead of renderUI ---
  conditionalPanel(
    condition = "output.page_tracker == 'page1'",
    # UI for Page 1: Map and Controls
    fluidRow(
      column(
        width = 4,
        class = "sidebar-panel",
        div(class = "sidebar-title-banner", "JustGreen"),
        div(class = "info-box",
            p(
              "Use the selectors below to evaluate the average vegetation levels and related health impacts of the 200 most populated cities in the United States."
            )
        ),
        selectInput(
          "city_selector",
          "Select City:",
          choices = c("Select a City" = "", 
                      sort(cityGPKG$fullCity)),
          selected = "Fort Collins city, Colorado"
        ),
        selectInput("map1_selector", 
                    "Health Benefit Selector:", 
                    choices = c("Current Vegetation Levels",
                                "Lives Saved",
                                "Stroke Cases Prevented",
                                "Dementia Cases Prevented")),
        div(class = "info-box",
          div(id = "pop_info",tags$b("Population Over 20: "), textOutput("population_output", inline = TRUE)),
          div(id = "ndvi_info", tags$b("Current Vegetation Levels:"), textOutput("ndvi_output", inline = TRUE)),
          div(id = "ls_info", tags$b("Lives Saved:"), textOutput("LS_output", inline = TRUE)),
          div(id = "stroke_info", tags$b("Stroke Cases Prevented:"), textOutput("SCP_output", inline = TRUE)),
          div(id = "deme_info", tags$b("Dementia Cases Prevented:"), textOutput("DCP_output", inline = TRUE)), 
          br(), 
          div(
            p("Population is derived from census data.", br(), 
              "Vegetation level values and range from 0-1." , br(),
              "Mortality, stroke, and dementia are all reported as rates normalizes to incidences per 100,000 people")
          )
        ),
        shinyBS::bsTooltip(id = "pop_info", title = "Total population over the age of 20 for the selected city.", placement = "right", trigger = "hover"),
        shinyBS::bsTooltip(id = "ndvi_info", title = "Amount of vegetation measures by Normalized Difference Vegetation Index (NDVI)", placement = "right", trigger = "hover"),
        shinyBS::bsTooltip(id = "ls_info", title = "Lives per 100,000 saved due to vegetation exposure.", placement = "right", trigger = "hover"),
        shinyBS::bsTooltip(id = "stroke_info", title = "Stroke incidences per 100,000 prevented due to vegetation exposure.", placement = "right", trigger = "hover"),
        shinyBS::bsTooltip(id = "deme_info", title = "Dementia cases per 100,000 prevented due to vegetation exposure.", placement = "right", trigger = "hover"),
        
        # add in the figure here 
        plotlyOutput("gauge_chart"),
        actionButton("to_page2", "View Census Tract Details"),
        actionButton("to_page3", "About Page")
        
      ),
      # location of the map element 
      column(
        style = "height: 90vh; overflow-y: auto;", # The key CSS
        width = 8, class = "map-panel", leafletOutput("map1", height = "80vh"))
    )
  ),
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
  )
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
  
  
  # Palette for mean NDVI 
  pal1 <- colorNumeric(palette = "BuGn", domain = as.numeric(cityGPKG$meanNDVI))
  # palette for lives saved 
  pal2 <- colorNumeric(palette = "PuBuGn", domain = as.numeric(cityGPKG$ls_Mortality_Rate) , reverse = TRUE)
  # palette for stroke cases reduced 
  pal3 <- colorNumeric(palette = "BuPu", domain = as.numeric(cityGPKG$ls_Stroke_Rate) , reverse = TRUE)
  # palette for demetia cases reduced 
  pal4 <- colorNumeric(palette = "OrRd", domain = as.numeric(cityGPKG$ls_Dementia_Rate) , reverse = TRUE)
  
  # Render the leaflet map for Page 1
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
      )
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
    } else if (input$map1_selector == "Lives Saved") {
      current_pal <- pal2
      data_col <- "ls_Mortality_Rate"
    } else if (input$map1_selector == "Stroke Cases Prevented") {
      current_pal <- pal3
      data_col <- "ls_Stroke_Rate"
    } else { # Default case: "Measured NDVI"
      current_pal <- pal4
      data_col <- "ls_Dementia_Rate"
    }
    
    # Redraw the map layers with the selected data and palette
    proxy %>%
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
  selectedData <- reactive({
    req(input$city_selector)
    cityDF %>% dplyr::filter(fullCity == input$city_selector)
  })
  
  output$population_output <- renderText({ format(selectedData()$popOver20_2023, big.mark = ",") })
  output$ndvi_output <- renderText({ round(selectedData()$meanNDVI, 2) })
  output$LS_output <- renderText({ paste0(abs(round(selectedData()$ls_Mortality_Rate, 2))) })
  output$SCP_output <- renderText({ paste0(abs(round(selectedData()$ls_Stroke_Rate, 2))) })
  output$DCP_output <- renderText({ paste0(abs(round(selectedData()$ls_Dementia_Rate, 2))) })
  
  # --- Bar graph rendering ---
  output$gauge_chart <- renderPlotly({
    req(input$city_selector)
    
    allCitys <- cityDF |>
      dplyr::select(cityFormat, meanNDVI, ls_Mortality_Rate, ls_Stroke_Rate, ls_Dementia_Rate)
    
    selectedData1 <- selectedData() |>
      dplyr::select(cityFormat, meanNDVI, ls_Mortality_Rate, ls_Stroke_Rate, ls_Dementia_Rate)
    
    
    # coditional selection based on map visualization 
    # Determine which palette and data to use
    if (input$map1_selector == "Current Vegetation Levels") {
      palette1 <- brewer.pal(n =9, name = "BuGn")
      selectedRate <-  round(selectedData1$meanNDVI[1],2)
      average_value <-  round(mean(allCitys$meanNDVI, na.rm = TRUE),2)
      range_min <-  round(min(allCitys$meanNDVI, na.rm = TRUE),2)
      range_max <-  round(max(allCitys$meanNDVI, na.rm = TRUE),2)
    } else if (input$map1_selector == "Lives Saved") {
      palette1 <- brewer.pal(n =9, name = "PuBuGn")
      selectedRate <-  round(selectedData1$ls_Mortality_Rate[1],2)
      average_value <- round(mean(allCitys$ls_Mortality_Rate, na.rm = TRUE),2)
      range_min <-  round(min(allCitys$ls_Mortality_Rate, na.rm = TRUE),2)
      range_max <-  round(max(allCitys$ls_Mortality_Rate, na.rm = TRUE),2)
    } else if (input$map1_selector == "Stroke Cases Prevented") {
      palette1 <- brewer.pal(n =9, name = "BuPu")
      selectedRate <-  round(selectedData1$ls_Stroke_Rate[1],2)
      average_value <-  round(mean(allCitys$ls_Stroke_Rate, na.rm = TRUE),2)
      range_min <-  round(min(allCitys$ls_Stroke_Rate, na.rm = TRUE),2)
      range_max <-  round(max(allCitys$ls_Stroke_Rate, na.rm = TRUE),2)
    } else { # Default case: "Measured NDVI"
      palette1 <- brewer.pal(n =9, name = "OrRd")
      selectedRate <-  round(selectedData1$ls_Dementia_Rate[1],2)
      average_value <-  round(mean(allCitys$ls_Dementia_Rate, na.rm = TRUE),2)
      range_min <-  round(min(allCitys$ls_Dementia_Rate, na.rm = TRUE),2)
      range_max <-  round(max(allCitys$ls_Dementia_Rate, na.rm = TRUE),2)
    }
  
    
    # generate the plot with NDVI values first 
    create_gauge_chart(
      city_name = selectedData1$cityFormat[1],
      city_value = selectedRate,
      average_value = average_value,
      range_min = range_min,
      range_max = range_max,
      palette = palette1
    )
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)

