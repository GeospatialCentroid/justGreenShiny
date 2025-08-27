# Load libraries
library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
library(DT)
library(shinyBS)

# parameters and datasets -------------------------------------------------
zoom_switch <- 9 
# read in city object 
cityGPKG <- sf::st_read("data/top200_simple.gpkg")
cityDF <- as.data.frame(cityGPKG)
cityCentroid <- sf::st_read("data/top200_centroid.gpkg")

ui <- fluidPage(
  # --- Custom CSS for banners and layout ---
  # includeCSS("www/styles.css"), # Assuming you have a www/styles.css file
  tags$head(
    tags$style(HTML("
      .sidebar-panel { padding: 15px; background-color: #f8f9fa; border-right: 1px solid #dee2e6; height: 100vh; }
      .map-panel { padding: 0; }
      .sidebar-title-banner { background-color: #006837; color: white; padding: 10px; text-align: center; font-size: 1.5em; }
      .sidebar-bottom-banner { background-color: #f0f0f0; color: #333; padding: 5px; text-align: center; font-size: 0.8em; position: absolute; bottom: 0; width: 100%; left:0; }
      .info-box { border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-top: 15px; margin-bottom: 15px; background-color: #ffffff; }
      .tooltip-inner {
          background-color: #006837; /* A dark green to match your theme */
          color: #FFF; /* White text for contrast */
          max-width: 500px; /* Adjust the maximum width */
          text-align: center; /* Align text to the left if needed */}
      .tooltip.right .tooltip-arrow { border-right-color: #006837;}
    "))
  ),
  
  # --- UI defined with conditionalPanel instead of renderUI ---
  conditionalPanel(
    condition = "output.page_tracker == 'page1'",
    # UI for Page 1: Map and Controls
    fluidRow(
      column(
        width = 4,
        class = "sidebar-panel",
        div(class = "sidebar-title-banner", "JustGreen"),
        p("Use the selectors below..."),
        selectInput(
          "city_selector",
          "Select City:",
          choices = c("Select a City" = "", sort(cityGPKG$fullCity)),
          selected = ""
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
          div(id = "deme_info", tags$b("Dementia Cases Prevented:"), textOutput("DCP_output", inline = TRUE))
        ),
        shinyBS::bsTooltip(id = "pop_info", title = "Total population over the age of 20 for the selected city.", placement = "right", trigger = "hover"),
        shinyBS::bsTooltip(id = "ndvi_info", title = "Amount of vegetation measures by Normalized Difference Vegetation Index (NDVI)", placement = "right", trigger = "hover"),
        shinyBS::bsTooltip(id = "ls_info", title = "Lives per 100,000 saved due to vegetation exposure.", placement = "right", trigger = "hover"),
        shinyBS::bsTooltip(id = "stroke_info", title = "Stroke incidences per 100,000 prevented due to vegetation exposure.", placement = "right", trigger = "hover"),
        shinyBS::bsTooltip(id = "deme_info", title = "Dementia cases per 100,000 prevented due to vegetation exposure.", placement = "right", trigger = "hover"),
        
        # checkboxGroupInput("confidence_checkboxes", "Confidence Level:", choices = c("Low Confidence", "High Confidence", "10% Increase")),
        # div(class = "bar-graph-panel", h5("Estimated Lives Saved"), plotlyOutput("bar_graph", height = "200px")),
        # div(style = "margin-top: 20px;", actionButton("to_page2", "Go to Census Tract", class = "btn-block btn-primary")),
        # br(),
        # div(style = "margin-top: 10px;", actionButton("to_page3", "About Page", class = "btn-block btn-primary")),
        # div(class = "sidebar-bottom-banner", "Updated 08-2025")
      ),
      column(width = 8, class = "map-panel", leafletOutput("map1", height = "100vh"))
    )
  ),
  conditionalPanel(
    condition = "output.page_tracker == 'page2'",
    # UI for Page 2
    fluidRow(
      column(12, h2("Page 2: Census Tract Summary"), actionButton("to_page1", "Back to City Summary"))
    )
  ),
  conditionalPanel(
    condition = "output.page_tracker == 'page3'",
    # UI for Page 3
    fluidRow(
      column(12, h2("Page 3: About Page"), actionButton("to_page1", "Back to City Summary"))
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
        radius = 5,
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
        radius = 5,
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
  })
    
    
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
  })
  
  
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
  output$bar_graph <- renderPlotly({
    req(input$city_selector)
    
    selectedData1 <- selectedData() %>%
      dplyr::select(cityFormat, meanNDVI, popOver20_2023, mortalityRate, ls_Mortality,
                    ls_Mortality_low, ls_Mortality_high, ls_Mortality10)
    
    data <- data.frame(
      Category = "Current Lives Saved",
      Value = abs(selectedData1$ls_Mortality),
      fill_color = "#31a354"
    )
    if ("Low Confidence" %in% input$confidence_checkboxes) {
      data <- rbind(data, data.frame(Category = "Low Confidence", Value = abs(selectedData1$ls_Mortality_low), fill_color = "#78c679"))
    }
    if ("High Confidence" %in% input$confidence_checkboxes) {
      data <- rbind(data, data.frame(Category = "High Confidence", Value = abs(selectedData1$ls_Mortality_high), fill_color = "#c2e699"))
    }
    if ("10% Increase" %in% input$confidence_checkboxes) {
      data <- rbind(data, data.frame(Category = "10% Increase", Value = abs(selectedData1$ls_Mortality10), fill_color = "#006837"))
    }
    
    plot_ly(data = data, x = ~Category, y = ~Value, color = ~I(fill_color), type = "bar") %>%
      layout(
        title = list(text = paste0(selectedData1$cityFormat, "; Pop over 20: ", format(selectedData1$popOver20_2023, big.mark = ",")), font = list(size=10)),
        xaxis = list(title = NA),
        yaxis = list(title = "Lives Saved")
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

