# app.R
# This application demonstrates conditional UI rendering in R Shiny.

# Install required packages if you don't have them
# install.packages("shiny")
# install.packages("leaflet")
# install.packages("ggplot2")
# install.packages("plotly")  # New library for Plotly charts

# Load necessary libraries
library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
library(DT)



# parameters and datasets -------------------------------------------------
zoom_switch <- 9 
# read in city object 
cityGPKG <- sf::st_read("data/top200_simple.gpkg")
cityDF <- as.data.frame(cityGPKG)
cityCentroid <- sf::st_read("data/top200_centroid.gpkg")


# Define the user interface (UI)
ui <- fluidPage(# --- Custom CSS for banners and layout ---
  includeCSS("www/styles.css"),
  # The UI is now a single output that will render the correct page
  uiOutput("page_content")
  )

# Define the server logic
server <- function(input, output, session) {
  # --- Create mock data for city locations and bar graph values ---
  cities <- data.frame(
    name = c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix"),
    lat = c(40.71, 34.05, 41.88, 29.76, 33.45),
    lng = c(-74.01, -118.24, -87.63, -95.37, -112.07),
    bar_value = c(65, 80, 50, 75, 40) # Mock data for the bar graph
  )
  
  # Reactive value to store the current page
  current_page <- reactiveVal("page1")
  
  # Observers to switch pages
  observeEvent(input$to_page1, {
    current_page("page1")
  })
  
  observeEvent(input$to_page2, {
    current_page("page2")
  })
  observeEvent(input$to_page3, {
    current_page("page3")
  })
  
  # Render the UI based on the current_page value
  output$page_content <- renderUI({
    if (current_page() == "page1") {
      # UI for Page 1: Map and Controls (map on right)
      fluidRow(
        column(
          width = 4,
          class = "sidebar-panel",
          div(class = "sidebar-title-banner", "JustGreen"),
          p("Use the selectors below to look at health benifits provided by exposure to green vegetation for the two hundred most populated cities within the United States."),
          selectInput(
            "city_selector",
            "Select City:",
            choices = sort(cityGPKG$fullCity)
          ),
          selectInput(
            "ndvi_selector",
            "NDVI Selector:",
            choices = c("Measured NDVI", "10% increase")
          ),
          # Text outputs for city data
          tags$b("Population Over 20:"),
          textOutput("population_output"),
          tags$b("Average NDVI:"),
          textOutput("ndvi_output"),
          tags$b("Mortality Rate:"),
          textOutput("rate_output"),
          checkboxGroupInput(
            "confidence_checkboxes",
            "Confidence Level:",
            choices = c("Low Confidence", "High Confidence", "10% Increase")
          ),
          div(
            class = "bar-graph-panel",
            h5("Estimated Lives Save"),
            plotlyOutput("bar_graph", height = "200px")
          ),
          div(
            style = "margin-top: 20px;",
            actionButton("to_page2", "Go to Census Tract", class = "btn-block btn-primary")
          ),
          br(),
          div(
            style = "margin-top: 10px;",
            actionButton("to_page3", "About Page", class = "btn-block btn-primary")
          ),
          div(class = "sidebar-bottom-banner", "Updated 08-2025")
        ),
        column(
          width = 8,
          class = "map-panel",
          leafletOutput("my_map")
        )
      )
    } else if (current_page() == "page2") {
      # UI for Page 2: Map on Right with a card object
      fluidRow(
        div(class = "sidebar-title-banner", "Census Tract Summary"),
        column(
          width = 8,
          div(
            class = "cardMap",
            leafletOutput("my_map2")),
          div(
            # class = "cardTable",
            DT::dataTableOutput('table')
            )
          ),
        column(
          width = 4 ,
          div(
            div(
              actionButton("to_page2", "Go to Census Tract", class = "btn-block btn-primary")
            ),
            div(
              actionButton("to_page3", "About Page", class = "btn-block btn-primary")
            ),
            class = "card",
            plotlyOutput("plotly_plot1", height = "250px"),
            plotlyOutput("plotly_plot2", height = "250px"),
            plotlyOutput("plotly_plot3", height = "250px")
          )
        )
      )
    } else {
      # UI for Page 3: About Page
      fluidRow(
        column(
          width = 4,
          class = "sidebar-panel",
          div(class = "sidebar-title-banner", "About"),
          actionButton("to_page1", "Go to City Summary", class = "btn-block btn-primary"),
          div(
            actionButton("to_page2", "Go to Census Tract Summary", class = "btn-block btn-primary")
          ),
          div(class = "sidebar-bottom-banner", "Updated 08-2025")
        ),
        column(
          width = 8,
          div(
            class = "container",
            style = "padding: 20px;",
            h2("About This Application"),
            p(
              "This application demonstrates relationship between exposure to green plants as measured through NDVI and health outcomes."
            ),
            p(
              "."
            ),
            h4("References "),
            tags$ul(
              tags$li("past paper 1 "),
              tags$li("past paper 2"),
              tags$li(
                "Current Paper"
              )
            ),
            h4("Credits:"),
            p(
              "Developed using the R Shiny, Leaflet, ggplot2, and plotly packages."
            )
          )
        )
      )
    }
  })

  # palette 
  ## this will need to be a reactive feature at some point 
  pal1 <- colorNumeric(
    palette = "BuGn", #YlGn worked ok too 
    domain = as.numeric(cityGPKG$meanNDVI)
  )
  
  
  # Render the leaflet map for Page 1
  output$my_map <- renderLeaflet({
    # req(current_page() == "page1")
    leaflet() %>%
      addTiles() %>%
      setView(lng = -99.9018,
              lat = 39.3812,
              zoom = 4) %>%
      addCircleMarkers(
        data = cityCentroid,
        group = "cityPoints",
        radius = 5,
        stroke = FALSE,
        fillColor = ~pal1(meanNDVI),
        fillOpacity = 0.8,
        popup = ~popup
      )|>
      addPolygons(
        data = cityGPKG,
        group = "cityPoly",
        fillColor = ~pal1(meanNDVI),
        color = "black", # Outline color
        weight = 0.5, # Outline thickness
        fillOpacity = 0.8,
        popup = ~popup
      )
  })
  # leaflet proxy to change the marker or polygons 
  # This "observer" listens for changes in the map's zoom level
  ### this is not working as expected, check how the leaflet proxy funtion is working. 
  ### that said it does work, even with the fact both items stay present the work
  ### consider switch to a 
  observeEvent(input$map_zoom, {
    req(current_page() == "page1")
    # Get the proxy for the existing map to modify it
    proxy <- leafletProxy("my_map")
    current_zoom <- input$map_zoom
    
    if (current_zoom >= zoom_switch) {
      # If zoomed in, show polygons and hide markers
      proxy %>%
        showGroup("cityPoly") %>%
        hideGroup("cityPoints")
    } else {
      # If zoomed out, show markers and hide polygons
      proxy %>%
        showGroup("cityPoints") %>%
        hideGroup("cityPoly")
    }
  })
  
  # Render the leaflet map for Page 2
  output$my_map2 <- renderLeaflet({
    req(current_page() == "page2")
    leaflet() %>%
      addTiles() %>%
      setView(lng = -99.9018,
              lat = 39.3812,
              zoom = 4) %>%
      addCircleMarkers(
        data = cityCentroid,
        group = "cityPoints",
        radius = 5,
        stroke = FALSE,
        fillColor = ~pal1(meanNDVI),
        fillOpacity = 0.8,
        popup = ~popup
      )
  })
  
  # render the table for page 2 
  output$table <- DT::renderDataTable(iris)
  
  # Observer for marker clicks, only on page 1 where the dropdown exists
  observeEvent(input$my_map_marker_click, {
    req(current_page() == "page1")
    clicked_city <- input$my_map_marker_click$id
    updateSelectInput(session, "city_selector", selected = clicked_city)
  })
  

  # render the text outputs  ------------------------------------------------
  selectedData  <- reactive({cityDF |>
    dplyr::filter(fullCity == input$city_selector)}) 
  
  output$population_output <- renderText({format(selectedData()$popOver20_2023,  big.mark = ",")}) 
  output$ndvi_output <- renderText({round(selectedData()$meanNDVI, 2) }) 
  output$rate_output <- renderText({paste0(round(selectedData()$mortalityRate,4))}) 
  
  # Render the dynamic bar graph for Page 1
  output$bar_graph <- renderPlotly({
    req(current_page() == "page1")
    
    
    # select the city and pull all values of interest 
    selectedData1  <- cityDF |>
      dplyr::filter(fullCity == input$city_selector) |>
      # dplyr::filter(fullCity == "Madison city, Wisconsin") |>
      dplyr::select(cityFormat, meanNDVI, popOver20_2023, mortalityRate, ls_Mortality, 
                    ls_Mortality_low ,ls_Mortality_high,ls_Mortality10)
    
    data <- data.frame(
      Category = "Current Lives Saved",
      Value = abs(selectedData1$ls_Mortality),
      fill_color = "#31a354"
    )
    if ("Low Confidence" %in% input$confidence_checkboxes) {
      data <- rbind(
        data,
        data.frame(
          Category = "Low Confidence",
          Value = abs(selectedData1$ls_Mortality_low),
          fill_color = "#78c679"
        )
      )
    }
    if ("High Confidence" %in% input$confidence_checkboxes) {
      data <- rbind(
        data,
        data.frame(
          Category = "High Confidence",
          Value = abs(selectedData1$ls_Mortality_high),
          fill_color = "#c2e699"
        )
      )
    }
    if ("10% Increase" %in% input$confidence_checkboxes) {
      data <- rbind(
        data,
        data.frame(
          Category = "10% Increase",
          Value = abs(selectedData1$ls_Mortality10),
          fill_color = "#006837"
        )
      )
    }
    # data$Category <- factor(
    #   data$Category,
    #   levels = c(
    #     "Current Lives Saved",
    #     "Low Confidence",
    #     "High Confidence",
    #     "10% Increase" 
    #   )
    # )
    
    
    plot_ly(
      data = data,
      x = ~Category,
      y = ~Value,
      color = ~I(fill_color),
      type = "bar"
    )%>% layout(
      title = paste0(selectedData1$cityFormat, "; 2023 population over 20: ", 
                     format(selectedData1$popOver20_2023,  big.mark = ",")),
      xaxis = list(title = NA), # How can we hide this title?
      yaxis = list(title = "Lives Saved")      # How can we change this title?
    )
    
    
  })
  
  # Render the Plotly plots for Page 2
  output$plotly_plot1 <- renderPlotly({
    req(current_page() == "page2")
    plot_ly(
      data = cities,
      x = ~ name,
      y = ~ bar_value,
      type = "bar",
      name = "Bar Value"
    )
  })
  
  output$plotly_plot2 <- renderPlotly({
    req(current_page() == "page2")
    plot_ly(
      data = cities,
      labels = ~ name,
      values = ~ bar_value,
      type = "pie",
      name = "Bar Value"
    )
  })
  output$plotly_plot3 <- renderPlotly({
    req(current_page() == "page2")
    plot_ly(
      data = cities,
      labels = ~ name,
      values = ~ bar_value,
      type = "pie",
      name = "Bar Value"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
