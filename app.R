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
            choices = c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix")
          ),
          selectInput(
            "ndvi_selector",
            "NDVI Selector:",
            choices = c("Measured NDVI", "10% increase")
          ),
          # Text outputs for city data
          tags$b("Population:"),
          textOutput("population_output"),
          tags$b("Average NDVI:"),
          textOutput("ndvi_output"),
          tags$b("Rate:"),
          textOutput("rate_output"),
          checkboxGroupInput(
            "confidence_checkboxes",
            "Confidence Level:",
            choices = c("Low Confidence", "High Confidence", "10% Increase")
          ),
          div(
            class = "bar-graph-panel",
            h5("Estimated Lives Save"),
            plotOutput("bar_graph", height = "200px")
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
  
  # Render the leaflet map for Page 1
  output$my_map <- renderLeaflet({
    req(current_page() == "page1")
    leaflet() %>%
      addTiles() %>%
      setView(lng = -99.9018,
              lat = 39.3812,
              zoom = 4) %>%
      addMarkers(
        data = cities,
        lng = ~ lng,
        lat = ~ lat,
        popup = ~ name,
        layerId = ~ name
      )
  })
  
  # Render the leaflet map for Page 2
  output$my_map2 <- renderLeaflet({
    req(current_page() == "page2")
    leaflet() %>%
      addTiles() %>%
      setView(lng = -99.9018,
              lat = 39.3812,
              zoom = 4) %>%
      addMarkers(
        data = cities,
        lng = ~ lng,
        lat = ~ lat,
        popup = ~ name,
        layerId = ~ name
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
  
  # Render the dynamic bar graph for Page 1
  output$bar_graph <- renderPlot({
    req(current_page() == "page1")
    base_value <- cities$bar_value[cities$name == input$city_selector]
    data <- data.frame(
      Category = "Measured",
      Value = base_value,
      fill_color = "#3f6e91"
    )
    if ("Low Confidence" %in% input$confidence_checkboxes) {
      data <- rbind(
        data,
        data.frame(
          Category = "Low Confidence",
          Value = base_value * 0.9,
          fill_color = "#6c98ba"
        )
      )
    }
    if ("High Confidence" %in% input$confidence_checkboxes) {
      data <- rbind(
        data,
        data.frame(
          Category = "High Confidence",
          Value = base_value * 1.1,
          fill_color = "#6c98ba"
        )
      )
    }
    if ("10% Increase" %in% input$confidence_checkboxes) {
      data <- rbind(
        data,
        data.frame(
          Category = "10% Increase",
          Value = base_value * 1.1,
          fill_color = "#6c98ba"
        )
      )
    }
    data$Category <- factor(
      data$Category,
      levels = c(
        "Mortality",
        "Dementia",
        "Stroke"
      )
    )
    ggplot(data, aes(x = Category, y = Value, fill = fill_color)) +
      geom_bar(stat = "identity") +
      scale_fill_identity() +
      theme_minimal() +
      labs(x = NULL, y = "Value") +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, 100)
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
