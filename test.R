# Load libraries
library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
library(DT)
library(shinyBS)
library(RColorBrewer)
library(bslib)

# Parameters and datasets
zoom_switch <- 9
circleRadius <- 10
cityGPKG <- sf::st_read("data/top200_simple.gpkg")
cityDF <- read.csv("data/top200.csv")
cityCentroid <- sf::st_read("data/top200_centroid.gpkg")

# Source functions and modules
source("functions/gaugeChart.R")
source("modules/Map1.R")
source("modules/selector1.R")
source("modules/gauge1.R")

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  # --- Custom CSS for banners and layout ---
  includeCSS("www/styles.css"),

  # page 1  -----------------------------------------------------------------
  navset_card_underline(
    id = "navbar",
    title = div(
      img(src = "CSU-Symbol-r-w.png", height = "50px"),
      tags$span(
        "GreenGuide",
        style = "font-size: 24px; font-weight: bold; margin-left: 10px;"
      )
    ),

    nav_panel(
      title = "City Overview",
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          selectInput(
            inputId = "citySelect",
            label = "Select a city",
            choices = c(
              "Select a city",
              cityDF$fullCity
            )
          ),
          selectInput(
            inputId = "mapSelector",
            label = "Map Display Options",
            choices = c(
              "Current Vegetation Levels",
              "Lives Saved",
              "Stroke Cases Prevented",
              "Dementia Cases Prevented"
            )
          ),
          # City info module UI
          cityInfoUI("cityInfo"),
          tags$hr(),
          actionButton("reset_view", "Reset Map View")
        ),
        # Map module UI
        mapUI("map"),
        # Gauge module UI
        gaugeUI("gauge")
      )
    ),

    # page 2 ------------------------------------------------------------------
    nav_panel(
      title = "Census Tract Detail",
      p("Census tract level details will go here")
    ),

    # page 3 ------------------------------------------------------------------
    nav_panel(
      title = "About",
      p("About page content")
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # Reactive values for communication between modules
  selected_city <- reactiveVal("")

  # Update selected city from input
  observeEvent(input$citySelect, {
    if (input$citySelect != "Select a city") {
      selected_city(input$citySelect)
    }
  })

  # Map module - returns clicked city
  clicked_city <- mapServer(
    "map",
    cityGPKG = cityGPKG,
    cityCentroid = cityCentroid,
    selected_city = selected_city,
    map_selector = reactive(input$mapSelector)
  )

  # Update selected city from map click
  observeEvent(clicked_city(), {
    req(clicked_city())
    selected_city(clicked_city())
    updateSelectInput(session, "citySelect", selected = clicked_city())
  })

  # City info module
  cityInfoServer(
    "cityInfo",
    cityDF = cityDF,
    selected_city = selected_city
  )

  # Gauge module
  gaugeServer(
    "gauge",
    cityDF = cityDF,
    cityGPKG = cityGPKG,
    selected_city = selected_city,
    map_selector = reactive(input$mapSelector)
  )

  # Reset map view
  observeEvent(input$reset_view, {
    selected_city("")
    updateSelectInput(session, "citySelect", selected = "Select a city")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
