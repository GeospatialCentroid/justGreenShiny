# Load libraries
library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
library(DT)
library(shinyBS)
library(RColorBrewer)
library(bslib)

# Parameters 
zoom_switch <- 9
circleRadius <- 10
# datasets for the landing page 
cityGPKG <- readRDS("data/citiesGPKG.rds")
healthData <- readRDS("data/healthData.rds")
cityDF <- healthData[[1]]
# might want to change to read in on census track page 
tractsDF <- healthData[[2]]
# appended data to the gpkg 



# cityGPKG <- sf::st_read("data/top200_simple.gpkg")
# cityDF <- read.csv("data/top200.csv")
cityCentroid <- readRDS("data/centroidGPKG.rds")

# Source functions and modules
source("functions/gaugeChart.R")
source("modules/Map1.R")
source("modules/selector1.R")
source("modules/gauge1.R")
source("modules/Map2.R")
source("modules/tractInfo.R")

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(
    version = 5, 
    preset = "minty", 
    primary = "#1E4D2B",
    secondary = "#558B6E"
  ),
  # --- Custom CSS for banners and layout ---
  includeCSS("www/styles.css"),

  # page 1  -----------------------------------------------------------------
  navset_card_pill(
    id = "navbar",
    title = div(
      img(src = "CSU-Symbol-r-K.png",style = "height: 100px; width: auto;" ),
      tags$span(
        "JustGreen",
        style = "font-size: 24px; font-weight: bold; margin-left: 10px;"
      )
    ),

    nav_panel(
      title = "Country Overview",
      layout_sidebar(
        sidebar = sidebar(
          width = "30%",
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
          # Gauge module UI
          gaugeUI("gauge")
        ),
        # Map module UI
        mapUI("map")
      )
    ),

    # page 2 ------------------------------------------------------------------
    nav_panel(
      title = "City Review",
      layout_sidebar(
        sidebar = sidebar(
          width = "30%",
          selectInput(
            inputId = "citySelect2",
            label = "Select a city",
            choices = c(
              "Select a city",
              cityDF$fullCity
            )
          ),
          selectInput(
            inputId = "tractMetric",
            label = "Display Metric",
            choices = c(
              "Current Vegetation Levels",
              "Lives Saved",
              "Stroke Cases Prevented",
              "Dementia Cases Prevented",
              "Social Vulnerability (RPL)"
            )
          ),
          # Tract info module UI
          tractInfoUI("tractInfo")
        ),
        # Tract map module UI
        tractMapUI("tractMap")
      )
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
  # Shared reactive value for selected city across both pages
  selected_city <- reactiveVal("")

  # Page 1 - Country Overview ------------------------------------------------

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

  # Page 2 - City Review ------------------------------------------

  selected_tract <- reactiveVal("")

  # Update Page 2 city selector when selected_city changes
  observe({
    if (selected_city() != "") {
      updateSelectInput(session, "citySelect2", selected = selected_city())
    }
  })

  # Update selected city from Page 2 input
  observeEvent(input$citySelect2, {
    if (input$citySelect2 != "Select a city") {
      selected_city(input$citySelect2)
    }
  })

  # Tract map module - returns clicked tract and tract data
  tract_map_return <- tractMapServer(
    "tractMap",
    selected_city = selected_city,
    cityGPKG = cityGPKG,
    tractsDF = tractsDF,
    tract_metric = reactive(input$tractMetric),
    active_tab = reactive(input$navbar)
  )

  # Update selected tract from map click
  observeEvent(tract_map_return$clicked_tract(), {
    req(tract_map_return$clicked_tract())
    selected_tract(tract_map_return$clicked_tract())
  })
  # Tract info module
  tractInfoServer(
    "tractInfo",
    selected_city = selected_city,
    selected_tract = selected_tract,
    tract_data = tract_map_return$tract_data
  )
}

# Run the application
shinyApp(ui = ui, server = server)
