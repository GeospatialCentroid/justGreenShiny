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

# read in summary html data 
citySummary <- readRDS("data/citySummary.rds")


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
  includeCSS("www/styles.css"),
  
  # page 1  -----------------------------------------------------------------
  navset_card_pill(
    id = "navbar",
    
    # --- UPDATED TITLE SECTION ---
    title = div(
      style = "display: flex; align-items: center;", 
      
      # 1. Logo
      img(src = "CSU-Symbol-r-K.png", style = "height: 80px; width: auto; margin-right: 15px;"),
      
      # 2. Text Block
      div(
        style = "display: flex; flex-direction: column; justify-content: center;",
        
        # Main Title (Changed color to Dark Grey)
        tags$span(
          "JustGreen",
          class = "app-main-title"
          ),
        
        # Subtitle (Reduced size)
        tags$span(
          "The health impacts of green spaces on the 200",
          tags$br(),
          "most populated cities in the USA",
          class = "app-subtitle"
          )
      )
    ),
    # -----------------------------
    
    nav_panel(
      title = "200 City Overview",
      layout_sidebar(
        sidebar = sidebar(
          width = "30%",
          selectInput(
            inputId = "citySelect",
            label = "Select a city",
            choices = c(
              "Select a city",
              sort(cityDF$fullCity)
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
          cityInfoUI("cityInfo"),
          gaugeUI("gauge")
        ),
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
          tractInfoUI("tractInfo"),
          tags$hr(), 
          downloadButton("downloadReport", "Download Report", class = "btn-primary w-100")
        ),
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

  # Page 1 - 200 City Overview ------------------------------------------------

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
  # --- UPDATED DOWNLOAD HANDLER ---
  output$downloadReport <- downloadHandler(
    filename = function() {
      req(selected_city())
      clean_name <- gsub(" ", "_", selected_city())
      paste0(clean_name, "_Report.html")
    },
    content = function(file) {
      req(selected_city(), selected_city() != "Select a city")
      
      # Start the progress bar
      shiny::withProgress(
        message = 'Generating Report',
        detail = 'Initializing...',
        value = 0, {
          
          # Wait 1 second
          Sys.sleep(1)
          
          # Step 1: Look up data
          incProgress(0.3, detail = paste("Retrieving data for", selected_city(), "..."))
          Sys.sleep(1) # Wait 1 second
          
          # Use the pre-loaded 'citySummary' object
          if (selected_city() %in% names(citySummary)) {
            report_content <- citySummary[[selected_city()]]
          } 
          else if (is.data.frame(citySummary)) {
            # Update 'html_content' to your actual column name
            report_content <- citySummary$html_content[citySummary$fullCity == selected_city()]
          } else {
            report_content <- NULL
          }
          
          # Step 2: Finalize
          incProgress(0.7, detail = "Finalizing download...")
          Sys.sleep(1) # Wait 1 second
          
          # Step 3: Write to file
          if (!is.null(report_content) && length(report_content) > 0) {
            writeLines(as.character(report_content), file)
          } else {
            writeLines("<h1>Report not available for this city</h1>", file)
          }
        }
      ) # End withProgress
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
