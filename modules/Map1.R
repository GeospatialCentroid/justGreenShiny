# Map Module UI
mapUI <- function(id) {
  ns <- NS(id)

  card(
    id = "map_card",
    full_screen = TRUE,
    card_header("City Health & Vegetation Map"),
    leafletOutput(ns("map"), height = "85vh"),
    tags$div(
      class = "footer-banner",
      tags$img(src = "rojosLogo.png", height = "80px"),
      tags$span(
        "Rojos Lab - Geospatial Centroid",
        tags$br(),
        "Colorado State University © 2025"
      ),
      tags$img(
        src = "centroid_white_gray_logo_CROPPED.png",
        height = "60px"
      )
    )
  )
}

# Map Module Server
mapServer <- function(id, cityGPKG, cityCentroid, selected_city, map_selector) {
  moduleServer(id, function(input, output, session) {
    # Map palettes
    pal1 <- colorNumeric(
      palette = "BuGn",
      domain = as.numeric(cityGPKG$meanNDVI)
    )
    pal2 <- colorNumeric(
      palette = "PuBuGn",
      domain = as.numeric(cityGPKG$ls_Mortality_Rate)
    )
    pal3 <- colorNumeric(
      palette = "BuPu",
      domain = as.numeric(cityGPKG$ls_Stroke_Rate)
    )
    pal4 <- colorNumeric(
      palette = "OrRd",
      domain = as.numeric(cityGPKG$ls_Dementia_Rate)
    )
  
    # define popup elements 
    
    
    # Render initial map
    output$map <- renderLeaflet({
      
      # adjust order of the color palette
      
      leaflet() |>
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") |>
        addTiles(group = "OpenStreetMap") |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") |>
        setView(lng = -99.9018, lat = 39.3812, zoom = 3) |>
        addCircleMarkers(
          data = cityCentroid,
          group = "cityPoints",
          radius = 10,
          stroke = FALSE,
          layerId = ~fullCity,
          fillColor = ~ pal1(meanNDVI),
          fillOpacity = 0.8,
          popup = ~popup,
          label = ~fullCity
        ) |>
        addPolygons(
          data = cityGPKG,
          group = "cityPoly",
          fillColor = ~ pal1(meanNDVI),
          color = "black",
          weight = 0.5,
          layerId = ~fullCity,
          fillOpacity = 0.8,
          popup = ~popup,
          label = ~fullCity
        ) |>
        addLayersControl(
          baseGroups = c("CartoDB", "OpenStreetMap", "Esri Imagery"),
          options = layersControlOptions(collapsed = FALSE)
        ) |>
        addLegend(
          pal = pal1,
          values = cityGPKG$meanNDVI,
          title = "NDVI",
          layerId = "legend",
          na.label = ""
        )
    })

    # Update map colors based on selector
    observeEvent(
      map_selector(),
      {
        proxy <- leafletProxy("map")
        proxy |> clearGroup("cityPoly") |> clearGroup("cityPoints")

        palette_info <- switch(
          map_selector(),
          "Current Vegetation Levels" = list(
            pal = pal1,
            col = "meanNDVI",
            title = "Greenness level (NDVI)"
          ),
          "Lives Saved" = list(
            pal = pal2,
            col = "ls_Mortality_Rate",
            title = "Lives Save per 100,000"
          ),
          "Stroke Cases Prevented" = list(
            pal = pal3,
            col = "ls_Stroke_Rate",
            title = "Stroke Cases Prevented per 100,000"
          ),
          "Dementia Cases Prevented" = list(
            pal = pal4,
            col = "ls_Dementia_Rate",
            title = "Dementia Cases Prevented per 100,000"
          )
        )

        proxy |>
          removeControl(layerId = "legend") |>
          addCircleMarkers(
            data = cityCentroid,
            group = "cityPoints",
            layerId = ~fullCity,
            radius = 10,
            stroke = FALSE,
            fillColor = ~ palette_info$pal(get(palette_info$col)),
            fillOpacity = 0.8,
            popup = ~popup,
            label = ~fullCity
          ) |>
          addPolygons(
            data = cityGPKG,
            group = "cityPoly",
            layerId = ~fullCity,
            fillColor = ~ palette_info$pal(get(palette_info$col)),
            color = "black",
            weight = 0.5,
            fillOpacity = 0.8,
            popup = ~popup,
            label = ~fullCity
          ) |>
          addLegend(
            pal = palette_info$pal,
            values = cityGPKG[[palette_info$col]][
              !is.na(cityGPKG[[palette_info$col]])
            ],
            title = palette_info$title,
            layerId = "legend"
          )
      },
      ignoreInit = TRUE
    )

    # Zoom behavior
    observe({
      req(input$map_zoom)
      proxy <- leafletProxy("map")

      if (input$map_zoom >= 9) {
        proxy |> showGroup("cityPoly") |> hideGroup("cityPoints")
      } else {
        proxy |> showGroup("cityPoints") |> hideGroup("cityPoly")
      }
    })

    # Highlight selected city
    observeEvent(
      selected_city(),
      {
        req(selected_city())
        proxy <- leafletProxy("map")
        proxy |> removeShape("highlight_shape")

        if (selected_city() != "") {
          selected_city_coords <- cityCentroid[
            cityCentroid$fullCity == selected_city(),
          ] |>
            sf::st_coordinates()

          longitude <- as.numeric(selected_city_coords[1, "X"])
          latitude <- as.numeric(selected_city_coords[1, "Y"])

          proxy |> setView(lng = longitude, lat = latitude, zoom = 11)

          selected_polygon <- cityGPKG[cityGPKG$fullCity == selected_city(), ]

          proxy |>
            addPolygons(
              data = selected_polygon,
              layerId = "highlight_shape",
              color = "#FCAF62",
              weight = 3,
              group = "highlight",
              fillOpacity = 0
            )
        }
      },
      ignoreInit = TRUE
    )

    # Return clicked city (either shape or marker)
    clicked_city <- reactive({
      if (!is.null(input$map_shape_click)) {
        return(input$map_shape_click$id)
      } else if (!is.null(input$map_marker_click)) {
        return(input$map_marker_click$id)
      }
      return(NULL)
    })

    # IMPORTANT: Return the reactive
    return(clicked_city)
  })
}
