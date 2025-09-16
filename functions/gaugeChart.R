# # testing 
# cityGPKG <- sf::st_read("data/top200_simple.gpkg")
# cityDF <- as.data.frame(cityGPKG)
# 
# selectedData1 <- cityDF[1, ]
# 
# selectedRate <- round(selectedData1$meanNDVI[1],2)
# average_value = round(mean(cityDF$meanNDVI, na.rm = TRUE), 2)
# range_min =  round(min(cityDF$meanNDVI, na.rm = TRUE), 2)
# range_max = round(max(cityDF$meanNDVI, na.rm = TRUE),2)
# 
# 


# --- 3. A reusable function to create the gauge chart ---
create_gauge_chart <- function(city_name, city_value, average_value, range_min, range_max, palette) {
  
  # Helper function to convert a value to coordinates on the gauge's arc
  value_to_coords <- function(value) {
    # Map value to an angle from 180 (left) to 0 (right) degrees
    angle_deg <- 180 * (1 - (value - range_min) / (range_max - range_min))
    angle_rad <- angle_deg * pi / 180
    
    # Calculate x and y based on a semi-circle with radius 0.4 centered at (0.5, 0.1)
    x_coord <- 0.5 + 0.36 * cos(angle_rad)
    y_coord <- 0.1 + 0.36 * sin(angle_rad)
    return(list(x = x_coord, y = y_coord))
  }
  
  # Get coordinates for the orange circle
  coords <- value_to_coords(city_value)
  
  # Create a color palette for the background gradient
  # gradient_colors <- colorRampPalette(c("#E0F2F7", "#2ca25f"))(11)
  gradient_colors <- palette
  
  # Calculate the total range and the width of each step
  total_range <- range_max - range_min
  step_width <- total_range / length(gradient_colors)
  
  fig <- plot_ly(
    type = "indicator",
    mode = "gauge",
    value = city_value, # This value is used for the gauge background fill
    gauge = list(
      shape = "bullet",  # This is the key argument for a horizontal gauge
      axis = list(
        range = list(range_min, range_max),
        tickwidth = 0, # Hide ticks
        showticklabels = FALSE
      ),
      bar = list(color = "white", thickness = 0), # Hide the default value bar
      steps = lapply(0:10, function(i) {
        list(
          range = c(range_min + (i*step_width), range_min + (i+ 1) * step_width) ,
          color = gradient_colors[i + 1]
        )
      })
    )
  ) |>
    # Add the custom orange circle marker
    add_trace(
      type = 'scatter',
      mode = 'markers',
      x = list(coords$x),
      y = list(coords$y),
      marker = list(color = '#F39C12', size = 20, symbol = 'Line', line = list(width=2, color="white")),
      showlegend = FALSE,
      hoverinfo = 'none', # Disable hover text for the circle
      layer = "above"
    ) %>%
    layout(
      paper_bgcolor = 'white',
      plot_bgcolor = 'white',
      # Set a fixed aspect ratio and hide the axes
      xaxis = list(range = c(0, 1), showgrid = F, zeroline = F, showticklabels = F),
      yaxis = list(range = c(0, 1), showgrid = F, zeroline = F, showticklabels = F),
      shapes = list(), # Clear any default shapes
      # Add all the custom text labels
      annotations = list(
        # City Name and Value
        list(x = 0.5, y = 0.95, text = paste("<b>", city_name, "</b>"), showarrow = F, xanchor = 'center', yanchor = 'middle', font = list(size=18)),
        list(x = 0.5, y = 0.25, text = paste0("<b>", city_value, "</b>"), showarrow = F, xanchor = 'center', yanchor = 'middle', font = list(size=24)),
        list(x = 0.5, y = 0.15, text = paste0("Avg: ", average_value), showarrow = F, xanchor = 'center', yanchor = 'middle', font = list(size=14, color="grey")),
        
        # Range labels
        list(x = 0.1, y = 0.05, text = paste0(range_min), showarrow = F, xanchor = 'center', yanchor = 'middle', font = list(size=14)),
        list(x = 0.9, y = 0.05, text = paste0(range_max), showarrow = F, xanchor = 'center', yanchor = 'middle', font = list(size=14))
      )
    )
  
  return(fig)
}

# testing 


# 
# # generate the plot with NDVI values first 
# create_gauge_chart(
#   city_name = "Bamba",
#   city_value = 5050,
#   average_value = 6200,
#   range_min = 100,
#   range_max = 10000,
#   palette = colorRampPalette(c("#E0F2F7", "#23225f"))(11)
# )


