# # testing 
# cityGPKG <- sf::st_read("data/top200_simple.gpkg")
# cityDF <- as.data.frame(cityGPKG)
# 
# selectedData1 <- cityDF[1, ]
# 
# selectedRate <- round(selectedData1$meanNDVI[1],2)
# valueRange <- round(range(cityGPKG$meanNDVI), 2)
# colorPalette <- brewer.pal(n = 8,name = "BuGn")

# average_value = round(mean(cityDF$meanNDVI, na.rm = TRUE), 2)


gaugePlot <- function(selectedRate, valueRange, colorPalette, title = "NDVI", height = 250, width = 700) {
  
  # Dynamic Step and Color Generation
  num_steps <- length(colorPalette)
  breaks <- seq(from = valueRange[1], to = valueRange[2], length.out = num_steps + 1)
  gauge_steps <- lapply(1:num_steps, function(i) {
    list(range = c(breaks[i], breaks[i+1]), color = colorPalette[i])
  })
  
  # Build the Plot
  fig <- plot_ly(
    type = "indicator",
    mode = "gauge",
    value = selectedRate,
    
    # --- CHANGE 1: Move the gauge element up to reduce space ---
    domain = list(y = c(0, 0.9)), # Was c(0, 0.8)
    
    gauge = list(
      shape = "bullet",
      axis = list(range = valueRange),
      steps = gauge_steps,
      bar = list(thickness = 0),
      threshold = list(
        value = selectedRate,
        thickness = 0.9,
        line = list(color = "orange", width = 4)
      )
    )
  ) %>%
    layout(
      title = list(
        text = title,
        y = 0.95,
        x = 0.5,
        xanchor = 'center',
        yanchor = 'top',
        font = list(size = 22)
      ),
      height = height,
      # width = width,
      # --- CHANGE 2: Reduce the top margin for a tighter fit ---
      margin = list(t = 40, b = 40) # Was t = 80
    )
  
  return(fig)
}

