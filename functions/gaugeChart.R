gaugePlot <- function(
    selectedRate,
    valueRange,
    colorPalette,
    title = "NDVI", 
    height = 250, 
    width = 700
) {
  # Dynamic Step and Color Generation
  num_steps <- length(colorPalette)
  breaks <- seq(
    from = valueRange[1],
    to = valueRange[2],
    length.out = num_steps + 1
  )
  gauge_steps <- lapply(1:num_steps, function(i) {
    list(range = c(breaks[i], breaks[i + 1]), color = colorPalette[i])
  })
  
  # Build the Plot
  fig <- plot_ly(
    type = "indicator",
    mode = "gauge",
    value = selectedRate,
    
    # Use full container space
    domain = list(x = c(0, 1), y = c(0, 1)), 
    
    gauge = list(
      shape = "bullet",
      axis = list(range = valueRange),
      steps = gauge_steps,
      bar = list(thickness = 0), # Hide the bar
      threshold = list(
        value = selectedRate,
        thickness = 0.8, 
        line = list(color = "#E69F00", width = 5) 
      )
    )
  ) %>%
    layout(
      height = height,
      
      # --- MARGIN FIX ---
      # Increased 'b' (bottom) from 15 to 30 to make room for axis numbers
      margin = list(l = 15, r = 15, t = 15, b = 30),
      
      # Transparent background
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
  
  return(fig)
}