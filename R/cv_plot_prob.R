#' Plot Probability of Exceedance for Precipitation
#'
#' This function takes a data frame containing a precipitation time series, 
#' filters out zero precipitation values, calculates the probability of exceedance, 
#' and generates a scatter plot with a logarithmic y-axis.
#'
#' @param data A data frame with two columns: "date" (date or datetime) and 
#'             "precipitation" (numeric).
#' @return A ggplot object displaying the probability of exceedance of nonzero precipitation.
#'          The returned plots look best when saved at the size 16.5 x 12 cm.
#'          You can easily change the font sizes using theme().
#' @examples
#' cv_plot_prob(eg_TS)
#' @import ggplot2
#' @import dplyr
#' @import lubridate
#' @import scales
#' @export
cv_plot_prob <- function(data, variable = "precipitation") {
  
  value <- precipitation <- p_e <- NULL
  
  #search for the variable
  var_lower <- tolower(variable)
  if (!startsWith(var_lower, "p") && !startsWith(var_lower, "t")) {
    stop("The variable argument must start with 'p' for precipitation or 't' for temperature.")
  }
  
  #bring the variable to the required name
  if (startsWith(var_lower, "p")) {
    variable <- "precipitation"
  } else if (startsWith(var_lower, "t")) {
    variable <- "temperature"
  } else {
    stop("The variable argument must start with 'p' for precipitation or 't' for temperature.")
  }
  
  colnames(data)[2] <- "value"
  
  
  # Filter out zero precipitation values
  data_nz <- if (variable == "precipitation") {
    filter(data, value != 0)
  } else {
    data
  }
  # Calculate probability of exceedance
  data_nz$rank <- rank(data_nz$value)
  data_nz$p_ne <- data_nz$rank / (nrow(data_nz) + 1)
  data_nz$p_e <- 1 - data_nz$p_ne
  
  # Generate plot
  plot <- ggplot() +
    geom_point(data = data_nz, aes(x = value, y = p_e), size = 0.5) + 
    scale_y_log10(labels = scales::label_comma()) + 
    ylab("Probability of exceedance") + 
    xlab("Value") +
    theme(
      axis.title.x = element_text(size = 9, colour = "gray25"),
      axis.title.y = element_text(size = 9, colour = "gray25"),
      plot.title = element_text(hjust = 0, size = 9.5),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "gray25", linewidth = 0.3),
      axis.text.x = element_text(size = 8, colour = "gray25"),
      axis.text.y = element_text(size = 8, colour = "gray25"),
      plot.margin = unit(c(3, 3, 0.5, 0.5), "mm"),
      axis.ticks.length.x = unit(-0.5, "mm"),
      axis.ticks.length.y = unit(-0.5, "mm"),
      axis.ticks = element_line(color = "gray25", size = 0.3),
      text = element_text(color = "gray25")
    )
  
  return(plot)
}
