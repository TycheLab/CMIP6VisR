#' Plot Precipitation Time Series
#'
#' This function takes a data frame containing a precipitation time series, 
#' calculates basic statistics (mean, standard deviation, and proportion of zero values),
#' and generates a ggplot visualization.
#'
#' @param data A data frame with two columns: "date" (date or datetime) and 
#'             "precipitation" (numeric).
#' @return A ggplot object displaying the precipitation time series with summary statistics in the title.
#'          Note: The recommended aspect ratio and size for saving the plots,
#'          based on the dimensions specified in the plot function, are a width of 16.5 cm and a height of 6 cm.
#' @examples
#' data("eg_TS")
#' cv_plot_TS(eg_TS)
#' @import ggplot2
#' @importFrom stats sd
#' @export
cv_plot_TS <- function(data) {
  
  precipitation <- mean_val <- sd_val <-  p0  <- NULL
  
   
  # Ensure correct column names
  if (!all(c("date", "precipitation") %in% colnames(data))) {
    stop("Data frame must contain columns named 'date' and 'precipitation'.")
  }
  
  # Compute statistics
  mean_val <- mean(data$precipitation, na.rm = TRUE)
  sd_val <- sd(data$precipitation, na.rm = TRUE)
  p0 <- sum(data$precipitation == 0, na.rm = TRUE) / nrow(data)
  
  # Generate plot
  plot <- ggplot(data, aes(x = date, y = precipitation), linewidth = 0.05) + 
    geom_line(color = "gray20") + 
    ylab("Precipitation") + 
    xlab("Date") +
    ggtitle(paste0("mean = ", round(mean_val, 2), 
                   ", sd = ", round(sd_val, 2), 
                   ", and P0 = ", round(p0, 2))) + 
    theme(
      axis.title.x = element_text(size = 9, colour = "gray25"),
      axis.title.y = element_text(size = 9, colour = "gray25"),
      plot.title = element_text(hjust = 0, size = 9.5),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "gray25", size = 0.3),
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


