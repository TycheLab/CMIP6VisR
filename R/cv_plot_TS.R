#' Plot Precipitation Time Series
#'
#' This function takes a data frame containing a precipitation time series, 
#' calculates basic statistics (mean, standard deviation, and proportion of zero values),
#' and generates a ggplot visualization.
#'
#' @param data A data frame with two columns: "date" (date or datetime) and 
#'             "precipitation" (numeric).
#' @return A ggplot object displaying the precipitation time series with summary statistics in the title.
#' @examples
#' file_path <- system.file("extdata", "eg_TS.rds", package = "CMIP6VisR")
#' data <- readRDS(file_path)
#' cv_plot_TS(data)
#' #Recommended save size
#' ggsave("plot_TS.jpg", height = 6, width = 16.5, units = "cm", dpi = 400)
#' @import ggplot2
#' @export
cv_plot_TS <- function(data) {
  
   
  # Ensure correct column names
  if (!all(c("date", "precipitation") %in% colnames(data))) {
    stop("Data frame must contain columns named 'date' and 'precipitation'.")
  }
  
  # Compute statistics
  mean_val <- mean(data$precipitation, na.rm = TRUE)
  sd_val <- sd(data$precipitation, na.rm = TRUE)
  p0 <- sum(data$precipitation == 0, na.rm = TRUE) / nrow(data)
  
  # Generate plot
  plot <- ggplot(data, aes(x = date, y = precipitation), color = gray(0.2, linewidth = 0.05)) + 
    geom_line(color = gray(0.2)) + 
    ylab("Precipitation") + 
    xlab("Date") +
    ggtitle(paste0("mean = ", round(mean_val, 2), 
                   ", sd = ", round(sd_val, 2), 
                   ", and P0 = ", round(p0, 2))) + 
    theme(
      axis.title.x = element_text(size = 9, colour = gray(0.25)),
      axis.title.y = element_text(size = 9, colour = gray(0.25)),
      plot.title = element_text(hjust = 0, size = 9.5),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = gray(0.25), size = 0.3),
      axis.text.x = element_text(size = 8, colour = gray(0.25)),
      axis.text.y = element_text(size = 8, colour = gray(0.25)),
      plot.margin = unit(c(3, 3, 0.5, 0.5), "mm"),
      axis.ticks.length.x = unit(-0.5, "mm"),
      axis.ticks.length.y = unit(-0.5, "mm"),
      axis.ticks = element_line(color = gray(0.25), size = 0.3),
      text = element_text(color = gray(0.25))
    )
  
  return(plot)
}


