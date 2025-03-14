#' Plot precipitation time series
#'
#' Creates a time series plot from a data frame of precipitation data 
#' as returned by `cv_basin_daily_precip()`. Basic statistics (mean, standard deviation, and proportion of zero values),
#' are added to the title of the plot.
#'
#' @param data A data frame with two columns: \code{date} (date or datetime) and  
#' \code{precipitation} (numeric) as returned by `cv_basin_daily_precip()`.
#' @return A ggplot object displaying the precipitation time series with summary 
#' statistics in the title. The returned plots look best when saved at the size 
#' 16.5 x 6 cm You can easily change the font sizes using theme().
#' @examples
#' cv_plot_TS(eg_TS)
#' @import ggplot2
#' @importFrom stats sd
#' @seealso \code{\link{cv_basin_daily_precip}} \code{\link{cv_plot_prob}} \code{\link{cv_plot_season}}
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


