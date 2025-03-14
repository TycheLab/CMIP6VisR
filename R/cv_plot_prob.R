#' Plot probability of exceedance for precipitation
#'
#' Plots the exceedance probabilities of non-zero values in a precipitation time 
#' series as returned by `cv_basin_daily_precip()`, vs the precipitation value. 
#' Uses a logarithmic scale for the y-axis.
#'
#' @param data A data frame with two columns: \code{date} (date or datetime) and  
#' \code{precipitation} (numeric) as returned by `cv_basin_daily_precip()`.
#' @return A ggplot object displaying the probability of exceedance of nonzero 
#' precipitation. The returned plots look best when saved at the size 
#' 16.5 x 12 cm. You can easily change the font sizes using theme().
#' @examples
#' cv_plot_prob(eg_TS)
#' @import ggplot2
#' @import dplyr
#' @import lubridate
#' @import scales
#' @export
#' @seealso \code{\link{cv_basin_daily_precip}} \code{\link{cv_plot_TS}} \code{\link{cv_plot_season}}

cv_plot_prob <- function(data) {
  
  precipitation <- p_e <- NULL
  
  # Ensure correct column names
  if (!all(c("date", "precipitation") %in% colnames(data))) {
    stop("Data frame must contain columns named 'date' and 'precipitation'.")
  }
  
  # Filter out zero precipitation values
  data_nz <- data %>% filter(precipitation != 0)
  
  # Calculate probability of exceedance using the 
  data_nz$rank <- rank(data_nz$precipitation)
  data_nz$p_ne <- data_nz$rank / (nrow(data_nz) + 1)
  data_nz$p_e <- 1 - data_nz$p_ne
  
  # Generate plot
  plot <- ggplot() +
    geom_point(data = data_nz, aes(x = precipitation, y = p_e), size = 0.5) + 
    scale_y_log10(labels = scales::label_comma()) + 
    ylab("Probability of exceedance") + 
    xlab("Precipitation") +
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
