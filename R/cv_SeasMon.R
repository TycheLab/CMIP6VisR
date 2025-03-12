#' Plot Seasonal Precipitation Distribution
#'
#' This function takes a data frame with a precipitation time series, assigns seasons based on months, 
#' computes the proportion of zero precipitation values (P0) and seasonal means for nonzero precipitation, 
#' and generates a violin plot showing the seasonal distribution of precipitation.
#'
#' @param data A data frame with two columns: "date" (date or datetime) and 
#'             "precipitation" (numeric).
#' @return A ggplot object displaying the seasonal distribution of nonzero precipitation, 
#'         with mean values and P0 labeled.
#' @examples
#' \dontrun{
#' data <- readRDS("data/eg_TS.rds")
#' colnames(data) <- c("date", "precipitation")
#' cv_SeasMon(data)
#' #recommended save size
#' gggsave("plot_seasALL.jpg", height = 14, width = 16.5, units = "cm", dpi = 400)
#' }
#' @import ggplot2
#' @import dplyr
#' @importFrom lubridate month
#' @importFrom ggpubr ggarrange
#' @export
cv_SeasMon <- function(data) {
  
  
  # Ensure correct column names
  if (!all(c("date", "precipitation") %in% colnames(data))) {
    stop("Data frame must contain columns named 'date' and 'precipitation'.")
  }
  
  # Extract month and assign seasons
  data$month <- lubridate::month(data$date)
  data <- data %>% 
    dplyr::mutate(season = case_when(
      month %in% c(9, 10, 11) ~ "Fall",
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer"
    ))
  
  # Compute proportion of zero precipitation (P0)
  p0_seas <- data %>% 
    dplyr::group_by(season) %>% 
    dplyr::summarise(p0 = sum(precipitation == 0, na.rm = TRUE) / n())
  
  # Filter out zero precipitation values
  data_nz <- data %>% dplyr::filter(precipitation != 0)
  
  # Compute seasonal mean for nonzero precipitation
  mean_seas <- data_nz %>% 
    dplyr::group_by(season) %>% 
    dplyr::summarise(mean_val = mean(precipitation, na.rm = TRUE))
  
  # Compute seasonal mean for nonzero precipitation
  tot_mon <- data_nz %>% 
    dplyr::group_by(month) %>% 
    dplyr::summarise(tot_val = sum(precipitation, na.rm = TRUE))
  
  # Generate plot
  plot1 <- ggplot() + 
    geom_violin(data = data_nz, aes(x = season, y = precipitation, fill = season), linewidth = 0.2) + 
    geom_point(data = mean_seas, aes(x = season, y = mean_val), shape = 4) +
    geom_text(data = mean_seas, aes(x = season, y = mean_val, 
                                    label = round(mean_val, 2)), vjust = -0.8, size = 2.2) + 
    geom_label(data = p0_seas, aes(x = season, y = max(data_nz$precipitation), 
                                   label = paste0("P0 = ", round(p0, 2))), size = 2.2) + 
    scale_fill_manual(values = c("Summer" = "#3da83d",
                                 "Spring" = "#FFC3A0",
                                 "Fall" = "#9B2335",
                                 "Winter" = "#B0E0E6")) +
    ylab("Seasonal average nonzero precipitation") +
    xlab("Season") +
    theme(
      legend.text = element_text(size = 7),
      axis.title.x = element_text(size = 9, colour = gray(0.25)),
      axis.title.y = element_text(size = 9, colour = gray(0.25)),
      legend.title = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = gray(0.25), size = 0.3),
      axis.text.x = element_text(size = 8, colour = gray(0.25)),
      axis.text.y = element_text(size = 8, colour = gray(0.25)),
      plot.title = element_text(hjust = 0),
      plot.margin = unit(c(3,3,0.5,0.5), "mm"),
      axis.ticks.length.x = unit(-0.5, "mm"),
      axis.ticks.length.y = unit(-0.5, "mm"),
      axis.ticks = element_line(color = gray(0.25), size = 0.3),
      legend.position = 'none',
      legend.spacing.x = unit(3.5, "mm"),
      legend.spacing.y = unit(0.1, "mm"),
      strip.background = element_blank(),
      legend.key.size = unit(3, "mm"),
      legend.key.width = unit(3, "mm"),
      legend.key = element_rect(fill = "transparent"),
      text = element_text(color = gray(0.25)),
      legend.box.margin = margin(0.5, 0.5, 0.5, 0.5)
    )
  plot1
  
  plot2 <- ggplot() + 
    geom_col(data = tot_mon, aes(x = as.factor(month), y = tot_val),
             linewidth = 0.2, fill = "skyblue") + 
    ylab("Monthly total nonzero precipitation") +
    xlab("Month") +
    theme(
      legend.text = element_text(size = 7),
      axis.title.x = element_text(size = 9, colour = gray(0.25)),
      axis.title.y = element_text(size = 9, colour = gray(0.25)),
      legend.title = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = gray(0.25), size = 0.3),
      axis.text.x = element_text(size = 8, colour = gray(0.25)),
      axis.text.y = element_text(size = 8, colour = gray(0.25)),
      plot.title = element_text(hjust = 0),
      plot.margin = unit(c(3,3,0.5,0.5), "mm"),
      axis.ticks.length.x = unit(-0.5, "mm"),
      axis.ticks.length.y = unit(-0.5, "mm"),
      axis.ticks = element_line(color = gray(0.25), size = 0.3),
      legend.position = 'none',
      legend.spacing.x = unit(3.5, "mm"),
      legend.spacing.y = unit(0.1, "mm"),
      strip.background = element_blank(),
      legend.key.size = unit(3, "mm"),
      legend.key.width = unit(3, "mm"),
      legend.key = element_rect(fill = "transparent"),
      text = element_text(color = gray(0.25)),
      legend.box.margin = margin(0.5, 0.5, 0.5, 0.5)
    )
  plot2
  
  gg <- ggpubr::ggarrange(plot1, plot2, nrow = 2, ncol = 1)
  return(gg)
}


