#' Plot Seasonal Distribution of a Hydroclimatic Variable
#'
#' This function takes a data frame with a hydroclimatic time series (precipitation or temperature), assigns seasons based on months, 
#' computes the proportion of zero precipitation values (P0) and seasonal means for nonzero values, 
#' and generates a violin plot showing the seasonal distribution.
#'
#' @param data A data frame with two columns: "date" (date or datetime) and a variable column (e.g., "precipitation" or "temperature").
#' @param variable A character string, either "precipitation" or "temperature". Determines plotting behavior and calculation of P0.
#' @return A ggplot object showing seasonal distribution with summary values and optional P0.
#' @examples
#' cv_plot_season(data, variable = "precipitation")
#' @import ggplot2
#' @importFrom dplyr filter mutate group_by summarise n
#' @importFrom lubridate month year
#' @importFrom ggpubr ggarrange
#' @export
cv_plot_season <- function(data, variable = "precipitation") {
  
  value <- precipitation <- season <- mean_val <- p0 <- tot_val <- NULL
  
  if (!variable %in% c("precipitation", "temperature")) {
    stop("The variable argument must be either 'precipitation' or 'temperature'.")
  }
  
  if (!all(c("date", variable) %in% colnames(data))) {
    stop(paste("Data frame must contain 'date' and", variable, "columns."))
  }
  
  colnames(data)[2] <- "value"
  data$month <- month(data$date)
  data$year <- year(data$date)
  
  data <- data %>% 
    mutate(season = case_when(
      month %in% c(9, 10, 11) ~ "Fall",
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer"
    ))
  
  if (variable == "precipitation") {
    p0_seas1 <- data %>%
      group_by(season, year) %>%
      summarise(p0 = sum(value == 0, na.rm = TRUE) / n(), .groups = "drop")
    
    p0_seas <- p0_seas1 %>%
      group_by(season) %>%
      summarise(p0 = mean(p0), .groups = "drop")
  }
  
  data_nz <- if (variable == "precipitation") {
    filter(data, value != 0)
  } else {
    data
  }
  
  mean_seas <- data_nz %>%
    group_by(season) %>%
    summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")
  
  tot_mon1 <- data_nz %>%
    group_by(month, year) %>%
    summarise(tot_val1 = if (variable == "precipitation") {
      sum(value, na.rm = TRUE)
    } else {
      mean(value, na.rm = TRUE)
    }, .groups = "drop")
  
  tot_mon <- tot_mon1 %>%
    group_by(month) %>%
    summarise(tot_val = mean(tot_val1, na.rm = TRUE), .groups = "drop")
  
  plot1 <- ggplot() + 
    geom_violin(data = data_nz, aes(x = season, y = value, fill = season), linewidth = 0.2) + 
    geom_point(data = mean_seas, aes(x = season, y = mean_val), shape = 4) +
    geom_text(data = mean_seas, aes(x = season, y = mean_val, 
                                    label = round(mean_val, 2)), vjust = -0.8, size = 2.2) +
    (if (variable == "precipitation") {
      list(geom_label(data = p0_seas, aes(x = season, y = max(data_nz$value), 
                                          label = paste0("P0 = ", round(p0, 2))), size = 2.2))
    } else {
      list()
    }) +
    scale_fill_manual(values = c("Summer" = "#3da83d", "Spring" = "#FFC3A0",
                                 "Fall" = "#9B2335", "Winter" = "#B0E0E6")) +
    ylab(if (variable == "precipitation") {
      "Seasonal average precipitation"
    } else {
      "Seasonal average temperature"
    }) +
    xlab("Season") +
    theme(
      legend.text = element_text(size = 7),
      axis.title.x = element_text(size = 9, colour = "gray25"),
      axis.title.y = element_text(size = 9, colour = "gray25"),
      legend.title = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "gray25", size = 0.3),
      axis.text.x = element_text(size = 8, colour = "gray25"),
      axis.text.y = element_text(size = 8, colour = "gray25"),
      plot.title = element_text(hjust = 0),
      plot.margin = unit(c(3,3,0.5,0.5), "mm"),
      axis.ticks.length.x = unit(-0.5, "mm"),
      axis.ticks.length.y = unit(-0.5, "mm"),
      axis.ticks = element_line(color = "gray25", size = 0.3),
      legend.position = 'none',
      legend.spacing.x = unit(3.5, "mm"),
      legend.spacing.y = unit(0.1, "mm"),
      strip.background = element_blank(),
      legend.key.size = unit(3, "mm"),
      legend.key.width = unit(3, "mm"),
      legend.key = element_rect(fill = "transparent"),
      text = element_text(color = "gray25"),
      legend.box.margin = margin(0.5, 0.5, 0.5, 0.5)
    )
  
  
  plot2 <- ggplot() + 
    geom_col(data = tot_mon, aes(x = as.factor(month), y = tot_val),
             linewidth = 0.2, fill = "skyblue") + 
    ylab(if (variable == "precipitation") {
      "Monthly total nonzero precipitation"
    } else {
      "Monthly average temperature"
    }) +
    xlab("Month") +
    theme(
      legend.text = element_text(size = 7),
      axis.title.x = element_text(size = 9, colour = "gray25"),
      axis.title.y = element_text(size = 9, colour = "gray25"),
      legend.title = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "gray25", size = 0.3),
      axis.text.x = element_text(size = 8, colour = "gray25"),
      axis.text.y = element_text(size = 8, colour = "gray25"),
      plot.title = element_text(hjust = 0),
      plot.margin = unit(c(3,3,0.5,0.5), "mm"),
      axis.ticks.length.x = unit(-0.5, "mm"),
      axis.ticks.length.y = unit(-0.5, "mm"),
      axis.ticks = element_line(color = "gray25", size = 0.3),
      legend.position = 'none',
      legend.spacing.x = unit(3.5, "mm"),
      legend.spacing.y = unit(0.1, "mm"),
      strip.background = element_blank(),
      legend.key.size = unit(3, "mm"),
      legend.key.width = unit(3, "mm"),
      legend.key = element_rect(fill = "transparent"),
      text = element_text(color = "gray25"),
      legend.box.margin = margin(0.5, 0.5, 0.5, 0.5)
    )
  
  ggarrange(plot1, plot2, nrow = 2)
}
