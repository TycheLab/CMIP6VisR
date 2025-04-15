#' Plot Spatial Precipitation or Temperature Distribution
#'
#' This function takes a NetCDF file containing daily time series of precipitation or temperature,
#' extracts spatial and temporal features, and plots one of three options: daily nonzero frequency,
#' average monthly totals, or average annual maxima.
#'
#' @param nc_file A path to a NetCDF (.nc) file readable by the `stars` package.
#' @param stat_type One of "daily", "monthly", or "annual" to indicate which type of statistic to visualize.
#'                  "daily" provides the nonzero average of daily values during the whole period given in the NC file.
#'                  "monthly" provides the average monthly totals (precipitation) or means (temperature).
#'                  "annual" provides the annual maxima of daily values during the whole duration.
#' @param variable A string starting with 'p' (precipitation) or 't' (temperature).
#' @return A ggplot object showing the specified variable's distribution over space.
#'         Note: We are using the default Viridis color scale which can be overwritten. 
#'         The returned plots look best when saved at the size 14 x 12 cm.
#'         You can easily change the font sizes using theme().
#' @examples
#' cv_plot_map("Pincher_ck.nc", stat_type = "monthly", variable = "temperature")
#' cv_plot_map("Pincher_ck.nc", stat_type = "annual", variable = "precip")
#' @importFrom ggplot2 theme
#' @importFrom stars read_stars st_rasterize st_set_dimensions geom_stars
#' @importFrom sf  st_as_sf st_drop_geometry
#' @importFrom dplyr filter summarise mutate group_by select right_join rowwise ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate year month
#' @importFrom units drop_units
#' @export
cv_plot_map <- function(nc_file, stat_type = "daily", variable = "precipitation") {
  stat_type <- tolower(stat_type[1])
  var_lower <- tolower(variable)
  
  if (!startsWith(var_lower, "p") && !startsWith(var_lower, "t")) {
    stop("The variable argument must start with 'p' for precipitation or 't' for temperature.")
  }
  
  variable <- if (startsWith(var_lower, "p")) "precipitation" else "temperature"
  valid_types <- c("daily", "monthly", "annual")
  
  if (!stat_type %in% valid_types) {
    stop("Invalid 'stat_type'. Please choose one of: 'daily', 'monthly', or 'annual'.")
  }
  
  mytheme <- theme(
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    axis.text.y = element_text(size = 5.5),
    axis.text.x = element_text(size = 5.5, hjust = 0.4),
    axis.title = element_text(size = 8),
    legend.text.align = 0.5,
    legend.title.align = 0.5,
    legend.box.just = "center",
    legend.justification = "center",
    legend.position = 'bottom',
    legend.key.size = unit(0.2, "cm"),
    legend.key.width = unit(1.2, "cm"),
    text = element_text(size = 7, color = gray(0.25))
  )
  
  nc_data <- read_stars(nc_file)
  sf_obj <- st_as_sf(nc_data, as_points = FALSE, merge = FALSE)
  sf_obj[] <- lapply(sf_obj, function(col) if (inherits(col, "units")) drop_units(col) else col)
  cols <- names(sf_obj)[sapply(sf_obj, is.numeric)]
  
  if (stat_type == "daily") {
    daily_stat <- if (variable == "precipitation") {
      sf_obj %>% rowwise() %>% mutate(result = mean(c_across(all_of(cols)) != 0, na.rm = TRUE))
    } else {
      sf_obj %>% rowwise() %>% mutate(result = mean(c_across(all_of(cols)), na.rm = TRUE))
    }
    daily_stat <- daily_stat %>% ungroup() %>% select(result, geometry)
    stars_raster <- st_rasterize(daily_stat["result"])
    
    fill_label <- if (variable == "precipitation") "Nonzero Avg Daily Precipitation (mm)" else "Nonzero Avg Daily Temperature (°C)"
    
    p <- ggplot() +
      geom_stars(data = stars_raster) +
      coord_equal() +
      scale_fill_viridis_c(
        fill_label,
        direction = if (variable == "precipitation") -1 else 1,
        trans = scales::pseudo_log_trans(),
        na.value = "transparent"
      ) +
      theme_minimal() + xlab("Longitude") + ylab("Latitude") + mytheme +
      guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
    
    print(p)
    
  } else {
    sf_long <- sf_obj %>%
      st_drop_geometry() %>%
      mutate(id = row_number()) %>%
      pivot_longer(cols = matches("^\\d{4}-\\d{2}-\\d{2}$"), names_to = "date", values_to = "value") %>%
      mutate(date = as.Date(date), year = year(date))
    
    if (stat_type == "monthly") {
      sf_long <- sf_long %>% mutate(month = month(date, label = TRUE, abbr = TRUE))
      
      monthly_data <- if (variable == "precipitation") {
        sf_long %>% group_by(id, year, month) %>% summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
          group_by(id, month) %>% summarise(avg_total = mean(total, na.rm = TRUE), .groups = "drop")
      } else {
        sf_long %>% group_by(id, month) %>% summarise(avg_total = mean(value, na.rm = TRUE), .groups = "drop")
      }
      
      sf_avg_monthly <- sf_obj %>% mutate(id = row_number()) %>% select(id, geometry) %>% right_join(monthly_data, by = "id") %>% st_as_sf()
      raster_list <- sf_avg_monthly %>% split(.$month) %>% lapply(function(sf_month) st_rasterize(sf_month["avg_total"]))
      
      stars_monthly <- do.call(c, c(raster_list, along = "month"))
      stars_monthly <- st_set_dimensions(stars_monthly, "month", values = levels(factor(sf_avg_monthly$month, levels = month.abb)))
      
      fill_label <- if (variable == "precipitation") "Avg Monthly Precipitation (mm)" else "Avg Monthly Temperature (°C)"
      
      p <- ggplot() +
        geom_stars(data = stars_monthly) +
        coord_equal() +
        facet_wrap(~month) +
        scale_fill_viridis_c(
          fill_label,
          direction = if (variable == "precipitation") -1 else 1,
          trans = scales::pseudo_log_trans(),
          na.value = "transparent"
        ) +
        theme_minimal() + xlab("Longitude") + ylab("Latitude") + mytheme +
        theme(panel.spacing.x = unit(1, "lines")) +
        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      print(p)
      
    } else if (stat_type == "annual") {
      annual_max <- sf_long %>% group_by(id, year) %>% summarise(max_val = max(value, na.rm = TRUE), .groups = "drop")
      avg_annual_max <- annual_max %>% group_by(id) %>% summarise(result = mean(max_val, na.rm = TRUE), .groups = "drop")
      sf_annual_max <- sf_obj %>% mutate(id = row_number()) %>% select(id, geometry) %>% inner_join(avg_annual_max, by = "id") %>% st_as_sf()
      stars_max <- st_rasterize(sf_annual_max["result"])
      
      fill_label <- if (variable == "precipitation") "Annual Max of Daily Precip (mm)" else "Annual Max of Daily Temp (°C)"
      
      p <- ggplot() +
        geom_stars(data = stars_max) +
        coord_equal() +
        scale_fill_viridis_c(
          fill_label,
          direction = if (variable == "precipitation") -1 else 1,
          na.value = "transparent"
        ) +
        theme_minimal() + xlab("Longitude") + ylab("Latitude") + mytheme +
        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      print(p)
    }
  }
}
