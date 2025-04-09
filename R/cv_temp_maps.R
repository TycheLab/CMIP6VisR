#' Plot Spatial Temperature Distribution
#'
#' This function takes a NetCDF file containing daily Temperature time series, extracts spatial and temporal features,
#' and plots one of three options: daily nonzero frequency, average monthly totals, or average annual maxima.
#'
#' @param nc_file A path to a NetCDF (.nc) file readable by the `stars` package.
#' @param stat_type One of "daily", "monthly", or "annual" to indicate which type of statistic to visualize.
#'                  "daily" provides the nonzero average of daily temperature during the whole period given in the NC file.
#'                  "monthly" provides the average Temperature within every month during the whole duration.
#'                  "annual" provides the annual maxima of daily average Temperature during the whole duration 
#'                  (not commonly used in applications).
#' @return A ggplot object showing the specified Temperature distribution over space.Note: We are using the default "plasma"
#'          color scale which could be overwritten. 
#'          The returned plots look best when saved at the size 14 x 12 cm.
#'          You can easily change the font sizes using theme().
#' @examples
#' plot_precip_map("Pincher_ck.nc", stat_type = "monthly")
#' @import ggplot2
#' @importFrom stars read_stars st_rasterize st_set_dimensions geom_stars
#' @importFrom sf  st_as_sf st_drop_geometry
#' @importFrom dplyr filter summarise mutate group_by select right_join rowwise ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate year month
#' @importFrom units drop_units
#' @export
plot_temp_map <- function(nc_file, stat_type = c("daily", "monthly", "annual")) {
  stat_type <- tolower(stat_type[1])
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
    daily_nz <- sf_obj %>%
      rowwise() %>%
      mutate(temp_mean = mean(c_across(all_of(cols)), na.rm = TRUE)) %>%
      ungroup() %>%
      select(temp_mean, geometry)
    
    stars_daily_nz <- st_rasterize(daily_nz["temp_mean"])
    
    p <- ggplot() +
      geom_stars(data = stars_daily_nz) +
      coord_equal() +
      scale_fill_viridis_c(
        "Nonzero Average Daily Temperature (°C)",
        option = "plasma",
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
      pivot_longer(
        cols = matches("^\\d{4}-\\d{2}-\\d{2}$"),
        names_to = "date",
        values_to = "value"
      ) %>%
      mutate(date = as.Date(date), year = year(date))
    
    if (stat_type == "monthly") {
      sf_long <- sf_long %>% mutate(month = month(date, label = TRUE, abbr = TRUE))
      
      
      monthly_avg <- sf_long %>%
        group_by(id, month) %>%
        summarise(avg_total = mean(value, na.rm = TRUE), .groups = "drop")
      
      sf_avg_monthly <- sf_obj %>%
        mutate(id = row_number()) %>%
        select(id, geometry) %>%
        right_join(monthly_avg, by = "id") %>%
        st_as_sf()
      
      raster_list <- sf_avg_monthly %>%
        split(.$month) %>%
        lapply(function(sf_month) st_rasterize(sf_month["avg_total"]))
      
      stars_monthly <- do.call(c, c(raster_list, along = "month"))
      stars_monthly <- st_set_dimensions(
        stars_monthly, "month",
        values = levels(factor(sf_avg_monthly$month, levels = month.abb))
      )
      
      p <- ggplot() +
        geom_stars(data = stars_monthly) +
        coord_equal() +
        facet_wrap(~month) +
        scale_fill_viridis_c(
          "Average Monthly Temperature (°C)",
          option = "plasma",
          trans = scales::pseudo_log_trans(),
          na.value = "transparent"
        ) +
        theme_minimal() + xlab("Longitude") + ylab("Latitude") + mytheme +
        theme(panel.spacing.x = unit(1, "lines")) +
        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      print(p)
      
    } else if (stat_type == "annual") {
      annual_max <- sf_long %>%
        group_by(id, year) %>%
        summarise(max_precip = max(value, na.rm = TRUE), .groups = "drop")
      
      avg_annual_max <- annual_max %>%
        group_by(id) %>%
        summarise(annual_max_avg = mean(max_precip, na.rm = TRUE), .groups = "drop")
      
      sf_annual_max <- sf_obj %>%
        mutate(id = row_number()) %>%
        select(id, geometry) %>%
        inner_join(avg_annual_max, by = "id") %>%
        st_as_sf()
      
      stars_max <- st_rasterize(sf_annual_max["annual_max_avg"])
      
      p <- ggplot() +
        geom_stars(data = stars_max) +
        coord_equal() +
        scale_fill_viridis_c(
          "Annual Maxima of daily Temperature (°C)",
          option = "plasma",
          na.value = "transparent"
        ) +
        theme_minimal() + xlab("Longitude") + ylab("Latitude") + mytheme +
        guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
      
      print(p)
    }
  }
}

