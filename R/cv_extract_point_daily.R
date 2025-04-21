#' Extracts daily precipitation or temperature for a specific location
#' 
#' @description
#' Extracts a daily time series from a single CMIP6 raster location, which can
#' be used to compare the CMIP6 values against station values. The location
#' of the data in the CMIP6 data (its zone and raster location) is found using 
#' \code{cv_find_point}. 
#' @param variable_file_name Required. String containing the name of the NetCDF 
#' file with the CMIP6 daily precipitation or temperature, including the file path.
#' @param grid_lon Required. Longitude of the grid cell containing the location,
#' as returned by \code{cv_find_point}.
#' @param grid_lat Required. Latitude of grid cell containing the location,
#' as returned by \code{cv_find_point}.
#' @author Kevin Shook
#' @seealso \code{\link{cv_find_point}} \code{\link{cv_extract_all_basin_daily}} \code{\link{cv_extract_mean_basin_daily}} 
#' @importFrom terra rast ext extract varnames
#' @returns Returns a data frame with 2 columns: `date` and either `precipitation` 
#' or `temperature`, depending on the file name. The
#' `date` is a standard \R date over the interval 2015-01-01 to 2100-12-31, and the 
#' `precipitation` or `temperature` is the value at the specified grid location. The returned time series 
#' can be plotted using
#'  \code{cv_plot_TS}, \code{cv_plot_prob}, and \code{cv_plot_season}.
#' @export
#'
#' @examples \dontrun{
#' point_long <- -110.456
#' point_lat <- 52.19
#' point_loc <- cv_find_point(point_long, point_lat)
#' point_loc
#' precip_file <- "pr_day_ACCESS-CM2_ssp126_r2i1p1f1_gn_20150101-21001231_cannc_SPQM_07.nc"
#' grid_lon <- point_loc[1]
#' grid_lat <- point_loc[2]
#' point_precip <- cv_extract_point_daily(precip_file, grid_lon, grid_lat)
#' }
#' 
cv_extract_point_daily <- function(variable_file_name = NULL, 
                                  grid_lon = NULL,
                                  grid_lat = NULL){
  if (is.null(variable_file_name) | (variable_file_name == "")) {
    stop("cv_extract_point_daily requires a NetCDF file")
  }

  if (!file.exists(variable_file_name))
    stop(variable_file_name, " does not exist")
  
  if (is.null(grid_lon) | (grid_lon > 0)) {
    stop("cv_extract_point_daily requires a grid longitude")
  }  
  
  if (is.null(grid_lat) | (grid_lat > 90)) {
    stop("cv_extract_point_daily requires a grid latitude")
  } 
  
  # set data file as a raster
  r <- rast(variable_file_name)
  
  # get file type
  variable_name <- varnames(r)[1]
  


  if (variable_name == "pr")
    variable_name <- "precipitation"
  else
    variable_name <- "temperature"
  
  # check to make sure that point is actually in the variable file
  extent <- as.vector(ext(r))
  
  if (grid_lon < extent[1] | grid_lon > extent[2] | grid_lat < extent[3] | grid_lat > extent[4]) {
    stop("Location is outide the NetCDF file")
  }
    
  
  # set up dates
  start_date <- as.Date("2015-01-01")
  end_date <- as.Date("2100-12-31")
  date <- seq(from = start_date, to = end_date, by = 1)
  df <- data.frame(date)
  
  # set up location to extract data
  xy_df <- data.frame(grid_lon, grid_lat)
  names(xy_df) <- c("lon", "lat")
  xy <- vect(xy_df, crs = "+proj=longlat +datum=WGS84")
  
  # extract values and add to dates
  values <- extract(r, xy)
  df <- data.frame(date, t(values)[-1])
  names(df)[2] <- variable_name
  return(df)
}