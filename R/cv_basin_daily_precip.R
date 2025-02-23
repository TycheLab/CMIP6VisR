#' Calculates basin-averaged daily precipitation
#' 
#' @description
#' Extracts the daily precipitation values from CMIP6 NetCDF files, and calculates
#' the mean precipitation for a given basin for each time interval. The original files are in 
#' longitude-latitude projection, so the areas of the cells varies. The cell areas
#' are used to weight the precipitation when computing the basin mean precipitation.  
#' 
#' The CMIP6 data are arranged by zone in 9 files. As a given basin may lie over more than 
#' one zone, it may necessary to read in data from more than one NetCDF file when computing
#' the basin mean precipitation.
#' 
#' @param netcdf_directory Required. Directory containing NetCDF files. 
#' @param scenario Required. Full name of scenario to be used. This is the file name omitting the zone number.
#' @param basin_zone_area Required. A list object returned by `cv_clip_basin()` which contains the zone numbers to be used,
#' the basin area within each zone, and rasters of each zone containing the area of each
#' element.
#' @param temp_file If `TRUE` (the default) then temporary files will be used when extracting the values
#' from the NetCDF files. This option is slower than keeping all the values in 
#' memory (which is what occurs if `temp_file = TRUE`), _but_ allows the function
#' to work with very large basins, which may require more memory than is available. 
#' 
#' 
#' @author Kevin Shook
#' @seealso \code{\link{cv_clip_basin}} 
#' @importFrom stringr str_sub
#' @importFrom terra crop
#' @importFrom terra global
#' @returns Reruns a data frame with 2 columns: `date` and `precipitation`. The
#' `date` is a standard \R date over the interval 2015-01-01 to 2100-12-31, and the 
#' `precipitation` is the basin mean value.
#' @export
#'
#' @examples \donttest{
#' library(terra)
#' az_raster <- cv_zone_area_raster()
#' fpath <- system.file("extdata", "07BF001.shp", package = "CMIP6VisR")
#' basin_vector <- vect(fpath)
#' basin_areas <- cv_clip_basin(az_raster, basin_vector)
#' netcdf_directory <- "."
#' values <- cv_basin_daily_precip(netcdf_directory = netcdf_directory,
#'                                basin_zone_area = basin_areas,
#'                                temp_file = FALSE)
#' }

cv_basin_daily_precip <- function(netcdf_directory = ".", 
                                  scenario = "pr_day_ACCESS-CM2_ssp126_r2i1p1f1_gn_20150101-21001231_cannc_SPQM_", 
                                  basin_zone_area = NULL,
                                  temp_file = TRUE) {
  # check parameter values
  
  if (is.null(scenario) | (scenario == "")) {
    stop("cv_basin_daily_precip requires a scenario")
  }
  if (missing(basin_zone_area)  | is.null(basin_zone_area)) {
    stop("cv_basin_daily_precip requires a basin_zone_area object")
  }
  
  if (missing(netcdf_directory)  | is.null(netcdf_directory) | (netcdf_directory == "")) {
    netcdf_directory <- "./"
  }
  
  
  # assemble file names
  zones <- basin_zone_area$zone
  areas <- basin_zone_area$area
  num_zones <- length(zones)
  num_areas <- length(areas)
  weightings <- areas / sum(areas)
  
  if (num_zones != num_areas)
    stop("numbers of zones and areas are not equal")

  # check if last character in scenario is an underscore and add one if necessary
  scenario_last_char <- str_sub(scenario, start = -1)
  if (scenario_last_char != "_")
    scenario <- paste0(scenario, "_")
  
  # check if last character in directory is a backslash and add one if necessary
  netcdf_last_char <- str_sub(netcdf_directory, start = -1)
  if (netcdf_last_char != "/")
    netcdf_directory <- paste0(netcdf_directory, "/")
  
  start_date <- as.Date("2015-01-01")
  end_date <- as.Date("2100-12-31")
  date <- seq(from = start_date, to = end_date, by = 1)
  df <- data.frame(date)
  
  for (i in 1:num_zones) {
   # assemble file name
   netcdf_file_name <- paste0(netcdf_directory, scenario, "0", zones[i], ".nc")
   
   # check to be sure that the file exists
   if (!file.exists(netcdf_file_name))
     stop(netcdf_file_name, " does not exist")
   
   # get raster of areas
   area_raster <- basin_zone_area$raster[[i]]
   
   r <- rast(netcdf_file_name)

   # first, crop netcdf to extent of area raster
   
   if (!temp_file) {
     cropped <- crop(r, area_raster, mask = TRUE)
     weighted_precip <- global(cropped, 
                             fun = "mean", 
                             weights = area_raster, 
                             na.rm = TRUE)
    } else{
    crop_file <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".tif")
    cropped <- crop(r, area_raster, mask = TRUE, filename = crop_file)
    weighted_precip <- global(cropped, 
                              fun = "mean", 
                              weights = area_raster)
    weighted_precip <- weighted_precip * weightings[i]
    }
   df <- cbind(df, weighted_precip)
   names(df)[i + 1] <- zones[i]
  }
  
  if (num_zones  == 1)
    names(df)[2] <- "precipitation"
  else {
    df$precipitation <- rowSums(df[, -1])
    df <- data.frame(df$date, df$precipitation)
    names(df) <- c("date", "precipitation")
  }
 return(df)   
}