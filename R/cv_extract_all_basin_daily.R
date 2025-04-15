#' Extracts daily precipitation or temperatures for all locations in a basin
#' 
#' @description
#' Extracts the daily precipitation or temperature values from CMIP6 NetCDF files for all locations
#' in a specified basin.  
#' 
#' The CMIP6 data are arranged by zone in 9 files. As a given basin may lie over more than 
#' one zone, it may necessary to read in data from more than one NetCDF file when computing
#' the basin mean precipitation.
#' 
#' @param netcdf_directory Required. Directory containing NetCDF files. 
#' @param scenario Required. Full name of scenario to be used. This is the file 
#' name omitting the zone number. The first character of the scenario name is used to
#' determine the name of the variable returned.
#' @param basin_zone_area Required. A list object returned by `cv_clip_basin()` 
#' which contains the zone numbers to be used, the basin area within each zone, 
#' and rasters of each zone containing the area of each
#' element.
#' @param temp_file If `TRUE` (the default) then temporary files will be used when extracting the values
#' from the NetCDF files. This option is slower than keeping all the values in 
#' memory (which is what occurs if `temp_file = TRUE`), _but_ allows the function
#' to work with very large basins, which may require more memory than is available. 
#' @param output_file_name If `NULL`, then the precipitation values
#' will not be written to a file. If a file name is specified, then the values _will_
#' be written to a file. Note that if the filename exists, then the values will be 
#' overwritten.
#' 
#' @author Kevin Shook
#' @seealso \code{\link{cv_basin_daily_precip}} 
#' @importFrom stringr str_sub
#' @importFrom terra crop global rast merge writeCDF
#' @returns Returns a \code{SpatRaster} object of the daily precipitation for all
#' locations in the specified basin. Optionally writes the precipitation values to 
#' a specified NetCDF file.
#' @export
#'
#' @examples \donttest{
#' # This function is marked to not be tested as it requires local copies of the CMIP6 data files
#' # which are many GB in size
#' library(terra)
#' az_raster <- cv_zone_area_raster()
#' fpath <- system.file("extdata", "07BF001.shp", package = "CMIP6VisR")
#' basin_vector <- vect(fpath)
#' basin_areas <- cv_clip_basin(az_raster, basin_vector)
#' netcdf_directory <- "."
#' all_precip <- cv_extract_all_basin_daily(netcdf_directory = netcdf_directory,
#'                                basin_zone_area = basin_areas,
#'                                temp_file = FALSE)
#' }

cv_extract_all_basin_daily <- function(netcdf_directory = ".", 
                                  scenario = "pr_day_ACCESS-CM2_ssp126_r2i1p1f1_gn_20150101-21001231_cannc_SPQM_", 
                                  basin_zone_area = NULL,
                                  temp_file = TRUE,
                                  output_file_name = NULL) {
  # check parameter values
  
  if (is.null(scenario) | (scenario == "")) {
    stop("cv_extract_all_basin_daily requires a scenario")
  }
  if (missing(basin_zone_area)  | is.null(basin_zone_area)) {
    stop("cv_extract_all_basin_daily requires a basin_zone_area object")
  }
  
  if (missing(netcdf_directory)  | is.null(netcdf_directory) | (netcdf_directory == "")) {
    netcdf_directory <- "./"
  }
  
  
  # assemble file names
  zones <- basin_zone_area$zone
  areas <- basin_zone_area$area
  num_zones <- length(zones)


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
  
  if (num_zones == 1) {
    i <- 1
    netcdf_file_name <- paste0(netcdf_directory, scenario, "0", zones[i], ".nc")

    # get variable from first character of file name
    base_name <- basename(netcdf_file_name)
    first_char <- substr(base_name, 1, 1)

    
    # check to be sure that the file exists
    if (!file.exists(netcdf_file_name))
      stop(netcdf_file_name, " does not exist")
    
    r <- rast(netcdf_file_name)
    area_raster <- basin_zone_area$raster[[1]]
    # crop netcdf to extent of area raster
    
    if (!temp_file) {
     cropped <- terra::crop(r, area_raster, mask = TRUE) 
    }
    else{
      crop_file <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".tif")
      cropped <- terra::crop(r, area_raster, mask = TRUE, 
                             filename = crop_file,
                             overwrite = TRUE) 
    }
   if (!is.null(output_file_name))
      writeCDF(cropped, output_file_name, overwrite = TRUE, varname = "pr")
    return(cropped)
  } else {
      
  # get raster of areas
    # more than 1 zone
    for (i in 1:num_zones) {
      
      # get raster of areas
      area_raster <- basin_zone_area$raster[[i]]
      
      # assemble file name
      netcdf_file_name <- paste0(netcdf_directory, scenario, "0", zones[i], ".nc")
      
      # check to be sure that the file exists
      if (!file.exists(netcdf_file_name))
        stop(netcdf_file_name, " does not exist")
      
      r <- rast(netcdf_file_name)
      
      # crop netcdf to extent of area raster
      
      if (!temp_file) {
        cropped <- terra::crop(r, area_raster, mask = TRUE)
      } else{
        crop_file <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".tif")
        cropped <- terra::crop(r, area_raster, mask = TRUE, filename = crop_file)
      }
     if (i == 1) 
       all <- cropped
     else {
       # join together
       if (!temp_file) {
         all <- terra::merge(all, cropped)
       } else{
         all_file <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".tif")
         all <- terra::merge(all, cropped, filename = all_file)
       }
     }   
    }
  }  # if
  if (!is.null(output_file_name))
    writeCDF(all, output_file_name, overwrite = TRUE, varname = "pr")
  return(all)
}