#' Calculate daily mean precipitation for each of 11 Canadian major river basins
#'
#' @param nc_dir Directory containing daily precipitation NetCDF files
#' @param filespec Name of all required files, including wild cards
#' @param out_file Output file including path
#'
#' @returns Writes all daily mean precipitation values to the specified file.
#' @export
#' @import terra sp
#'
#' @examples
cv_all_basin_daily_precip <- function(nc_dir = ".", 
                            filespec = "*.nc", 
                            out_file = "daily.nc") {
  
  
  
}