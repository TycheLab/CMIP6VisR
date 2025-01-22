#' Concatenate daily precipitation files
#'
#' @param nc_dir Directory containing daily precipitation NetCDF files
#' @param filespec Name of all required files, including wild cards
#' @param out_file Output file including path
#'
#' @returns Writes all daily mean precipitation values to the specified file.
#' @export
#' @import terra 
#'
#' @examples
cv_all_basin_daily_precip <- function(nc_dir = ".", 
                            filespec = "*.nc", 
                            out_file = "daily.nc") {
  
  files <- list.files(path = nc_dir, pattern = filespec)
  files <- paste0(nc_dir, "/", files)
  for (i in 1:length(files)) {
    filename <- files[i]
    raster <- terra::rast(filename)
    
    if (i == 1) {
      total_raster <- raster
    } else {
      total_raster <- sprc(total_raster, raster)
      rm(raster)
      total_raster <- merge(total_raster)
      gc()
    }
  }
 writeRaster(total_raster, out_file)                 
}