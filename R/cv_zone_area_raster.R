#' Gets zones and areas for all grid locations
#' @description
#' Uses the data frame \code{zone_grid_df}. Returns a raster containing two layers, the zone (1-9) and the area (km^2^) for
#' each grid location in the CMIP6 files. The raster raster returned has 415 rows (latitudes in 0.1 degrees) 
#' and 883 columns (longitudes in 0.1 degrees). The zone number is used to determine the file(s) to read in
#' for computing the basin mean precipitation statistics for a given Canadian hydrometric basin. The 
#' 
#'
#' @returns Returns a \pkg{terra} \code{SpatRaster} object (415 rows x 883 columns x 2 layers) of all Canadian grid locations.
#' @export
#' @importFrom terra rast
#' @author Heba Abdelmoaty Kevin SHook

#' @examples
#' zone_area_grid <- cv_zone_area_raster()
#' library(terra)
#' plot(zone_area_grid)
#' 
cv_zone_area_raster <- function() {
  zone_grid_df <- zone_grid_df
  zone_area_grid <- rast(zone_grid_df, type  = "xyz", crs = "epsg:4326")
  return(zone_area_grid)
}