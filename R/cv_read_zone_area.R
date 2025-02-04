#' Reads zones and areas for all grid locations
#' @description
#' Reads the data file \option{ZoneGrid_Area.tif} from the package directory 
#' \option{inst/extdata}. The file is a raster containing two layers, the zone (1-9) and the area (km^2^) for
#' each grid location in the CMIP6VisR files. The raster raster returned has 415 rows (latitudes in 0.1 degrees) 
#' and 883 columns (longitudes in 0.1 degrees). The zone number is used to determine the file(s) to read in
#' for computing the basin mean precipitation statistics for a given Canadian hydrometric basin. The 
#' 
#'
#' @returns Returns a \pkg{terra} \code{SpatRaster} object (415 rows x 883 columns x 2 layers) of all Canadian grid locations.
#' @export
#' @importFrom terra rast
#' @author Heba Abdelmoaty Kevin SHook

#' @examples
#' zone_area_grid <- cv_read_zone_area()
#' 
cv_read_zone_area <- function() {
  fpath <- system.file("extdata", "ZoneGrid_Area.tif", package = "CMIP6VisR")
  zone_area_grid <- rast(fpath)
  return(zone_area_grid)
}