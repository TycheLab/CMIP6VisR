#' Get zones and areas that clip rasters from a given basin polygon
#'
#' @description
#' Takes the \code{SpatRaster} that represents zones and areas and
#' clips them from polygon \code{basin}
#' @author Kostas Andreadis
#' @param za_rast \code{SpatRaster} of the zones and areas
#' @param basin \code{SpatVector} of the basin to clip to
#' @import terra
#' @export
#' @return Returns a list with 3 elements: 1) `raster` (rasters of cell areas for each zone),
#' 2) `area` total area of each zone in the basin, and 3) `zone` zone numbers.
#' @seealso \code{\link{cv_basin_daily_precip}} 
#' @examples{
#' library(terra)
#' az_raster <- cv_zone_area_raster()
#' fpath <- system.file("extdata", "07BF001.shp", package = "CMIP6VisR")
#' basin_vector <- vect(fpath)
#' basin_areas <- cv_clip_basin(az_raster, basin_vector)
#' }
#' 
cv_clip_basin <- function(za_rast, basin) {
  result <- terra::crop(za_rast, basin, mask = TRUE)
  zones <- subset(result, 1)
  areas <- subset(result, 2)
  rasts <- lapply(
      unique(zones)[[1]],
      function(v) {
          terra::trim(terra::mask(areas, zones, maskvalues = v, inverse = TRUE))
      }
  )
  total_areas <- rapply(rasts, function(r) {
    sum(values(r), na.rm = TRUE)
  })
  return(list(raster = rasts, area = total_areas, zone = unique(zones)[[1]]))
}
