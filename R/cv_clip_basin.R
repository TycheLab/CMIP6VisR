#' Get zones and areas that clip rasters from a given basin polygon
#'
#' @description
#' Takes the \code{SpatRaster} that represents zones and areas and
#' clips them from polygon \code{basin}.
#'
#' @author Kostas Andreadis
#'
#' @param za_rast \code{SpatRaster} of the zones and areas.
#' @param basin \code{SpatVector} of the basin to clip to.
#'
#' @importFrom terra crop mask trim subset values
#' @export
#'
#' @return Returns a list with 3 elements:
#' \itemize{
#'   \item `raster` (rasters of cell areas for each zone).
#'   \item `area` (total area of each zone in the basin).
#'   \item `zone` (zone numbers).
#' }
#'
#' @seealso \code{\link{cv_basin_daily_precip}}
#'
#' @examples
#' \dontrun{
#' library(terra)
#' az_raster <- cv_zone_area_raster()
#' fpath <- system.file("extdata", "07BF001.shp", package = "CMIP6VisR")
#' basin_vector <- vect(fpath)
#' basin_areas <- cv_clip_basin(az_raster, basin_vector)
#' }
#'
cv_clip_basin <- function(za_rast, basin) {
  result <- terra::crop(za_rast, basin, mask = TRUE)
  zones <- terra::subset(result, 1)
  areas <- terra::subset(result, 2)
  
  # Ensure zones is a vector before applying unique()
  zone_values <- terra::values(zones)  # Extract numeric values from raster
  zone_values <- na.omit(zone_values)  # Remove NA values
  zone_values <- unique(zone_values)   # Get unique zone numbers
  
  # Apply unique() safely
  rasts <- lapply(
    zone_values,
    function(v) {
      terra::trim(terra::mask(areas, zones, maskvalues = v, inverse = TRUE))
    }
  )
  
  total_areas <- rapply(rasts, function(r) {
    sum(terra::values(r), na.rm = TRUE)
  })
  
  return(list(raster = rasts, area = total_areas, zone = zone_values))
}
