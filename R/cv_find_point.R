#' Locates a specific point in the CMIP6 data rasters
#' @description
#' Finds the closest CMIP6 raster location (zone and raster longitude and latitude) 
#' to a point specified by its longitude 
#' and latitude. The distances from the raster centres are found using functions from 
#' \pkg{geosphere}.

#' @param longitude Required. Longitude (decimal degrees) of the point
#' @param latitude Required. Latitude (decimal degrees) of the point
#' @param fast Optional. If \code{TRUE} (the default), the faster, but slightly less accurate
#' \code{distVincentySphere} method will be used to determine the distance between the
#' coordinates of the specified point and that of all the CMIP6 raster points. If \code{FALSE},
#' then the more accurate \code{distVincentyEllipsoid} method is used. The difference
#' between the distances of the two methods is typically very small.
#' @author Kevin Shook
#' @returns Returns a vector of raster cell coordinates, zone number, cell area (km) and the distance 
#' between point and cell centre (m). 
#' @importFrom geosphere distVincentyEllipsoid
#' @importFrom geosphere distVincentySphere
#' @export
#'
#' @examples{
#' point_long <- -110.456
#' point_lat <- 52.195
#' point_loc <- cv_find_point(point_long, point_lat)
#' point_loc
#' }
cv_find_point <- function(longitude, latitude, fast = TRUE){
  zone_grid_df <- zone_grid_df
  grid_lon_lat <- as.matrix(zone_grid_df[, c("long", "lat")])
  
  if (fast) 
    zone_grid_df$distance <- distVincentySphere(c(longitude, latitude), grid_lon_lat)
  else
    zone_grid_df$distance <- distVincentyEllipsoid(c(longitude, latitude), grid_lon_lat)
  
  min_dist_loc <- which.min(zone_grid_df$distance)
  rownames(zone_grid_df) <- NULL
  return_val <- as.numeric(zone_grid_df[min_dist_loc,])
  names(return_val) <- names(zone_grid_df)
  return_val[3] <- floor(return_val[3])
  return(return_val)
}
