##' Get zones and areas that clip rasters from a given basin polygon
##'
##' Takes the \code{SpatRaster} that represents zones and areas and
##' clips them from polygon \code{basin}
##' @return
##' @author Kostas Andreadis
##' @param za_rast \code{SpatRaster} of the zones and areas
##' @param basin \code{SpatVector} of the basin to clip to
##'
##' @import terra
cv_clip_basin <- function(za_rast, basin) {
  result <- terra::crop(za_rast, basin, mask=TRUE)
  zones <- subset(result, 1)
  areas <- subset(result, 2)
  rasts <- lapply(
      unique(zones)[[1]],
      function(v) {
          (terra::mask(areas, zones, maskvalues=v, inverse = TRUE))
      }
  )
  total_areas <- rapply(rasts, function(r) {
    sum(values(r), na.rm=TRUE)
  })
  return(list(raster=rasts, area=total_areas, zone=unique(zones)[[1]]))
}
