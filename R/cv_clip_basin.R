##' Get zones and areas that clip rasters from a given basin polygon
##'
##' Takes the \code{SpatRaster} that represents zones and areas and
##' clips them from polygon \code{basin}
##' @return
##' @author Kostas Andreadis
##' @param za_rast \code{SpatRaster} of the zones and areas
##' @param basin \code{SpatVector} of the basin to clip to
##'
cv_clip_basin <- function(za_rast, basin) {
    result <- crop(za_rast, basin)
    zones <- values(result)[, 1]
    areas <- values(result)[, 2]
    return(list(zones, areas))
}
