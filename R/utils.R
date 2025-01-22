cv_nc_to_df <- function(nc){
  fn <- nc
  YourBrick <- raster::brick(fn)
  
  points <- raster::rasterToPoints(YourBrick)
  pts <- as.data.frame(points)
  
  #Removing NA points
  pts <- pts[pts$x != "NA" & pts$y != "NA",]
  
  return(pts)
}