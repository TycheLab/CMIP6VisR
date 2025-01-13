cv_nc_to_df <- function(nc){
  fn <- nc
  YourBrick <- raster::brick(fn)
  
  points <- raster::rasterToPoints(YourBrick)
  pts <- as.data.frame(points)
  
  #Removing NA points
  pts1 <- subset(pts,pts$x != "NA",)
  pts <- subset(pts1, pts$y != "NA",)
  
  return(pts)
}