cv_map_basins <- function(nc_dir = ".", 
                          filespec = "*.nc", 
                          out_file = "daily.nc") {
  files <- list.files(path = nc_dir, pattern = filespec)
  files <- paste0(nc_dir, "/", files)
  basins <- unique(cor_cat$cat)
  for (i in 1:length(files)) {
    filename <- files[i]
    raster <- terra::rast(filename)
    
    for (j in 1:11){
      basin <- basins[j]
      cat <- cor_cat[cor_cat$cat == basin, 1:2]
      names(cat) <- c("lon", "lat")
      v1 <- vect(cat, geom = c("lon", "lat"))
      
      x <- r %in% c("water", "urban")
      m <- mask(r, x, maskvalues=FALSE)
      
    }
  }
    
}