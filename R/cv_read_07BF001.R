cv_read_07BF001 <- function(){
  fpath <- system.file("extdata", "07BF001.shp", package = "CMIP6VisR")
  vector <- vect(fpath)
  return(vector)
}
