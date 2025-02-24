## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = TRUE,
                      echo = TRUE,
                      fig.width = 7, 
                      warning = FALSE,
                      message = FALSE)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(CMIP6VisR)
library(sf)
library(terra)

## ----warning=FALSE------------------------------------------------------------
# convert the dataframe of zones to raster (built-in the package)
zone_area_grid <- cv_zone_area_raster() 


# Read the required shapefile
fpath <- system.file("extdata", "07BF001.shp", package = "CMIP6VisR")
basin_vector <- vect(fpath)


req_data <- cv_clip_basin(zone_area_grid, basin_vector)


#Printing the zones that are required for the targeted area
print(req_data[["zone"]])

## ----message=FALSE, warning=FALSE---------------------------------------------
# Isolating the raster outputs
raster_list <- req_data$raster

# Merge the individual rasters using terra::merge
merged_raster <- do.call(terra::merge, req_data$raster)

# Plot the merged raster
terra::plot(merged_raster, main = expression("Grid Areas in " ~ km^2))



