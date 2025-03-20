## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = TRUE,
                      echo = TRUE,
                      fig.width = 7, 
                      warning = FALSE,
                      message = FALSE)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(CMIP6VisR)
library(terra)

## -----------------------------------------------------------------------------
# Read the shapefile containing the basin
fpath <- system.file("extdata", "07BF001.shp", package = "CMIP6VisR")
basin_vector <- vect(fpath)
plot(basin_vector)

## ----warning=FALSE------------------------------------------------------------
# convert the dataframe of zones to raster (built-in the package)
zone_area_grid <- cv_zone_area_raster() 

req_data <- cv_clip_basin(zone_area_grid, basin_vector)

# printing the zones that are required for the targeted area
print(req_data[["zone"]])

## ----message=FALSE, warning=FALSE---------------------------------------------
# Isolating the raster outputs
raster_list <- req_data$raster

terra::plot(raster_list[[1]], main = expression("Zone 4, Grid Areas in " ~ km^2))

terra::plot(raster_list[[2]], main = expression("Zone 7, Grid Areas in " ~ km^2))

# Merge the individual rasters using terra::merge
merged_raster <- do.call(terra::merge, req_data$raster)

# Plot the merged raster
terra::plot(merged_raster, main = expression("Both zones, Grid Areas in " ~ km^2))


## ----eval=FALSE---------------------------------------------------------------
# values <- cv_basin_daily_precip(netcdf_directory = "./data/",
#                                 scenario = "pr_day_ACCESS-CM2_ssp126_r2i1p1f1_gn_20150101-21001231_cannc_SPQM_",
#                                 basin_zone_area = req_data,
#                                 temp_file = FALSE)

## ----include=FALSE------------------------------------------------------------
data("eg_TS")
values = eg_TS

## -----------------------------------------------------------------------------
head(values)

## -----------------------------------------------------------------------------
cv_plot_TS(values)

## -----------------------------------------------------------------------------
cv_plot_season(values)

## -----------------------------------------------------------------------------
cv_plot_prob(values)

