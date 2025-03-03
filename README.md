README
================

# CMIP6VisR

## Visualization and Analysis of CMIP6 Hydroclimatic Data

CMIP6VisR  facilitates the use of the
bias-corrected dataset for CMIP6 projections of daily average
temperature and total precipitation, SPQM-CMIP6-CAN (Abdelmoaty et al.,
2025 <https://doi.org/10.1038/s41597-025-04396-z>). This dataset is designed for operational applications, preserving
key statistical properties, including observed marginal properties,
trends, and variability while enabling a smooth transition from
historical observations to future projections. The package allows users
to identify the relevant data files covering their target
region or basin. Additionally, it provides grid area information and
enables the estimation of area-weighted average precipitation time
series derived from the bias-corrected product.

## Install

Copy and paste to get the latest version of CMIP6VisR directly 

`if (!require('devtools')) {install.packages('devtools'); library(devtools)} `
`install_github('TycheLab/CMIP6VisR', upgrade = 'never')`


## Funding

The package was fully funded by the Global Water Futures - Core
Modelling and Forecasting (<https://gwf.usask.ca/core-modelling/>)
program, University of Saskatchewan.

## Authors

Coded by: Kevin Shook, Konstantinos Andreadis, and Heba Abdelmoaty
Conceptual design by Simon Michael Papalexiou. Maintained by: Kevin
Shook.

## References

Abdelmoaty, H.M., Rajulapati, C.R., Nerantzaki, S.D. et
al. Bias-corrected high-resolution temperature and precipitation
projections for Canada. Sci Data 12, 191 (2025).
<https://doi.org/10.1038/s41597-025-04396-z>
