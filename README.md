README
================

# CMIP6VisR

## Visualization and Analysis of CMIP6 Hydroclimatic Data

CMIP6VisR is an R package that facilitates the usage of the
bias-corrected dataset for CMIP6 projections of daily average
temperature and total precipitation, SPQM-CMIP6-CAN (Abdelmoaty et al.,
2025). This dataset is designed for operational applications, preserving
key statistical properties, including observed marginal properties,
trends, and variability while enabling a smooth transition from
historical observations to future projections. The package allows users
to identify and download relevant dataset files covering their target
region or basin. Additionally, it provides grid area information and
enables the estimation of area-weighted average precipitation time
series derived from the bias-corrected product.

## Install

``` r
## copy-paste to get the latest version of CMIP6VisR

if (!require('devtools')) {install.packages('devtools'); library(devtools)} 
```

    ## Loading required package: devtools

    ## Warning: package 'devtools' was built under R version 4.3.3

    ## Loading required package: usethis

    ## Warning: package 'usethis' was built under R version 4.3.3

``` r
install_github('TycheLab/CMIP6VisR', upgrade = 'never')
```

    ## Using GitHub PAT from the git credential store.

    ## Downloading GitHub repo TycheLab/CMIP6VisR@HEAD

    ## ── R CMD build ─────────────────────────────────────────────────────────────────
    ##          checking for file 'C:\Users\hebam\AppData\Local\Temp\RtmpukUjz7\remotes45606caf3c4b\TycheLab-CMIP6VisR-f293379eea92899dd5ddd8d9fa11f21c6f9cda95/DESCRIPTION' ...     checking for file 'C:\Users\hebam\AppData\Local\Temp\RtmpukUjz7\remotes45606caf3c4b\TycheLab-CMIP6VisR-f293379eea92899dd5ddd8d9fa11f21c6f9cda95/DESCRIPTION' ...   ✔  checking for file 'C:\Users\hebam\AppData\Local\Temp\RtmpukUjz7\remotes45606caf3c4b\TycheLab-CMIP6VisR-f293379eea92899dd5ddd8d9fa11f21c6f9cda95/DESCRIPTION'
    ##       ─  preparing 'CMIP6VisR':
    ##    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
    ##       ─  checking for LF line-endings in source and make files and shell scripts
    ##   ─  checking for empty or unneeded directories
    ##       ─  building 'CMIP6VisR_1.0.0.tar.gz'
    ##      
    ## 

    ## Installing package into 'C:/Users/hebam/AppData/Local/Temp/RtmpiQAUFN/temp_libpath3f7c50ed7d5a'
    ## (as 'lib' is unspecified)

``` r
library(CMIP6VisR)

?`CMIP6VisR-package`
```

## Funding

The package was fully funded by the Global Water Futures - Core
Modelling and Forecasting (<https://gwf.usask.ca/core-modelling/>)
program, University of Saskatchewan.

## Authors

Coded by: Kevin Shook, Konstantinos Andreadis, and Heba Abdelmoaty
Conceptual design by: Simon Michael Papalexiou Maintained by: Kevin
Shook

## References

Abdelmoaty, H.M., Rajulapati, C.R., Nerantzaki, S.D. et
al. Bias-corrected high-resolution temperature and precipitation
projections for Canada. Sci Data 12, 191 (2025).
<https://doi.org/10.1038/s41597-025-04396-z>
