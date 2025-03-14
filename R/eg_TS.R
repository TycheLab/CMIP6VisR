#' eg_TS
#' 
#' A dataframe containing daily precipitation time series for a basin.
#' 
#' @format A dataframe with 31,411 rows and 2 columns.
#' @source CMIP6 simulation: "_ACCESS-CM2_ssp126_r2i1p1f1_gn_20150101-21001231"
#' @details 
#' This timeseries represents a weighted average daily precipitation time series over the basin 
#' given in the package. It comes from one simulation of the CMIP6 model ACCESS-CM2 under 
#' the SSP1-2.6 scenario.
#' 
#' Variables: 
#' \describe{
#'   \item{date}{Date of the observation (YYYY-MM-DD format)}
#'   \item{precipitation}{Daily precipitation amount (mm)}
#' }
#' 
"eg_TS"
NULL
