% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv_plot_season.R
\name{cv_plot_season}
\alias{cv_plot_season}
\title{Plot seasonal precipitation distribution}
\usage{
cv_plot_season(data)
}
\arguments{
\item{data}{A data frame with two columns: \code{date} (date or datetime) and
\code{precipitation} (numeric) as returned by \code{cv_basin_daily_precip()}.}
}
\value{
A ggplot object displaying the seasonal distribution of nonzero precipitation,
with mean values and P0 labelled. The returned plots look best when saved at
the size 16.5 x 14 cm. You can easily change the font sizes using theme().
}
\description{
Creates seasonal plots of a precipitation time series, as returned by \code{cv_basin_daily_precip()}. Assigns seasons based on the month,
computes the proportion of zero precipitation values (P0) and seasonal means for nonzero precipitation,
and generates a violin plot showing the seasonal distribution of precipitation.
}
\examples{
cv_plot_season(eg_TS)
}
\seealso{
\code{\link{cv_basin_daily_precip}} \code{\link{cv_plot_TS}} \code{\link{cv_plot_prob}}
}
