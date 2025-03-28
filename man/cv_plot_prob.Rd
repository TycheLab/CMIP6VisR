% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv_plot_prob.R
\name{cv_plot_prob}
\alias{cv_plot_prob}
\title{Plot probability of exceedance for precipitation}
\usage{
cv_plot_prob(data)
}
\arguments{
\item{data}{A data frame with two columns: \code{date} (date or datetime) and
\code{precipitation} (numeric) as returned by \code{cv_basin_daily_precip()}.}
}
\value{
A ggplot object displaying the probability of exceedance of nonzero
precipitation. The returned plots look best when saved at the size
16.5 x 12 cm. You can easily change the font sizes using theme().
}
\description{
Plots the exceedance probabilities of non-zero values in a precipitation time
series as returned by \code{cv_basin_daily_precip()}, vs the precipitation value.
Uses a logarithmic scale for the y-axis.
}
\examples{
cv_plot_prob(eg_TS)
}
\seealso{
\code{\link{cv_basin_daily_precip}} \code{\link{cv_plot_TS}} \code{\link{cv_plot_season}}
}
