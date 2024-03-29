% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tsp.trend.filter.R
\name{tsp.trend.filter}
\alias{tsp.trend.filter}
\title{Trend filter Plot}
\usage{
tsp.trend.filter(x, type = c("HP", "BK", "CF", "BW", "TR"), table = TRUE)
}
\arguments{
\item{x}{time series object of class "ts"}

\item{type}{filter type, the filter types are:
"HP" (Hodrick-Prescott) by default,
"BK" (Baxter-King),
"CF" (Christiano-Fitzgerald),
"BW" (Butterworth)
and "TR" (trigonometric regression).}

\item{table}{print table with basic statistic}
}
\description{
Use the mfilter package to filter a
time series and plot the trend using ggplot2
}
\seealso{
\code{\link[stats]{decompose}}
}
\author{
Victor Espinoza
}
