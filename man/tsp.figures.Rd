% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tsp.figures.R
\name{tsp.figures}
\alias{tsp.figures}
\title{time series figures plot}
\usage{
tsp.figures(x = x, type = c("multiplicative", "additive"), table = T)
}
\arguments{
\item{x}{time series, ts class object}

\item{type}{type of decompose model.}

\item{table}{logical. If TRUE (default), table is printed}
}
\description{
Plot figures for a time series
}
\details{
if time series frequency is > 1, decompose model can be used to
obtain figures values for each season. This funtion use ggplot2 to
draw this figures. If multiplicative model is used (default),
"figures - 1" values is drawed for better interpretation. for
additive model figures is drawed.
}
\seealso{
\code{\link{decompose}}
}
\author{
Victor Espinoza
}
