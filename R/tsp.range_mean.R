#' Range mean plot for time series
#'
#' @param x univariate time series object, created with ts()
#'
#' @return table and plot with range mean information
#' @export
#'
#' @examples
#' library(tseries.plot)
#' tsp.range_mean(AirPassengers)
#'
tsp.range_mean <- function(x) {
  if(!is(x,"ts")){stop("only for ts class")}
  if(is(x,"mts")){stop("multivariate time series not supported")}
  if(frequency(x)==1){stop("only seasonal ts object, frequency>1")}
  x <- na.omit(x)
  df <- data.frame(value = as.numeric(x),
                   year = trunc(time(x)))

  plot_df <- data.frame(year = unique(df$year),
                        mean = as.numeric(with(df,by(data = df,
                                                     INDICES = year,
                                                     FUN = {function(x){mean(x$value)}}))),
                        range = as.numeric(with(df,by(data = df,
                                                      INDICES = year,
                                                      FUN = {function(x){max(x$value)-min(x$value)}}))))

  plot <- ggplot2::ggplot(data = plot_df, ggplot2::aes(mean,range)) +
    ggplot2::geom_density_2d_filled(show.legend = F) +
    ggplot2::geom_point(size=5,alpha=0.3,show.legend = F)

  print(plot)
  return(plot_df)
}
