#' @title Trend Decomposed Plot
#' @description Use the base decompose function to
#' plot a trend from a time series object using ggplot2
#' @param x time series object of class "ts"
#' @param type decomposition type multiplicative (default)
#' or additive.
#' @author Victor Espinoza
#' @seealso
#' \code{\link[stats]{decompose}}
#' @import ggplot2
#' @export
#'
tsp.trend.decompose <- function(x,
                                type = c("multiplicative","additive"))
{
type <- match.arg(arg = type,
                    choices = c("multiplicative","additive"))

d <- decompose(x,type = type)

trend <- d$trend
trend <- na.omit(trend)

plot <- ggplot() +
  geom_line(aes(as.numeric(time(x)),as.numeric(x),
                lty = "serie")) +
  geom_line(aes(as.numeric(time(trend)),
                as.numeric(trend),
                lty = "trend")) +
  labs(x = "time", y = "serie", lty = "type", caption = paste(d$type,"model")) +
  scale_linetype_manual(values = c("serie" = 2, "trend" = 1))

plot
}
