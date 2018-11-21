#' @title Trend filter Plot
#' @description Use the mfilter package to filter a
#' time series and plot the trend using ggplot2
#' @param x time series object of class "ts"
#' @param type filter type, the filter types are:
#' "HP" (Hodrick-Prescott) by default,
#' "BK" (Baxter-King),
#' "CF" (Christiano-Fitzgerald),
#' "BW" (Butterworth)
#' and "TR" (trigonometric regression).
#' @param table print table with basic statistic
#' @author Victor Espinoza
#' @seealso
#' \code{\link[stats]{decompose}}
#' @import ggplot2 mFilter gridExtra
#' @export
#'


tsp.trend.filter <- function(x,
                             type = c("HP","BK","CF","BW","TR"),
                             table = TRUE)
{

  if(class(x)!="ts"){stop("Only for ts class")}

  type   <- match.arg(arg = type,
                      choices = c("HP","BK","CF","BW","TR")
  )

  filter <- mFilter(x = x, filter = type)

  plot.1 <- ggplot() +
    geom_line(aes(as.numeric(time(filter$trend)),
                  as.numeric(filter$trend),lty = "trend")) +
    geom_line(aes(as.numeric(time(filter$x)),
                  as.numeric(filter$x),  lty = "serie")) +
    labs(x = "time", y = "value", lty = "type") +
    scale_linetype_manual(values = c("trend" = 1,
                                     "serie" = 2))

  plot.2 <- ggplot() +
    geom_line(aes(as.numeric(time(filter$cycle)),
                  as.numeric(filter$cycle), lty = "cycle")) +
    labs(x = "time", y = "value", lty = "type", caption = filter$title)

  if(table == T){print(filter)}

  grid.arrange(plot.1,plot.2)
}
