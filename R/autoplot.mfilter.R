#' @title autoplot for mFilter
#' @description Add mFilter to autplot
#' @param x mFilter class object
#' @seealso \code{\link[mFilter]{mFilter}}
#' @import ggplot2 gridExtra mFilter
#' @export

autoplot.mFilter <- function(x)
{
filter <- x

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
  labs(x = "time", y = "value", color = "type", caption = filter$title)

suppressWarnings(grid.arrange(plot.1,plot.2))
}
