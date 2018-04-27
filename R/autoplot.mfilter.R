autoplot.mFilter <- function(x)
{
filter <- x

plot.1 <- ggplot() +
  geom_line(aes(as.numeric(time(filter$trend)),
                as.numeric(filter$trend),color = "trend")) +
  geom_line(aes(as.numeric(time(filter$x)),
                as.numeric(filter$x),  color = " serie")) +
  labs(x = "time", y = "value", color = "type")

plot.2 <- ggplot() +
  geom_line(aes(as.numeric(time(filter$cycle)),
                as.numeric(filter$cycle), color = "cycle")) +
  labs(x = "time", y = "value", color = "type", caption = filter$title)

suppressWarnings(grid.arrange(plot.1,plot.2))
}
