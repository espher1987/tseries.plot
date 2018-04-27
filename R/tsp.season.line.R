#' @title Season plot by line
#' @description Plot using ggplot2 a
#' time series using year as group
#' @param x time series to plot,
#' only ts class
#' @import ggplot2
#' @export

tsp.season.line <- function(x){

  if(class(x)!="ts"){stop("only for ts class")}

year <- trunc.Date(time(x))
season <- cycle(x)

df.plot <- data.frame(year,season,x)
df.plot
plot <- ggplot(df.plot) +
  geom_line(aes(as.numeric(season),
                as.numeric(x),
                color = as.numeric(year),
                group = as.numeric(year))) +
  labs(color = "year", y = "value", x = "season") +
  scale_color_continuous(breaks = c(min(year),
                                    round(median(year)),
                                    max(year)),
                         low = "white",
                         high = "grey2") +
  scale_x_continuous(breaks = 1:frequency(x))

message(paste(
  "N :", length(x),"","",
  "start :",start(x)[1],"","",
  "end: ",end(x)[1],"","",
  "frequency :",frequency(x)))

plot
}
