tsp.range.mean <- function(x, table = T) {
  if(class(x)!="ts"){stop("only for ts class")}
  year <- trunc.Date(time(x))
  df <- data.frame(x,year)

  mr <- df %>% group_by(year) %>% summarise(mean = mean(x),
                                            range = max(x)-min(x))

  plot <- ggplot(data = mr, aes(mean,range)) +
    geom_point() +
    geom_point() +
    geom_smooth(method = "loess")

  if(table == T){ print.data.frame(mr,rowname = F)}
  plot
}
