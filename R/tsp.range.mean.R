#' @title Range Mean Plot
#' @description Plot a Range Mean Plot using ggplot2 package
#' @param x ts class object
#' @param table logical, if true (default) print mean and range values
#' @seealso
#' \code{\link[base]{mean}}
#' \code{\link[base]{max}}
#' \code{\link[base]{min}}
#' @export
#'
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
