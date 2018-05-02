#' @title time series plot by year
#' @description Draw a boxplot of a time series, using year as grouping factor
#' @param x
#' time series, ts class object
#' @param table
#' logical, if TRUE (default) print a table with year,
#' median, mean, sd and variance values
#' @details Draw a boxplot for a time series object created with ts()
#' using year as grouping factor.
#' @author Víctor Espinoza
#' @seealso
#' \code{\link[stats]{ts}}
#' \code{\link[base]{trunc.Date}}
#' \code{\link[stats]{time}}
#' @import ggplot2
#' @export

tsp.year <- function(x = x,
                     table = TRUE){
if(!is.ts(x)){stop("only for ts class")}

  t <- time(x)
  year <- trunc.Date(x = t, units = "year")
  factor.year <- factor(year)
  plot <- ggplot() +
    geom_boxplot(mapping = aes(x = factor.year,
                               y = as.numeric(x))) +
    labs(x = "year",
         y = "serie")

  df.table <- data.frame(factor.year,x)
  df.table <- df.table %>%
    group_by(year = factor.year) %>%
    summarise(median = median(x),
                       mean = mean(x),
                       sd = sd(x),
                       var = var(x))
  if(table == T){print.data.frame(df.table)}
  plot
}
