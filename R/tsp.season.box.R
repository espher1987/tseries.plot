#' @title Season plot using boxplot
#' @description Plot using ggplot2 a
#' time series object, ts class. Season is
#' used as factor to group also use
#' decompose method to remove trend
#' @param x time series object of class "ts"
#' @param type decomposition type multiplicative (default)
#' or additive.
#' @param table logical, if true(default) median, mean, sd and var
#' values are printed, using season as group
#' @import ggplot2
#' @importFrom stats decompose na.omit cycle median sd var time
#' @importFrom dplyr group_by summarize
#' @export

tsp.season.box <- function(x = x,
                           type  = c("none","multiplicative","additive"),
                           table = TRUE)
{
if(class(x)!="ts"){stop("Only for ts class")}

type <- match.arg(arg     = type,
                  choices = c("none","multiplicative","additive"))

if(type == "none"){
  x <- x
}

if(type == "multiplicative"){
  x <- decompose(x,type = type)
  x <-x$x/x$trend
  x <- na.omit(x)}

if(type == "additive"){
  x <- decompose(x,type = type)
  x <-x$x-x$trend
  x <- na.omit(x)}

  season <- cycle(x)
  season.factor <- factor(season)
  df.plot  <- data.frame(season.factor,x)
  plot <- ggplot(df.plot) +
    geom_boxplot(mapping = aes(x = season.factor,
                               y = as.numeric(x))) +
    labs(x = "Season",
         y =  "Serie", caption = paste(type,"model"))

  df.table  <- df.plot %>% group_by(season = season.factor) %>% summarise(median  = median(x),
                                                                          mean    = mean(x),
                                                                          sd      = sd(x),
                                                                          var     = var(x))
  if(table == T){print.data.frame(df.table)}
  plot
}
