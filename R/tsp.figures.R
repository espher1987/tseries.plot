#' @title time series figures plot
#' @description Plot figures for a time series
#' @param x time series, ts class object
#' @param type type of decompose model.
#' @param table logical. If TRUE (default), table is printed
#' @details
#' if time series frequency is > 1, decompose model can be used to
#' obtain figures values for each season. This funtion use ggplot2 to
#' draw this figures. If multiplicative model is used (default),
#' "figures - 1" values is drawed for better interpretation. for
#' additive model figures is drawed.
#' @author Victor Espinoza
#' @seealso \code{\link{decompose}}
#' @export
#' @import ggplot2

tsp.figures <- function(x = x, type = c("multiplicative","additive"), table = T){

  if(class(x)!="ts"){stop("only for ts class")}
  if(frequency(x)==1){stop("only for ts with frequency > 1")}

  type <- match.arg(arg = type,
                    choices = c("multiplicative","additive"))

  d <- decompose(x =  x, type = type)

  if(start(x)[2]!=1){
  figure <- d$seasonal[{frequency(x)-start(x)[2]+2}:{frequency(x)-start(x)[2]+1+frequency(x)}]}

  if(start(x)[2]==1){
  figure <- d$figure}

  if(type == "multiplicative"){figure.plot <- figure - 1}
  if(type == "additive"){figure.plot <- figure}

  lab.y <- ifelse(type == "multiplicative", "figures -1", "figures")

   plot <- ggplot() +
    geom_col(mapping = aes( x = 1:length(figure), y = figure.plot)) +
    labs(x = "season", y = lab.y, caption = paste(d$type,"model")) +
    scale_x_continuous(breaks = 1:length(d$figure))


  if(type == "multiplicative"){
    df.table <- data.frame(season = 1:length(figure),
                         figure,
                         diff = figure - 1)}
  if(type == "additive")
    {df.table <- data.frame(season = 1:length(figure),
                           figure)
    }

  if(table == T){print.data.frame(df.table)}
  plot
}
