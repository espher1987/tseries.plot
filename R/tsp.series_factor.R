#' Time series with factor variable plot
#'
#' @param x Univaritate Time series, "ts" class object
#' @param factor "factor" class object
#' @param color logical, True (default) print plot with colors
#' @param ggplot logial, True (default) use ggplot2 device
#'
#' @return
#' @export
#'
#' @examples
#'series <- ts(rnorm(120,120,120),start = c(2012,1),frequency = 12)
#'factor <- factor(x = rbinom(120,1,0.9),levels = 0:1,labels = c("No crisis","Crisis"))
#'tsp.series_factor(series,factor)
tsp.series_factor <- function(x,factor,color = TRUE, gg = TRUE){
  if(!is.ts(x)){
    stop("use ts object class for x parameter")
  }
  if(!is.factor(factor)){
    stop("use factor object for factor parameter")
  }
  if(!is.logical(color)){
    stop("use logical values for color parameter")
  }
  if(!is.logical(gg)){
  stop("use logical values for gg parameter")
  }
  t <- zoo::as.Date(zoo::index(zoo::as.zoo(x)))
  n <- length(x)
  f <- factor
  i <- seq_along(x)

  if (isTRUE(color)) {
    colors <- factor(x = f,levels = levels(f),labels = rainbow(n = nlevels(f),s = 0.5))
  } else{
    colors <- factor(x = f,levels = levels(f),labels = gray.colors(n = nlevels(f),start = 1,end = 0.5))
  }
  if(isTRUE(gg)) {
    ggplot2::ggplot() +
      labs(x = "time",y = "value",fill = "Factor")+
      ggplot2::geom_rect(ggplot2::aes(xmin = i-1,
                    xmax = i,
                    ymin = min(x),
                    ymax = max(x),
                    fill = f)) +
      ggplot2::geom_line(mapping = ggplot2::aes(i,x)) +
      ggplot2::scale_x_continuous(breaks = round(quantile(i),digits = 0),
                                  labels = trunc(time(x))[round(quantile(i))])+
      if(color){
        ggplot2::scale_fill_discrete()
      }else{
        ggplot2::scale_fill_grey(start = 1,end = 0.5)
      }
  }else{
    par <- par(xaxs = "i",yaxs = "i")
    plot(i,as.numeric(x),type = "n",xlab = "time",axes = FALSE,ylab = "value")
    rect(xleft = i-1,xright = i,ybottom = min(x),ytop = max(x),
         col = as.character(colors),
         border = as.character(colors))
    lines(i,as.numeric(x))
    legend(x = "topleft",
           legend = levels(f),
           fill = levels(colors))
    axis(side = 1,at = round(quantile(i),digits = 0), labels = trunc(time(x))[round(quantile(i))])
    axis(side = 2,at = round(quantile(x)),gap.axis = 1)
    par(par)
  }
}
