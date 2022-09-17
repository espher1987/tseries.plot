#' Time series with nominal variable
#'
#' @param series univariable timeseries object of class ts
#' @param nominal dichotomous(binary) vector to use as categorical variable
#' @param ... arguments to be passed to plot()
#'
#' @return plot object
#' @export
#'
#' @examples
#' series <- rnorm(120,120,120)
#' nominal <- rbinom(120,1,0.5)
#' tsp.nominal(series,nominal)
#'
tsp.nominal <- function(series,nominal,...){

  if(!{length(series)==length(nominal)}){
    stop("series and nominal variables length differ")
  }
  if(!is.numeric(series)){
    stop("series is not numerical")
  }
  if(!all(nominal %in% c(0,1))){
    stop("nominal should be dichotomous (binary) variable with only 0 and 1 values")
  }

x <- series
f <- (nominal)
time <- zoo::index(zoo::as.zoo(x))
index = seq_along(f)

xright <- ifelse(f==1,index+1,NA)

plot(index,x,
             type = "n",axes = F,...)
rect(xleft = index,
     ybottom = min(x)*0.1,
     ytop = max(x),
     xright = xright,
     col = "gray",
     border = NA)
lines(index,x)
axis(side = 1,
     at = index,
     labels = time,outer = F)
axis(side = 2,at = round(seq(0,max(x),
                             length.out = 10)),
     outer = F,pos = 1)
}
