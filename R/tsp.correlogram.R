#' @title correlogram plot from a time series
#' @description Plot Acf, Pacf and Portmanteau test for ts or arima class object
#' @param x
#' ts or Arima object
#' @param type
#' type of portmanteau test to use.
#' "Ljung-Box" (default) and "Box-Pierce" are allowed.
#' @param lag.max
#' lag max used in acf, pacf and portmanteau test.
#' If lag.max is NA (default), lag.max = 10 for non seasonal ts
#' or Arima residual and 2*frequency(m) for seasonal ts or Arima residual.
#' @param table
#' logical. if TRUE (default) a table is displayed
#' @seasonal if TRUE (default) acf and pacf plot
#' show seasonal lag color different from regular lag
#' @details
#' plot Acf, Pacf and portmanteau test for
#' a time series object (ts class) or Arima residual
#' (Arima class) created with Arima() or arima(). If x
#' is Arima class object, residual are used to plot, and
#' fitdf inside portmanteau test is fitted to: lag-length(x$coef).
#' If table = TRUE, a table with number of lag, acf and pacf values
#' is displayed, with Q stat, degree of freedom and p-value for
#' portmanteau test.
#' @author Víctor Espinoza
#' @seealso
#' \code{\link[stats]{ts}}
#' \code{\link[stats]{arima}}
#' \code{\link[stats]{Box.test}}
#' \code{\link[forecast]{Arima}}
#' \code{\link[forecast]{Acf}}
#' \code{\link[forecast]{Pacf}}
#' \code{\link[forecast]{checkresiduals}}
#' @import ggplot2 forecast gridExtra
#' @export

tsp.correlogram <- function(x,
                            type = c("Ljung-Box","Box-Pierce"),
                            lag.max = NA,
                            table = TRUE,
                            seasonal = FALSE){

  # logical test for object class

  if(!is.ts(x) & !is.Arima(x)){
    stop("Only for ts or Arima object class")}


  type <- match.arg(arg = type,
                    choices = c("Ljung-Box","Box-Pierce"))

  # logical test for creation of objects

  if(is.Arima(x)){
    fitdf <- length(x$coef)
    m     <- frequency(fitted(x))
    serie <- x$residuals}

  if(is.ts(x)){
    fitdf  <- 0
    m      <- frequency(x)
    serie  <- x}

  if(is.na(lag.max)){
    lag.max <- ifelse(m==1,
                      10,
                      2*m)}
  if(lag.max > length(serie)){
    lag.max <- length(serie)/2
  }

  # Acf Pacf table

  acf  <- Acf(serie,  lag.max = lag.max, plot = F)$acf
  acf  <- acf[2:(length(acf))]
  pacf <- Pacf(serie, lag.max = lag.max, plot = F)$acf

  # White noise table
  lag     <- 1:lag.max
  p.value <- numeric()
  q.stat  <- numeric()
  df      <- numeric()

  for(i in (fitdf+1):lag.max){
    p.value[i] <- Box.test(x = serie,
                           lag = i,
                           type = type,
                           fitdf = fitdf)$p.value

    q.stat[i] <- Box.test(x = serie,
                          lag = i,
                          type = type,
                          fitdf = fitdf)$statistic

    df[i]     <- Box.test(x = serie,
                          lag = i,
                          type = type,
                          fitdf = fitdf)$parameter
  }

  # create a factor from p value to color in points
  color <- ifelse(p.value < 0.05, "p-value < 0.05", "p-value > 0.05")

  # Only seasonal
  if(!is.logical(seasonal)){stop("season must be logical")}

  if(seasonal == TRUE){
  season <- 1:lag.max %in% seq(frequency(serie),lag.max,by=frequency(serie)) %>%
    ifelse(yes = "black",no = "grey")}

  if(seasonal == FALSE){
    season <- rep(1,lag.max)}

  # final table
  df.plot <- data.frame(lag,acf,pacf,q.stat,df,p.value,color,season)

  if(table == T){print.data.frame(round(df.plot[1:6],digits = 5),row.names = F)}

  # x breaks
  ifelse(lag.max <= 10,
         breaks  <- 1:lag.max,
         breaks <- seq(from = lag.max/4,
                       to = lag.max,
                       length.out = 4)
  )

  #limit for acf and pacf
  limit <- 1.96/sqrt(length(serie))

  # y limits
  y.limit <- max(abs(df.plot[2:3]))

  if(limit > y.limit)
  {y.limit <-  limit}


  # Acf plot
  plot.acf <- ggplot(data = df.plot) +
    geom_segment(aes(x    = lag,    y = rep(0,length(lag)),
                     xend = lag, yend = acf),
                 color = season) +
    geom_hline(yintercept = c(1,-1)*limit, color = "blue", lty = 2) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = breaks,
                       limits = c(1,lag.max)) +
    scale_y_continuous(limits = c(-1,1)*y.limit) + labs(x = "",
                                                        y = "acf")

  # Pacf plot
  plot.pacf <- ggplot(df.plot) +
    geom_segment(aes(x    = lag,    y = rep(0,length(lag)),
                     xend = lag, yend = pacf),
                 color = season) +
    geom_hline(yintercept = c(1,-1)*limit, color = "blue", lty = 2) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = breaks,
                       limits = c(1,lag.max)) +
    scale_y_continuous(limits = c(-1,1)*y.limit) + labs(x = "",
                                                y = "pacf")
  # white noise plot

  plot.white <- ggplot(na.omit(df.plot)) + # to avoid warnings na.omit is used
    geom_point(aes(lag,round(p.value,2)))+
    geom_hline(yintercept = 0.05, color = "red", lty = 2) +
    geom_hline(yintercept = 0.01, color = "red", lty = 3) +
    labs(caption = paste("portmanteau test:",type,"","IC=","1.96/sqrt(length(serie))"), x = "lag",
         y = "p-value") +
    scale_x_continuous(breaks = breaks,
                       limits = c(1,lag.max)) +
    scale_y_continuous(breaks = c(0.05,
                                  round(max(p.value),digits = 2),
                                  round((max(p.value)-0.05)/2+0.05,digits = 2)
    )
    )

  # All plot combined
  plots <- list(plot.acf,plot.pacf,plot.white)
  grid.arrange(grobs = plots, ncol = 1)
}
