#' Breakpoints plot for time series
#'
#' @param model breakpointsfull or breakpoints class object created with strucchange::breakpoints()
#' @param level confidence level used in stats::confint()
#' @param ... additional argument(s) for stats::confint()
#'
#' @return
#' @export
#'
#' @examples
tsp.break <- function(model, level = NULL,...){
  if(!is(model,c("breakpointsfull","breakpoints"))){
    stop("only breakpointsfull or breakpoints class object created with strucchange::breakpoints() is accepted")
  }
  if(as.logical(prod(is.na(breakdates(model,level = NULL))))){
    stop("No breakpoints detected")
  }
  if(is.null(level)){
    level <- 0.95
  }

  data <- data.frame(time = zoo::as.Date.ts(fitted(model)),
                     value = model$y,
                     fitted = fitted(model))

  ci <- confint(model,level = level,...)

  l <- character()
  m <- character()
  h <- character()

  for( i in 1:nrow(ci$confint)){
    l[i] <- as.character(data$time[ci$confint[,1][i]])
    m[i] <- as.character(data$time[ci$confint[,2][i]])
    h[i] <- as.character(data$time[ci$confint[,3][i]])}

  bk <- data.frame(low = as.Date(l),
                   mean = as.Date(m),
                   high = as.Date(h))

  plot <- ggplot() +
    geom_line(data = data,
              aes(time,value,lty = "serie")) +
    geom_line(data = data,
              aes(time,fitted,lty = "fitted")) +
    geom_segment(data = bk,mapping = aes(x = low,
                                         y = min(data$value),
                                         xend = high,
                                         yend = min(data$value)),
                 size =2, color = "red") +
    geom_segment(data = bk,mapping = aes(x = low,
                                         y = max(data$value),
                                         xend = high,
                                         yend = max(data$value)),
                 size =2, color = "red") +
    geom_vline(data = bk,mapping = aes(xintercept = mean),
               lty = 2, color = "red") +
    scale_linetype_manual(values = c(fitted = 2,
                                     serie = 1))
return(plot)
}
