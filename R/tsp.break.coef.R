#' Break points coefficients and t values
#'
#' @param data an object of class ts or as.ts() supported
#' @param breaks numbers of maximum breaks to found
#' @param model type of model to be used in breakpoints, trend (default) or const (for constant only)
#' @param log log transformation used in data for log percent change
#' @param plot logical, plot breaks using graphics
#' @param mfcol vector of length  2 to define plot matrix
#' @param ... arguments passed to breakpoints
#'
#' @details Create a linear model to determinate structural breaks
#'
#' @return tibble object
#' @export
#' @examples
#' library(vars)
#' data("Canada")
#' tsp.break.coef(Canada)
#'
tsp.break.coef <- function(data, breaks = NULL, model = c("trend", "const"), log = FALSE,plot = TRUE,mfcol = NULL,...){

  model <- match.arg(model, c("trend", "const"))

  if(log == TRUE){
    if(min(data)<=0){
      stop("Log transform is not posible, at least one values is 0 or negative")
    }
  }
  if(log == TRUE){data <- log(data);message("Log transformation used")}

  if(!is(data,c("zoo","ts"))){
    warning("Only ts objects supported")
  }

  if(is(data,"zoo")){
    data <- as.ts(data)
    warning("Object of class zoo detected, converted to ts, date index converted")
  }

  if(is(data,"ts") & is.null(ncol(data))){
    ncol = 1
    trend <- seq_along(data)
    if(model == "trend"){
      result <- strucchange::breakpoints(data~trend,breaks = breaks,...)}
    if(model == "const"){
      result <-  strucchange::breakpoints(data~1,breaks = breaks,...)}
        if(plot == TRUE){
        plot(data)
        lines(fitted(result))
        lines(confint(result))
        result <- broom::tidy(lmtest::coeftest(result))
        return(result)}}else{
  coefs <- list()
  for (i in 1:ncol(data)) {
    trend <- seq_along(data[,i])
    if(model == "trend"){
    coefs[[i]] <- strucchange::breakpoints(data[,i]~trend, breaks = breaks,...)}
    if(model == "const"){
    coefs[[i]] <- strucchange::breakpoints(data[,i]~1, breaks = breaks,...)}
  }
  names(coefs) <- colnames(data)
  suppressMessages(
  result <- tibble::tibble(names = colnames(data),
         coefs = lapply(coefs, lmtest::coeftest)) %>%
    dplyr::rowwise(names) %>%
    dplyr::summarise(broom::tidy(coefs)) %>%
    dplyr::rename(variable = names))

  if(plot == TRUE){
    if(prod(is.null(mfcol))){
      mfcol <- c(ncol(data),1)
    }
  par <- par(mfcol = mfcol,
             mar = c(4,4,1,1))
  for (i in 1:ncol(data)) {
    plot(data[,i],ylab = colnames(data)[i])
    lines(fitted(coefs[[i]]),lty = 2)
    lines(confint(coefs[[i]]),lty = 1)
  }
  par(par)
  }
  return(result)}
}
