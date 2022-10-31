#' Break points coefficients and t values
#'
#' @param data an object of class ts or as.ts() supported
#' @param breaks numbers of maximum breaks to found
#' @param model type of model to be used in breakpoints, trend (default) or const (for constant only)
#' @param log log transformation used in data for log percent change
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
tsp.break.coef <- function(data,
                           breaks = NULL,
                           model = c("trend", "const"),
                           log = FALSE,
                           plot = TRUE,
                           mfcol = NULL,...){

  if(any(sapply(data,is.na))){stop("NA values detected")}

  model <- match.arg(model, c("trend", "const"))

  if(log == TRUE){
    if(min(data)<=0){
      stop("Log transform is not posible, at least one values is 0 or negative")
    }
    data <- log(data);message("Log transformation used")
  }

  if(!is(data,c("zoo","ts"))){
    warning("Only ts objects supported")
  }

  if(is(data,"zoo")){
    data <- as.ts(data)
    warning("Object of class zoo detected, converted to ts, date index converted")
  }

  if(is(data,"ts") & is.null(ncol(data))){
    ncol = 1
    if (model == "trend") {
      trend <- seq_along(data)
      bk <- strucchange::breakpoints(data~trend, breaks = breaks,...)
    }
    if(model == "const"){
      bk <- strucchange::breakpoints(data~1, breaks = breaks,...)}
      bk <- list(bk)
      names(bk) <- "value"
    }else{
    ncol <- ncol(data)
    bk <- list()
    for (i in 1:ncol) {
    trend <- seq_along(data[,i])
    if(model == "trend"){
    bk[[i]] <- strucchange::breakpoints(data[,i]~trend, breaks = breaks,...)}
    if(model == "const"){
    bk[[i]] <- strucchange::breakpoints(data[,i]~1, breaks = breaks,...)}
    }
    names(bk) <- colnames(data)
    }

  suppressMessages(
  result <- tibble::tibble(names = names(bk),
         bk = lapply(bk, lmtest::coeftest)) %>%
    dplyr::rowwise(names) %>%
    dplyr::summarise(broom::tidy(bk)) %>%
    dplyr::rename(variable = names) %>%
    dplyr::mutate(begin = stringr::word(term,1),
                  end = stringr::word(term,3),
                  term = stringr::word(term,4)) %>%
    dplyr::relocate(variable,term,begin,end))

  if(plot == TRUE){
    seg <- purrr::map_dfc(bk,breakfactor) %>%
      dplyr::mutate(time = zoo::as.Date.ts(fitted(bk[[1]]))) %>%
      tidyr::pivot_longer(-time) %>%
      dplyr::rename(segment = value)

    fitted <- purrr::map_dfc(bk,fitted.values) %>%
      dplyr::mutate(time = zoo::as.Date.ts(fitted(bk[[1]]))) %>%
      tidyr::pivot_longer(-c(time)) %>%
      dplyr::rename(value_fitted = value)

    observed <- purrr::map_dfc(bk,function(data){data$y}) %>%
      dplyr::mutate(time = zoo::as.Date.ts(fitted(bk[[1]]))) %>%
      tidyr::pivot_longer(-c(time)) %>%
      dplyr::rename(value_observed = value)

    suppressMessages(
      plot_data <- dplyr::full_join(fitted,seg) %>%
        group_by(name,segment) %>%
        mutate(i = 1:length(segment),
               b = ifelse(i == max(i),as.character(time),NA)) %>%
        full_join(observed))

    pl <- ggplot2::ggplot(plot_data)+
      ggplot2::geom_line(ggplot2::aes(time,
                                      value_observed,
                                      lty = "observed"))+
      ggplot2::geom_line(ggplot2::aes(time,
                                      value_fitted,
                                      group = segment,
                                      lty = "fitted")) +
      ggplot2::geom_segment(ggplot2::aes(x = as.Date(b),
                                         y = -Inf,
                                         xend = as.Date(b),
                                         yend = Inf),
                            lty = 3) +
      ggplot2::scale_linetype_manual(values = c(fitted = 2,
                                                observed = 1)) +
      ggplot2::facet_grid(ggplot2::vars(name),
                          scales = "free_y")
  suppressWarnings(print(pl))
  }
  return(result)
}
