#' Predicts and plot VAR model
#'
#' @param model VAR class objects from vars::VAR()
#' @param n_ahead integer, number of periods to forecast
#' @param color color used for min and max values in forecast
#' @param n_col number of columns used in plot
#' @param ... arguments passed to vars::predict()
#'
#' @return plot from gridExtra::grid.arrange()
#' @export
#'
#' @examples
#' library(vars)
#' library(tseries.plot)
#' data("Canada")
#' model <- VAR(Canada,3)
#' tsp.var.forecast(model,n_ahead = 4,color = "blue",n_col = 2)
#'
tsp.var.forecast <- function(model,n_ahead = 12,color = NULL,n_col = NULL,...){

  if(is.null(color)) {
    color  <-  rgb(0.7,0.7,0.7)
  }
  if(is.null(n_col)){
    n_col = 2
  }

  n <- nrow(getElement(model,"y"))

  if(n_col>ncol(getElement(model,"y"))) {
    warning("number of column > number of variables; fixed to number of variables")
    n_col <- ncol(getElement(model,"y"))
  }
frq <- frequency(getElement(model,"y"))
sta <- start(getElement(model,"y"))

pred <- predict(model,n.ahead = n_ahead,...)

t <- vector(mode = "numeric",length = n+n_ahead)
t <- time(ts(t,start = sta,frequency = frq))
f <- getElement(pred,"fcst")
f_table <- vector(mode = "list",length = 0L)

for (i in 1:length(f)) {
f_table[[length(f_table)+1]] <- tibble::as_tibble(f[[i]]) %>%
  dplyr::mutate(index = {n+1}:{{n}+{n_ahead}}) %>%
    tibble::add_column(var = names(f[i]))
}

forecast <- tibble::tibble(f_table) %>%
  tidyr::unnest(f_table) %>%
  dplyr::group_by(var) %>%
  dplyr::mutate(time = t[{n+1}:length(t)])

observed <- tibble::as_tibble(getElement(model,"y")) %>%
  dplyr::mutate(time = t[1:n]) %>%
  tidyr::pivot_longer(-time) %>%
  dplyr::rename(var = name)


plot_data <- dplyr::full_join(observed,forecast,by = c("time","var"))

vars <- names(getElement(model,"varresult"))
plot_list <- vector(mode = "list",length = 0L)

for (v in 1:length(vars)) {

    plot_list[[length(plot_list)+1]] <- plot_data %>%
    dplyr::filter(var == vars[v]) %>%
      ggplot2::ggplot()+
      ggplot2::geom_ribbon(ggplot2::aes(x=time,
                  ymin = lower,
                  ymax= upper),
              fill = color,
              color = color) +
      ggplot2::geom_line(aes(time,value)) +
      ggplot2::geom_line(aes(time,fcst)) +
      ggplot2::labs(y = vars[[v]])
}
return(suppressWarnings(suppressMessages(gridExtra::grid.arrange(grobs = plot_list,
                                                ncol = n_col))))
}
