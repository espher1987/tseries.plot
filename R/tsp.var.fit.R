#' Fitted and observed values from VAR model
#'
#' @param model 'varest' class object from vars::VAR()
#'
#' @return grobs object from gridExtra::grid.arrange()
#' @export
#'
#' @examples
tsp.var.fit <- function(model){

  if(class(model)!="varest"){
    stop("Only 'varest' class object supported")
  }

  plot_list <- vector(mode = "list",length = 0L)

  for (i in 1:length(model$varresult)) {
    test <- model$varresult[[i]]
      t <- time(model$y[,i])
      l <- length(t)
      f <- fitted(test)
      o <- test$model$y
      t <- t[{lag+1}:l]
      data <- data.frame(t,f,o)

plot_list[[length(plot_list)+1]]  <- ggplot2::ggplot(data,ggplot2::aes(x = t))+
    ggplot2::geom_line(ggplot2::aes(y = f, color = "fitted"))+
    ggplot2::geom_line(ggplot2::aes(y = o, color = "observed")) +
    ggplot2::labs(color = "Type",
                  x = "time",
                  y = "value")}
  plot_final <- gridExtra::grid.arrange(grobs = plot_list,
                          ncol = 2)
  return(plot_final)
  }
