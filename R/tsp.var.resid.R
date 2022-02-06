#' Residuals plot from VAR model
#'
#' @param model 'varest' class object from vars::VAR()
#'
#' @return grobs object from gridExtra::grid.arrange()
#' @export
#'
#' @examples
tsp.var.resid <- function(model){

  if(class(model)!="varest"){
    stop("Only 'varest' class object supported")
  }

  plot_list <- vector(mode = "list",length = 0L)
  lag <- model$p
  for (i in 1:length(model$varresult)) {

    r <- ts(model$varresult[[i]]$residuals,end = end(model$y),frequency = frequency(model$y))

    plot_list[[length(plot_list)+1]] <- forecast::autoplot(r)+
      ggplot2::labs(y = paste(names(model$varresult[i]),"(residuals)"),
                    title = NULL)
    plot_list[[length(plot_list)+1]] <- forecast::ggAcf(r) +
    ggplot2::labs(y = paste(names(model$varresult[i]),"(Acf)"),
                  title = NULL)
    plot_list[[length(plot_list)+1]] <- forecast::ggPacf(r)+
      ggplot2::labs(y = paste(names(model$varresult[i]),"(Pacf)"),
                    title = NULL)}
  return(suppressMessages(
    gridExtra::grid.arrange(grobs = plot_list,
                            ncol = 3)))
}
