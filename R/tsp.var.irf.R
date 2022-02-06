#' Irf VAR plot
#'
#' @param irf irf class object created by vars::irf() function
#'
#' @return A ggplot2 object class with irf from vars::irf()
#' @export
#' @importFrom dplyr as_tibble tally mutate bind_rows
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_line geom_hline facet_grid vars scale_x_continuous
#'
tsp.var.irf <- function(irf){

  if (class(irf) %in% "varirf") {
  } else{
    stop("Only 'varirf' class object from vars::irf()")
  }


  fortify <- function(data){
    result <- vector(mode = "list",length = 0L)
    for (d in 1:length(data)) {
      result[[length(result)+1]]<- tibble::tibble(imp = names(data)[d],
                                          lag = 0:{nrow(data[[d]])-1},
                                          tibble::as_tibble(data[[d]]))
    }
    data <- dplyr::unnest(tibble::tibble(result),cols = result)
    return(data)}

  data_irf <- fortify(irf$irf)
  data_lower <- fortify(irf$Lower)
  data_upper <- fortify(irf$Upper)
  suppressMessages(
  plot_data <- tibble::add_column(data_irf,type = "mean") %>%
    dplyr::full_join(tibble::add_column(data_lower,type = "lower")) %>%
    dplyr::full_join(tibble::add_column(data_upper,type = "upper")) %>%
    dplyr::pivot_longer(cols = -c(imp,lag,type)) %>%
    dplyr::mutate(imp = paste(imp,"(imp.)"),
           name = paste(name,"(res.)")))

  plot <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(aes(x = lag,value,
                  lty = type),show.legend = F) +
    ggplot2::facet_grid(cols = vars(imp),
               rows = vars(name)) +
    ggplot2::scale_linetype_manual(values = c("lower"=2,
                                     "upper"=2,
                                     "mean"=1)) +
    ggplot2::geom_hline(yintercept = 0,lty = 3) +
    ggplot2::scale_x_continuous(labels = as.integer)
  return(plot)
  }
