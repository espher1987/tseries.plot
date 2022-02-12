#' Irf VAR plot
#'
#' @param irf irf class object created by vars::irf() function
#' @param color color fill for ggplot2::geom_ribbon()
#' @return A ggplot2 object class with irf from vars::irf()
#' @export
#' @importFrom dplyr as_tibble tally mutate bind_rows
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_line geom_hline facet_grid vars scale_x_continuous
#'
tsp.var.irf <- function(irf,color = NULL){

  if (class(irf) %in% "varirf") {
  } else{
    stop("Only 'varirf' class object from vars::irf()")
  }


  plot <- purrr::map_df(irf[1:3],
                .f = ~as.data.frame(.x),
                .id = "type") %>%
    dplyr::group_by(type) %>%
    dplyr::mutate(lag = seq_along(type)-1) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(cols = -c(type,lag),
                        names_to = c("impulse","response"),
                        names_sep = "\\.",
                        values_to = "value") %>%
    tidyr::pivot_wider(id_cols = c(impulse,response,lag),
                       names_from = type,
                       values_from = value) %>%
    dplyr::mutate(impulse = paste(impulse,"(imp)",sep = ""),
                  response = paste(response,"(res)",sep = "")) %>%
    ggplot2::ggplot(aes(x = lag)) +
    {if(is.null(color)){ggplot2::geom_ribbon(ggplot2::aes(ymin = Lower,
                                                          ymax = Upper),
                                             fill = NA,
                                             linetype = 2,
                                             color = 1)}else{
                         ggplot2::geom_ribbon(ggplot2::aes(ymin = Lower,
                                                           ymax = Upper),
                                              fill = color)
                                      }}+
    ggplot2::geom_line(aes(y=irf)) +
    ggplot2::facet_grid(cols = ggplot2::vars(impulse),
                        row = ggplot2::vars(response)) +
    ggplot2::geom_hline(yintercept = 0,linetype = 3) +
    ggplot2::scale_x_continuous(labels = as.integer)
  return(plot)
  }
