#' FEVD plot on ggplot2
#'
#' @param fevd A fevd class object created by vars::fevd()
#' @param type Geom type to use
#'
#' @return a ggplot2 object class
#' @importFrom tidyr gather
#' @importFrom dplyr as_tibble mutate bind_rows
#' @importFrom ggplot2 ggplot geom_area aes facet_grid vars
#' @export
#'
#' @examples
#'

tsp.var.fevd <- function(fevd,type = c("area","col")){
  if(class(fevd) != "varfevd"){stop("only varfevd object supported")}
  type <- match.arg(type,c("area","col"))

  plot <- purrr::map_df(fevd,.f = ~as.data.frame(.x),.id = "variable") %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(lag = seq_along(variable)) %>%
    tidyr::pivot_longer(-c(variable,lag)) %>%
    ggplot2::ggplot(ggplot2::aes(lag,value,fill = name)) +
    ggplot2::facet_grid(rows = ggplot2::vars(variable))+
    {if(type=="area"){ggplot2::geom_area()}else{ggplot2::geom_col()}} +
    ggplot2::scale_x_continuous(n.breaks = length(fevd[[1]][,1]))
  return(plot)
}
