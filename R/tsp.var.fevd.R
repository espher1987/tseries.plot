#' FEVD plot on ggplot2
#'
#' @param x A fevd class object created by vars::fevd()
#'
#' @return a ggplot2 object class
#' @importFrom tidyr gather
#' @importFrom dplyr as_tibble mutate bind_rows
#' @importFrom ggplot2 ggplot geom_area aes facet_grid vars
#' @export
#'
#' @examples
#'

tsp.var.fevd <- function(x){
  if(class(x) != "varfevd"){stop("only varfevd object supported")}
  fevd <- x
# to avoid --as-cran check problem
  name      <- NULL
  lag       <- NULL
  value     <- NULL
  variable  <- NULL

nvars <- length(fevd)
nlag <- length(fevd[[1]][,1])
df.plot <- list()

for (i in 1:nvars) {
  variable <- rep(names(fevd[i]),nlag)
df.plot[[i]]  <- fevd[[i]] %>%
    as_tibble %>%
    mutate(name = paste("FEVD for",variable),
           lag = 1:nlag)
}

df.plot <- bind_rows(df.plot)
df.plot <-df.plot %>%
  gather(key = "variable",value = "value",-name,-lag)

ggplot(df.plot) + geom_area(aes(factor(lag),value,
                                group = variable, fill = variable)) +
  labs(x = "lag") +
  facet_grid(rows = vars(name))}
