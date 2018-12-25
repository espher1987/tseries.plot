#' Irf VAR plot
#'
#' @param x irf class object created by vars::irf() function
#'
#' @return A ggplot2 object class
#' @export
#' @importFrom dplyr as_tibble tally mutate bind_rows
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot geom_ribbon aes geom_line geom_hline facet_grid vars scale_x_continuous
#'

tsp.var.irf <- function(x){

  if(class(x) != "varirf"){stop("only varirf object supported")}
  irf <- x
  # to avoid --as-cran check problem
  impulse     <- NULL
  lag         <- NULL
  value       <- NULL
  response    <- NULL

  nlag <- irf$irf[[1]] %>% as_tibble() %>% tally() %>% as.numeric() -1
  nimp <- length(irf$impulse)
  nres <- length(irf$response)
  impulsenames <- irf$impulse

  df.mean <- list()
  df.lower <- list()
  df.upper <- list()


  for (i in 1:nimp) {
    impulse <- rep(paste("impulse:",names(irf$irf[i])),nlag+1)
    df.mean[[i]]  <-   irf$irf[[i]] %>%
      as_tibble() %>%
      mutate(impulse = impulse,
             lag = 0:nlag) %>%
      gather("response",
             value = "value",
             -impulse,
             -lag)
  }


if(irf$boot == T){
  for (i in 1:nimp) {
    impulse <- rep(paste("impulse:",names(irf$irf[i])),nlag+1)
    df.lower[[i]]  <-   irf$Lower[[i]] %>%
      as_tibble() %>%
      mutate(impulse = impulse,
             lag = 0:nlag) %>%
      gather("response",
             value = "value",
             -impulse,
             -lag)
  }

  for (i in 1:nimp) {
    impulse <- rep(paste("impulse:",names(irf$irf[i])),nlag+1)
    df.upper[[i]]  <-   irf$Upper[[i]] %>%
      as_tibble() %>%
      mutate(impulse = impulse,
             lag = 0:nlag) %>%
      gather("response",
             value = "value",
             -impulse,
             -lag)
  }

  df.mean  <- bind_rows(df.mean)
  df.lower <- bind_rows(df.lower)
  df.upper <- bind_rows(df.upper)

  plot <-  ggplot(df.mean) +
    geom_ribbon(mapping = aes(x     = df.mean$lag,
                              ymin  = df.lower$value,
                              ymax = df.upper$value),
                alpha = 0.2) +
    geom_line(data = df.mean,mapping = aes(lag,value)) +
    geom_hline(yintercept = 0, color = "red", lty = 2) +
    labs(x = "lag", y = "response") +
    facet_grid(rows = vars(response),cols = vars(impulse)) +
    scale_x_continuous(breaks = 0:nlag)
}

if(irf$boot == F){
  df.mean  <- bind_rows(df.mean)

  plot <-  ggplot(df.mean) +
    geom_line(mapping = aes(lag,value)) +
    geom_hline(yintercept = 0, color = "red", lty = 2) +
    labs(x = "lag", y = "response") +
    facet_grid(rows = vars(response),cols = vars(impulse)) +
    scale_x_continuous(breaks = 0:nlag)

}

  print(plot)

}
