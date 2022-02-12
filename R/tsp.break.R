#' Breakpoints plot for time series
#'
#' @param model breakpointsfull or breakpoints class object created with strucchange::breakpoints()
#' @param level confidence level used in stats::confint()
#' @param ... additional argument(s) for stats::confint()
#'
#' @return
#' @export
#'
#' @examples
tsp.break <- function(model, level = NULL,...){
  if(!is(model,c("breakpointsfull","breakpoints"))){
    stop("only breakpointsfull or breakpoints class object created with strucchange::breakpoints() is accepted")
  }
  if(as.logical(prod(is.na(breakdates(model,level = NULL))))){
    stop("No breakpoints detected")
  }
  if(is.null(level)){
    level <- seq(0.99,0.90,length = 10)
  }

  c <- length(level)
  bd <- breakdates(model)
  nb <- length(breakdates(model))


conf <- vector(mode = "list",length = 0L)
for(b in 1:nb){
  temp_data <- data.frame(b = rep(b,c),
                          ci = rep(NA,c),
                          ld =rep(NA,c),
                          hd =rep(NA,c))
    for (i in 1:c) {
      temp_data[i,2] <- level[i]
      ci <- confint(model,level = level[i],...)
      l <- ci$confint[b,1]
      h <- ci$confint[b,3]
      temp_data[i,3] <- time(fitted(model))[l]
      temp_data[i,4] <- time(fitted(model))[h]
    }
  conf[[length(conf)+1]] <- temp_data
}

brk <- tibble::tibble(conf) %>%
  dplyr::mutate(date = bd) %>%
  dplyr::rowwise(date) %>%
  tidyr::unnest(conf)

df <- data.frame(time = time(fitted(model)),
                 fitted = fitted(model),
                 observed = model$y)

df <- tidyr::pivot_longer(df,-time)

plot <- ggplot2::ggplot()+
  ggplot2::geom_segment(ggplot2::aes(x = brk$ld,xend = brk$hd,
                                     y = c(min(df$value)),
                                     yend=c(min(df$value)),
                                     color = brk$ci),size = 4)+
  ggplot2::geom_segment(ggplot2::aes(x = brk$ld,
                                     xend = brk$hd,
                                     y = c(max(df$value)),
                                     yend=c(max(df$value)),
                                     color = brk$ci),size = 3)+
  ggplot2::geom_line(data = df,ggplot2::aes(x=time,
                                            y=value,
                                            linetype=name)) +
  ggplot2::geom_vline(xintercept = strucchange::breakdates(model),
             linetype = 3) +
  ggplot2::labs(x= "time",y = "value",color = "level",linetype = "type") +
  ggplot2::scale_linetype_manual(values = c("fitted"=2,
                                   "observed"=1))
return(plot)
}
