#' Granger Causality test for multivariate VAR models
#' @description Perform Granger causality block test based on Wald Chi-Squared
#' @param model VAR object created with vars::VAR()
#'
#' @return tibble class data with causality test for each pair
#' @export
#'
#' @examples
#' library(tseries.plot)
#' library(vars)
#' data("Canada")
#' tsp.granger(VAR(Canada,2))
tsp.granger  <- function(model){
  if (class(model) %in% "varest") {
  } else{
    stop("Only 'varest' class object from vars::VAR()")
  }

  result <- vector(mode = "list",length = 0L)
  for (i in 1:ncol(model$y)) {
    caused <- colnames(model$y)[i]
    cause <- colnames(model$y)[colnames(model$y)!=caused]

    test <- model$varresult[[caused]]
    for (c in 1:length(cause)) {
      variable <- cause[c]
      criteria <- stringr::str_flatten(paste(variable,".",sep = ""),collapse = "|")
      criteria <- stringr::str_starts(names(coef(test)),criteria)
      index <- 1:length(coef(test))
      Terms <- index[criteria]
      t <- aod::wald.test(Sigma = vcov(test),b = coef(test),Terms = Terms)
      result[[length(result)+1]] <- tibble(caused = caused,
                                           sign = "<-",
                                           cause = variable,
                                           Chisq = t$result$chi2[[1]],
                                           df = t$result$chi2[[2]],
                                           p_value = t$result$chi2[[3]],
                                           H0 = stringr::str_flatten(paste(names(t$b[t$Terms]),t$H0,sep = "="),","))
    }
  }
  res <- tidyr::unnest(tibble::tibble(result),cols = result)
  return(res)
}
