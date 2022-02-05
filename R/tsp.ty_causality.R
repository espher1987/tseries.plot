#' Toda and Yamamoto Causality Test
#'
#' @param data object of class mts created with ts()
#' @param p lag order for VAR, use vars::VARselect()
#' @param d max integration order I(d), use Unit Root test to determinate
#' @param print_model Logical, print VAR model used for test
#' @param ... extra parameters used in vars::VAR()
#'
#' @return list object with VAR and Toda-Yamamoto causality test
#' @export
#'
#' @examples
#' library(tseries.plot)
#' library(vars)
#' data("Canada")
#' tsp.toda_yamamoto(Canada,1,1)

tsp.toda_yamamoto <- function(data,p=1,d=1,print_model = FALSE,...){
  result <- vector(length = 0L)
  model <- vars::VAR(y = data,p = {p+d},...)

  if (print_model == TRUE) {
    print(model)
  }

  for (i in 1:ncol(data)) {

    caused <- colnames(data)[i]
    cause <- colnames(data)[colnames(data) != caused]
    test <- model$varresult[[caused]]

    criteria_1 <- paste(colnames(data),sep = ".")
    criteria_1 <- stringr::str_flatten(criteria_1,"|")
    test_1 <- stringr::str_starts(names(coef(test)),criteria_1)

    criteria_2 <- paste("l",{p+1}:{p+d},sep = "")
    criteria_2 <- stringr::str_flatten(criteria_2,"|")
    test_2 <- !{stringr::str_ends(names(coef(test)),criteria_2)}

    #Variable a comprobar
    for (c in 1:length(cause)) {
      variable <- cause[c]
      test_3 <- stringr::str_starts(names(coef(test)),variable)

      index <- 1:length(coef(test))
      Terms <- index[test_1 & test_2 & test_3]

      t <- aod::wald.test(Sigma = vcov(test),
                     b = coef(test),
                     Terms = Terms)

      result[length(result)+1]<- list(data.frame(caused = caused,
                                                 sign = "<-",
                                                 cause = variable,
                                                 chisq = t$result$chi2[[1]],
                                                 df = t$result$chi2[[2]],
                                                 p = t$result$chi2[[3]],
                                                 H0 = stringr::str_flatten(paste(names(t$b)[t$Terms],t$H0,sep = "="),collapse = ",")))
    }
  }
  ty_result <- tidyr::unnest(dplyr::rowwise(tibble::tibble(result)),cols = result)
  res <- list("model" = model,
              "result" = ty_result)
  return(res)
}
