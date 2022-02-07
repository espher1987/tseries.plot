#' Toda and Yamamoto Causality Test
#'
#' @param data object of class mts created with ts()
#' @param p lag order for VAR, use vars::VARselect(), if NULL (default) automatic value is calculated
#' @param d max integration order I(d), if NULL (default) automatic value is calculated
#' @param criteria information criteria for automatic p order, used in vars::VARselect()
#' @param test Type of unit root test to use for automatic d value, used in forecast::ndiffs()
#' @param ... extra parameters used in vars::VAR()
#'
#' @return list object with VAR and Toda-Yamamoto causality test
#' @export
#'
#' @examples
#' library(tseries.plot)
#' library(vars)
#' data("Canada")
#' tsp.toda_yamamoto(Canada,1,1,type="both")
tsp.toda_yamamoto <- function(data,p=NULL,d=NULL,criteria = c("aic","hq","sc","fpe"),test = c("adf","kpss","pp"),...){

  if(prod(as.numeric(sapply(data,is.numeric)))==0){
    stop("Only numeric variables on matrix")
  }

  if(prod(complete.cases(data))==0){
    stop("NA values in data")
  }

  if (length(colnames(data))==0) {
    stop("Only named columns matrix supported")
  }

  if(!is.null(d)){
    warning("manual selection for d")
  }

  if(!is.null(p)){
    warning("manual selection for p")
  }

  if(is.null(d)){
    test <- match.arg(test,c("adf","kpss","pp"))
    d <- max(sapply(data, function(x){forecast::ndiffs(x,test = test)}))
    print(paste("Automatic selection for 'd' using",test,"test:d_max=",d))
  }

  if(is.null(p)){
    ic <- match.arg(stringr::str_to_upper(criteria),c("AIC","HQ","SC","FPE"))
    ic <- paste(ic,"(n)",sep = "")

    p <- getElement(vars::VARselect(Canada),"selection")[ic]
    print(paste("Automatic selection for 'p' using",ic,"criteria:p=",p))
  }

  result <- vector(length = 0L)
  model <- vars::VAR(y = data,p = {p+d},...)

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
