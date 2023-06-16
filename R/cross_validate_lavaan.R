#' cross_validate_lavaan
#'
#' Cross-validate a model fitted with lavaan using a separate test set. Currently
#' only supports single-group models fitted with (full information) maximum likelihood.
#' @param lavaan_model model fitted with lavaan
#' @param test_set raw data set used for cross-validation
#' @examples
#' # example code
#'
#' @return -2 log-Likelihood of the test-set
#' @export
cross_validate_lavaan <- function(lavaan_model, test_set){

  train_data <- lavaan::lavInspect(lavaan_model, what = "data")
  test_set <- test_set[, colnames(train_data)]

  implied <- lavaan::lavInspect(lavaan_model, "implied")
  m2ll <- 0

  if(is.null(implied$mean)){
    implied$mean <- apply(train_data[complete.cases(train_data),],2,mean)
  }

  for(i in 1:nrow(test_set)){

    is_missing <- is.na(test_set[i,])

    if(all(is_missing))
      next

    implied_means_no_na <- implied$mean[!is_missing]
    implied_cov_no_na <- implied$cov[!is_missing, !is_missing]

    m2ll <- m2ll + (-2)*mvtnorm::dmvnorm(x = test_set[i,!is_missing, drop = FALSE],
                                         mean = implied_means_no_na,
                                         sigma = implied_cov_no_na,
                                         log = TRUE)

  }

  class(m2ll) <- "-2-log-Likelihood"

  return(m2ll)

}
