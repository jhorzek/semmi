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

  cv_set <- lavaan_model@Options
  cv_set <- cv_set[cv_set %in% names(lavaan::lavOptions())]
  cv_set$do.fit <- FALSE
  cv_set$data <- test_set
  cv_set$model <- lavaan::parTable(lavaan_model)

  cv_fit <- do.call("lavaan",
                    args = cv_set)

  return(-2*logLik(cv_fit))

}
