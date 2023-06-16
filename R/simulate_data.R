#' simulate_PolDem
#'
#' Simulate example data based on the PoliticalDemocracy data set.
#' @param sample_size sample size
#' @return data and model used to simulate the data
#' @examples
#' set.seed(2343)
#' data <- simulate_PolDem()
#' @export
simulate_PolDem <- function(sample_size = 100){

  model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'

  fit <- lavaan::sem(model,
                     data = lavaan::PoliticalDemocracy,
                     meanstructure = TRUE)

  implied <- lavaan::lavInspect(object = fit,
                                what = "implied")

  data <- mvtnorm::rmvnorm(n = sample_size,
                           mean = implied$mean,
                           sigma = implied$cov)
  colnames(data) <- colnames(implied$cov)
  return(
    list(
      data = data,
      true_moments = implied)
  )
}
