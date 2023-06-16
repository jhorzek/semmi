#' bootstrap_specification_search
#'
#' Provides bootstrapped specification search for lavaan models. Specification search is an exploratory
#' strategy, where parameters are added to the model if the modification indices
#' are significant. This practice has been criticized for capitalization on chance
#' (see [MacCallum et al., 1992](https://psycnet.apa.org/doi/10.1037/0033-2909.111.3.490)).
#' That is, the model may adapt too closely to the data and start fitting noise.
#' In bootstrapped specification search, random samples are drawn from the original
#' data set. These samples have the same size as the original data, but may contain
#' subjects multiple times. bootstrap_specification_search will apply specification
#' search to each of the samples and return the number of times a specific modification
#' has been added to the model. The objective is to make specification search less
#' dependent on the sample, that is, to improve the stability. Importantly, this is
#' still an exploratory method and the SEM is no longer confirmatory.
#'
#' @param model syntax for a lavaan model
#' @param data data used by the lavaan model
#' @param alpha significance level for the modification indices
#' @param operators which parameters should be considered to be added to the model?
#' Default: all types of parameters. If set to "=~", only loadings are added. If
#' set to "~" only regressions are added. If set to "~~" only covariances are added.
#' @param n_bootstrap_samples number of bootstrap samples. The more the merrier!
#' @param max_iter maximal number of tries: Bootstrapped samples may fail to fit.
#' Therefore, we set an upper bound on the number of draws.
#' @param ... option to pass arguments to the lavaan sem-function
#' @return object of class Boot_Spec_Search with modifications in each
#' bootstrap sample.
#' @examples
#' # The following example is adapted from ?lavaan::sem
#' library(semmi)
#' model <- '
#'   # latent variable definitions
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + a*y2 + b*y3 + c*y4
#'      dem65 =~ y5 + a*y6 + b*y7 + c*y8
#'
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'
#'   # residual correlations
#'   # none specified -> we want to use specification
#'   # search to check for residual correlations
#' '
#'
#' fit <- sem(model, data = PoliticalDemocracy)
#' spec_searched <- bootstrap_specification_search(model = model,
#'                                       data = PoliticalDemocracy,
#'                                       operators = "~~",
#'                                       n_bootstrap_samples = 50) # should be much higher!
#' spec_searched
#' @md
#' @export
bootstrap_specification_search <- function(model, data, alpha = .05,
                                           operators = c("=~", "~", "~~"),
                                           n_bootstrap_samples,
                                           max_iter = 100*n_bootstrap_samples,
                                           ...){

  N <- nrow(data) # sample size
  it <- 1         # iteration counter
  sucessful <- 0  # number of sucessful bootstrap iterations

  result <- data.frame(sample = integer(),
                       modification = character(),
                       mi = numeric())

  pb <- txtProgressBar(min = 0, max = n_bootstrap_samples, style = 3)

  while (it <= max_iter){
    if(sucessful >= n_bootstrap_samples)
      break

    # sample
    bootstrap_sample <- data[sample(1:N, size = N, replace = TRUE), , drop = FALSE]

    # check if a model can be fitted in the current sample:
    lavaan_model <- tryCatch(
      expr = {sem(model, data = bootstrap_sample, ...)},
      warning = function(w){
        return("skip")
      },
      error = function(e){
        return("skip")
      })

    # check if there was a warning or an error:
    if(is.character(lavaan_model)){
      # skip to next bootstrap iteration
      it <- it + 1
      next
    }

    sucessful <- sucessful + 1
    setTxtProgressBar(pb = pb, value = sucessful)

    suppressMessages(
      spec_search_it <- specification_search(model = model,
                                             data = bootstrap_sample,
                                             alpha = alpha,
                                             operators = operators,
                                             previous_model = NULL,
                                             ... = ...)
    )

    if(length(spec_search_it$added$label) > 0)
      result <- rbind(
        result,
        data.frame(sample = sucessful,
                   modification = spec_search_it$added$label,
                   mi = spec_search_it$added$mi
        )
      )

    it <- it + 1
  }

  if(it == max_iter){
    warning("Reached maximal number of iterations. It seems that many of the bootstrap samples resulted in issues with lavaan.")
  }

  returns <- list(result = result,
                  n_bootstrap_samples = n_bootstrap_samples)

  class(returns) <- "Boot_Spec_Search"

  return(returns)
}

#' show.Boot_Spec_Search
#'
#' show results of specification_search
#' @param object object of class Boot_Spec_Search
#' @return nothing
#' @export
show.Boot_Spec_Search <- function(object){
  cat("Results of bootstrapped specification search:\n")
  cat(paste0(rep("_", nchar("Results of bootstrapped specification search:")), collapse = ""), "\n")

  n_bootstrap_samples <- object$n_bootstrap_samples

  object$result |>
    group_by(modification) |>
    summarise("% added" = (n()/n_bootstrap_samples)*100) |>
    print()
}

#' summary.Boot_Spec_Search
#'
#' show results of specification_search
#' @param object object of class Boot_Spec_Search
#' @param ... not used
#' @return nothing
#' @export
summary.Boot_Spec_Search <- function(object, ...){
  cat("Results of bootstrapped specification search:\n")
  cat(paste0(rep("_", nchar("Results of bootstrapped specification search:")), collapse = ""), "\n")

  n_bootstrap_samples <- object$n_bootstrap_samples

  object$result |>
    group_by(modification) |>
    summarise("% added" = (n()/n_bootstrap_samples)*100) |>
    print()
}
