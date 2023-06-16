#' subsample_specification_search
#'
#' Provides subsample specification search for lavaan models. Specification search is an exploratory
#' strategy, where parameters are added to the model if the modification indices
#' are significant. This practice has been criticized for capitalization on chance
#' (see [MacCallum et al., 1992](https://psycnet.apa.org/doi/10.1037/0033-2909.111.3.490)).
#' That is, the model may adapt too closely to the data and start fitting noise.
#' In subsampled specification search, random samples are drawn from the original
#' data set. These samples have a smaller size than the original data and each subject
#' may be present once or not at all in the subset. subsample_specification_search will apply specification
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
#' @param n_subsamples number of subsmaples to use. The more the merrier!
#' @param max_iter maximal number of tries: Subsmaples samples may fail to fit.
#' Therefore, we set an upper bound on the number of draws.
#' @param ... option to pass arguments to the lavaan sem-function
#' @return object of class Subsample_Spec_Search with modifications in each
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
#' spec_searched <- subsample_specification_search(model = model,
#'                                       data = PoliticalDemocracy,
#'                                       operators = "~~",
#'                                       n_subsamples = 50) # should be much higher!
#' spec_searched
#' @md
#' @export
subsample_specification_search <- function(model, data, alpha = .05,
                                           operators = c("=~", "~", "~~"),
                                           n_subsamples,
                                           max_iter = 100*n_subsamples,
                                           ...){

  N <- nrow(data) # sample size
  it <- 1         # iteration counter
  sucessful <- 0  # number of sucessful bootstrap iterations

  if(n_subsamples >= N)
    stop("n_subsamples must be smaller than the sample size of the data set.")

  result <- data.frame(sample = integer(),
                       modification = character(),
                       mi = numeric())

  pb <- txtProgressBar(min = 0, max = n_subsamples, style = 3)

  while (it <= max_iter){
    if(sucessful >= n_subsamples)
      break

    # sample
    subsample <- data[sample(1:N, size = n_subsamples, replace = FALSE), , drop = FALSE]

    # check if a model can be fitted in the current sample:
    lavaan_model <- tryCatch(
      expr = {sem(model, data = subsample, ...)},
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
                                             data = subsample,
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
    warning("Reached maximal number of iterations. It seems that many of the subsamples resulted in issues with lavaan.")
  }

  returns <- list(result = result,
                  n_subsamples = n_subsamples)

  class(returns) <- "Subsample_Spec_Search"

  return(returns)
}

#' show.Subsample_Spec_Search
#'
#' show results of specification_search
#' @param object object of class Subsample_Spec_Search
#' @return nothing
#' @export
show.Subsample_Spec_Search <- function(object){
  cat("Results of bootstrapped specification search:\n")
  cat(paste0(rep("_", nchar("Results of bootstrapped specification search:")), collapse = ""), "\n")

  n_subsamples <- object$n_subsamples

  object$result |>
    group_by(modification) |>
    summarise("% added" = (n()/n_subsamples)*100) |>
    print()
}

#' summary.Subsample_Spec_Search
#'
#' show results of specification_search
#' @param object object of class Subsample_Spec_Search
#' @param ... not used
#' @return nothing
#' @export
summary.Subsample_Spec_Search <- function(object, ...){
  cat("Results of bootstrapped specification search:\n")
  cat(paste0(rep("_", nchar("Results of bootstrapped specification search:")), collapse = ""), "\n")

  n_subsamples <- object$n_subsamples

  object$result |>
    group_by(modification) |>
    summarise("% added" = (n()/n_subsamples)*100) |>
    print()
}
