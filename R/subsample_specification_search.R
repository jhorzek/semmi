#' resample_specification_search
#'
#' Provides resample specification search for lavaan models. Specification search is an exploratory
#' strategy, where parameters are added to the model if the modification indices
#' are significant. This practice has been criticized for capitalization on chance
#' (see [MacCallum et al., 1992](https://psycnet.apa.org/doi/10.1037/0033-2909.111.3.490)).
#' That is, the model may adapt too closely to the data and start fitting noise.
#' In resampled specification search, random samples are drawn from the original
#' data set. These samples have a smaller size than the original data and each subject
#' may be present once or not at all in the subset. resample_specification_search will apply specification
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
#' @param N_subsets sample size in subsets
#' @param number_of_resamples number of resamples to use. The more the merrier!
#' @param max_iter maximal number of tries: Subsmaples samples may fail to fit.
#' Therefore, we set an upper bound on the number of draws.
#' @param ... option to pass arguments to the lavaan sem-function
#' @return object of class resample_Spec_Search with modifications in each
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
#' spec_searched <- resample_specification_search(model = model,
#'                                       data = PoliticalDemocracy,
#'                                       operators = "~~",
#'                                       N_subsets = 70,
#'                                       number_of_resamples = 5) # should be much higher!
#' spec_searched
#' @md
#' @export
resample_specification_search <- function(model, data, alpha = .05,
                                           operators = c("=~", "~", "~~"),
                                           N_subsets,
                                           number_of_resamples,
                                           max_iter = 100*number_of_resamples,
                                           ...){

  # save input
  internal <- c(as.list(environment()), ...)


  N <- nrow(data) # sample size
  it <- 1         # iteration counter
  sucessful <- 0  # number of sucessful bootstrap iterations

  if(N_subsets >= N)
    stop("N_subsets must be smaller than the sample size of the data set.")

  result <- data.frame(sample = integer(),
                       modification = character(),
                       mi = numeric())

  pb <- txtProgressBar(min = 0, max = number_of_resamples, style = 3)

  while (it <= max_iter){
    if(sucessful >= number_of_resamples)
      break

    # sample
    resample <- data[sample(1:N, size = N_subsets, replace = FALSE), , drop = FALSE]

    # check if a model can be fitted in the current sample:
    lavaan_model <- tryCatch(
      expr = {lavaan::sem(model, data = resample, ...)},
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
                                             data = resample,
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
    warning("Reached maximal number of iterations. It seems that many of the resamples resulted in issues with lavaan.")
  }

  returns <- list(result = result,
                  number_of_resamples = number_of_resamples,
                  internal = internal)

  class(returns) <- "resample_Spec_Search"

  return(returns)
}

#' show.resample_Spec_Search
#'
#' show results of specification_search
#' @param object object of class resample_Spec_Search
#' @return tibble with summarized results
#' @method show resample_Spec_Search
#' @export
show.resample_Spec_Search <- function(object){
  cat("Results of bootstrapped specification search:\n")
  cat(paste0(rep("_", nchar("Results of bootstrapped specification search:")), collapse = ""), "\n")

  number_of_resamples <- object$number_of_resamples

  return(
    object$result |>
      dplyr::group_by(.data[["modification"]]) |>
      dplyr::summarise("% added" = (dplyr::n()/number_of_resamples)*100)
  )
}

#' summary.resample_Spec_Search
#'
#' show results of specification_search
#' @param object object of class resample_Spec_Search
#' @param ... not used
#' @return tibble with summarized results
#' @method summary resample_Spec_Search
#' @export
summary.resample_Spec_Search <- function(object, ...){
  cat("Results of bootstrapped specification search:\n")
  cat(paste0(rep("_", nchar("Results of bootstrapped specification search:")), collapse = ""), "\n")

  number_of_resamples <- object$number_of_resamples

  return(
    object$result |>
      dplyr::group_by(.data[["modification"]]) |>
      dplyr::summarise("% added" = (dplyr::n()/number_of_resamples)*100)
  )
}


#' plot.resample_Spec_Search
#'
#' Plot the results of a bootstrap specification search
#' @param x object of class resample_Spec_Search
#' @param y not used
#' @param ... not used
#' @return ggplot2 object
#' @method plot resample_Spec_Search
#' @export
plot.resample_Spec_Search <- function(x, y = NULL, ...){

  number_of_resamples <- x$number_of_resamples

  summarized <- x$result |>
    dplyr::group_by(.data[["modification"]]) |>
    dplyr::summarise("% added" = (dplyr::n()/number_of_resamples)*100)

  return(
    ggplot2::ggplot(summarized,
                    ggplot2::aes(x = .data[["modification"]],
                                 y = .data[["% added"]])) +
      ggplot2::geom_col() +
      ggplot2::ylab("% of models, where this parameter was added") +
      ggplot2::xlab("Parameter label") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  )
}
