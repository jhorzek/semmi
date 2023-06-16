#' specification_search
#'
#' Provides specification search for lavaan models. Specification search is an exploratory
#' strategy, where parameters are added to the model if the modification indices
#' are significant. This practice has been criticized for capitalization on chance
#' (see [MacCallum et al., 1992](https://psycnet.apa.org/doi/10.1037/0033-2909.111.3.490)).
#' That is, the model may adapt too closely to the data and start fitting noise.
#' Thus, specification search should be used very carefully; e.g., by combining it
#' with cross-validation.
#'
#' @param model syntax for a lavaan model
#' @param data data used by the lavaan model
#' @param alpha significance level for the modification indices
#' @param operators which parameters should be considered to be added to the model?
#' Default: all types of parameters. If set to "=~", only loadings are added. If
#' set to "~" only regressions are added. If set to "~~" only covariances are added.
#' @param previous_model INTERNAL: Do not use this manually! lavaan syntax for
#' a previously estimated model
#' @param added INTERNAL: Do not use this manually. Vector with modifications already
#' added to the model
#' @param ... option to pass arguments to the lavaan sem-function
#' @return object of class Spec_Search with (1) a lavaan model including all
#' modifications and (2) a data.frame with labels of added modifications and the
#' modification index value
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
#' spec_searched <- specification_search(model = model,
#'                                       data = PoliticalDemocracy,
#'                                       operators = "~~")
#' spec_searched
#' @md
#' @export
specification_search <- function(model, data, alpha = .05,
                                 operators = c("=~", "~", "~~"),
                                 previous_model = NULL,
                                 added = data.frame("label" = character(),
                                                    "mi"    = numeric()),
                                 ...){

  if(!is(model, "character"))
    stop("model must be a lavaan syntax, not a fitted model")

  lavaan_model <- tryCatch(
    expr = {lavaan::sem(model, data = data, ...)},
    warning = function(w){
      message("Stopped specification search: Adding further parameters resulted in warning in lavaan.")
      return("stop")
    },
    error = function(e){
      message("Stopped specification search: Adding further parameters resulted in error in lavaan.")
      return("stop")
    })

  # check if there was a warning or an error:
  if(is.character(lavaan_model)){
    spec_search_result <- list(lavaan_model = previous_model,
                               added = added)
    class(spec_search_result) <- "Spec_Search"
    return(spec_search_result)
  }

  MI <- lavaan::modificationIndices(lavaan_model, alpha = alpha)

  # modify only specified parameter types:
  MI <- MI[MI$op %in% operators,]

  # add modification
  if(max(MI$mi) > qchisq(p=1-alpha, df=1)){
    add_par <- which.max(MI$mi)
    add_par <- paste0(MI$lhs[which.max(MI$mi)],
                      MI$op[which.max(MI$mi)],
                      MI$rhs[which.max(MI$mi)])

    added <- rbind(added,
                   data.frame("label" = add_par,
                              "mi" = max(MI$mi))
                   )
    model <- paste0(model, "\n",
                    add_par
    )
    return(specification_search(
      model = model,
      data = data,
      alpha = alpha,
      previous_model = lavaan_model,
      added = added,
      operators = operators,
      ... = ...
    )
    )
  }

  message("Stopped specification search: No more significant modifications found.")

  spec_search_result <- list(lavaan_model = lavaan_model,
                                 added = added)
  class(spec_search_result) <- "Spec_Search"

  return(
    spec_search_result
  )
}

#' show.Spec_Search
#'
#' show results of specification_search
#' @param object object of class Spec_Search
#' @return nothing
#' @method show Spec_Search
#' @export
show.Spec_Search <- function(object){
  cat("Results of specification search:\n")
  cat(paste0(rep("_", nchar("Results of specification search:")), collapse = ""), "\n")
  cat("Added parameters:\n")
  print(object$added)
  cat("Final lavaan model:\n")
  cat(paste0(rep("_", nchar("Final lavaan model:")), collapse = ""), "\n")
  show(object$lavaan_model)
}

#' summary.Spec_Search
#'
#' show results of specification_search
#' @param object object of class Spec_Search
#' @param ... not used
#' @return nothing
#' @method summary Spec_Search
#' @export
summary.Spec_Search <- function(object, ...){
  cat("Results of specification search:\n")
  cat(paste0(rep("_", nchar("Results of specification search:")), collapse = ""), "\n")
  cat("Added parameters:\n")
  print(object$added)
  cat("Final lavaan model:\n")
  cat(paste0(rep("_", nchar("Final lavaan model:")), collapse = ""), "\n")
  summary(object$lavaan_model)
}
