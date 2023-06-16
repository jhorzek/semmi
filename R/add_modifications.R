#' add_modifications
#'
#' Adds modifications identified by bootstrap_specification_search or
#' resample_specification_search to the lavaan model. Both
#' bootstrap_specification_search and resample_specification_search provide
#' modifications and the number of times these modifications have been added
#' to models in bootstrap samples or resamples of the original data.
#' add_modifications adds some or all of these modificactions to the final model.
#' To this end, specify threshold_percent_added - modifications that have been added
#' in >= threshold_percent_added % of the models will be present in the final model.
#' @param  spec_search_result result from bootstrap_specification_search or
#' resample_specification_search
#' @param threshold_percent_added modifications must have been added to
#' threshold_percent_added % of the models to be added to the final model
#' @return the lavaan syntax and fitted final model
#' @examples
#' library(semmi)
#' set.seed(12345)
#'
#' data <- simulate_PolDem(sample_size = 10000)$data
#'
#' base_model <- '
#'   # latent variable definitions
#'      eta1 =~ x1 + x2 + x3
#'      eta2 =~ y1 + a*y2 + b*y3 + c*y4
#'      eta3 =~ y5 + a*y6 + b*y7 + c*y8
#'
#'   # regressions
#'     eta2 ~ eta1
#'     eta3 ~ eta1 + eta2
#' '
#'
#' train_set <- sample(1:nrow(data),
#'                     size = 400)
#'
#' fit_base <- sem(model = base_model,
#'                 data = data[train_set,])
#'
#' specification_searched <- specification_search(model = base_model,
#'                                                data = data[train_set,],
#'                                                operators = "~~")
#'
#' resample_spec_search <- resample_specification_search(model = base_model,
#'                                                         data = data[train_set,],
#'                                                         alpha = .05,
#'                                                         operators = "~~",
#'                                                         number_of_resamples = 10,
#'                                                         N_subsets = 80,
#'                                                         missing = "ml",
#'                                                         std.lv = FALSE)
#'
#' final_model <- add_modifications(resample_spec_search,
#'                                  threshold_percent_added = 25)
#'
#' # cross-validate
#' ## without resampling:
#' cross_validate_lavaan(lavaan_model = specification_searched$lavaan_model,
#'                       test_set = data[-train_set,])
#'
#' ## with resampling:
#' cross_validate_lavaan(lavaan_model = final_model$lavaan_model,
#'                       test_set = data[-train_set,])
#' @export
add_modifications <- function(spec_search_result,
                              threshold_percent_added){

  if(!(is(spec_search_result, "Boot_Spec_Search") |
       is(spec_search_result, "resample_Spec_Search"))){
    stop("spec_search_result must be an object returned by bootstrap_specification_search or resample_specification_search")
  }

  if(threshold_percent_added > 100 | threshold_percent_added < 0){
    stop("threshold_percent_added should be between 0 and 100, expressing the percentage of models where the modification was present.")
  }

  if(is(spec_search_result, "Boot_Spec_Search")){
    number_of_samples <- spec_search_result$n_bootstrap_samples
  }
  if(is(spec_search_result, "resample_Spec_Search")){
    number_of_samples <- spec_search_result$number_of_resamples
  }

  add <- spec_search_result$result |>
    dplyr::group_by(.data[["modification"]]) |>
    dplyr::summarise("added" = (dplyr::n()/number_of_samples)*100 >= threshold_percent_added)

  add <- add[add$added,]

  # add kept modifications

  model <- paste0(spec_search_result$internal$model, "\n",
                  paste0(add$modification, collapse = "\n")
  )

  call_args <- c(list(
    model = model,
    data = spec_search_result$internal$data),
    spec_search_result$internal[names(spec_search_result$internal) %in%
                                  names(lavaan::lavOptions())]
  )

  lavaan_model <- tryCatch(expr = do.call("sem", args = call_args),
                           error = function(e){
                             return("Lavaan model resulted in errors")
                             warning("Lavaan model resulted in errors")
                           })
  return(list(
    lavaan_model = lavaan_model,
    added_modifications = add
  ))

}


