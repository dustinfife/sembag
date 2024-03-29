fit_rf_sem = function(formula, data, ...) {

  results = tryCatch(lavaan::sem(formula, data), error = function(e) e)
  if ("error" %in% class(results)) return(NULL) else return(results)
}

# for sems, the formula is actually the model string
#' Title
#'
#' @param formula
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
variable_sampler_sem = function(formula, ...) {

  # parse out the variables
  variable_names = parse_model_code(formula, return_observed_as_vector=FALSE)
  observed = variable_names$observed
  latents  = variable_names$latents

  # randomly sample the variables
  sampled_variables = variable_sampler(observed, mtry = get_mtry_sem(observed))

  # rewrite the model
  sampled_sem_model = mapply(sem_write_one_line, latents[sampled_variables$numbers], sampled_variables$variables) %>%
    paste0(collapse="\n")

  return(sampled_sem_model)

}

get_variables = function(model, ...) {

}

sem_write_one_line = function(latent, observed) {
  paste0(latent, " =~ ", paste0(observed, collapse = " + "))
}


get_mtry_sem = function(observed) {
 1:length(observed) %>% purrr::map(function(x) return(max(3,
                                                          sqrt(length(observed[[x]])))))
}
