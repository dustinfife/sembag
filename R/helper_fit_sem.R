parse_model_code = function(model, return_observed_as_vector = TRUE) {

  lines  = unlist(strsplit(model, "\n"))
  models = unlist(sapply(lines, strsplit, "=~"))
  latents  = models[seq(1, length(models), by=2)] %>% trimws() %>% remove_names()
  obs = models[seq(2, length(models), by=2)] %>% trimws() %>% remove_names()

  if (return_observed_as_vector) {
    observed = unlist(lapply(obs, strsplit, " + ", fixed=TRUE))
  } else {
    observed = sapply(obs, strsplit, " + ", fixed=TRUE) %>% remove_names()
  }

  return(list(observed=observed, latents=latents))

  # add later: extracting covariances and regressions
}


remove_names = function(object) {
  names(object) = NULL
  return(object)
}
