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

loss_sem = function(fit, data) {
  observed_cov = cov(data[,lavaan::lavNames(fit)])
  implied_cov  = lavaan::fitted(fit)$cov
  f = log(det(implied_cov)) +
    matrix_trace(implied_cov %*% solve(implied_cov)) -
    log(det(observed_cov)) - ncol(observed_cov)
  chi = (nrow(data)-1)*f
  return(chi)
}

matrix_trace = function(matrix) {
  sum(diag(matrix))
}

# loss function needs to be computed from the imputed data
loss_sem_coef = function(fit) {
  # store the coefficients for each variable
  coefficients = standardizedSolution(fit)
  factor_loadings = coefficients$op == "=~"
  coef_names   = coefficients$rhs[factor_loadings]
  latent_names = coefficients$lhs[factor_loadings]
  coef_values  = coefficients$est.std[factor_loadings]
  return(data.frame(observed = coef_names, latent = latent_names, coef = coef_values))
}

loss_sem_chisq = function(fit) {
  lavaan::fitMeasures(fit, "chisq")
}
