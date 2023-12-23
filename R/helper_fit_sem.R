parse_model_code = function(model, return_observed_as_vector = TRUE) {

  # where user puts + then more observed variables, put on one line
  all_on_one_line = gsub("[ ]+?[+]\n[ ]+?", "+", model) %>%
    {gsub(pattern = "\n[ ]+?[+]", replacement = "+", x = .)}
  remove_line_breaks = gsub("\n[ ]*\n", "\n", all_on_one_line)
  latents_on_one_line = strsplit(remove_line_breaks, "\n") %>%
    unlist() %>%
    trimws() %>%
    .[.!=""]
  models = sapply(latents_on_one_line, strsplit, "=~") %>%
    unlist()
  latents  = models[seq(1, length(models), by=2)] %>%
    trimws() %>%
    sembag:::remove_names()
  missing_latent = which(latents=="")
  if (length(missing_latent)>0) latents = latents[-missing_latent]
  obs = models[seq(2, length(models), by=2)] %>% trimws() %>% remove_names()

  if (return_observed_as_vector) {
    observed = unlist(lapply(obs, strsplit, "+", fixed=TRUE))
  } else {
    observed = sapply(obs, strsplit, "+", fixed=TRUE) %>% remove_names()
  }

  return(list(observed=observed, latents=latents))

  # add later: extracting covariances and regressions
}

loss_sem = function(fit, data=NULL, spearman_brown=TRUE, parcel_sizes=NULL) {

  # check to see if model actually fit
  if (class(fit)[1] != "lavaan") return(NA)

  observed_names = lavaan::lavNames(fit)
  if (is.null(data)) data = lavInspect(test_fit, "data")

  # I should probably have a check to make sure all variables are numeric
  if (!all(lapply(data[,observed_names], is.numeric)%>%unlist)) {
    stop("You have one or more variables that are not numeric. Please remove those and run again.")
  }

  implied_cov = ram_matrix_adjustment_sb(fit, parcel_sizes=parcel_sizes, spearman_brown=spearman_brown)
  observed_cov = lavaan::inspect(fit, "sampstat")$cov


  f = log(det(implied_cov)) +
      matrix_trace(observed_cov %*% solve(implied_cov)) -
      log(det(observed_cov)) - ncol(observed_cov)
  chi = (nrow(data))*f  # apparently lavaan multiplies by n, not n-1.
                        # see https://groups.google.com/g/lavaan/c/aiODQLfzzrc
  loss_sem_chisq(fit)
  return(chi)
}

permute_variables = function(fit, data, formula, ...) {

  # check to see if model actually fit
  if (class(fit)[1] != "lavaan") return(NA)

  # get the variable names
  names_i = lavaan::lavNames(fit)
  chi_shuffled = as.list(names_i) %>% setNames(names_i)

  # loop through variables and iterate
  for (i in 1:length(names_i)) {
    nm = names_i[i]
    data_shuffled = data
    data_shuffled[,nm] = sample(data[,nm])

    # refit the data
    fit_shuffled = fit_rf_sem(formula, data_shuffled)

    chi_shuffled_i = tryCatch(loss_sem(fit_shuffled, data_shuffled, ...),
                            error = function(e) e)

    if ("error" %in% class(chi_shuffled)) chi_shuffled[[i]] = NULL else chi_shuffled[[i]] = chi_shuffled_i

  }
  return(chi_shuffled)
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

aggregate_vi = function(i,list) {
  whole_matrix = lavNames()
  df = list$vi %>% as.data.frame()
  names(uni)
}
