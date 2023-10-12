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


  #obs[obs==""] = NULL

  if (return_observed_as_vector) {
    observed = unlist(lapply(obs, strsplit, "+", fixed=TRUE))
  } else {
    observed = sapply(obs, strsplit, "+", fixed=TRUE) %>% remove_names()
  }

  return(list(observed=observed, latents=latents))

  # add later: extracting covariances and regressions
}



spearman_brown_adjustment = function(variable, A, items, prophecy_items = 10) {
  r = A[variable,]
  n = prophecy_items/items
  adjusted_factor_loadings = (n*r)/(1 + (n-1)*r)
  return(adjusted_factor_loadings)
}


#fit = test_fit
#parcel_sizes = read.csv("~/Downloads/parcels.csv")
# next: find out where I call loss_sem and figure out how to supply "parcel_sizes" argument
loss_sem = function(fit, data, spearman_brown=TRUE, parcel_sizes=NULL) {

  # check to see if model actually fit
  if (class(fit)[1] != "lavaan") return(NA)

  observed_names = lavaan::lavNames(fit)
  # I should probably have a check to make sure all variables are numeric
  if (!all(lapply(data[,observed_names], is.numeric)%>%unlist)) {
    stop("You have one or more variables that are not numeric. Please remove those and run again.")
  }

  # extract the RAM matrices so I can adjust for reliability using spearman brown
  if (spearman_brown | is.null(parcel_sizes)) {
    ram = lav2ram(fit)
    A = ram$A
    A_new = matrix(0,nrow=nrow(A), ncol=ncol(A))

    # identify factor loadings
    variables = row.names(ram$A)
    #parcel_sizes = data.frame(variables = row.names(A), parcel_size = round(runif(length(variables), 1, 20)))
    parcel_sizes_i = parcel_sizes[parcel_sizes$variable %in% variables,]
    for (i in 1:length(variables)) {
        A_new[i,] = spearman_brown_adjustment(variables[i], A, parcel_sizes_i$items[i])
    }

    # now re-estimate the chi square with this new matrix
    Fmat = ram$F
    I = matrix(0, nrow=nrow(A), ncol=ncol(A))
      diag(I) = 1
    S = ram$S
    implied_cov = Fmat %*% solve(I-A) %*% S %*% t(solve(I-A)) %*% t(Fmat)
    observed_cov = cov2cor(lavaan::inspect(fit, "sampstat")$cov)
  } else {
    observed_cov = cov(data[,observed_names], use="pairwise.complete.obs")
    implied_cov  = lavaan::fitted(fit)$cov
  }


  f = log(det(implied_cov)) +
    matrix_trace(implied_cov %*% solve(implied_cov)) -
    log(det(observed_cov)) - ncol(observed_cov)
  chi = (nrow(data)-1)*f
  return(chi)
}

# taken from the simstandard package
lav2ram = function(fit) {
  {
    pt <- lavaan::standardizedSolution(fit)
    pt$id <- 1:nrow(pt)
    v_all <- unique(c(pt$lhs, pt$rhs)) %>% str_subset(pattern="")
    v_latent <- unique(pt$lhs[pt$op == "=~"])
    v_observed <- v_all[!(v_all %in% v_latent)]
    k <- length(v_all)
    A <- matrix(0, nrow = k, ncol = k, dimnames = list(v_all,
                                                       v_all))
    S <- matrix(0, nrow = k, ncol = k, dimnames = list(v_all,
                                                       v_all))
    F <- A
    diag(F) <- 1
    F <- F[v_observed, ]
    for (i in pt[pt[, "op"] == "=~", "id"]) {
      A[pt$rhs[i], pt$lhs[i]] <- pt$est.std[i]
    }
    for (i in pt[pt[, "op"] == "~", "id"]) {
      A[pt$lhs[i], pt$rhs[i]] <- pt$est.std[i]
    }
    for (i in pt[(pt[, "op"] == "~~"), "id"]) {
      S[pt$lhs[i], pt$rhs[i]] <- pt$est.std[i]
      S[pt$rhs[i], pt$lhs[i]] <- pt$est.std[i]
    }
    list(A = A, S = S, F = F)
  }
}

permute_variables = function(fit, data, formula, ...) {

  # check to see if model actually fit
  if (class(fit)[1] != "lavaan") return(NA)

  # get the variable names
  names_i = lavaan::lavNames(fit)
  chi_shuffled = as.list(names_i) %>% setNames(names_i)
  # loop through variables and iterate


  for (i in names_i) {
    data_shuffled = data
    data_shuffled[,i] = sample(data[,i])

    # refit the data
    fit_shuffled = fit_rf_sem(formula, data_shuffled)
    chi_shuffled = tryCatch(loss_sem(fit, data_shuffled, ...),
                            error = function(e) e)
    if ("error" %in% class(chi_shuffled)) chi_shuffled[i] = NULL else chi_shuffled[i] = chi_shuffled

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
