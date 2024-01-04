spearman_brown_error = function(variable, A, items) {

  if (!all(names(items) %in% c("variable", "items"))) {
    stop("Your parcel file needs to have exactly two columns named 'variable' and 'items'.")
  }
  # if (!all(items$variable %in% unlist(dimnames(A)[2]))) {
  #   which_doesnt_belong = items$variable[!(items$variable %in% unlist(dimnames(A)[2]))]
  #   msg = paste0("One or more of the variables in your parcel file do not match what's in your model. Specifically: ", paste0(items$variable[which_doesnt_belong], collapse=","))
  #   stop(msg)
  # }
}

# taken from the simstandard package
# note that these are STANDARDIZED factor loadings
lav2ram = function(fit) {
  pt <- lavaan::standardizedSolution(fit)
  pt$id <- 1:nrow(pt)
  v_all <- unique(c(pt$lhs, pt$rhs)) %>% stringr::str_subset(pattern="")
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

# extract the RAM matrices so I can adjust for reliability using spearman brown
# extract the RAM matrices so I can adjust for reliability using spearman brown
ram_matrix_adjustment_sb = function(fit, parcel_sizes, spearman_brown=TRUE, prophecy_items = 5) {

  # just return model-implied variance/covariance matrix
  if (!spearman_brown | is.null(parcel_sizes)) {
    return(lavaan::fitted(fit)$cov)
  }

  ram = lav2ram(fit)
  A = ram$A
  A_new = matrix(0,nrow=nrow(A), ncol=ncol(A))

  # identify factor loadings
  variables = row.names(ram$A)
  parcel_sizes_i = parcel_sizes[parcel_sizes$variable %in% variables,]

  # adjust the factor loadings
  for (i in 1:length(variables)) {
    sb_adjustment = spearman_brown_adjustment(variables[i], A, parcel_sizes_i, prophecy_items)
    if (!is.null(sb_adjustment)) A_new[,i] = sb_adjustment
  }

  # now re-estimate the chi square with this new matrix
  Fmat = ram$F
  I = matrix(0, nrow=nrow(A), ncol=ncol(A))
  diag(I) = 1
  S = ram$S
  implied_cor = Fmat %*% solve(I-A_new) %*% S %*% t(solve(I-A_new)) %*% t(Fmat)

    # need to adjust by model-implied standard deviations (not actual standard deviations)
  sds = sqrt(diag(fitted(fit)$cov))
  implied_cov = lavaan::cor2cov(implied_cor, sds)
  # convert correlation matrix to covariance matrix
  return(implied_cov)
}

spearman_brown_adjustment = function(variable, A, items, prophecy_items = 5) {

  # error checking
  spearman_brown_error(variable, A, items)

  # extract column names
  column_names = unlist(dimnames(A)[2])
  r = A[,column_names == variable]
  items$items = prophecy_items/items$items

  # identify items that are the same in both
  v = column_names %in% items$variable
  # adjust factor loadings for those variables user provides
  adjusted_factor_loadings = r
  adjusted_factor_loadings[v] =  sb_calculation(r[v], items$items)

  # make an adjustment for items with loadings > 1
  if (any(is.na(adjusted_factor_loadings))) return (NULL)
  condition = tryCatch(adjusted_factor_loadings>1)
  if ("error" %in% class(condition)) {

  }
  adjusted_factor_loadings[condition] = r[condition]
  return(adjusted_factor_loadings)
}

sb_calculation = function(rho, n_proportion) {
  (n_proportion*rho)/(1 + (n_proportion-1)*rho)
}
