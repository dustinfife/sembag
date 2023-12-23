

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
ram_matrix_adjustment_sb = function(fit, spearman_brown, parcel_sizes) {

  # just return model-implied variance/covariance matrix
  if (!spearman_brown | is.null(parcel_sizes)) {
    return(lavaan::fitted(fit)$cov)
  }

  ram = lav2ram(fit)
  A = ram$A
  A_new = matrix(0,nrow=nrow(A), ncol=ncol(A))

  # extract observed variable names
  observed_variables = lavNames(fit)

  # subset parcel size matrix to only those we observe in this model
  parcel_sizes_i = parcel_sizes[parcel_sizes$variable %in% observed_variables,]

  A_columns_to_modify = which(apply(A, 2, function(x) !all(x==0)))
  browser()
  # loop through A matrix, one column at a time, and adjust them
  for (i in 1:length(A_columns_to_modify)) {
    items = 5/parcel_sizes_i$items
    rows_I_need = row.names(A) %in% observed_variables
    columns_I_need = A_columns_to_modify[i]
    A_values_I_need = A[rows_I_need, columns_I_need]
    sb_adjustment = (items*A_values_I_need)/(1 + (items-1)*A_values_I_need)
    A_new[rows_I_need, columns_I_need] = sb_adjustment
  }

  # now re-estimate the chi square with this new matrix
  Fmat = ram$F
  I = matrix(0, nrow=nrow(A), ncol=ncol(A))
  diag(I) = 1
  S = ram$S
  implied_coZ = Fmat %*% solve(I-A_new) %*% S %*% t(solve(I-A_new)) %*% t(Fmat)
  return(implied_cov)
}

spearman_brown_adjustment = function(variable, A, items, prophecy_items = 5) {
  browser()
  # error checking
  spearman_brown_error(variable, A, items)

  # extract column names
  column_names = unlist(dimnames(A)[2])
  r = A[,column_names == variable]
  items = prophecy_items/items

  # identify items that are the same in both
  v = column_names %in% variable
  # adjust factor loadings for those variables user provides
  adjusted_factor_loadings = r
  adjusted_factor_loadings[v] =  (items*r[v])/(1 + (items-1)*r[v])

  # make an adjustment for items with loadings > 1
  adjusted_factor_loadings[adjusted_factor_loadings>1] = r[adjusted_factor_loadings>1]
  return(adjusted_factor_loadings)
}

