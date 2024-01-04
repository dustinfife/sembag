#' Title
#'
#' @param data the dataset
#' @param formula A formula of the form ~ a + b + c. Note, there is no need to
#' put an outcome variable in the formula
#' @param fit_function a custom function to use for estimating parameters
#' @param iterations number of iterations
#' @importFrom magrittr `%>%`
#'
#' @return a list
#' @export
#'
#'
sembag_inloop = function(iteration = 1, data, formula, iterations,
                      fit_function = NULL,
                      variable_sampler = NULL,
                      data_sampler = NULL,
                      validation_function = NULL,
                      mtry = NULL, ...) {

  # make sure all variables actually exist
  variable_names = parse_model_code(formula, return_observed_as_vector=FALSE)
  observed = variable_names$observed %>% unlist %>%trimws
  variables_not_in_dataset = !(observed %in% names(data))
  if (sum(variables_not_in_dataset)>0) {
    message = paste0("The following variables you're trying to model are not in the dataset: \n\n",
                     paste0(observed[variables_not_in_dataset], collapse=" "))
    stop(message)
  }

  cat(paste0("Iteration ", iteration, "\n"))

  # define a loop
  formula_i = return_formula_i(formula, variable_sampler)

  # partition the data (into training and validation)
  data_sample = return_data_i(data, data_sampler)
  training_i   = data_sample$training
  validation_i = data_sample$validation


  # fit the test set
  fit_i = fit_data_i(formula_i, training_i, fit_function, ...)
  cov_i = lavaan::inspect(fit_i, "sampstat")$cov

  # fit to the validation set by subtracting the observed variance/covariance matrix of the validation dataset
  # from the model-implied variance/covariance matrix

  variables_i = lavaan::inspect(fit_i, "sampstat")$cov %>% dimnames() %>% pluck(1)
  training_varcov = cov(validation_i[,variables_i], use="pairwise.complete.obs")
  chi_training = cov_to_chi(cov_i, training_varcov, n=nrow(validation_i))

  # loop through all variables
  vi_i = 1:length(variables_i)
  for (i in 1:length(variables_i)) {
    # shuffle data
    data_shuffled = shuffle_column_i(validation_i, variables_i[i])[,variables_i]

    # compute varcov
    shuffled_i_varcov = cov(data_shuffled, use="pairwise.complete.obs")

    if (det(shuffled_i_varcov)<=0 | det(training_varcov)<=0) browser()

    # recompute chi from new varcov
    chi_shuffled_i = cov_to_chi(shuffled_i_varcov, training_varcov, nrow(validation_i)) %>%
      tryCatch()
    if (!("error" %in% class(chi_shuffled_i))) vi_i[i] = chi_shuffled_i - chi_training
  }


  # compute the loss function for the results
  return(list(variables = variables_i, vi = vi_i))

}


#' Use sembag
#'
#' @param data
#' @param formula A formula of the form ~ a + b + c. Note, there is no need to
#' put an outcome variable in the formula
#' @param fit_function
#' @param variable_sampler
#' @param data_sampler
#' @param validation_function
#' @param mtry
#' @param iterations
#' @param ... other arguments passed to other functions inside sembag
#'
#' @importFrom magrittr `%>%`
#'
#' @return
#' @export
#'
#' @examples
#'
sembag = function(data, formula, iterations=500,
                      fit_function = NULL,
                      variable_sampler = NULL,
                      data_sampler = NULL,
                      validation_function = NULL,
                      mtry = NULL, ...) {
  # require(parallel)
  #
  # cores    = parallel::detectCores()*.8
  # clusters = parallel::makeCluster(cores)
  # clusterEvalQ(clusters, library("sembag"))
  # clusterEvalQ(clusters, library("magrittr"))

  results = 1:iterations %>% purrr::map(sembag_inloop,
            data=data, formula=formula, iterations = iterations,
            fit_function = fit_function, variable_sampler = variable_sampler,
            validation_function = validation_function,
            mtry = mtry, ...)

  var_names = parse_model_code(formula)$observed %>% trimws
  d = data.frame(matrix(nrow=iterations, ncol=length(var_names))) %>%
    setNames(var_names)

  for (i in 1:nrow(d)) {
    vi_results = results[[i]]$vi
    vars_selected = results[[i]]$variables
    d[i,vars_selected] = vi_results
  }

  # very oddly, this doesn't work....I keep getting NaN for colmeans
  #varimp = colMeans(d, na.rm=T)
  varimp = var_names %>% map(function(x) { mean(d[,x], na.rm=T)}) %>% set_names(var_names) %>% unlist

  # parallel::stopCluster(clusters)
  #return(list(results=results, varimp=varimp))
  return(varimp)
}



# This can also be used for mixed effect models! But I need to have users be able
# to define what is versus is not randomly selected
# That may mean I'll have to have a custom bootstrapper for stratified sampling

# I need to parse out the fit function first, I think because that will dictate
# how variables are sampled
