#' Title
#'
#' @param data
#' @param formula A formula of the form ~ a + b + c. Note, there is no need to
#' put an outcome variable in the formula
#' @param fit_function
#' @param iterations
#' @importFrom magrittr `%>%`
#'
#' @return
#' @export
#'
#' @examples
#'
flexforest_inloop = function(iteration = 1, data, formula, iterations,
                      fit_function = NULL,
                      variable_sampler = NULL,
                      data_sampler = NULL,
                      validation_function = NULL,
                      mtry = NULL) {

  # define a loop
  formula_i = return_formula_i(formula, variable_sampler)

  # partition the data (into training and validation)
  data_sample = return_data_i(data, data_sampler)
  training_i   = data_sample$training
  validation_i = data_sample$validation

  # fit the test set
  fit_i = fit_data_i(formula_i, training_i, fit_function)

  # fit to the validation set
  validation_i = validation_fit_i(fit_i, data=validation_i, validation_function)

  # compute the loss function for the results
  return(list(oob = validation_i, variables = formula_i))

}

flexforest = function(data, formula, iterations=500,
                      fit_function = NULL,
                      variable_sampler = NULL,
                      data_sampler = NULL,
                      validation_function = NULL,
                      mtry = NULL) {
  require(parallel)

  cores    = parallel::detectCores()*.8
  clusters = parallel::makeCluster(cores)
  clusterEvalQ(clusters, library("flexforest"))
  clusterEvalQ(clusters, library("magrittr"))
  results = clusterApply(clusters, 1:iterations, fun = flexforest_inloop,
            data=data, formula=formula, iterations = iterations,
            fit_function = fit_function, variable_sampler = variable_sampler,
            validation_function = validation_function,
            mtry = mtry)
  parallel::stopCluster(clusters)
  return(results)
}



# This can also be used for mixed effect models! But I need to have users be able
# to define what is versus is not randomly selected
# That may mean I'll have to have a custom bootstrapper for stratified sampling

# I need to parse out the fit function first, I think because that will dictate
# how variables are sampled
