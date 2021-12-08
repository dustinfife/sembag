#' Title
#'
#' @param data
#' @param formula A formula of the form ~ a + b + c. Note, there is no need to
#' put an outcome variable in the formula
#' @param fit_function
#' @param iterations
#'
#' @return
#' @export
#'
#' @examples
#'
flexforest = function(data, formula, iterations,
                      fit_function = NULL,
                      variable_sampler = NULL,
                      data_sampler = NULL,
                      loss_function = NULL,
                      validation_function = NULL,
                      mtry = NULL) {

  # do necessary checks

  # define a loop

  formula_i = return_formula_i(formula, variable_sampler)

  # partition the data (into training and validation)
  data_sample = return_data_i(data, data_sampler)
  training_i   = data_sample$training
  validation_i = data_sample$validation

  # fit the function and store results
  fit_i = fit_data_i(formula_i, data_i, fit_function)

  # fit to the validation set
  validation_i = validation_fit_i(fit_i, data=validation_i, validation_function)

  # compute the loss function for the results
  loss_i = loss_function_i(fit_i, loss_function)

}


# This can also be used for mixed effect models! But I need to have users be able
# to define what is versus is not randomly selected
# That may mean I'll have to have a custom bootstrapper for stratified sampling

# I need to parse out the fit function first, I think because that will dictate
# how variables are sampled
