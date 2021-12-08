formula = formula(paste0("~", paste0(names(test), collapse="+")))
data = test
mtry = NULL
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
flexforest = function(data, formula, fit_function, iterations,
                      variable_sampler = formula,
                      mtry = NULL) {

  # do necessary checks

  # extract variable elements
  variables = all.vars(formula)

  # This can also be used for mixed effect models! But I need to have users be able
  # to define what is versus is not randomly selected
  # That may mean I'll have to have a custom bootstrapper for stratified sampling

  # I need to parse out the fit function first, I think because that will dictate
  # how variables are sampled

  # define a loop

  # partition the data (into training and validation)
  data_sample = bootstrap_sample(data)
  training_i   = data_sample$training
  validation_i = data_sample$validation

  # randomly sample the variables
  if (is.null(mtry)) mtry = sqrt(length(variables))
  variables_i = variable_sampler(variables, mtry)

  # fit the function and store results

  # compute the loss function for the results

}
