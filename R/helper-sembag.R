bootstrap_sample = function(data, prop=.67) {
  # bootstrap the same size as the data
  bootstrapped_data = data[sample(1:nrow(data), replace=T),]
  training   = bootstrapped_data[1:(nrow(data)*prop),]
  validation = bootstrapped_data[((nrow(data)*prop)+1):nrow(data),]
  list(training=training, validation=validation)
}


# this will do stratified sampling
# it expects either a list (for multiple groups)
# or a vector

get_mtry = function(variables, mtry=NULL) {
  if (!is.null(mtry)) return(mtry)
  if (is.list(variables)) return(lapply(variables, function(x) sqrt(length(x))))
  sqrt(length(variables))
}

variable_sampler = function(variables, mtry=NULL) {

  if (is.null(mtry)) mtry = get_mtry(variables)
  if (!is.list(variables)) return(sample(variables, size=mtry))

  # sample latent variables
  latent_numbers = sample(1:length(variables), size=sqrt(length(variables)))
  variables_sampled = lapply(latent_numbers, function(x) variables[[x]])
  mtry_sampled      = lapply(latent_numbers, function(x) mtry[[x]])
  variables = 1:length(variables_sampled) %>%
              purrr::map(function(x) sample(variables_sampled[[x]], size=mtry_sampled[[x]], replace=T))
  return(list(variables=variables, numbers=latent_numbers))
}

return_formula_i = function(formula, variable_sampler=NULL) {
  if (is.null(variable_sampler)) {
    variables = all.vars(formula)
    predictors = variables[-1]
    response   = variables[1]
    predictors_i = variable_sampler(predictors)
    return(formula(paste0(response, " ~ ", paste0(predictors_i, collapse=" + "))))
  } else {
    return(variable_sampler_sem(formula))
  }
}

return_data_i = function(data, data_sampler = NULL) {
  if (is.null(data_sampler)) {
    return(bootstrap_sample(data))
  } else {
    return(data_sampler(data))
  }
}

fit_data_i = function(formula_i, data_i, fit_function=NULL, ...) {

  if (!is.null(fit_function)) {
    return(fit_function(formula_i, data_i, ...))
  } else {
    return(lm(formula_i, data_i, ...))
  }
}

validation_fit_i = function(fit_i, data_i, validation_function, ...) {

  if (!is.null(validation_function)) {
    results = tryCatch(validation_function(fit_i, data_i, ...),
                       error = function(e) e)
    if ("error" %in% class(results)) return(NULL) else return(results)
  } else {
    outcome = all.vars(formula(fit_i))
    observed = fit_i$model[,outcome]
    predicted = predict(fit_i, newdata=data_i, ...)
    sse = sum((observed-predicted)^2)
    sse
  }
}

loss_function_i = function(model_i, loss_function=NULL) {
  if (is.null(loss_function)) {
    observed_y = all.vars(formula(model_i))[1]
    return(sum((fitted(model_i) - observed_y)^2))
  } else {
    return(loss_function(model_i))
  }
}

loss_two_step = function(model_i) {
  # find the fit of the original model

  # find the fit of a saturated model
}

fit_function_check = function(data, model, formula) {
  # make sure formula follows the form of [y]~x+b+c+[d], where anything in brackets
  # should not be randomized
}
