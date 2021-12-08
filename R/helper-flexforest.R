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
  sqrt(length(x))
}

variable_sampler = function(variables, mtry=NULL) {
  if (is.null(mtry)) mtry = get_mtry(variables)
  if (!is.list(variables)) return(sample(variables, size=mtry))
  variables = 1:length(variables) %>%
              map(function(x) sample(variables[[x]], size=mtry[[x]])) %>%
              unlist
  return(variables)
}

fit_function_check = function(data, model, formula) {
  # make sure formula follows the form of [y]~x+b+c+[d], where anything in brackets
  # should not be randomized
}

`%.%` = function(a, b) {
  paste0(a, b)
}
