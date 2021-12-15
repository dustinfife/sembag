
flexforest_inloop(data=uni, formula=uni_mod, iterations = 10,
  fit_function = fit_rf_sem, variable_sampler = variable_sampler_sem,
  validation_function = loss_sem,
  mtry = mtry)


start = Sys.time()
results = flexforest(data=uni, iterations = 100,
             formula = uni_mod, fit_function = fit_rf_sem, variable_sampler = variable_sampler_sem,
           validation_function = loss_sem)
end = Sys.time()
end - start

results$varimp
require(lavaan)
i = 1



require(purrr)
1:length(results) %>% map(aggregate_vi)
x
