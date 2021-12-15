flexforest_inloop(data=test, formula=test_model, iterations = 10,
  fit_function = fit_rf_sem, variable_sampler = variable_sampler_sem,
  validation_function = loss_sem,
  mtry = mtry)


as.list(letters[1:4]) %>% setNames(letters[1:4])

start = Sys.time()
results = flexforest(data=test, iterations = 50,
             formula = test_model, fit_function = fit_rf_sem, variable_sampler = variable_sampler_sem,
           validation_function = loss_sem)
end = Sys.time()
end - start
