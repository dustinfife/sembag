start = Sys.time()
flexforest(data=test, formula=y1 ~ x1 + x2 + x3 + x4 + x5 + z1 + z2 + z3 + z4  + z5, iterations = 10)
end = Sys.time()
end - start
flexforest_inloop(data=test, formula=y1 ~ x1 + x2 + x3 + x4 + x5 + z1 + z2 + z3 + z4  + z5, iterations = 10)

start = Sys.time()
flexforest(data=test, iterations = 50,
             formula = test_model, fit_function = fit_rf_sem, variable_sampler = variable_sampler_sem,
           validation_function = loss_sem)
end = Sys.time()
end - start
