
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

require(tidyverse)
start = Sys.time()
results = flexforest(data=bi, iterations = 1000,
                     formula = bi_mod, fit_function = fit_rf_sem,
                     variable_sampler = variable_sampler_sem,
                     validation_function = loss_sem)
end = Sys.time()
end - start

results[[1]]

xd = data.frame(names=names(results$varimp)[1:50]  , value=results$varimp[1:50])  %>%
  mutate(names=factor(names, levels=paste0("x", 1:50), ordered=T))
yd = data.frame(names=names(results$varimp)[51:100], value=results$varimp[51:100])%>%
  mutate(names=factor(names, levels=paste0("y", 1:50), ordered=T))
require(flexplot)
flexplot(value~names, data=xd)
flexplot(value~names, data=yd)

