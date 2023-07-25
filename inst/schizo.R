require(tidyverse)
d = read.csv("Datasets/schizo_bootstrapped.csv") %>%
  mutate(sfs_ic1 = as.numeric(sfs_ic1))
source("inst/schizo_model.R")

# check variables are in dataset
require(tidyverse)
parse_model_code(full.model)
observed = parse_model_code(full.model)$observed %>% trimws
d[,observed] # it was able to find all variables in our model. Yay!
head(d[,observed])
# run the model
mode(d[,observed[1]])
a = lapply(d[,observed], function(x) {if (mode(x)=="numeric") {return(NA)} else {return(mode(x))}}) %>% unlist
a[!is.na(a)]
d$sfs_ic1
observed[1]
results = sembag:::sembag(data=d, iterations = 1000,
                          formula = full.model,
                          fit_function = sembag:::fit_rf_sem,
                          variable_sampler = sembag:::variable_sampler_sem,
                          validation_function = sembag:::loss_sem,
                          mtry=20)
results
nrow(d)
grep("assist", names(d), value=T)
d[,c("assist3", "assist6", "olife_3")]
d[,grep("psqi", names(d))]
(sort(results$varimp))
# some of these items are not numeric: psqi4, psqi15, psqi1-4, psqi26
# these items do not exist:
  # pdi_think17, pdi_true3, pdi_true21, pdi_dis19, pdi_dis17 pdi_true11 pdi_end10 pdi_dis8 pdi_dis4 pqb_dis29 pdi_end8
  # pdi_end11 pdi_true15 pqb_dis33 pqb_dis39 pdi_end2 pdi_dis5 pqb_dis23 pqb_dis3 pdi_think6 pdi_think2 pdi_think10 pdi_true9 pdi_dis3
  # pqb_dis1 pqb_dis5 pqb_dis7 pqb_dis9 pqb_dis13 pqb_dis15 pqb_dis17 pqb_dis21 pqb_dis25 pqb_dis27 pqb_dis31 pqb_dis35 pqb_dis37
  # pdi_end3 pdi_end4 pdi_end6 pdi_end7 pdi_end12 pdi_end14 pdi_end15 pdi_end16 pdi_end18 pdi_end19 pdi_end20
  # pdi_dis1 pdi_dis7 pdi_dis9 pdi_dis11 pdi_dis12 pdi_dis13 pdi_dis15 pdi_dis16 pdi_dis20 pdi_dis21
  # pdi_think1 pdi_think4 pdi_think5 pdi_think8 pdi_think9 pdi_think12 pdi_think13 pdi_think14 pdi_think16 pdi_think18 pdi_think20 pdi_think21 pdi_true1 pdi_true2 pdi_true5 pdi_true6 pdi_true7 pdi_true10 pdi_true13 pdi_true14 pdi_true17 pdi_true18
  # pdi_true19

names(results)


save(results, file="inst/schizo_sembag_results.Rdat")
results$varimp# sembag_inloop(data=d, formula=full.model, iterations = 10,
#                                fit_function = fit_rf_sem, variable_sampler = variable_sampler_sem,
#                                validation_function = loss_sem,
#                                mtry = 10)
# start = Sys.time()
# results
# end = Sys.time()
# end - start
