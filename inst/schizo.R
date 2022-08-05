d = read.csv("/Users/dustinfife/Dropbox/research/Statistical Framework/O'Kane Dissertation/Datasets/practice_data_bootstrapped.csv")
d[,"pdi_think17"]
#(?<!a)b # b not preceded by a
#a(?!b) a not followed by b
full.model = '
  cannabis =~ assist3 + assist6 + assist9 + assist12 + assist15 + assist18
  extraversion =~ bfi1 + bfi2 + bfi3 + bfi4 + bfi5 + bfi6 + bfi7 + bfi8
  neuroticism =~ bfi9 + bfi10 + bfi11 + bfi12 + bfi13 + bfi14 + bfi15 + bfi16
  sleep =~ psqi5 + psqi6 + psqi7 + psqi8 + psqi9 + psqi10 + psqi11 + psqi12 + psqi13 + psqi14 + psqi16 + psqi17 + psqi18 + psqi19 + psqi20 + psqi21 + psqi22 + psqi23 + psqi24 + psqi25
  social_desirability =~ sds_1 + sds_2 + sds_3 + sds_4 + sds_5 + sds_6 + sds_7 + sds_8 + sds_9 + sds_10
  pos.schizotypy =~ spq_3 + spq_7 + spq_10 + spq_12 + spq_13 +
     spq_18 + spq_20 + spq_21 + spq_23 + spq_25 + spq_26 +
     spq_27 + spq_28 + spq_29 + olife_1 + olife_2 + olife_3 +
     olife_4 + olife_5 + olife_6 + olife_7 + olife_8 + olife_9 +
     olife_10 + olife_11 + olife_12 + olife_13 + olife_14 + olife_15 +
     olife_16 + olife_17 + olife_18 + olife_19 + olife_20 +
     olife_21 + olife_22 + olife_23 + mssb_2 + mssb_5 + mssb_8 +
     mssb_11 + mssb_14 + mssb_17 + mssb_20 + mssb_23 + mssb_26 +
     mssb_29 + mssb_32 + mssb_35 + mssb_38 + wss_mi_1 + wss_mi_2 +
     wss_mi_3 + wss_mi_4 + wss_mi_5 + wss_mi_6 + wss_mi_7 + wss_mi_8 +
     wss_mi_9 + wss_mi_10 + wss_mi_11 + wss_mi_12 + wss_mi_13 +
     wss_mi_14 + wss_mi_15 + wss_per_1 + wss_per_2 + wss_per_3 +
     wss_per_4 + wss_per_5 + wss_per_6 + wss_per_7 + wss_per_8 +
     wss_per_9 + wss_per_10 + wss_per_11 + wss_per_12 + wss_per_13 +
     wss_per_14 + wss_per_15 + pqb_end1 + pqb_end3 + pqb_end5 + pqb_end7 +
     pqb_end9 + pqb_end13 + pqb_end15 + pqb_end17 + pqb_end21 +
     pqb_end23 + pqb_end25 + pqb_end27 + pqb_end29 + pqb_end31 + pqb_end33 +
     pqb_end35 + pqb_end37 + pqb_end39 + r_gpts1 + r_gpts2 + r_gpts3 +
     r_gpts4 + r_gpts5 + r_gpts6 + r_gpts7 + r_gpts8 + r_gpts9 + r_gpts10 +
     r_gpts11 + r_gpts12 + r_gpts13 + r_gpts14 + r_gpts15 + r_gpts16 +
     r_gpts17 + r_gpts18 + rhs1 + rhs2 + rhs3 + rhs4 + rhs5 + rhs6 +
     rhs7 + rhs8 + rhs9 + rhs10 + rhs11 + rhs12 + rhs13 + rhs14 +
     rhs15 + rhs16 + rhs17 + rhs18 + rhs19 + rhs20 + pdi_end1 +
     pdi_end5 +
     pdi_end9 + pdi_end13 +
     pdi_end17 + pdi_end21 + pdi_dis2 +
     pdi_dis6 +  pdi_dis10 + pdi_dis14 +
     pdi_dis18 +
     pdi_think3 + pdi_think7 +
     pdi_think11 +
     pdi_think15 +
     pdi_think19 +
     pdi_true4 +
     pdi_true8 +
     pdi_true12 +
     pdi_true16 + pdi_true20
'
sqrt(ncol(d))
#head(d[,grep("pdi", names(d))])
# regex to remove carriage return + instances
#full.model = gsub("[\n\r][ ]?[+]", "+", full.model)
#strsplit(full.model, "\n")
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



# sembag_inloop(data=d, formula=full.model, iterations = 10,
#                                fit_function = fit_rf_sem, variable_sampler = variable_sampler_sem,
#                                validation_function = loss_sem,
#                                mtry = 10)
# start = Sys.time()
# results
# end = Sys.time()
# end - start
