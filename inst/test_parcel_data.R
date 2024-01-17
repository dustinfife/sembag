require(tidyverse)
require(lavaan)

#sembag:::parse_model_codeode(parcel_mod)
head(parcel_items)
summary(parcel_fit)

for (i in 1:10) {
results = sembag:::sembag(data=parcel_data, iterations = 1000,
                          formula = parcel_mod,
                          fit_function = sembag:::fit_rf_sem,
                          variable_sampler = sembag:::variable_sampler_sem,
                          validation_function = sembag:::loss_sem,
                          mtry=5, spearman_brown=FALSE, parcel_sizes=parcel_items)

  save(results, file=paste0("~/Downloads/parcel_test_nosb-", i, ".rdata"))
}
files = list.files(path = "~/Downloads", pattern="parcel_test_nosb-", full.names = T)
for (i in 1:length(files)) {
  load(files[i])
  results
  if (i==1) {
    importance_measures = data.frame(matrix(nrow=length(files), ncol=length(results)))
    names(importance_measures) = names(results)
  }
  importance_measures[i,] = results
}

sort(colMeans(importance_measures))
# i=1
# head(importance_measures)
# sort(importance_measures)
# parcel_means = 1:5
# for (i in 1:5) {
#   parcel_group = gsub("p_", "", names(importance_measures)) %>% substr(1,1) == i
#   parcel_means[i] = median(importance_measures[parcel_group]%>%as.numeric)
#
# }
# names(parcel_means) = paste0("p", 1:5)
# parcel_means
# mean((importance_measures[parcel_group])%>%as.numeric)

# aggregate  info
var_names = names(importance_measures)
vi_means = colMeans(importance_measures, na.rm=T)
ordered_variables = unique(names(sort(vi_means, decreasing = T)))
sembag_results =
  data.frame(variable = var_names,
             means=vi_means) %>%
  arrange(desc(means))
sembag_results = sembag_results %>%
  mutate(variable = factor(variable, ordered=T, levels=ordered_variables))

# create empty column for latent variable
sembag_results$rank= 1:nrow(sembag_results)
sembag_results$latent = NA; sembag_results$latent2 = NA


###############

# source the original model so we can identify latent/observed variables from it
observed_latent_pairs = sembag:::parse_model_code(parcel_mod, return_observed_as_vector=F)

# Apply the function to each sublist in the list
observed_latent_pairs$observed <- lapply(observed_latent_pairs$observed, function(x) trimws(x))

# create function to find latent variable associated with observed variable
find_observed_in_list = function(x) {
  index = which(sapply(observed_latent_pairs$observed, function(sublist) x %in% sublist))
  latent_name = observed_latent_pairs$latents[index]
}
i = 1
options(warn=1)
for (i in 1:nrow(sembag_results)){
  current_variable = sembag_results$variable[i]
  latent_variable = find_observed_in_list(current_variable)
  if (length(latent_variable)>1) {
    sembag_results$latent[i] = latent_variable[1]
    sembag_results$latent2[i] = latent_variable[-1]
  } else {
    sembag_results$latent[i] = latent_variable
  }
  # note: some variables load onto more than one latent!!!!!
}
