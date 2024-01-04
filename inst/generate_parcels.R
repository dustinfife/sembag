require(tidyverse)
require(lavaan)
set.seed(1212)


# one latent variable with 20 parcels, decreasing in reliability.
n = 1500
parcels = 20

# generate a variance/covariance matrix of latent variables
latents = rnorm(n)

# function to simulate items
linear = function(x, f1) {
  score = x*f1 + rnorm(length(f1), 0, sqrt(1-x^2))
  return(flexplot::rescale(score, 50, 20)%>%round)
}

d = data.frame(matrix(nrow=n, ncol=parcels))
factor_loadings = seq(from=.9, to=.1, length.out=parcels) %>% round(digits=2)

i = 1
for (i in 1:ncol(d)) {
  # simulate a bunch of parcels
  parcel_sums   = rep(factor_loadings[i], times=10)  %>% purrr::map_dfc(~ linear(.x, latents)) %>% rowMeans
  d[,i] = parcel_sums
  names(d)[i] = paste0("p_", i, "_", factor_loadings[i])
}
parcel_data = d

# write lavaan syntax for this
parcel_mod = paste0("f1 =~ ", paste0(names(d), collapse=" + "))
parcel_items = data.frame(variable=names(d), items = 10)
parcel_fit = sem(parcel_mod, data=d)
usethis::use_data(parcel_data, overwrite=T)
usethis::use_data(parcel_fit, overwrite=T)
usethis::use_data(parcel_mod, overwrite=T)
usethis::use_data(parcel_items, overwrite=T)
summary(parcel_fit)



require(tidyverse)
require(lavaan)
set.seed(1212)


# one latent variable with 20 parcels, decreasing in reliability.
n = 50000
parcels = 4

# generate a variance/covariance matrix of latent variables
latents = rnorm(n)

# function to simulate items
linear = function(x, f1) {
  score = x*f1 + rnorm(length(f1), 0, sqrt(1-x^2))
  return(flexplot::rescale(score, 50, 20)%>%round)
}

d = data.frame(matrix(nrow=n, ncol=parcels))
factor_loadings = seq(from=.9, to=.1, length.out=parcels) %>% round(digits=2)

i = 1
for (i in 1:ncol(d)) {
  # simulate a bunch of parcels
  parcel_sums   = rep(factor_loadings[i], times=1)  %>% purrr::map_dfc(~ linear(.x, latents)) %>% rowMeans
  d[,i] = parcel_sums
  names(d)[i] = paste0("p_", i, "_", factor_loadings[i])
}
single_factor = d

# write lavaan syntax for this
single_factor_model = paste0("f1 =~ ", paste0(names(d), collapse=" + "))
single_factor_fit = sem(single_factor_model, data=single_factor)
usethis::use_data(single_factor, overwrite=T)
usethis::use_data(single_factor_model, overwrite=T)
usethis::use_data(single_factor_fit, overwrite=T)
usethis::use_data(parcel_items, overwrite=T)
summary(single_factor_fit, standardized=T)
nrow(single_factor)










# generate two latent variables that have identical factor loadings, post SB
n = 1500

# generate a variance/covariance matrix of latent variables
cor_latent = matrix(0, nrow=2, ncol=2)
diag(cor_latent) = 1

# simulate raw data for variance/covariance matrix
latents = MASS::mvrnorm(n, mu=c(0,0), Sigma = cor_latent)

# function to simulate items
linear = function(x, f1) {
  score = x*f1 + rnorm(length(f1), 0, sqrt(1-x^2))
  return(flexplot::rescale(score, 50, 20)%>%round)
}

d = data.frame(matrix(nrow=nrow(latents), ncol=ncol(cor_latent)))
d_all = list(d, d)


sb_values = matrix(
  c(.8, 16,
    sb_calculation(.8, .5), 8), byrow=T, ncol=2
)


for (i in 1:ncol(latents)) {
  # simulate a bunch of parcels
  parcel_one   = rep(sb_values[i,1], times=sb_values[i,2])  %>% purrr::map_dfc(~ linear(.x, latents[,i]))  %>%  set_names(paste0("p_1_", i, "_", 1:sb_values[i,2])) %>% rowMeans
  parcel_two   = rep(sb_values[i,1], times=sb_values[i,2])  %>% purrr::map_dfc(~ linear(.x, latents[,i]))  %>%  set_names(paste0("p_2_", i, "_", 1:sb_values[i,2] )) %>% rowMeans
  parcel_three = rep(sb_values[i,1], times=sb_values[i,2])  %>% purrr::map_dfc(~ linear(.x, latents[,i]))  %>%  set_names(paste0("p_3_", i, "_", 1:sb_values[i,2] )) %>% rowMeans

  d_all[[i]] = cbind(parcel_one, parcel_two, parcel_three) %>% data.frame %>% set_names(paste0("p_", i, "_", 1:3))
}

head(d_all[[1]])

parcel_data = do.call(cbind.data.frame, d_all)

# write lavaan syntax for this
parcel_mod = '
f1 =~ p_1_1 + p_1_2 + p_1_3
f2 =~ p_2_1 + p_2_2 + p_2_3
'
head(parcel_data)
parcel_items = data.frame(variable=names(parcel_data), items = c(16,16,16,8,8,8))
parcel_fit = sem(parcel_mod, data=parcel_data)
usethis::use_data(parcel_data, overwrite=T)
usethis::use_data(parcel_fit, overwrite=T)
usethis::use_data(parcel_mod, overwrite=T)
usethis::use_data(parcel_items, overwrite=T)
summary(parcel_fit)











# set global parameters
n = 1500

# generate a variance/covariance matrix of latent variables
cor_latent = matrix(.3, nrow=5, ncol=5)
diag(cor_latent) = 1

# simulate raw data for variance/covariance matrix
latents = MASS::mvrnorm(n, mu=rep(0, 5), Sigma = cor_latent)

# function to simulate items
linear = function(x, f1) { x*f1 + rnorm(length(f1), 0, sqrt(1-x^2)) }

d = data.frame(matrix(nrow=nrow(latents), ncol=5))
d_all = list(d, d, d, d, d)
for (i in 1:ncol(latents)) {
  # simulate a bunch of parcels
  parcel_one   = rep(.8, times=15) %>% purrr::map_dfc(~ linear(.x, latents[,i]))  %>%  set_names(paste0("p_1_", i, "_", 1:15)) %>% rowSums
  parcel_two   = rep(.8, times=10)  %>% purrr::map_dfc(~ linear(.x, latents[,i]))  %>%  set_names(paste0("p_2_", i, "_", 1:10 )) %>% rowSums
  parcel_three = rep(.8, times=5)  %>% purrr::map_dfc(~ linear(.x, latents[,i]))  %>%  set_names(paste0("p_3_", i, "_", 1:5 )) %>% rowSums
  parcel_four  = rep(.1, times=5)  %>% purrr::map_dfc(~ linear(.x, latents[,1]))  %>%  set_names(paste0("p_4_", i, "_", 1:5 )) %>% rowSums
  parcel_five  = rep(.1, times=10)  %>% purrr::map_dfc(~ linear(.x, latents[,1]))  %>%  set_names(paste0("p_5_", i, "_", 1:10 )) %>% rowSums

  d_all[[i]] = cbind(parcel_one, parcel_two, parcel_three, parcel_four, parcel_five) %>% data.frame %>% set_names(paste0("p_", i, "_", 1:5))
}

parcel_data = do.call(cbind.data.frame, d_all)

# write lavaan syntax for this
parcel_mod = '
f1 =~ p_1_5 + p_1_2 + p_1_3 + p_1_4 + p_1_1
f2 =~ p_2_5 + p_2_2 + p_2_3 + p_2_4 + p_2_1
f3 =~ p_3_5 + p_3_2 + p_3_3 + p_3_4 + p_3_1
f4 =~ p_4_5 + p_4_2 + p_4_3 + p_4_4 + p_4_1
f5 =~ p_5_5 + p_5_2 + p_5_3 + p_5_4 + p_5_1
'
parcel_items = data.frame(variable=names(parcel_data), items = c(15,10,5,5,10))
usethis::use_data(parcel_data, overwrite=T)
usethis::use_data(parcel_mod, overwrite=T)
usethis::use_data(parcel_items, overwrite=T)
