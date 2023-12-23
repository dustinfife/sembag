require(tidyverse)
set.seed(1212)

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
  parcel_one   = rep(.6, times=10) %>% purrr::map_dfc(~ linear(.x, latents[,1]))  %>%  set_names(paste0("p_1_", i, "_", 1:10)) %>% rowSums
  parcel_two   = rep(.6, times=5)  %>% purrr::map_dfc(~ linear(.x, latents[,1]))  %>%  set_names(paste0("p_2_", i, "_", 1:5 )) %>% rowSums
  parcel_three = rep(.9, times=3)  %>% purrr::map_dfc(~ linear(.x, latents[,1]))  %>%  set_names(paste0("p_3_", i, "_", 1:3 )) %>% rowSums
  parcel_four  = rep(.1, times=7)  %>% purrr::map_dfc(~ linear(.x, latents[,1]))  %>%  set_names(paste0("p_4_", i, "_", 1:7 )) %>% rowSums
  parcel_five  = rep(.3, times=5)  %>% purrr::map_dfc(~ linear(.x, latents[,1]))  %>%  set_names(paste0("p_5_", i, "_", 1:5 )) %>% rowSums

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
parcel_items = data.frame(variable=names(parcel_data), items = c(10,5,3,7,5))
usethis::use_data(parcel_data, overwrite=T)
usethis::use_data(parcel_mod, overwrite=T)
usethis::use_data(parcel_items, overwrite=T)
