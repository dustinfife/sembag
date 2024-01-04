# components:
# 1. A loss function
# 2. An algorithm of the form fit(data, model, ...)
  # I will have to figure out the standardized objects to return from that
  # such as the loss function (#1).
  # Also, this will contain the *entire* model from which RF randomly samples
# 3. Other stuff for RF
set.seed(12121)
require(tidyverse)
latent_matrix = matrix(
  c(1, .3, .4,
    .3, 1, .5,
    .4, .5, 1),
  nrow = 3
)
latents = data.frame(MASS::mvrnorm(200, mu=c(0,0,0), Sigma = latent_matrix)) %>%
  set_names("x", "y", "z")
f = function(x, f1) return(x = x*f1 + rnorm(length(f1), 0, sqrt(1-x^2)))
x = runif(5) %>%
  purrr::map_dfc(f, latents$x) %>%
  set_names(paste0("x", 1:5))
y = runif(5) %>%
  purrr::map_dfc(f, latents$y) %>%
  set_names(paste0("y", 1:5))
z = runif(5) %>%
  purrr::map_dfc(f, latents$z) %>%
  set_names(paste0("z", 1:5))

test = data.frame(cbind(x,y,z))
usethis::use_data(test, overwrite=T)




# create sem model for this -----------------------------------------------
test_model = '
z =~ z1 + z2 + z3 + z4 + z5
x =~ x1 + x2 + x3 + x4 + x5
y =~ y1 + y2 + y3 + y4 + y5
'
usethis::use_data(test_model, overwrite=T)
require(lavaan)
test_fit = sem(test_model, data=test)
usethis::use_data(test_fit, overwrite = T)


x = rnorm(5000)
reliable_data = runif(5, .95, .96) %>%
  purrr::map_dfc(f, x) %>%
  set_names(paste0("z", 1:5))
reliable_model = '
z =~ z1 + z2 + z3 + z4 + z5
'
reliable_fit = sem(reliable_model, reliable_data)
usethis::use_data(reliable_data, overwrite=T)
usethis::use_data(reliable_model, overwrite=T)
usethis::use_data(reliable_fit, overwrite=T)
