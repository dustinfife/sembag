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
x = runif(20) %>%
  purrr::map_dfc(f, latents$x) %>%
  set_names(paste0("x", 1:20))
y = runif(50) %>%
  purrr::map_dfc(f, latents$y) %>%
  set_names(paste0("y", 1:50))
z = runif(75) %>%
  purrr::map_dfc(f, latents$z) %>%
  set_names(paste0("z", 1:75))

test = data.frame(cbind(x,y,z))
usethis::use_data(test, overwrite=T)




# create sem model for this -----------------------------------------------

model = paste0("x=~", paste0(names(select(data, starts_with("x"))), collapse=" + "), collapse="") %.% "
  " %.% paste0("y=~", paste0(names(select(data, starts_with("y"))), collapse=" + "), collapse="") %.% "
  " %.% paste0("z=~", paste0(names(select(data, starts_with("z"))), collapse=" + "), collapse="")

require(lavaan)
lavaan(model, data=test)
