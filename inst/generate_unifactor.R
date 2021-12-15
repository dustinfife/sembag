set.seed(2323)
require(tidyverse)
# create latent variable
x = rnorm(3000)

# create regular factor loadings
rescale_round = function(x) {
  x = x %>% flexplot:::rescale(5,2) %>%
    flexplot:::floor_ceiling(1,10)
  return(round(x))
}
linear = function(x, f1) {
  x = (x = x*f1 + rnorm(length(f1), 0, sqrt(1-x^2))) %>%
    rescale_round
  return(x)
}
interaction = function(x1, x2, x1x2, f1) {

  x_1 = x1*f1 + rnorm(length(f1), 0, sqrt(1-x1^2)) %>%
    rescale_round
  x_2 = x2*f1 + x1x2*x1 + rnorm(length(f1), 0, .25*sqrt(1-x1^2-x1x2^2)) %>%
    rescale_round
  return(data.frame(x_1=x_1, x_2=x_2))
}
nonlinear = function(a, b, f1) {
  x = -1* (sqrt(a^2 + 4*b*(f1-2)) + a)/(2*b) + rnorm(length(f1), 0, .25)
  #x = rescale(x, 5, 2)
  return(x)
}

plot(x,nonlinear(.5, -.2, x))
no_effect = function(x, f) {
  rnorm(length(f), 5, 2) %>% rescale_round()
}

linear_x = runif(5, .5, .9) %>% purrr::map_dfc(linear, x) %>%
  set_names(paste0("x", 1:5))
interaction_x = runif(2, .3, .6) %>% purrr::map_dfc(interaction,
                                        x2=runif(5, .2, .5),
                                        x1x2 = runif(5, -.6, -.4),
                                        f1 = x) %>%
  round() %>%
  set_names(paste0("x", 6:9))
#nonlinear_x = runif(5, .3, .5) %>% purrr::map_dfc(nonlinear, -.2, x) %>%
#  set_names(paste0("x", 10:14))

no_effect_x = 1:5 %>% map_dfc(no_effect, x) %>% set_names(paste0("x", 15:19))


uni = data.frame(cbind(linear_x, interaction_x, no_effect_x, latent=x))
head(uni)
require(flexplot)
flexplot(latent~x11, data=uni)
flexplot(latent~x13, data=uni)
latent_uni = x
uni_mod = '
f =~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x15+x16+x17+x18+x19
'
usethis::use_data(uni_mod, overwrite=T)
usethis::use_data(uni, overwrite=T)
usethis::use_data(latent_uni, overwrite=T)

paste0(names(uni), collapse="+")
head(uni)


# create nonlinear effects
