set.seed(2323)
require(tidyverse)
# create latent variable
x = rnorm(3000)
y = .4*x + rnorm(length(x), 0, sqrt(1-.4^2))

# create regular factor loadings
rescale_round = function(x) {
  x = x %>% flexplot:::rescale(5,2) %>%
    flexplot:::floor_ceiling(1,10)
  return(round(x))
}

linear = function(x, f1) {
  x = (x = x*f1 + rnorm(length(f1), 0, sqrt(1-x^2)))# %>%
    #rescale_round
  return(x)
}

linear_x = seq(from=.95, to=.1, length.out=50) %>% purrr::map_dfc(linear, x) %>%
  set_names(paste0("x", 1:50))
linear_y = seq(from=.95, to=.1, length.out=50) %>% purrr::map_dfc(linear, y) %>%
  set_names(paste0("y", 1:50))
bi = data.frame(cbind(linear_x, linear_y))

bi_mod = '
f1 =~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x25+x26+x27+x28+x29+x30+x31+x32+x33+x34+x35+x36+x37+x38+x39+x40+x41+x42+x43+x44+x45+x46+x47+x48+x49+x50
f2 =~ y1+y2+y3+y4+y5+y6+y7+y8+y9+y10+y11+y12+y13+y14+y15+y16+y17+y18+y19+y20+y21+y22+y23+y24+y25+y26+y27+y28+y29+y30+y31+y32+y33+y34+y35+y36+y37+y38+y39+y40+y41+y42+y43+y44+y45+y46+y47+y48+y49+y50
'
usethis::use_data(bi_mod, overwrite=T)
usethis::use_data(bi, overwrite=T)

