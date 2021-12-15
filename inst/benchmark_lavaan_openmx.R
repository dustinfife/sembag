require(tidySEM)
#install.packages("tidySEM")
lav_syntax = '
  f =~ x1
  f =~ x2
  f =~ x3
  f =~ x4
  f =~ x5
  f =~ x6
  f =~ x7
  f =~ x8
  f =~ x9
'
mx_mod = as_ram(lav_syntax, data=uni)

# fit lavaan model
require(lavaan)
start_lav = Sys.time()
mod_lav = cfa(lav_syntax, uni)
time_lav  = Sys.time() - start_lav

# fit mx model
require(OpenMx)
start_omx = Sys.time()
mod_omx = mxRun(mx_mod)
time_omx  = Sys.time() - start_omx

time_lav
time_omx

?sem
