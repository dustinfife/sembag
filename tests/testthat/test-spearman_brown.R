# create asymmetric matrix
a = lav2ram(test_fit)$A

# create parcels data
variables = dimnames(a)[[1]][-c(1:3)]
parcel_sizes = data.frame(variable=variables, items = 5)


test_that("spearman_brown_adjustment works", {
  # create asymmetric matrix
  a = lav2ram(test_fit)$A

  # make no adjustment
  unchanged = spearman_brown_adjustment("z", a, parcel_sizes, prophecy_items = 5)
  expect_true(all(a[,1]==unchanged))

  # increase the reliability
  higher_rho = spearman_brown_adjustment("z", a, items=parcel_sizes, prophecy_items = 10)
  expect_true(all((higher_rho) >= (a[,1])))

  # decrease the reliability
  lower_rho = spearman_brown_adjustment("z", a, items=parcel_sizes, prophecy_items = 3)
  expect_true(all((lower_rho) <= (a[,1])))
})

test_that("spearman_brown_error", {
  # create asymmetric matrix
  a = lav2ram(test_fit)$A

  # create parcels data
  variables = dimnames(a)[[1]][-c(1:3)]
  parcel_sizes = data.frame(variables=variables, items = 5)

  # when it's called "variables" instead of variable
  expect_error(spearman_brown_error(variables, a, parcel_sizes))

  # when one of the variables isn't in the a matrix
  parcel_sizes[1,1] = "asdfasdf"
  names(parcel_sizes)[1] = "variable"
  expect_error(spearman_brown_error(variables, a, parcel_sizes))
})

test_that("lav2ram works", {
  a = lav2ram(test_fit)$A
  # extract factor loadings
  fl = lavaan::lavInspect(test_fit, "std")$lambda
  expect_true(a["z1","z"] == fl["z1", "z"])

  # extract variances
  s = lav2ram(test_fit)$S
  lavaan::lavInspect(test_fit, "std")$psi
  s
})

test_that("ram_matrix_adjustment_sb works", {

  original_varcov = fitted(test_fit)$cov
  # with no adjustment, the implied and sb-adjusted are the same
  sb_adjusted_varcov = ram_matrix_adjustment_sb(test_fit, parcel_sizes)
  expect_equal(sum(sb_adjusted_varcov - original_varcov), 0, tolerance=.001)

  # increasing items makes it more reliable
  sb_adjusted_varcov = ram_matrix_adjustment_sb(test_fit, parcel_sizes, prophecy_items = 10)
  expect_true(sum(sb_adjusted_varcov - original_varcov)>0)

  # decreasing items makes it less reliable
  sb_adjusted_varcov = ram_matrix_adjustment_sb(test_fit, parcel_sizes, prophecy_items = 2)
  expect_true(sum(sb_adjusted_varcov - original_varcov)<0)

})


