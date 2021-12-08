test_that("get_mtry works", {
  expect_equal(get_mtry(letters[1:20], 5), 5)
  expect_true(length(get_mtry(list(letters[1:5], letters[6:11])))==2)
  expect_equal(get_mtry(letters[1:20]), sqrt(20))
})

test_that("variable_sampler works", {
  expect_true(length(variable_sampler(letters[1:20])) == 4)
  expect_true(length(variable_sampler(list(letters[1:15], letters[16:24])))==6)
})
