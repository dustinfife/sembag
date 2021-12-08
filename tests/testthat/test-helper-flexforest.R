test_that("get_mtry works", {
  expect_equal(get_mtry(letters[1:20], 5), 5)
  expect_true(length(get_mtry(list(letters[1:5], letters[6:11])))==2)
  expect_equal(get_mtry(letters[1:20]), sqrt(20))
})

test_that("variable_sampler works", {
  expect_true(length(variable_sampler(letters[1:20])) == 4)
  expect_true(length(variable_sampler(list(letters[1:15], letters[16:24])))==6)
})

test_that("return_formula_i works", {
  expect_equal(length(all.vars(return_formula_i(y~a + b + c + d + e + f + g + h + i))), 4)
  sem_mod = return_formula_i(test_model, variable_sampler_sem)
  expect_equal(length(unlist(strsplit(sem_mod, "\n"))), 3)
  expect_equal(length(unlist(strsplit(sem_mod, "+", fixed=T))), 7)
})

test_that("fit_data_i works", {
  expect_equal(fit_data_i(y~a + x, data=bootstrap_sample(small)$training)%>%class, "lm")
  mod_i = return_formula_i(test_model, variable_sampler_sem)
  fit_data_i(mod_i, data=bootstrap_sample(test)$training, fit_rf_sem)
})
