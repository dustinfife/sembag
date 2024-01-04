test_that("parse_model_code works", {
  string = parse_model_code(test_model)
  expect_true(all(sort(string$observed) == sort(names(test))))
  expect_true(all(string$latent %in% c("z", "y", "x")))
})

test_that("loss_sem works", {

  # make sure it returns NA if it's not a lavaan object
  expect_true(is.na(loss_sem(test)))

  # make sure loss returns original chi square value
  estimated_loss = loss_sem(test_fit, spearman_brown = FALSE) %>% round(digits=2)
  lavaan_loss = loss_sem_chisq(test_fit) %>% round(digits=2)
  expect_true(estimated_loss==lavaan_loss)

  # make sure it throws an error if a variable isn't numeric
  loss_sem(test_fit, data=test)
})

test_that("permute_variables works", {
  # make sure shuffled chi's on average are larger than observed chis
  permuted_vars = permute_variables(reliable_fit, formula = reliable_model)
  expect_true(mean(permuted_vars%>%unlist())>  loss_sem_chisq(reliable_fit))
})

test_that("permute_variable_i works", {
  estimated_loss = loss_sem(single_factor_fit, spearman_brown = FALSE) %>% round(digits=2)

  fitMeasures(single_factor_fit)["srmr"]
  permute_variable_i(single_factor_fit, single_factor_model, single_factor, "p_1_0.9", spearman_brown=FALSE)
  permute_variable_i(single_factor_fit, single_factor_model, single_factor, "p_4_0.1", spearman_brown=FALSE)
  permute_variable_i(single_factor_fit, single_factor_model, single_factor, "p_3_0.37", spearman_brown=FALSE)
  permute_variable_i(single_factor_fit, single_factor_model, single_factor, "p_4_0.1", spearman_brown=FALSE)
})



test_that("shuffle_column_i works", {
  small_shuffled = shuffle_column_i(small, "y")
  expect_true(identical(small_shuffled[,-1], small[,-1]))
  expect_false(identical(small_shuffled[,1], small[,1]))
})

summary(single_factor_fit, standardized=T)

