test_that("loss_sem works", {
  # make sure loss returns original chi square value
  estimated_loss = loss_sem(test_fit, spearman_brown = FALSE)
  lavaan_loss = loss_sem_chisq(test_fit)
  expect_true(estimated_loss==lavaan_loss)
})

test_that("permuate_variables works", {
  #permuate_variables(fit, )
})
