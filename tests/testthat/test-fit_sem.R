test_that("parse_model_code works", {
  res = parse_model_code(test_model)
  expect_equal(res$observed[1:5], "z" %.% 1:5)
  expect_equal(res$latents, c("z", "x", "y"))
  expect_equal(parse_model_code(test_model, F)$observed %>% length, 3)
})

test_that("loss_sem_coef works", {
  expect_equal(dim(loss_sem_coef(test_fit)), c(15,3))
})

test_that("loss_sem_chisq works", {
  expect_equal(loss_sem_chisq(test_fit) %>% as.numeric,  96.1742, tolerance= .001)
})
