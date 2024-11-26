library(testthat)
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("myncurve returns correct mu, sigma, and probability", {
  result <- myncurve(5, 3, 5)
  expect_equal(result$mu, 5)
  expect_equal(result$sigma, 3)
  expect_equal(result$prob_x_less_a, pnorm(5, mean = 5, sd = 3), tolerance = 1e-8)
})
