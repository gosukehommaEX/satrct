# Expected values: inst/validation/reference/test_values.txt (independent Python implementation)

test_that("mse_rct matches the independent Python values", {
  expect_equal(mse_rct(40), 0.1, tolerance = 1e-12)
  expect_equal(mse_rct(30, sigma_t = 1.2, sigma_c = 0.8, r = 2 / 3), 0.136, tolerance = 1e-12)
})

test_that("mse_rct recycles vector arguments", {
  expect_equal(mse_rct(c(40, 30), sigma_t = c(1, 1.2), sigma_c = c(1, 0.8), r = c(0.5, 2 / 3)),
               c(0.1, 0.136), tolerance = 1e-12)
  expect_length(mse_rct(c(10, 20, 40)), 3L)
  expect_error(mse_rct(c(10, 20, 30), sigma_t = c(1, 2)), "common length")
})

test_that("mse_rct is minimised by Neyman allocation", {
  r_grid <- seq(0.05, 0.95, by = 0.01)
  r_best <- r_grid[which.min(mse_rct(60, sigma_t = 2, sigma_c = 1, r = r_grid))]
  expect_equal(r_best, 2 / 3, tolerance = 0.02)
})

test_that("mse_rct rejects invalid arguments", {
  expect_error(mse_rct(0), "'n'")
  expect_error(mse_rct(-5), "'n'")
  expect_error(mse_rct(NA_real_), "'n'")
  expect_error(mse_rct("10"), "'n'")
  expect_error(mse_rct(10, r = 1), "'r'")
  expect_error(mse_rct(10, sigma_t = 0), "'sigma_t'")
  expect_error(mse_rct(10, sigma_c = -1), "'sigma_c'")
})
