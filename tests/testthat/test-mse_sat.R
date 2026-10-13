# Expected values: inst/validation/reference/test_values.txt (independent Python implementation)

test_that("mse_sat matches the independent Python values", {
  expect_equal(mse_sat(40, n_e = 100, bias = 0.1, tau = 0.05), 0.0475, tolerance = 1e-12)
  expect_equal(mse_sat(30, n_e = Inf, bias = -0.2, sigma_t = 1.2, sigma_c = 0.8), 0.088,
               tolerance = 1e-12)
})

test_that("mse_sat has the expected limiting cases", {
  # Fixed threshold, no bias: only the variance of the treated patients
  expect_equal(mse_sat(40), 1 / 40, tolerance = 1e-12)
  # The sign of the bias does not matter
  expect_equal(mse_sat(40, 100, bias = 0.3), mse_sat(40, 100, bias = -0.3), tolerance = 1e-12)
  # A fixed bias and a random bias of the same size give the same mean squared error
  expect_equal(mse_sat(40, 100, bias = 0.2), mse_sat(40, 100, tau = 0.2), tolerance = 1e-12)
})

test_that("mse_sat rejects invalid arguments", {
  expect_error(mse_sat(0), "'n'")
  expect_error(mse_sat(10, n_e = 0), "'n_e'")
  expect_error(mse_sat(10, tau = -0.1), "'tau'")
  expect_error(mse_sat(10, bias = Inf), "'bias'")
  expect_error(mse_sat(10, bias = NA_real_), "'bias'")
})
