# Expected values: inst/validation/reference/test_values.txt (independent Python implementation
# of equations 6 and 7 of Pocock (1976), cross-checked by numerical minimisation)

test_that("optimal_nc reproduces Example 2 of Pocock (1976)", {
  sigma_response <- sqrt(45 * 55)
  res <- optimal_nc(200, n_e = 83, tau = c(3, 5), sigma_t = sigma_response)
  expect_equal(res[["n_c"]], c(68.12150837988827, 77.42582417582418), tolerance = 1e-12)
  # Printed values: 68.1 and 77 (pp.186-187)
  expect_equal(round(res[["n_c"]], 1)[1], 68.1)
  expect_equal(round(res[["n_c"]])[2], 77)
  expect_equal(res[["n_eff"]][1], 63.756983240223455, tolerance = 1e-12)
  res <- optimal_nc(200, n_e = 88, tau = c(0.05, 0.1))
  expect_equal(res[["n_c"]], c(63.9344262295082, 76.59574468085107), tolerance = 1e-12)
  # Printed values: 63.9 and 77
  expect_equal(round(res[["n_c"]], 1)[1], 63.9)
  expect_equal(round(res[["n_c"]])[2], 77)
})

test_that("optimal_nc matches the independent Python values with unequal standard deviations", {
  res <- optimal_nc(60, n_e = 100, bias = 0.05, tau = 0.1, sigma_t = 1.2, sigma_c = 0.8)
  expect_equal(res[["n_c"]], 3.6825396825396868, tolerance = 1e-12)
  expect_equal(res[["n_eff"]], 33.86243386243386, tolerance = 1e-12)
  expect_equal(res[["mse"]], 0.04261555806087937, tolerance = 1e-10)
})

test_that("optimal_nc gives zero concurrent controls when the single-arm trial is optimal", {
  res <- optimal_nc(50, n_e = 200, bias = 0.1)
  expect_identical(res[["n_c"]], 0)
  expect_equal(res[["n_eff"]], 66.66666666666666, tolerance = 1e-12)
  expect_equal(res[["mse"]], 0.035, tolerance = 1e-12)
  # Condition with equal standard deviations: bias^2 + tau^2 <= sigma^2 (1 / n - 1 / n_e)
  b_limit <- sqrt(1 / 50 - 1 / 200)
  expect_identical(optimal_nc(50, n_e = 200, bias = b_limit * 0.99)[["n_c"]], 0)
  expect_true(optimal_nc(50, n_e = 200, bias = b_limit * 1.01)[["n_c"]] > 0)
})

test_that("the optimum is not improved by moving the number of concurrent controls", {
  res <- optimal_nc(120, n_e = 90, bias = 0.07, tau = 0.04, sigma_t = 1.3, sigma_c = 0.9)
  around <- mse_hybrid(120, n_c = res[["n_c"]] + c(-1, -0.1, 0.1, 1), n_e = 90, bias = 0.07,
                       tau = 0.04, sigma_t = 1.3, sigma_c = 0.9)[["mse"]]
  expect_true(all(res[["mse"]] <= around))
})

test_that("optimal_nc rejects invalid arguments", {
  expect_error(optimal_nc(0, n_e = 100), "'n'")
  expect_error(optimal_nc(50, n_e = 0), "'n_e'")
  expect_error(optimal_nc(50, n_e = 100, tau = -1), "'tau'")
})

test_that("optimal_nc gives a single-arm trial against an exact threshold", {
  res <- optimal_nc(60, n_e = Inf)
  expect_identical(res[["n_c"]], 0)
  expect_identical(res[["n_eff"]], Inf)
  expect_equal(res[["mse"]], 1 / 60, tolerance = 1e-12)
})

test_that("optimal_nc approaches Neyman allocation when the external control is uninformative", {
  res <- optimal_nc(90, n_e = 10, bias = 100, sigma_t = 2, sigma_c = 1)
  expect_equal(res[["n_c"]], 90 * 1 / 3, tolerance = 1e-3)
})
