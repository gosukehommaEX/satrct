# Expected values: inst/validation/reference/test_values.txt (independent Python implementation;
# the Python weight is found by numerical minimisation, hence the looser tolerance)

test_that("mse_hybrid matches the independent Python values", {
  res <- mse_hybrid(100, n_c = 20, n_e = 200, bias = 0.1)
  expect_equal(res[["weight"]], 0.23076923761495927, tolerance = 1e-7)
  expect_equal(res[["mse"]], 0.024038461538461543, tolerance = 1e-12)
  res <- mse_hybrid(100, n_c = 20, n_e = 200, bias = 0.1, weight = 0.5)
  expect_equal(res[["weight"]], 0.5)
  expect_equal(res[["mse"]], 0.02875, tolerance = 1e-12)
  res <- mse_hybrid(60, n_c = 15, n_e = 100, bias = 0.05, tau = 0.1, sigma_t = 1.2,
                    sigma_c = 0.8)
  expect_equal(res[["weight"]], 0.3069842988630211, tolerance = 1e-6)
  expect_equal(res[["mse"]], 0.0450979967514889, tolerance = 1e-10)
})

test_that("mse_hybrid reduces to the single-arm trial and to the RCT", {
  expect_equal(mse_hybrid(40, n_c = 0, n_e = 100, bias = 0.1, tau = 0.05)[["mse"]],
               mse_sat(40, 100, bias = 0.1, tau = 0.05), tolerance = 1e-12)
  expect_identical(mse_hybrid(40, n_c = 0, n_e = 100, bias = 0.1)[["weight"]], 0)
  expect_equal(mse_hybrid(40, n_c = 20, n_e = 100, bias = 0.1, weight = 1)[["mse"]],
               mse_rct(40), tolerance = 1e-12)
})

test_that("the default weight minimises the mean squared error", {
  w_grid <- seq(0, 1, by = 0.001)
  mse_grid <- mse_hybrid(80, n_c = 25, n_e = 150, bias = 0.08, tau = 0.03, sigma_t = 1.1,
                         weight = w_grid)[["mse"]]
  best <- mse_hybrid(80, n_c = 25, n_e = 150, bias = 0.08, tau = 0.03, sigma_t = 1.1)
  expect_true(best[["mse"]] <= min(mse_grid) + 1e-15)
  expect_equal(best[["weight"]], w_grid[which.min(mse_grid)], tolerance = 1e-2)
})

test_that("mse_hybrid rejects invalid arguments", {
  expect_error(mse_hybrid(40, n_c = 40, n_e = 100), "smaller than 'n'")
  expect_error(mse_hybrid(40, n_c = -1, n_e = 100), "'n_c'")
  expect_error(mse_hybrid(40, n_c = 0, n_e = 100, weight = 0.5), "requires 'n_c' > 0")
  expect_error(mse_hybrid(40, n_c = 10, n_e = 100, weight = 1.5), "'weight'")
})
