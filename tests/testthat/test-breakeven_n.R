# Expected values: inst/validation/reference/test_values.txt (independent Python implementation)

test_that("breakeven_n matches the independent Python values", {
  expect_equal(breakeven_n(0.2, n_e = 100), 60, tolerance = 1e-12)
  expect_equal(breakeven_n(0.3, n_e = Inf, tau = 0.1, sigma_t = 1.2, sigma_c = 0.8, r = 2 / 3),
               26.4, tolerance = 1e-12)
})

test_that("breakeven_n reproduces the example of Collignon et al. (2020)", {
  # 10 historical studies of 100 patients, between-study variance 0.02, sigma^2 = 1:
  # a randomized trial with 130 patients achieves the same variance (p.1964)
  n_star <- breakeven_n(0, n_e = 10 * 100, tau = sqrt((1 + 1 / 10) * 0.02))
  expect_equal(n_star, 130.43478260869566, tolerance = 1e-12)
  expect_equal(round(n_star), 130)
})

test_that("breakeven_n is the inverse of breakeven_bias", {
  for (b in c(0.05, 0.1, 0.3)) {
    n_star <- breakeven_n(b, n_e = 150, sigma_t = 1.3)
    expect_equal(breakeven_bias(n_star, n_e = 150, sigma_t = 1.3), b, tolerance = 1e-10)
    expect_equal(mse_sat(n_star, 150, bias = b, sigma_t = 1.3), mse_rct(n_star, sigma_t = 1.3),
                 tolerance = 1e-12)
  }
})

test_that("breakeven_n is infinite without bias and without sampling error in the control", {
  expect_identical(breakeven_n(0, n_e = Inf), Inf)
})

test_that("breakeven_n rejects invalid arguments", {
  expect_error(breakeven_n("a"), "'bias'")
  expect_error(breakeven_n(0.1, tau = -1), "'tau'")
  expect_error(breakeven_n(0.1, r = 1.2), "'r'")
})
