# Expected values: inst/validation/reference/test_values.txt (independent Python implementation)

test_that("breakeven_bias matches the independent Python values", {
  expect_equal(breakeven_bias(40, n_e = 100), 0.25495097567963926, tolerance = 1e-12)
  expect_equal(breakeven_bias(40, n_e = Inf), 0.2738612787525831, tolerance = 1e-12)
  expect_equal(breakeven_bias(30, n_e = 60, sigma_t = 1.2, sigma_c = 0.8, r = 2 / 3),
               0.2780887148615228, tolerance = 1e-12)
  expect_equal(breakeven_bias(20, n_e = 200, sigma_t = 2), 0.7615773105863909,
               tolerance = 1e-12)
})

test_that("breakeven_bias returns NA when the single-arm trial cannot win", {
  expect_true(is.na(breakeven_bias(40, n_e = 10)))
  expect_identical(is.na(breakeven_bias(40, n_e = c(10, 100))), c(TRUE, FALSE))
})

test_that("the two designs have equal mean squared error at the break-even bias", {
  settings <- list(
    list(n = 40, n_e = 100, sigma_t = 1, sigma_c = 1, r = 0.5),
    list(n = 25, n_e = Inf, sigma_t = 2, sigma_c = 2, r = 0.5),
    list(n = 30, n_e = 60, sigma_t = 1.2, sigma_c = 0.8, r = 2 / 3)
  )
  for (s in settings) {
    b <- breakeven_bias(s[["n"]], s[["n_e"]], s[["sigma_t"]], s[["sigma_c"]], s[["r"]])
    expect_equal(mse_sat(s[["n"]], s[["n_e"]], bias = b, sigma_t = s[["sigma_t"]],
                         sigma_c = s[["sigma_c"]]),
                 mse_rct(s[["n"]], s[["sigma_t"]], s[["sigma_c"]], s[["r"]]),
                 tolerance = 1e-12)
    # A random bias with the same standard deviation also gives equality
    expect_equal(mse_sat(s[["n"]], s[["n_e"]], tau = b, sigma_t = s[["sigma_t"]],
                         sigma_c = s[["sigma_c"]]),
                 mse_rct(s[["n"]], s[["sigma_t"]], s[["sigma_c"]], s[["r"]]),
                 tolerance = 1e-12)
  }
})

test_that("breakeven_bias agrees with the closed form and shrinks with n", {
  n <- c(10, 20, 40, 80, 160)
  expect_equal(breakeven_bias(n, n_e = 500, sigma_t = 1.5), 1.5 * sqrt(3 / n - 1 / 500),
               tolerance = 1e-12)
  expect_true(all(diff(breakeven_bias(n, n_e = 500)) < 0))
})

test_that("breakeven_bias rejects invalid arguments", {
  expect_error(breakeven_bias(0), "'n'")
  expect_error(breakeven_bias(10, n_e = -1), "'n_e'")
  expect_error(breakeven_bias(10, r = 0), "'r'")
})
