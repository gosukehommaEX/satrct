# Expected values: inst/validation/reference/test_values.txt (independent Python implementation)

test_that("compare_designs returns one row per combination with the documented columns", {
  res <- compare_designs(n = c(20, 40), delta = c(0.4, 0.6), n_e = c(50, Inf),
                         bias = c(0, 0.1), margin = 0.1)
  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 16L)
  expect_identical(names(res),
                   c("n", "delta", "n_e", "bias", "margin", "tau", "mse_rct", "mse_sat",
                     "breakeven_bias", "power_rct", "power_sat", "type1_sat",
                     "critical_margin", "alpha_matched"))
})

test_that("compare_designs agrees with the individual functions", {
  res <- compare_designs(n = c(20, 40), delta = 0.6, n_e = c(50, Inf), bias = c(0, 0.1),
                         margin = c(0, 0.1), tau = c(0, 0.05), sigma_t = 1.2, sigma_c = 0.9,
                         alpha = 0.05, r = 0.6)
  n <- res[["n"]]
  n_e <- res[["n_e"]]
  bias <- res[["bias"]]
  margin <- res[["margin"]]
  tau <- res[["tau"]]
  delta <- res[["delta"]]
  expect_equal(res[["mse_rct"]], mse_rct(n, 1.2, 0.9, 0.6), tolerance = 1e-12)
  expect_equal(res[["mse_sat"]], mse_sat(n, n_e, bias, tau, 1.2, 0.9), tolerance = 1e-12)
  expect_equal(res[["breakeven_bias"]], breakeven_bias(n, n_e, 1.2, 0.9, 0.6), tolerance = 1e-12)
  expect_equal(res[["power_rct"]], power_rct(n, delta, 1.2, 0.9, 0.05, 0.6), tolerance = 1e-12)
  expect_equal(res[["power_sat"]], power_sat(n, delta, n_e, bias, margin, tau, 1.2, 0.9, 0.05),
               tolerance = 1e-12)
  expect_equal(res[["type1_sat"]], power_sat(n, 0, n_e, bias, margin, tau, 1.2, 0.9, 0.05),
               tolerance = 1e-12)
  expect_equal(res[["critical_margin"]], critical_margin(n, delta, n_e, 1.2, 0.9, 0.6),
               tolerance = 1e-12)
  expect_equal(res[["alpha_matched"]],
               matched_alpha(n, delta, n_e, margin, 1.2, 0.9, 0.05, 0.6)[["alpha_matched"]],
               tolerance = 1e-12)
})

test_that("compare_designs matches the independent Python values", {
  res <- compare_designs(n = 40, delta = 0.5, n_e = 120, bias = 0.05, margin = 0.1, tau = 0.1)
  expect_equal(res[["power_sat"]], 0.6710187369426132, tolerance = 1e-12)
  expect_equal(res[["type1_sat"]], 0.025045014645129014, tolerance = 1e-12)
})

test_that("compare_designs requires single values for the standard deviations and levels", {
  expect_error(compare_designs(40, 0.5, sigma_t = c(1, 2)), "'sigma_t'")
  expect_error(compare_designs(40, 0.5, alpha = c(0.025, 0.05)), "'alpha'")
  expect_error(compare_designs(40, 0.5, r = c(0.5, 0.6)), "'r'")
})

test_that("compare_designs gives the nominal type I error when the bias equals the margin", {
  res <- compare_designs(n = c(20, 50), delta = 0.5, n_e = c(40, Inf), bias = 0.1, margin = 0.1,
                         alpha = 0.05)
  expect_equal(res[["type1_sat"]], rep(0.05, 4), tolerance = 1e-12)
})
