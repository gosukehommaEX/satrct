# Expected values: inst/validation/reference/test_values.txt (independent Python implementation)

test_that("n_sat matches the independent Python values", {
  expect_equal(n_sat(0.5), 31.39551893739636, tolerance = 1e-12)
  expect_equal(n_sat(0.5, n_e = 100), 45.76307327322687, tolerance = 1e-12)
  expect_equal(n_sat(0.5, n_e = 100, bias = 0.05, margin = 0.1), 63.291699186961466,
               tolerance = 1e-12)
  expect_equal(n_sat(0.5, method = "t"), 33.36720397881092, tolerance = 1e-8)
  expect_equal(n_sat(0.5, n_e = 100, method = "t"), 46.6654407108554, tolerance = 1e-8)
})

test_that("power_sat reaches the target power at n_sat", {
  expect_equal(power_sat(n_sat(0.5, n_e = 100, margin = 0.1), 0.5, n_e = 100, margin = 0.1),
               0.8, tolerance = 1e-12)
  expect_equal(power_sat(n_sat(0.5, power = 0.9, n_e = 200, method = "t"), 0.5, n_e = 200,
                         method = "t"),
               0.9, tolerance = 1e-8)
})

test_that("n_sat (t) against a threshold agrees with stats::power.t.test", {
  ref <- stats::power.t.test(power = 0.8, delta = 0.5, sd = 1, sig.level = 0.025,
                             type = "one.sample", alternative = "one.sided",
                             tol = 1e-12)[["n"]]
  expect_equal(n_sat(0.5, method = "t"), ref, tolerance = 1e-8)
})

test_that("n_sat is infinite when the target power cannot be reached", {
  # A small external control group caps the power
  expect_identical(n_sat(0.5, n_e = 20), Inf)
  expect_identical(n_sat(0.5, n_e = 20, method = "t"), Inf)
  # A margin at least as large as the effect
  expect_identical(n_sat(0.5, margin = 0.5), Inf)
  expect_identical(n_sat(0.5, margin = 0.6, method = "t"), Inf)
})

test_that("n_sat rejects invalid arguments", {
  expect_error(n_sat("a"), "'delta'")
  expect_error(n_sat(0.5, power = 0), "'power'")
  expect_error(n_sat(0.5, power = 0.01), "must exceed 'alpha'")
  expect_error(n_sat(0.5, n_e = -1), "'n_e'")
  expect_error(n_sat(0.5, sigma_t = 1, sigma_c = 2, method = "t"), "sigma_t == sigma_c")
})
