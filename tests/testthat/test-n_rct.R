# Expected values: inst/validation/reference/test_values.txt (independent Python implementation)

test_that("n_rct matches the independent Python values", {
  expect_equal(n_rct(0.5), 125.58207574958541, tolerance = 1e-12)
  expect_equal(n_rct(0.8, power = 0.9, sigma_t = 1.2, sigma_c = 0.8, alpha = 0.05, r = 2 / 3),
               54.59452686050835, tolerance = 1e-12)
  expect_equal(n_rct(0.5, method = "t"), 127.53152744970129, tolerance = 1e-8)
})

test_that("power_rct reaches the target power at n_rct", {
  expect_equal(power_rct(n_rct(0.5), 0.5), 0.8, tolerance = 1e-12)
  expect_equal(power_rct(n_rct(0.6, power = 0.9, method = "t"), 0.6, method = "t"), 0.9,
               tolerance = 1e-8)
})

test_that("n_rct (t) agrees with stats::power.t.test", {
  ref <- stats::power.t.test(power = 0.8, delta = 0.5, sd = 1, sig.level = 0.025,
                             type = "two.sample", alternative = "one.sided",
                             tol = 1e-12)[["n"]]
  expect_equal(n_rct(0.5, method = "t"), 2 * ref, tolerance = 1e-8)
})

test_that("n_rct rejects invalid arguments", {
  expect_error(n_rct(0), "'delta'")
  expect_error(n_rct(-0.5), "'delta'")
  expect_error(n_rct(0.5, power = 1), "'power'")
  expect_error(n_rct(0.5, power = 0.02), "must exceed 'alpha'")
  expect_error(n_rct(0.5, sigma_t = 1, sigma_c = 2, method = "t"), "sigma_t == sigma_c")
})

test_that("n_rct handles vector arguments with both methods", {
  expect_equal(n_rct(c(0.5, 0.8), method = "t"), c(127.53152744970129, 51.049262459814166),
               tolerance = 1e-8)
  expect_equal(n_rct(0.5, power = c(0.8, 0.9)),
               c(n_rct(0.5, power = 0.8), n_rct(0.5, power = 0.9)), tolerance = 1e-12)
  # A larger effect, a higher level or a lower power needs fewer patients
  expect_true(n_rct(0.6) < n_rct(0.5))
  expect_true(n_rct(0.5, alpha = 0.1) < n_rct(0.5))
  expect_true(n_rct(0.5, power = 0.7) < n_rct(0.5))
})
