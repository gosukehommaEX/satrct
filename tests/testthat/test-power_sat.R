# Expected values: inst/validation/reference/test_values.txt (independent Python implementation)

test_that("power_sat (z) matches the independent Python values", {
  expect_equal(power_sat(40, 0.5, n_e = 120, bias = 0.1, margin = 0.1), 0.7819066888284272,
               tolerance = 1e-12)
  expect_equal(power_sat(40, 0, n_e = 120, bias = 0.15, margin = 0.1), 0.04588800946993527,
               tolerance = 1e-12)
  expect_equal(power_sat(40, 0.5, n_e = 120, margin = 0.1, tau = 0.1), 0.5802510299921566,
               tolerance = 1e-12)
  expect_equal(power_sat(40, 0, n_e = 120, bias = 0.05, margin = 0.1, tau = 0.1),
               0.025045014645129014, tolerance = 1e-12)
  expect_equal(power_sat(40, 0.5, n_e = 120, bias = 0.05, margin = 0.1, tau = 0.1),
               0.6710187369426132, tolerance = 1e-12)
  expect_equal(power_sat(30, 0.8, n_e = Inf, sigma_t = 1.2, sigma_c = 0.8, alpha = 0.05),
               0.977605467524708, tolerance = 1e-12)
})

test_that("power_sat reproduces the one-arm power of Kopp-Schneider et al. (2024)", {
  # n = 25, effect 0.5, unit variance, alpha = 0.025: power 0.705 (p.7)
  expect_equal(power_sat(25, 0.5), 0.7054139024424572, tolerance = 1e-12)
  expect_equal(round(power_sat(25, 0.5), 3), 0.705)
  # The same test at the relaxed level 0.017 (Section 4.1)
  expect_equal(power_sat(25, 0.5, alpha = 0.017), 0.6480006841358448, tolerance = 1e-12)
})

test_that("power_sat (t) matches the independent Python values", {
  expect_equal(power_sat(40, 0.5, n_e = 120, bias = 0.1, margin = 0.1, method = "t"),
               0.7769602528908459, tolerance = 1e-8)
  expect_equal(power_sat(25, 0.5, method = "t"), 0.6697013638123328, tolerance = 1e-8)
})

test_that("power_sat (t) against a threshold agrees with stats::power.t.test", {
  for (n in c(8, 25, 70)) {
    ref <- stats::power.t.test(n = n, delta = 0.4, sd = 1.1, sig.level = 0.025,
                               type = "one.sample", alternative = "one.sided")[["power"]]
    expect_equal(power_sat(n, 0.4, n_e = Inf, sigma_t = 1.1, method = "t"), ref,
                 tolerance = 1e-10)
  }
})

test_that("the margin controls the type I error up to the margin and not beyond", {
  m <- 0.15
  expect_equal(power_sat(40, 0, n_e = 80, bias = m, margin = m), 0.025, tolerance = 1e-12)
  expect_equal(power_sat(40, 0, n_e = 80, bias = m, margin = m, method = "t"), 0.025,
               tolerance = 1e-8)
  expect_true(all(power_sat(40, 0, n_e = 80, bias = c(-0.1, 0, 0.1), margin = m) < 0.025))
  expect_true(all(power_sat(40, 0, n_e = 80, bias = c(0.2, 0.3), margin = m) > 0.025))
})

test_that("power_sat (t) approaches the z-test for large samples", {
  expect_equal(power_sat(20000, 0.03, n_e = 40000, method = "t"),
               power_sat(20000, 0.03, n_e = 40000), tolerance = 1e-3)
})

test_that("power_sat rejects invalid arguments", {
  expect_error(power_sat(40, 0.5, n_e = 0), "'n_e'")
  expect_error(power_sat(40, 0.5, tau = -1), "'tau'")
  expect_error(power_sat(40, 0.5, margin = NA_real_), "'margin'")
  expect_error(power_sat(40, 0.5, tau = 0.1, method = "t"), "tau = 0")
  expect_error(power_sat(40, 0.5, sigma_t = 1, sigma_c = 2, method = "t"), "sigma_t == sigma_c")
  expect_error(power_sat(1, 0.5, method = "t"), "degrees of freedom")
})
