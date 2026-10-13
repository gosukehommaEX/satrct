# Expected values: inst/validation/reference/test_values.txt (independent Python implementation)

test_that("critical_margin matches the independent Python values", {
  expect_equal(critical_margin(40, 0.8, n_e = 100), 0.32671361735203014, tolerance = 1e-12)
  expect_equal(critical_margin(40, 0.8, n_e = Inf), 0.4, tolerance = 1e-12)
  expect_equal(critical_margin(30, 0.6, n_e = 50, sigma_t = 1.2, sigma_c = 0.8, r = 2 / 3),
               0.19882525444566154, tolerance = 1e-12)
  expect_equal(critical_margin(40, 0.8, n_e = 10), -0.09442719099991592, tolerance = 1e-12)
})

test_that("the two designs have equal power at the critical margin for any alpha", {
  for (alpha in c(0.025, 0.05, 0.1)) {
    m_c <- critical_margin(40, 0.8, n_e = 100)
    expect_equal(power_sat(40, 0.8, n_e = 100, margin = m_c, alpha = alpha),
                 power_rct(40, 0.8, alpha = alpha), tolerance = 1e-12)
    m_c <- critical_margin(30, 0.6, n_e = 50, sigma_t = 1.2, sigma_c = 0.8, r = 2 / 3)
    expect_equal(power_sat(30, 0.6, n_e = 50, margin = m_c, sigma_t = 1.2, sigma_c = 0.8,
                           alpha = alpha),
                 power_rct(30, 0.6, sigma_t = 1.2, sigma_c = 0.8, alpha = alpha, r = 2 / 3),
                 tolerance = 1e-12)
  }
})

test_that("critical_margin follows the closed form and depends on n only through n / n_e", {
  n <- c(20, 40, 80)
  n_e <- c(30, 120, 400)
  expect_equal(critical_margin(n, 0.7, n_e), 0.7 * (1 - sqrt(1 + n / n_e) / 2),
               tolerance = 1e-12)
  expect_equal(critical_margin(40, 0.7, 100), critical_margin(80, 0.7, 200), tolerance = 1e-12)
})

test_that("critical_margin rejects invalid arguments", {
  expect_error(critical_margin(0, 0.5), "'n'")
  expect_error(critical_margin(10, NA_real_), "'delta'")
})

test_that("critical_margin changes sign with the effect and is zero at zero effect", {
  expect_equal(critical_margin(40, -0.8, n_e = 100), -critical_margin(40, 0.8, n_e = 100),
               tolerance = 1e-12)
  expect_identical(critical_margin(40, 0, n_e = 100), 0)
})
