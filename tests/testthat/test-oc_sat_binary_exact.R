# Expected values: inst/validation/reference/test_values.txt (independent Python implementation)

test_that("oc_sat_binary_exact reproduces the single-arm trial of Viele et al. (2014)", {
  # 200 patients against a threshold of 0.65: success with at least 144 responders (p.43)
  res <- oc_sat_binary_exact(200, p_t = 0.70, p0 = 0.65)
  expect_identical(res[["crit"]], 144)
  expect_equal(res[["size"]], 0.021306155488520693, tolerance = 1e-12)
  expect_equal(res[["prob_reject"]], 0.2971665229223571, tolerance = 1e-12)
  # The same trial tested against 0.70 controls the type I error up to 0.70 (p.45)
  res <- oc_sat_binary_exact(200, p_t = c(0.70, 0.75), p0 = 0.65, margin = 0.05)
  expect_identical(res[["crit"]], c(153, 153))
  expect_equal(res[["size"]][1], 0.0249327262625957, tolerance = 1e-12)
  expect_equal(res[["prob_reject"]], c(0.024932726262595357, 0.34580365552313935),
               tolerance = 1e-12)
})

test_that("oc_sat_binary_exact reproduces the rejection region of Kopp-Schneider et al. (2020)", {
  # 40 patients, p0 = 0.2, alpha = 0.05: reject for at least 13 responders, size 0.043 (p.365)
  res <- oc_sat_binary_exact(40, p_t = 0.3, p0 = 0.2, alpha = 0.05)
  expect_identical(res[["crit"]], 13)
  expect_equal(res[["size"]], 0.0432416223763237, tolerance = 1e-12)
  expect_equal(round(res[["size"]], 3), 0.043)
  expect_equal(res[["prob_reject"]], 0.4228190754965615, tolerance = 1e-12)
})

test_that("oc_sat_binary_exact reproduces Table 1 of Tang et al. (2010) for given rules", {
  # Rule: at least 19 of 50 responders, 0.30 versus 0.45: alpha 0.14, beta 0.13
  res <- oc_sat_binary_exact(50, p_t = 0.45, p0 = 0.30, crit = 19)
  expect_equal(res[["size"]], 0.14055987638889383, tolerance = 1e-12)
  expect_equal(res[["prob_reject"]], 0.8726548853375338, tolerance = 1e-12)
  expect_equal(round(c(res[["size"]], 1 - res[["prob_reject"]]), 2), c(0.14, 0.13))
  # Rule: at least 46 of 50 responders, 0.85 versus 0.95: alpha 0.11, beta 0.10
  res <- oc_sat_binary_exact(50, p_t = 0.95, p0 = 0.85, crit = 46)
  expect_equal(res[["size"]], 0.11210520805497712, tolerance = 1e-12)
  expect_equal(res[["prob_reject"]], 0.8963831898558561, tolerance = 1e-12)
  expect_equal(round(c(res[["size"]], 1 - res[["prob_reject"]]), 2), c(0.11, 0.10))
})

test_that("the decisions agree with stats::binom.test", {
  for (margin in c(0, 0.1)) {
    crit <- oc_sat_binary_exact(30, p_t = 0.5, p0 = 0.3, margin = margin, alpha = 0.05)[["crit"]]
    p_values <- vapply(0:30, function(y) {
      stats::binom.test(y, 30, p = 0.3 + margin, alternative = "greater")[["p.value"]]
    }, numeric(1))
    expect_identical(p_values <= 0.05, (0:30) >= crit)
  }
})

test_that("oc_sat_binary_exact rejects invalid arguments", {
  expect_error(oc_sat_binary_exact(20.5, 0.5, 0.3), "whole number")
  expect_error(oc_sat_binary_exact(c(20, 30), 0.5, 0.3), "single number")
  expect_error(oc_sat_binary_exact(20, 1.2, 0.3), "'p_t'")
  expect_error(oc_sat_binary_exact(20, 0.5, 0.3, margin = 0.7), "p0 \\+ margin")
  expect_error(oc_sat_binary_exact(20, 0.5, 0.3, crit = 22), "'crit'")
})
