# Expected values: inst/validation/reference/test_values.txt (independent Python implementation)

test_that("matched_alpha matches the independent Python values", {
  res <- matched_alpha(40, 0.8, n_e = 100, margin = 0.1)
  expect_equal(res[["alpha_matched"]], 0.22719125834089204, tolerance = 1e-10)
  expect_equal(res[["power"]], 0.9626003782950244, tolerance = 1e-12)
  expect_equal(res[["bias_crossing"]], 0.3267136173520298, tolerance = 1e-10)
  res <- matched_alpha(60, 0.6, n_e = 200)
  expect_equal(res[["alpha_matched"]], 0.4177875701706491, tolerance = 1e-10)
  expect_equal(res[["power"]], 0.9828375164458883, tolerance = 1e-12)
  expect_equal(res[["bias_crossing"]], 0.2579473724702583, tolerance = 1e-10)
  res <- matched_alpha(30, 0.6, n_e = 50, margin = 0.05, sigma_t = 1.2, sigma_c = 0.8,
                       alpha = 0.05, r = 2 / 3)
  expect_equal(res[["alpha_matched"]], 0.14887106351645157, tolerance = 1e-10)
  expect_equal(res[["power"]], 0.7209583793552741, tolerance = 1e-12)
  expect_equal(res[["bias_crossing"]], 0.19882525444566163, tolerance = 1e-10)
})

test_that("the returned quantities satisfy their defining identities", {
  for (m in c(0, 0.1, 0.3)) {
    res <- matched_alpha(40, 0.8, n_e = 100, margin = m)
    # The RCT at the matched level has the power of the single-arm trial
    expect_equal(power_rct(40, 0.8, alpha = res[["alpha_matched"]]), res[["power"]],
                 tolerance = 1e-12)
    expect_equal(power_sat(40, 0.8, n_e = 100, margin = m), res[["power"]], tolerance = 1e-12)
    # The single-arm type I error reaches the matched level at bias_crossing
    expect_equal(power_sat(40, 0, n_e = 100, bias = res[["bias_crossing"]], margin = m),
                 res[["alpha_matched"]], tolerance = 1e-12)
    # bias_crossing does not depend on the margin
    expect_equal(res[["bias_crossing"]], critical_margin(40, 0.8, n_e = 100), tolerance = 1e-12)
  }
})

test_that("matched_alpha returns a data frame with one row per setting", {
  res <- matched_alpha(c(20, 40, 80), 0.8, n_e = 100)
  expect_s3_class(res, "data.frame")
  expect_identical(names(res), c("alpha_matched", "power", "bias_crossing"))
  expect_identical(nrow(res), 3L)
})

test_that("matched_alpha rejects invalid arguments", {
  expect_error(matched_alpha(40, 0.8, alpha = 2), "'alpha'")
  expect_error(matched_alpha(40, 0.8, margin = "a"), "'margin'")
})
