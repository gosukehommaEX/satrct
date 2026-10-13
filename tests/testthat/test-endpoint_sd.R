# Expected values: inst/validation/reference/test_values.txt (independent Python implementation)

test_that("endpoint_sd matches the independent Python values", {
  expect_identical(endpoint_sd("continuous", sigma = 1.3), 1.3)
  expect_equal(endpoint_sd("binary", p = 0.3), 0.458257569495584, tolerance = 1e-12)
  expect_equal(endpoint_sd("survival", prob_event = 0.64), 1.25, tolerance = 1e-12)
  expect_equal(endpoint_sd("binary", p = c(0.5, 0.1)), c(0.5, 0.3), tolerance = 1e-12)
})

test_that("the survival standard deviation gives the Schoenfeld variance for 1:1 allocation", {
  # 4 / (number of events) with n * prob_event events in total
  s <- endpoint_sd("survival", prob_event = 0.7)
  expect_equal(mse_rct(120, sigma_t = s), 4 / (120 * 0.7), tolerance = 1e-12)
})

test_that("endpoint_sd requires the parameter of the chosen type", {
  expect_error(endpoint_sd("continuous"), "'sigma' is required")
  expect_error(endpoint_sd("binary"), "'p' is required")
  expect_error(endpoint_sd("survival"), "'prob_event' is required")
  expect_error(endpoint_sd("binary", p = 1), "'p'")
  expect_error(endpoint_sd("survival", prob_event = 0), "'prob_event'")
  expect_error(endpoint_sd("count", sigma = 1))
})
