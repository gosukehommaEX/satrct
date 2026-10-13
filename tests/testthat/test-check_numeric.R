test_that("check_numeric accepts admissible values and returns them invisibly", {
  expect_invisible(check_numeric(1, "x"))
  expect_identical(check_numeric(c(0.1, 0.5), "x", lower = 0, upper = 1), c(0.1, 0.5))
  expect_silent(check_numeric(Inf, "x", lower = 0, lower_open = TRUE, allow_inf = TRUE))
  expect_silent(check_numeric(3, "x", scalar = TRUE, integer = TRUE))
})

test_that("check_numeric rejects inadmissible values", {
  expect_error(check_numeric("a", "x"), "'x' must be a non-empty numeric")
  expect_error(check_numeric(numeric(0), "x"), "'x' must be a non-empty numeric")
  expect_error(check_numeric(c(1, NA), "x"), "'x' must be a non-empty numeric")
  expect_error(check_numeric(Inf, "x"), "'x' must be finite")
  expect_error(check_numeric(0, "x", lower = 0, lower_open = TRUE), "interval \\(0, Inf\\]")
  expect_error(check_numeric(1, "x", upper = 1, upper_open = TRUE), "'x' must lie")
  expect_error(check_numeric(c(1, 2), "x", scalar = TRUE), "'x' must be a single number")
  expect_error(check_numeric(2.5, "x", integer = TRUE), "'x' must be a whole number")
})

test_that("check_numeric reports open and closed bounds and skips infinite values for integers", {
  expect_error(check_numeric(1.5, "x", lower = 0, upper = 1), "interval \\[0, 1\\]")
  expect_error(check_numeric(1, "x", lower = 0, upper = 1, upper_open = TRUE),
               "interval \\[0, 1\\)")
  expect_silent(check_numeric(c(2, Inf), "x", allow_inf = TRUE, integer = TRUE))
})
