test_that("recycle_args recycles length-one arguments to the common length", {
  res <- recycle_args(a = 1, b = c(2, 3, 4), c = c(5, 6, 7))
  expect_identical(res, list(a = c(1, 1, 1), b = c(2, 3, 4), c = c(5, 6, 7)))
  expect_identical(recycle_args(a = 1, b = 2), list(a = 1, b = 2))
})

test_that("recycle_args rejects incompatible lengths", {
  expect_error(recycle_args(a = c(1, 2), b = c(1, 2, 3)), "common length")
})

test_that("recycle_args keeps the names and handles a single argument", {
  expect_identical(recycle_args(x = c(1, 2)), list(x = c(1, 2)))
  expect_identical(names(recycle_args(n = 1, n_e = c(1, 2))), c("n", "n_e"))
})
