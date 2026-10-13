# Expected values: inst/validation/reference/test_values.txt (independent Python implementation;
# the Python score test uses restricted maximum likelihood estimates found numerically)

test_that("oc_binary_exact matches the independent Python values", {
  expect_equal(oc_binary_exact(20, 20, p_t = 0.5, p_c = 0.3), 0.24462065791898732,
               tolerance = 1e-10)
  expect_equal(oc_binary_exact(20, 20, p_t = 0.5, p_c = 0.3, test = "pearson"),
               0.24462065791898732, tolerance = 1e-10)
  expect_equal(oc_binary_exact(20, 20, p_t = 0.5, p_c = 0.3, test = "pearson_cc"),
               0.1417904625331633, tolerance = 1e-10)
  expect_equal(oc_binary_exact(30, 60, p_t = c(0.4, 0.6), p_c = 0.3, margin = 0.1),
               c(0.026834964003022058, 0.4459430353874133), tolerance = 1e-10)
  expect_equal(oc_binary_exact(25, 40, p_t = 0.35, p_c = 0.4, margin = -0.05, alpha = 0.05),
               0.05471294328761529, tolerance = 1e-10)
})

test_that("oc_binary_exact reproduces the separate and pooled analyses of Viele et al. (2014)", {
  # Separate analysis, 200 per group, true rates 0.65 and 0.77: power around 70% (p.45)
  expect_equal(oc_binary_exact(200, 200, p_t = 0.77, p_c = 0.65, test = "pearson_cc"),
               0.7206241999920604, tolerance = 1e-10)
  # Pooling the 65/100 historical controls, true control rate 0.80: type I error
  # approaching 20% (p.45)
  expect_equal(oc_binary_exact(200, 200, p_t = 0.8, p_c = 0.8, test = "pearson_cc",
                               n_ext = 100, y_ext = 65),
               0.19617123786270887, tolerance = 1e-10)
})

test_that("the Pearson statistics reproduce stats::prop.test", {
  tables <- expand.grid(y_t = 0:8, y_c = 0:7)
  y_t <- tables[["y_t"]]
  y_c <- tables[["y_c"]]
  for (correct in c(FALSE, TRUE)) {
    z <- binary_z(y_t, 8, y_c, 7, 0, if (correct) "pearson_cc" else "pearson")
    p_ref <- mapply(function(a, b) {
      suppressWarnings(stats::prop.test(c(a, b), c(8, 7), alternative = "greater",
                                        correct = correct)[["p.value"]])
    }, y_t, y_c)
    expect_identical(is.na(z), is.na(p_ref))
    ok <- !is.na(z)
    expect_equal(stats::pnorm(z[ok], lower.tail = FALSE), p_ref[ok], tolerance = 1e-12)
  }
})

test_that("the score test equals the Pearson test without a margin", {
  tables <- expand.grid(y_t = 0:12, y_c = 0:9)
  expect_equal(binary_z(tables[["y_t"]], 12, tables[["y_c"]], 9, 0, "score"),
               binary_z(tables[["y_t"]], 12, tables[["y_c"]], 9, 0, "pearson"),
               tolerance = 1e-10)
})

test_that("the score test uses the restricted maximum likelihood estimates", {
  # Independent check: maximise the likelihood under p1 - p2 = margin numerically
  z_numeric <- function(y1, n1, y2, n2, margin) {
    loglik <- function(p1) {
      stats::dbinom(y1, n1, p1, log = TRUE) + stats::dbinom(y2, n2, p1 - margin, log = TRUE)
    }
    lower <- max(0, margin) + 1e-12
    upper <- min(1, 1 + margin) - 1e-12
    p1 <- stats::optimize(loglik, c(lower, upper), maximum = TRUE, tol = 1e-12)[["maximum"]]
    p2 <- p1 - margin
    (y1 / n1 - y2 / n2 - margin) / sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  }
  for (margin in c(-0.2, 0.1)) {
    tables <- expand.grid(y1 = 0:6, y2 = 0:9)
    z_ref <- mapply(z_numeric, tables[["y1"]], 6, tables[["y2"]], 9, margin)
    z <- binary_z(tables[["y1"]], 6, tables[["y2"]], 9, margin, "score")
    expect_equal(z, z_ref, tolerance = 1e-5)
  }
})

test_that("rejection probabilities increase with the treatment response probability", {
  prob <- oc_binary_exact(15, 30, p_t = seq(0.2, 0.8, by = 0.1), p_c = 0.3, margin = 0.05)
  expect_true(all(diff(prob) > 0))
  expect_true(all(prob >= 0 & prob <= 1))
})

test_that("oc_binary_exact rejects invalid arguments", {
  expect_error(oc_binary_exact(20, 20, 0.5, 0.3, margin = 0.1, test = "pearson"),
               "require margin = 0")
  expect_error(oc_binary_exact(20, 20, 0.5, 0.3, n_ext = 10, y_ext = 11), "'y_ext'")
  expect_error(oc_binary_exact(20, 20, 0.5, 0.3, margin = 1), "'margin'")
  expect_error(oc_binary_exact(0, 20, 0.5, 0.3), "'n_t'")
  expect_error(oc_binary_exact(20, 20, c(0.5, 0.6, 0.7), c(0.3, 0.4)), "common length")
})
