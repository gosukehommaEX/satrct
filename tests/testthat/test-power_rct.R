# Expected values: inst/validation/reference/test_values.txt (independent Python implementation)

test_that("power_rct (z) matches the independent Python values", {
  expect_equal(power_rct(40, 0.5), 0.35240885330636085, tolerance = 1e-12)
  expect_equal(power_rct(30, 0.8, sigma_t = 1.2, sigma_c = 0.8, alpha = 0.05, r = 2 / 3),
               0.7000175368713925, tolerance = 1e-12)
  # Kopp-Schneider et al. (2024), two-arm trial with 15 patients per group: 0.78 (p.11)
  expect_equal(power_rct(30, 1), 0.7819066888284272, tolerance = 1e-12)
})

test_that("power_rct (t) matches the independent Python values", {
  expect_equal(power_rct(40, 0.5, method = "t"), 0.33770836556345946, tolerance = 1e-8)
  expect_equal(power_rct(30, 1.5, sigma_t = 2, alpha = 0.05, r = 2 / 3, method = "t"),
               0.5965905962932812, tolerance = 1e-8)
})

test_that("power_rct (t) agrees with stats::power.t.test", {
  for (n_group in c(5, 20, 60)) {
    ref <- stats::power.t.test(n = n_group, delta = 0.5, sd = 1.3, sig.level = 0.025,
                               type = "two.sample", alternative = "one.sided")[["power"]]
    expect_equal(power_rct(2 * n_group, 0.5, sigma_t = 1.3, method = "t"), ref,
                 tolerance = 1e-10)
  }
})

test_that("power_rct gives the significance level at zero effect", {
  expect_equal(power_rct(40, 0, alpha = 0.025), 0.025, tolerance = 1e-12)
  expect_equal(power_rct(40, 0, alpha = 0.1, method = "t"), 0.1, tolerance = 1e-8)
})

test_that("power_rct (t) approaches the z-test for large samples", {
  expect_equal(power_rct(20000, 0.05, method = "t"), power_rct(20000, 0.05), tolerance = 1e-3)
})

test_that("power_rct rejects invalid arguments", {
  expect_error(power_rct(40, 0.5, alpha = 0), "'alpha'")
  expect_error(power_rct(40, 0.5, alpha = 1), "'alpha'")
  expect_error(power_rct(40, "a"), "'delta'")
  expect_error(power_rct(40, 0.5, method = "x"))
  expect_error(power_rct(40, 0.5, sigma_t = 1, sigma_c = 2, method = "t"), "sigma_t == sigma_c")
  expect_error(power_rct(2, 0.5, method = "t"), "n > 2")
})

test_that("power_rct recycles vector arguments", {
  expect_equal(power_rct(40, 0.5, alpha = c(0.025, 0.1)),
               c(0.35240885330636085, 0.6177540001719666), tolerance = 1e-12)
  expect_equal(power_rct(c(20, 40), 0.5, method = "t"),
               c(power_rct(20, 0.5, method = "t"), power_rct(40, 0.5, method = "t")),
               tolerance = 1e-12)
})

test_that("power_rct with endpoint_sd gives the Schoenfeld power for a time-to-event endpoint", {
  # 1:1 allocation, 150 patients, event probability 0.6, hazard ratio 0.7
  s <- endpoint_sd("survival", prob_event = 0.6)
  events <- 150 * 0.6
  expect_equal(power_rct(150, -log(0.7), sigma_t = s),
               stats::pnorm(sqrt(events / 4) * -log(0.7) - stats::qnorm(0.975)),
               tolerance = 1e-12)
})

test_that("power_rct (t) agrees with a patient-level simulation", {
  set.seed(20261013)
  n_sim <- 20000
  n_group <- 15
  y_t <- matrix(stats::rnorm(n_sim * n_group, mean = 0.8, sd = 1.2), n_sim, n_group)
  y_c <- matrix(stats::rnorm(n_sim * n_group, mean = 0, sd = 1.2), n_sim, n_group)
  m_t <- rowMeans(y_t)
  m_c <- rowMeans(y_c)
  s_p <- sqrt((rowSums((y_t - m_t)^2) + rowSums((y_c - m_c)^2)) / (2 * n_group - 2))
  t_stat <- (m_t - m_c) / (s_p * sqrt(2 / n_group))
  sim <- mean(t_stat > stats::qt(0.975, 2 * n_group - 2))
  ana <- power_rct(2 * n_group, 0.8, sigma_t = 1.2, method = "t")
  expect_lt(abs(sim - ana), 4 * sqrt(ana * (1 - ana) / n_sim))
})
