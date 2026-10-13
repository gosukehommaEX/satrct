# Compare the analytical results with patient-level simulation.
#
# Continuous endpoint: power and type I error of the z-test (known standard deviations) and
# the t-test for the RCT, the single-arm trial against an external control and against a
# threshold, with fixed and random bias and a margin; mean squared errors of the RCT, the
# single-arm trial and the hybrid design. These are exact results, and a difference within four
# Monte Carlo standard errors is a PASS.
# Time-to-event endpoint: power of the Cox model test against the normal approximation with
# endpoint_sd("survival"). This is an approximation, so the difference is reported as INFO.
#
# Run from the package root after devtools::load_all(). It takes about one minute.
# Output: inst/validation/results/validate_simulation.csv

source("inst/validation/judge.R")
check_loaded()
script <- "validate_simulation"
set.seed(20261013)
reps <- 20000
z_crit <- stats::qnorm(0.975)

# Means and pooled standard deviations of two groups of normal outcomes
sim_groups <- function(n1, n2, mu1, mu2, sd1, sd2 = sd1) {
  y1 <- matrix(stats::rnorm(reps * n1, mu1, sd1), reps, n1)
  m1 <- rowMeans(y1)
  ss1 <- rowSums((y1 - m1)^2)
  if (is.infinite(n2)) {
    return(list(m1 = m1, m2 = rep(mu2, reps), sp = sqrt(ss1 / (n1 - 1))))
  }
  y2 <- matrix(stats::rnorm(reps * n2, mu2, sd2), reps, n2)
  m2 <- rowMeans(y2)
  ss2 <- rowSums((y2 - m2)^2)
  list(m1 = m1, m2 = m2, sp = sqrt((ss1 + ss2) / (n1 + n2 - 2)))
}

judge_rate <- function(item, description, sim, ana) {
  se <- sqrt(ana * (1 - ana) / reps)
  judge(script, item, description, sprintf("%d replicates", reps), ana, sim, "abs",
        tol = 4 * se, note = "reference = analytical value; tolerance = 4 Monte Carlo SE")
}
judge_mse <- function(item, description, sq_err, ana) {
  judge(script, item, description, sprintf("%d replicates", reps), ana, mean(sq_err), "abs",
        tol = 4 * stats::sd(sq_err) / sqrt(reps),
        note = "reference = analytical value; tolerance = 4 Monte Carlo SE")
}
reject_z <- function(stat) mean(stat > z_crit)
reject_t <- function(stat, df) mean(stat > stats::qt(0.975, df))

res <- NULL

# ---- 1. RCT, 1:1 and 2:1 allocation ----
for (r in c(0.5, 2 / 3)) {
  n <- if (r == 0.5) 30 else 36
  n1 <- round(r * n)
  n0 <- n - n1
  s <- sim_groups(n1, n0, 0.6, 0, 1.2)
  diff <- s[["m1"]] - s[["m2"]]
  tag <- if (r == 0.5) "1to1" else "2to1"
  res <- rbind(
    res,
    judge_rate(paste0("rct_", tag, "_z"), sprintf("RCT %d:%d, n = %d, z-test", n1, n0, n),
               reject_z(diff / (1.2 * sqrt(1 / n1 + 1 / n0))),
               power_rct(n, 0.6, sigma_t = 1.2, r = r)),
    judge_rate(paste0("rct_", tag, "_t"), sprintf("RCT %d:%d, n = %d, t-test", n1, n0, n),
               reject_t(diff / (s[["sp"]] * sqrt(1 / n1 + 1 / n0)), n - 2),
               power_rct(n, 0.6, sigma_t = 1.2, r = r, method = "t")),
    judge_mse(paste0("rct_", tag, "_mse"), sprintf("RCT %d:%d, mean squared error", n1, n0),
              (diff - 0.6)^2, mse_rct(n, sigma_t = 1.2, r = r))
  )
}

# ---- 2. Single-arm trial with an external control, bias and margin ----
# External controls have mean -bias, so the comparison is biased by +bias
sat_cases <- data.frame(
  item = c("sat_ec_power", "sat_ec_type1_at_margin", "sat_ec_type1_beyond_margin"),
  delta = c(0.4, 0, 0),
  bias = c(0.1, 0.1, 0.2),
  stringsAsFactors = FALSE
)
for (i in seq_len(nrow(sat_cases))) {
  d <- sat_cases[["delta"]][i]
  b <- sat_cases[["bias"]][i]
  s <- sim_groups(30, 60, d, -b, 1)
  est <- s[["m1"]] - s[["m2"]]
  res <- rbind(
    res,
    judge_rate(paste0(sat_cases[["item"]][i], "_z"),
               sprintf("SAT n = 30 vs 60 external controls, delta = %.1f, bias = %.1f, margin = 0.1, z",
                       d, b),
               reject_z((est - 0.1) / sqrt(1 / 30 + 1 / 60)),
               power_sat(30, d, 60, b, 0.1)),
    judge_rate(paste0(sat_cases[["item"]][i], "_t"),
               sprintf("SAT n = 30 vs 60 external controls, delta = %.1f, bias = %.1f, margin = 0.1, t",
                       d, b),
               reject_t((est - 0.1) / (s[["sp"]] * sqrt(1 / 30 + 1 / 60)), 88),
               power_sat(30, d, 60, b, 0.1, method = "t"))
  )
}

# ---- 3. Single-arm trial against a threshold ----
s <- sim_groups(25, Inf, 0.5, 0, 1)
res <- rbind(
  res,
  judge_rate("sat_threshold_z", "SAT n = 25 against a threshold, z-test",
             reject_z(s[["m1"]] / (1 / sqrt(25))), power_sat(25, 0.5)),
  judge_rate("sat_threshold_t", "SAT n = 25 against a threshold, one-sample t-test",
             reject_t(s[["m1"]] / (s[["sp"]] / sqrt(25)), 24), power_sat(25, 0.5, method = "t"))
)

# ---- 4. Unequal standard deviations (z-test with known variances) ----
s <- sim_groups(40, 80, 0.5, -0.05, 1.5, 0.8)
est <- s[["m1"]] - s[["m2"]]
v <- 1.5^2 / 40 + 0.8^2 / 80
res <- rbind(
  res,
  judge_rate("sat_ec_unequal_sd_z", "SAT n = 40 vs 80, sigma_t = 1.5, sigma_c = 0.8, bias 0.05, margin 0.1",
             reject_z((est - 0.1) / sqrt(v)),
             power_sat(40, 0.5, 80, 0.05, 0.1, sigma_t = 1.5, sigma_c = 0.8)),
  judge_mse("sat_ec_unequal_sd_mse", "SAT mean squared error, unequal standard deviations",
            (est - 0.5)^2, mse_sat(40, 80, 0.05, sigma_t = 1.5, sigma_c = 0.8))
)

# ---- 5. Random bias (Pocock 1976): bias of each trial ~ N(0.05, 0.1^2) ----
b_i <- 0.05 + stats::rnorm(reps, sd = 0.1)
s <- sim_groups(40, 120, 0.5, 0, 1)
est <- s[["m1"]] - (s[["m2"]] - b_i)
res <- rbind(
  res,
  judge_rate("sat_random_bias_z", "SAT n = 40 vs 120 with random bias, margin 0.1, z-test",
             reject_z((est - 0.1) / sqrt(1 / 40 + 1 / 120)),
             power_sat(40, 0.5, 120, 0.05, 0.1, tau = 0.1)),
  judge_mse("sat_random_bias_mse", "SAT mean squared error with random bias",
            (est - 0.5)^2, mse_sat(40, 120, 0.05, 0.1))
)

# ---- 6. Hybrid design: 80 treated, 20 concurrent controls, 200 external controls ----
s_tc <- sim_groups(80, 20, 0.4, 0, 1)
# External controls have mean -0.1, a bias of 0.1
ext_mean <- rowMeans(matrix(stats::rnorm(reps * 200, -0.1, 1), reps, 200))
for (w_arg in list(NULL, 0.5)) {
  hyb <- mse_hybrid(100, 20, 200, bias = 0.1, weight = w_arg)
  w <- hyb[["weight"]]
  est <- s_tc[["m1"]] - (w * s_tc[["m2"]] + (1 - w) * ext_mean)
  tag <- if (is.null(w_arg)) "optimal_weight" else "weight_0.5"
  res <- rbind(res, judge_mse(paste0("hybrid_", tag),
                              sprintf("Hybrid design mean squared error, weight %.3f", w),
                              (est - 0.4)^2, hyb[["mse"]]))
}

# ---- 7. Time-to-event endpoint (normal approximation, INFO) ----
if (!requireNamespace("survival", quietly = TRUE)) {
  message("survival is not installed: the time-to-event simulation is not run.")
  res <- rbind(res, judge(script, "survival_unavailable", "survival is not installed", "", NA,
                          NA, "info"))
} else {
  reps_cox <- 2000
  lambda_c <- log(2) / 12
  hr <- 0.6
  follow_up <- 18
  sim_cox <- function(n1, n2, lambda1, lambda2, margin) {
    group <- rep(c(1, 0), c(n1, n2))
    stat <- vapply(seq_len(reps_cox), function(k) {
      t_event <- stats::rexp(n1 + n2, rep(c(lambda1, lambda2), c(n1, n2)))
      time <- pmin(t_event, follow_up)
      status <- as.numeric(t_event <= follow_up)
      fit <- survival::coxph(survival::Surv(time, status) ~ group)
      (-stats::coef(fit)[[1]] - margin) / sqrt(stats::vcov(fit)[1, 1])
    }, numeric(1))
    mean(stat > z_crit)
  }
  psi <- function(lambda) 1 - exp(-lambda * follow_up)
  # RCT 1:1 with 120 patients
  sim <- sim_cox(60, 60, lambda_c * hr, lambda_c, 0)
  ana <- power_rct(120, -log(hr), sigma_t = endpoint_sd("survival", prob_event = psi(lambda_c * hr)),
                   sigma_c = endpoint_sd("survival", prob_event = psi(lambda_c)))
  res <- rbind(res, judge(script, "survival_rct", "Cox model RCT 1:1, n = 120, HR 0.6",
                          sprintf("%d replicates", reps_cox), NA, sim - ana, "info",
                          note = sprintf("simulated %.4f, approximation %.4f (value = difference)",
                                         sim, ana)))
  # Single-arm trial with 60 patients and 120 external controls whose hazard is higher by
  # exp(0.1), i.e. a bias of 0.1 on the -log hazard ratio scale, with margin 0.05
  lambda_e <- lambda_c * exp(0.1)
  sim <- sim_cox(60, 120, lambda_c * hr, lambda_e, 0.05)
  ana <- power_sat(60, -log(hr), 120, bias = 0.1, margin = 0.05,
                   sigma_t = endpoint_sd("survival", prob_event = psi(lambda_c * hr)),
                   sigma_c = endpoint_sd("survival", prob_event = psi(lambda_e)))
  res <- rbind(res, judge(script, "survival_sat", "Cox model SAT 60 vs 120 external, bias 0.1, margin 0.05",
                          sprintf("%d replicates", reps_cox), NA, sim - ana, "info",
                          note = sprintf("simulated %.4f, approximation %.4f (value = difference)",
                                         sim, ana)))
}

finalize_results(res, "inst/validation/results")
