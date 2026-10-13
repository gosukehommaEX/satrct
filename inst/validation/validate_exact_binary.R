# Validate the exact calculations for binary endpoints and report the accuracy of the normal
# approximation.
#
# 1. oc_sat_binary_exact() and oc_binary_exact() against simulation (PASS within four Monte
#    Carlo standard errors). The decisions for each possible outcome are taken from
#    stats::binom.test() and stats::prop.test() where these tests apply, so the simulation is
#    independent of the package code; for the score test with a margin the decisions come from
#    the package statistic, which tests/testthat checks against numerically maximized
#    restricted likelihoods, so the simulation checks the enumeration.
# 2. The normal approximation of power_sat() and power_rct() with endpoint_sd("binary")
#    against the exact rejection probabilities (INFO, difference reported).
#
# Run from the package root after devtools::load_all().
# Output: inst/validation/results/validate_exact_binary.csv

source("inst/validation/judge.R")
check_loaded()
script <- "validate_exact_binary"
set.seed(20261013)
reps <- 40000

judge_rate <- function(item, description, sim, ana) {
  se <- sqrt(ana * (1 - ana) / reps)
  judge(script, item, description, sprintf("%d replicates", reps), ana, sim, "abs",
        tol = 4 * se, note = "reference = exact value; tolerance = 4 Monte Carlo SE")
}

res <- NULL

# ---- 1a. Single-arm exact binomial test against binom.test decisions ----
n <- 30
p0 <- 0.3
margin <- 0.05
reject_y <- vapply(0:n, function(y) {
  stats::binom.test(y, n, p = p0 + margin, alternative = "greater")[["p.value"]] <= 0.05
}, logical(1))
for (p_t in c(0.35, 0.55)) {
  y <- stats::rbinom(reps, n, p_t)
  res <- rbind(res, judge_rate(
    sprintf("sat_threshold_p%.2f", p_t),
    sprintf("Exact binomial test, n = 30, H0: p <= 0.35, true p = %.2f", p_t),
    mean(reject_y[y + 1]),
    oc_sat_binary_exact(n, p_t, p0, margin, alpha = 0.05)[["prob_reject"]]))
}

# ---- 1b. Two-group comparisons ----
# Decisions for all tables from stats::prop.test (Pearson tests) or from the package statistic
decision_table <- function(n_t, n_c, test, margin = 0, n_ext = 0, y_ext = 0, alpha = 0.025) {
  tables <- expand.grid(y_t = 0:n_t, y_c = 0:n_c)
  if (test == "score") {
    z <- satrct:::binary_z(tables[["y_t"]], n_t, tables[["y_c"]] + y_ext, n_c + n_ext, margin,
                           "score")
    rej <- !is.na(z) & z > stats::qnorm(1 - alpha)
  } else {
    p <- mapply(function(a, b) {
      suppressWarnings(stats::prop.test(c(a, b + y_ext), c(n_t, n_c + n_ext),
                                        alternative = "greater",
                                        correct = test == "pearson_cc")[["p.value"]])
    }, tables[["y_t"]], tables[["y_c"]])
    rej <- !is.na(p) & p < alpha
  }
  matrix(rej, n_t + 1, n_c + 1)
}
simulate_rate <- function(decisions, n_t, n_c, p_t, p_c) {
  y_t <- stats::rbinom(reps, n_t, p_t)
  y_c <- stats::rbinom(reps, n_c, p_c)
  mean(decisions[cbind(y_t + 1, y_c + 1)])
}

cases <- list(
  list(item = "rct_pearson_cc", text = "RCT 20 vs 20, Yates chi-squared test", n_t = 20,
       n_c = 20, test = "pearson_cc", margin = 0, n_ext = 0, y_ext = 0, p_t = 0.55, p_c = 0.3),
  list(item = "rct_pearson_null", text = "RCT 20 vs 20, Pearson test, type I error", n_t = 20,
       n_c = 20, test = "pearson", margin = 0, n_ext = 0, y_ext = 0, p_t = 0.3, p_c = 0.3),
  list(item = "sat_ec_score", text = "SAT 30 vs 60 external, score test, margin 0.1",
       n_t = 30, n_c = 60, test = "score", margin = 0.1, n_ext = 0, y_ext = 0, p_t = 0.6,
       p_c = 0.3),
  list(item = "sat_ec_score_null", text = "SAT 30 vs 60 external, score test at the margin",
       n_t = 30, n_c = 60, test = "score", margin = 0.1, n_ext = 0, y_ext = 0, p_t = 0.4,
       p_c = 0.3),
  list(item = "pooled_pearson_cc", text = "Concurrent 25 vs 25 pooled with fixed 12/40, Yates",
       n_t = 25, n_c = 25, test = "pearson_cc", margin = 0, n_ext = 40, y_ext = 12, p_t = 0.5,
       p_c = 0.35)
)
for (cs in cases) {
  dec <- decision_table(cs[["n_t"]], cs[["n_c"]], cs[["test"]], cs[["margin"]], cs[["n_ext"]],
                        cs[["y_ext"]])
  exact <- oc_binary_exact(cs[["n_t"]], cs[["n_c"]], cs[["p_t"]], cs[["p_c"]], cs[["margin"]],
                           test = cs[["test"]], n_ext = cs[["n_ext"]], y_ext = cs[["y_ext"]])
  res <- rbind(res, judge_rate(cs[["item"]], cs[["text"]],
                               simulate_rate(dec, cs[["n_t"]], cs[["n_c"]], cs[["p_t"]],
                                             cs[["p_c"]]),
                               exact))
}

# ---- 2. Accuracy of the normal approximation (INFO) ----
for (n in c(20, 40, 80)) {
  for (p_t in c(0.35, 0.55)) {
    exact <- oc_binary_exact(n, 2 * n, p_t, 0.3, margin = 0.05)
    approx <- power_sat(n, p_t - 0.3, n_e = 2 * n, margin = 0.05,
                        sigma_t = endpoint_sd("binary", p = p_t),
                        sigma_c = endpoint_sd("binary", p = 0.3))
    res <- rbind(res, judge(
      script, sprintf("normal_approx_sat_n%d_p%.2f", n, p_t),
      sprintf("SAT %d vs %d external, margin 0.05, p_t = %.2f: normal approximation minus exact",
              n, 2 * n, p_t),
      "exact score test", NA, approx - exact, "info",
      note = sprintf("exact %.4f, approximation %.4f", exact, approx)))
  }
  exact <- oc_binary_exact(n / 2, n / 2, 0.55, 0.3)
  approx <- power_rct(n, 0.25, sigma_t = endpoint_sd("binary", p = 0.55),
                      sigma_c = endpoint_sd("binary", p = 0.3))
  res <- rbind(res, judge(
    script, sprintf("normal_approx_rct_n%d", n),
    sprintf("RCT %d vs %d, 0.55 vs 0.30: normal approximation minus exact", n / 2, n / 2),
    "exact score test", NA, approx - exact, "info",
    note = sprintf("exact %.4f, approximation %.4f", exact, approx)))
}

finalize_results(res, "inst/validation/results")
