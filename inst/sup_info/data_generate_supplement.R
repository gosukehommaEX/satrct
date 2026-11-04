# Compute the results shown in the figures and tables of the Supporting Information and save
# them to inst/sup_info/results/data/. The figures and tables are drawn from these files by
# table_and_figure_supplement.R.
#
# Settings common to all results unless stated otherwise: one-sided significance level
# alpha = 0.025, z-tests, common standard deviation sigma = 1 and 1:1 randomization in the
# randomized controlled trial (RCT). N_E = Inf is the comparison with a fixed threshold.
#
# Before saving, the script checks relations that hold by construction and stops if one of
# them fails.
#
# Run from the package root after devtools::load_all().
# Output: inst/sup_info/results/data/*.csv

if (!exists("power_sat", mode = "function")) {
  stop("Run devtools::load_all() (or library(satrct)) before sourcing this script.",
       call. = FALSE)
}
start_time <- proc.time()
out_dir <- "inst/sup_info/results/data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
alpha <- 0.025

assert_close <- function(x, y, tol, what) {
  d <- max(abs(as.numeric(x) - as.numeric(y)))
  if (!is.finite(d) || d > tol) {
    stop(sprintf("Check failed: %s (largest difference %.3g).", what, d), call. = FALSE)
  }
  message(sprintf("Check passed: %s (largest difference %.1e).", what, d))
  invisible(d)
}
save_csv <- function(x, name) {
  utils::write.csv(x, file.path(out_dir, name), row.names = FALSE)
  message("Saved ", file.path(out_dir, name), " (", nrow(x), " rows)")
}

# ---- Figure S1: hybrid design with concurrent and external controls (Pocock 1976) ----
n_s1 <- 40
n_e_s1 <- 100
bias_s1 <- seq(0, 0.6, by = 0.005)
opt <- optimal_nc(n_s1, n_e_s1, bias_s1)
fig_s1 <- data.frame(
  n = n_s1,
  n_e = n_e_s1,
  bias = bias_s1,
  mse_rct = mse_rct(n_s1),
  mse_sat = mse_sat(n_s1, n_e_s1, bias_s1),
  n_c_opt = opt[["n_c"]],
  n_eff = opt[["n_eff"]],
  mse_hybrid = opt[["mse"]],
  breakeven_bias = breakeven_bias(n_s1, n_e_s1)
)
# The optimal hybrid design contains the SAT (n_c = 0) and the RCT (n_c = N / 2, all weight on
# the concurrent controls), so its mean squared error is not larger than either of them
assert_close(pmax(fig_s1[["mse_hybrid"]] - pmin(fig_s1[["mse_rct"]], fig_s1[["mse_sat"]]), 0),
             0, 1e-12,
             "Figure S1: the optimal hybrid design is at least as good as the SAT and the RCT")
save_csv(fig_s1, "figS1_hybrid.csv")

# ---- Figure S2: exact operating characteristics for a binary endpoint ----
# RCT with N / 2 patients per group (score test), SAT with N_E = 2 N external controls (score
# test with margin) and SAT against the threshold p_c (exact binomial test with margin). The
# power is evaluated at p_t and the type I error rate at the boundary of the null hypothesis,
# that is, p_t = p_c for the RCT and p_t = p_c + margin for the SAT (bias equal to the margin).
p_c <- 0.30
p_t <- 0.55
margin_s2 <- 0.05
sd_t <- endpoint_sd("binary", p = p_t)
sd_c <- endpoint_sd("binary", p = p_c)
n_s2 <- seq(20, 100, by = 2)
fig_s2 <- do.call(rbind, lapply(n_s2, function(n) {
  rct <- oc_binary_exact(n / 2, n / 2, c(p_t, p_c), p_c, alpha = alpha)
  ext <- oc_binary_exact(n, 2 * n, c(p_t, p_c + margin_s2), p_c, margin_s2, alpha = alpha)
  thr <- oc_sat_binary_exact(n, c(p_t, p_c + margin_s2), p_c, margin_s2, alpha = alpha)
  data.frame(
    alpha = alpha, n = n, p_t = p_t, p_c = p_c, margin = margin_s2,
    rct_exact_power = rct[1],
    rct_exact_type1 = rct[2],
    rct_normal_power = power_rct(n, p_t - p_c, sd_t, sd_c, alpha),
    ext_exact_power = ext[1],
    ext_exact_type1 = ext[2],
    ext_normal_power = power_sat(n, p_t - p_c, 2 * n, margin = margin_s2, sigma_t = sd_t,
                                 sigma_c = sd_c, alpha = alpha),
    thr_exact_power = thr[["prob_reject"]][1],
    thr_exact_type1 = thr[["prob_reject"]][2],
    thr_exact_size = thr[["size"]][1],
    thr_normal_power = power_sat(n, p_t - p_c, Inf, margin = margin_s2, sigma_t = sd_t,
                                 alpha = alpha)
  )
}))
# For the exact binomial test, the rejection probability at p_c + margin is the size
assert_close(fig_s2[["thr_exact_type1"]], fig_s2[["thr_exact_size"]], 1e-12,
             "Figure S2: type I error rate of the exact binomial test at the boundary")
save_csv(fig_s2, "figS2_binary.csv")

# ---- Table S1: z-test and t-test for small N ----
tab_s1 <- expand.grid(delta = c(0.5, 1), n = c(10, 20, 40), KEEP.OUT.ATTRS = FALSE)
tab_s1 <- tab_s1[c("n", "delta")]
tab_s1[["alpha"]] <- alpha
for (m in c("z", "t")) {
  tab_s1[[paste0("power_rct_", m)]] <- power_rct(tab_s1[["n"]], tab_s1[["delta"]],
                                                 alpha = alpha, method = m)
  tab_s1[[paste0("power_thr_", m)]] <- power_sat(tab_s1[["n"]], tab_s1[["delta"]], Inf,
                                                 alpha = alpha, method = m)
  tab_s1[[paste0("power_ext_", m)]] <- power_sat(tab_s1[["n"]], tab_s1[["delta"]],
                                                 2 * tab_s1[["n"]], alpha = alpha, method = m)
}
save_csv(tab_s1, "tableS1_z_t.csv")

# ---- Table S2: unequal allocation and unequal standard deviations ----
n_tab_s2 <- 40
delta_tab_s2 <- 0.5
tab_s2 <- expand.grid(sd_ratio = c(0.5, 1, 2), r = c(1 / 2, 2 / 3), n_e = c(Inf, 80),
                      KEEP.OUT.ATTRS = FALSE)
tab_s2 <- tab_s2[c("n_e", "r", "sd_ratio")]
tab_s2[["alpha"]] <- alpha
tab_s2[["n"]] <- n_tab_s2
tab_s2[["delta"]] <- delta_tab_s2
tab_s2[["sigma_t"]] <- 1
tab_s2[["sigma_c"]] <- tab_s2[["sd_ratio"]]
ma <- matched_alpha(n_tab_s2, delta_tab_s2, tab_s2[["n_e"]], 0, tab_s2[["sigma_t"]],
                    tab_s2[["sigma_c"]], alpha, tab_s2[["r"]])
tab_s2[["power_rct"]] <- power_rct(n_tab_s2, delta_tab_s2, tab_s2[["sigma_t"]],
                                   tab_s2[["sigma_c"]], alpha, tab_s2[["r"]])
tab_s2[["power_sat"]] <- power_sat(n_tab_s2, delta_tab_s2, tab_s2[["n_e"]],
                                   sigma_t = tab_s2[["sigma_t"]],
                                   sigma_c = tab_s2[["sigma_c"]], alpha = alpha)
tab_s2[["alpha_matched"]] <- ma[["alpha_matched"]]
tab_s2[["breakeven_bias_rel"]] <- breakeven_bias(n_tab_s2, tab_s2[["n_e"]],
                                                 tab_s2[["sigma_t"]], tab_s2[["sigma_c"]],
                                                 tab_s2[["r"]]) / delta_tab_s2
tab_s2[["critical_margin_rel"]] <- critical_margin(n_tab_s2, delta_tab_s2, tab_s2[["n_e"]],
                                                   tab_s2[["sigma_t"]], tab_s2[["sigma_c"]],
                                                   tab_s2[["r"]]) / delta_tab_s2
assert_close(ma[["power"]], tab_s2[["power_sat"]], 1e-12,
             "Table S2: matched_alpha() and power_sat() give the same power of the SAT")
save_csv(tab_s2, "tableS2_sensitivity.csv")

# ---- Table S3: uncertainty in the published estimates of the bias ----
# Type I error rate of the SAT without margin in the setting of Table 3 of the main text when the
# published point estimates of the bias are replaced by
#   mean_upper  the upper limit of the 95% credible interval of the mean bias (minus the log of
#               the lower limit for the hazard ratio), with the posterior median of tau;
#   tau_upper   the upper limit of the 95% credible interval of tau, with the posterior median
#               of the mean bias;
#   ivw_mean    the difference in inverse variance-weighted hazard ratios that Gupta et al.
#               report beside the arithmetic mean of the trial-level differences, with
#               tau = 0;
#   trial_sd    the published mean, with tau equal to the sample standard deviation of the
#               trial-level differences, which includes the sampling error of each trial.
# The first three are entered with their locations in inst/sup_info/bias_uncertainty_sources.csv.
# The trial-level differences of Gupta et al. are entered in
# inst/sup_info/gupta_trial_differences.csv and their published means are read from
# inst/scripts/empirical_bias_sources.csv. The designs are read from the results of
# inst/scripts/data_generate_main.R, so that they are those of Table 3.
table3_path <- "inst/scripts/results/data/table3_empirical_bias.csv"
if (!file.exists(table3_path)) {
  stop("Missing ", table3_path, ". Run inst/scripts/data_generate_main.R first.", call. = FALSE)
}
table3 <- utils::read.csv(table3_path, stringsAsFactors = FALSE)
design_s3 <- table3[!duplicated(table3[["n"]]),
                    c("alpha", "n", "n_e", "hazard_ratio", "prob_event", "delta", "sigma",
                      "alpha_matched", "breakeven_bias", "critical_margin", "power_rct")]
design_s3 <- design_s3[order(design_s3[["n"]]), ]
rownames(design_s3) <- NULL
stopifnot(all(design_s3[["alpha"]] == alpha))
sd_surv_s3 <- endpoint_sd("survival", prob_event = design_s3[["prob_event"]])
assert_close(design_s3[["sigma"]], sd_surv_s3, 1e-12,
             "Table S3: the standard deviation is that of Table 3")
assert_close(design_s3[["alpha_matched"]],
             matched_alpha(design_s3[["n"]], design_s3[["delta"]], design_s3[["n_e"]], 0,
                           sd_surv_s3, sd_surv_s3, alpha)[["alpha_matched"]],
             1e-12, "Table S3: alpha' is that of Table 3")
assert_close(design_s3[["power_rct"]],
             power_rct(design_s3[["n"]], design_s3[["delta"]], sd_surv_s3, sd_surv_s3, alpha),
             1e-12, "Table S5: the power of the RCT is that of Table 3")

known_measures_s3 <- c("hr_internal_vs_external", "log_hr_difference")
variation_levels <- c("mean_upper", "tau_upper", "ivw_mean", "trial_sd")
columns_s3 <- c("author", "bibkey", "endpoint", "analysis", "subset", "n_comparisons",
                "variation", "measure", "value", "tau")
uncertainty <- utils::read.csv("inst/sup_info/bias_uncertainty_sources.csv",
                               stringsAsFactors = FALSE)
if (!all(uncertainty[["measure"]] %in% known_measures_s3) ||
    !all(uncertainty[["variation"]] %in% setdiff(variation_levels, "trial_sd"))) {
  stop("Unknown measure or variation in inst/sup_info/bias_uncertainty_sources.csv.",
       call. = FALSE)
}

# Trial-level differences of Gupta et al. The sample standard deviation is taken over all
# trials, or over the trials other than the outlier for the estimates without it. The mean of
# the transcribed differences must agree with the published mean up to its rounding.
gupta_trials <- utils::read.csv("inst/sup_info/gupta_trial_differences.csv",
                                stringsAsFactors = FALSE)
gupta_published <- utils::read.csv("inst/scripts/empirical_bias_sources.csv",
                                   stringsAsFactors = FALSE)
gupta_published <- gupta_published[gupta_published[["bibkey"]] == "Gupta2025", ]
gupta_column <- c("Unadjusted" = "unadjusted", "Weighted" = "adjusted",
                  "Weighted, QBA" = "adjusted_qba")
stopifnot(nrow(gupta_published) == 6, all(gupta_published[["analysis"]] %in% names(gupta_column)),
          all(gupta_published[["measure"]] == "log_hr_difference"),
          sum(gupta_trials[["outlier"]]) == 1)
trial_sd <- do.call(rbind, lapply(seq_len(nrow(gupta_published)), function(i) {
  p <- gupta_published[i, ]
  keep <- if (p[["subset"]] == "All") {
    rep(TRUE, nrow(gupta_trials))
  } else {
    !gupta_trials[["outlier"]]
  }
  x <- gupta_trials[[gupta_column[[p[["analysis"]]]]]][keep]
  stopifnot(length(x) == p[["n_comparisons"]])
  assert_close(mean(x), p[["value"]], 5e-4,
               paste0("Table S3: the mean of the trial-level differences of Gupta et al. (",
                      p[["analysis"]], ", ", p[["subset"]], ") is the published mean"))
  out <- p
  out[["variation"]] <- "trial_sd"
  out[["tau"]] <- stats::sd(x)
  out
}))

sources_s3 <- rbind(uncertainty[columns_s3], trial_sd[columns_s3])
stopifnot(all(sources_s3[["subset"]] %in% c("All", "Outlier removed")))
key_s3 <- paste(sources_s3[["author"]], sources_s3[["endpoint"]], sources_s3[["analysis"]])
sources_s3 <- sources_s3[order(match(key_s3, unique(key_s3)),
                               match(sources_s3[["subset"]], c("All", "Outlier removed")),
                               match(sources_s3[["variation"]], variation_levels)), ]
rownames(sources_s3) <- NULL
sources_s3[["id"]] <- seq_len(nrow(sources_s3))
sources_s3[["bias_mean"]] <- ifelse(sources_s3[["measure"]] == "hr_internal_vs_external",
                                    -log(sources_s3[["value"]]), sources_s3[["value"]])
sources_s3[["tau_used"]] <- ifelse(is.na(sources_s3[["tau"]]), 0, sources_s3[["tau"]])

grid_s3 <- expand.grid(source_row = seq_len(nrow(sources_s3)),
                       design_row = seq_len(nrow(design_s3)), KEEP.OUT.ATTRS = FALSE)
tab_s3 <- cbind(sources_s3[grid_s3[["source_row"]], ], design_s3[grid_s3[["design_row"]], ])
rownames(tab_s3) <- NULL
tab_s3[["type1_sat"]] <- power_sat(tab_s3[["n"]], 0, tab_s3[["n_e"]],
                                   bias = tab_s3[["bias_mean"]], margin = 0,
                                   tau = tab_s3[["tau_used"]], sigma_t = tab_s3[["sigma"]],
                                   alpha = alpha)
tab_s3[["above_alpha_matched"]] <- tab_s3[["type1_sat"]] > tab_s3[["alpha_matched"]]
tab_s3[["tau_below_breakeven"]] <- tab_s3[["tau_used"]] < tab_s3[["breakeven_bias"]]
# Without a random bias, the type I error rate of the SAT exceeds alpha' exactly when the bias
# exceeds m_crit
k <- tab_s3[["tau_used"]] == 0
assert_close(tab_s3[["above_alpha_matched"]][k],
             tab_s3[["bias_mean"]][k] > tab_s3[["critical_margin"]][k], 0,
             "Table S3: with tau = 0, alpha' is exceeded exactly when the bias exceeds m_crit")
# Tables S4 and S5: the margins m_avg and m_0.95 of Table 4 of the main text, the type I error
# rate with m_avg and the power of the SAT with each margin, averaged over the distribution of
# the bias, under each variation of Table S3. They are saved with the results of Table S3.
z_alpha_s3 <- qnorm(alpha, lower.tail = FALSE)
v_sat_s3 <- mse_sat(tab_s3[["n"]], tab_s3[["n_e"]], sigma_t = tab_s3[["sigma"]])
sd_bias_avg_s3 <- sqrt(v_sat_s3 + tab_s3[["tau_used"]]^2)
# A bias margin is nonnegative. A negative value of either formula (the inverse
# variance-weighted mean of Gupta et al. with quantitative bias analysis) is replaced by zero,
# so that the averaged type I error rate is then below alpha and the mean bias no longer
# cancels from the averaged power. The unrestricted values are kept as margin_avg_signed and
# margin_q95_signed.
tab_s3[["margin_avg_signed"]] <- tab_s3[["bias_mean"]] +
  z_alpha_s3 * (sd_bias_avg_s3 - sqrt(v_sat_s3))
tab_s3[["margin_q95_signed"]] <- tab_s3[["bias_mean"]] + qnorm(0.95) * tab_s3[["tau_used"]]
tab_s3[["margin_avg"]] <- pmax(0, tab_s3[["margin_avg_signed"]])
tab_s3[["margin_q95"]] <- pmax(0, tab_s3[["margin_q95_signed"]])
tab_s3[["margin_avg_truncated"]] <- tab_s3[["margin_avg_signed"]] < 0
tab_s3[["margin_q95_truncated"]] <- tab_s3[["margin_q95_signed"]] < 0
tab_s3[["type1_sat_margin_avg"]] <- power_sat(tab_s3[["n"]], 0, tab_s3[["n_e"]],
                                              bias = tab_s3[["bias_mean"]],
                                              margin = tab_s3[["margin_avg"]],
                                              tau = tab_s3[["tau_used"]],
                                              sigma_t = tab_s3[["sigma"]], alpha = alpha)
for (v in c("avg", "q95")) {
  tab_s3[[paste0("power_sat_margin_", v, "_averaged")]] <-
    power_sat(tab_s3[["n"]], tab_s3[["delta"]], tab_s3[["n_e"]], bias = tab_s3[["bias_mean"]],
              margin = tab_s3[[paste0("margin_", v)]], tau = tab_s3[["tau_used"]],
              sigma_t = tab_s3[["sigma"]], alpha = alpha)
}
k <- !tab_s3[["margin_avg_truncated"]]
k95 <- !tab_s3[["margin_q95_truncated"]]
assert_close(tab_s3[["type1_sat_margin_avg"]][k], alpha, 1e-12,
             "Table S4: the type I error rate averaged over the bias equals alpha at m_avg")
stopifnot(all(tab_s3[["type1_sat_margin_avg"]][!k] < alpha))
message("Check passed: Table S4: with a margin truncated at zero, the averaged type I error ",
        "rate is below alpha (", sum(!k), " rows).")
assert_close(tab_s3[["power_sat_margin_avg_averaged"]][k],
             pnorm(tab_s3[["delta"]][k] / sd_bias_avg_s3[k] - z_alpha_s3), 1e-12,
             "Table S5: closed form of the averaged power of the SAT with m_avg")
assert_close(tab_s3[["power_sat_margin_q95_averaged"]][k95],
             pnorm((tab_s3[["delta"]][k95] - qnorm(0.95) * tab_s3[["tau_used"]][k95] -
                      z_alpha_s3 * sqrt(v_sat_s3[k95])) / sd_bias_avg_s3[k95]), 1e-12,
             "Table S5: closed form of the averaged power of the SAT with m_0.95")
assert_close(tab_s3[["power_sat_margin_avg_averaged"]][k] > tab_s3[["power_rct"]][k],
             tab_s3[["tau_below_breakeven"]][k], 0,
             paste("Table S5: averaged over the bias, the SAT with m_avg has more power than the",
                   "RCT exactly when tau is below b*"))
save_csv(tab_s3, "tableS3_bias_uncertainty.csv")

# ---- Table S6: bias margins fixed before the trial ----
# For the six estimates of Table 3 with tau > 0, the margins m_avg and m_0.95 of Table 4 are
# derived from the posterior medians (the planning distribution of the bias) and kept fixed,
# while the actual distribution of the bias has
#   planning    the posterior medians of the mean bias and of tau;
#   mean_upper  the upper limit of the 95% credible interval of the mean bias and the median of
#               tau;
#   tau_upper   the median of the mean bias and the upper limit of the 95% credible interval of
#               tau;
#   both_upper  both upper limits, a stress scenario rather than a joint credible bound.
# For each margin the results are the averaged type I error rate and the averaged power under
# the actual distribution and the proportion P(B > m) of trials whose bias exceeds the margin,
# in which the type I error rate given the bias exceeds alpha. Tables S4 and S5 instead derive
# the margins again from each variation.
plan_s6 <- table3[table3[["tau_used"]] > 0,
                  c("id", "author", "bibkey", "endpoint", "analysis", "subset", "n_comparisons",
                    "alpha", "n", "n_e", "delta", "sigma", "power_rct", "alpha_matched",
                    "critical_margin", "bias_mean", "tau_used", "margin_avg", "margin_q95")]
names(plan_s6)[names(plan_s6) == "bias_mean"] <- "bias_mean_plan"
names(plan_s6)[names(plan_s6) == "tau_used"] <- "tau_plan"
stopifnot(length(unique(plan_s6[["id"]])) == 6, all(plan_s6[["alpha"]] == alpha),
          all(plan_s6[["margin_avg"]] >= 0), all(plan_s6[["margin_q95"]] >= 0))
upper_of <- function(p, variation) {
  u <- uncertainty[uncertainty[["bibkey"]] == p[["bibkey"]] &
                     uncertainty[["endpoint"]] == p[["endpoint"]] &
                     uncertainty[["analysis"]] == p[["analysis"]] &
                     uncertainty[["subset"]] == p[["subset"]] &
                     uncertainty[["variation"]] == variation, ]
  stopifnot(nrow(u) == 1, u[["measure"]] == "hr_internal_vs_external")
  u
}
scenarios_s6 <- c("planning", "mean_upper", "tau_upper", "both_upper")
tab_s6 <- do.call(rbind, lapply(seq_len(nrow(plan_s6)), function(i) {
  p <- plan_s6[i, ]
  mean_upper <- upper_of(p, "mean_upper")
  tau_upper <- upper_of(p, "tau_upper")
  # the other parameter of each variation is the posterior median
  stopifnot(abs(mean_upper[["tau"]] - p[["tau_plan"]]) < 1e-12,
            abs(-log(tau_upper[["value"]]) - p[["bias_mean_plan"]]) < 1e-12)
  b_up <- -log(mean_upper[["value"]])
  t_up <- tau_upper[["tau"]]
  out <- p[rep(1, length(scenarios_s6)), ]
  out[["scenario"]] <- scenarios_s6
  out[["bias_mean_true"]] <- c(p[["bias_mean_plan"]], b_up, p[["bias_mean_plan"]], b_up)
  out[["tau_true"]] <- c(p[["tau_plan"]], p[["tau_plan"]], t_up, t_up)
  out
}))
rownames(tab_s6) <- NULL
tab_s6 <- tab_s6[order(tab_s6[["id"]], tab_s6[["n"]],
                       match(tab_s6[["scenario"]], scenarios_s6)), ]
rownames(tab_s6) <- NULL
for (v in c("avg", "q95")) {
  m <- tab_s6[[paste0("margin_", v)]]
  tab_s6[[paste0("type1_margin_", v)]] <- power_sat(tab_s6[["n"]], 0, tab_s6[["n_e"]],
                                                    bias = tab_s6[["bias_mean_true"]],
                                                    margin = m, tau = tab_s6[["tau_true"]],
                                                    sigma_t = tab_s6[["sigma"]], alpha = alpha)
  tab_s6[[paste0("power_margin_", v)]] <- power_sat(tab_s6[["n"]], tab_s6[["delta"]],
                                                    tab_s6[["n_e"]],
                                                    bias = tab_s6[["bias_mean_true"]],
                                                    margin = m, tau = tab_s6[["tau_true"]],
                                                    sigma_t = tab_s6[["sigma"]], alpha = alpha)
  tab_s6[[paste0("prob_bias_above_margin_", v)]] <- pnorm(m, mean = tab_s6[["bias_mean_true"]],
                                                          sd = tab_s6[["tau_true"]],
                                                          lower.tail = FALSE)
}
v_sat_s6 <- mse_sat(tab_s6[["n"]], tab_s6[["n_e"]], sigma_t = tab_s6[["sigma"]])
assert_close(tab_s6[["type1_margin_avg"]],
             pnorm((tab_s6[["bias_mean_true"]] - tab_s6[["margin_avg"]] -
                      z_alpha_s3 * sqrt(v_sat_s6)) /
                     sqrt(v_sat_s6 + tab_s6[["tau_true"]]^2)), 1e-12,
             "Table S6: closed form of the averaged type I error rate with a fixed margin")
k <- tab_s6[["scenario"]] == "planning"
assert_close(tab_s6[["type1_margin_avg"]][k], alpha, 1e-12,
             "Table S6: under the planning distribution, m_avg gives the averaged level alpha")
assert_close(tab_s6[["prob_bias_above_margin_q95"]][k], 0.05, 1e-12,
             "Table S6: under the planning distribution, the bias exceeds m_0.95 in 5% of trials")
assert_close(tab_s6[["power_margin_avg"]][k],
             table3[["power_sat_margin_avg_averaged"]][match(
               paste(tab_s6[["id"]][k], tab_s6[["n"]][k]),
               paste(table3[["id"]], table3[["n"]]))], 1e-12,
             "Table S6: under the planning distribution, the averaged power is that of Table 4")
save_csv(tab_s6, "tableS6_fixed_margin.csv")

# ---- Table S7: effective number of external controls and unequal event probabilities ----
# V_S in Table 3 treats the N_E = 2 N external controls as unweighted independent
# observations with the event probability of the controls. Weighting by a propensity score
# lowers the effective number of external controls, and a common event probability ignores
# that fewer patients have an event on the experimental treatment. Each scenario keeps the
# hazard ratio and the published estimates of Table 3 and changes one assumption:
#   reference  Table 3;
#   ne_n       an effective number of N external controls (half of 2 N);
#   ne_half    an effective number of N / 2 external controls (a quarter of 2 N);
#   psi_t      N_E = 2 N with event probability 0.7 for the controls and 1 - 0.3^0.6 for the
#              experimental treatment, as under exponential survival with a common follow-up
#              time, in which the probability of no event is (1 - 0.7)^HR.
# An event probability of the external controls other than that of the concurrent controls is
# covered by the effective number: with standard deviation sigma_E per patient, the mean of
# N_E external controls has the variance of N_E sigma_C^2 / sigma_E^2 controls with standard
# deviation sigma_C. The margin m_avg is derived again in each scenario.
design_s7 <- unique(table3[, c("n", "hazard_ratio", "prob_event", "delta")])
stopifnot(nrow(design_s7) == length(unique(table3[["n"]])),
          length(unique(design_s7[["prob_event"]])) == 1)
prob_event_s7 <- design_s7[["prob_event"]][1]
hazard_ratio_s7 <- design_s7[["hazard_ratio"]][1]
scenarios_s7 <- data.frame(
  scenario = c("reference", "ne_n", "ne_half", "psi_t"),
  ne_factor = c(2, 1, 0.5, 2),
  prob_event_t = c(prob_event_s7, prob_event_s7, prob_event_s7,
                   1 - (1 - prob_event_s7)^hazard_ratio_s7),
  prob_event_c = prob_event_s7)
estimates_s7 <- table3[table3[["tau_used"]] > 0 & !duplicated(table3[["id"]]),
                       c("id", "author", "bibkey", "endpoint", "analysis", "subset",
                         "n_comparisons", "bias_mean", "tau_used")]
grid_s7 <- expand.grid(estimate_row = seq_len(nrow(estimates_s7)),
                       design_row = seq_len(nrow(design_s7)),
                       scenario_row = seq_len(nrow(scenarios_s7)), KEEP.OUT.ATTRS = FALSE)
tab_s7 <- cbind(scenarios_s7[grid_s7[["scenario_row"]], ], design_s7[grid_s7[["design_row"]], ],
                estimates_s7[grid_s7[["estimate_row"]], ])
rownames(tab_s7) <- NULL
tab_s7[["alpha"]] <- alpha
tab_s7[["n_e"]] <- tab_s7[["ne_factor"]] * tab_s7[["n"]]
tab_s7[["sigma_t"]] <- endpoint_sd("survival", prob_event = tab_s7[["prob_event_t"]])
tab_s7[["sigma_c"]] <- endpoint_sd("survival", prob_event = tab_s7[["prob_event_c"]])
ma_s7 <- matched_alpha(tab_s7[["n"]], tab_s7[["delta"]], tab_s7[["n_e"]], 0, tab_s7[["sigma_t"]],
                       tab_s7[["sigma_c"]], alpha)
tab_s7[["power_rct"]] <- power_rct(tab_s7[["n"]], tab_s7[["delta"]], tab_s7[["sigma_t"]],
                                   tab_s7[["sigma_c"]], alpha)
tab_s7[["power_sat"]] <- ma_s7[["power"]]
tab_s7[["alpha_matched"]] <- ma_s7[["alpha_matched"]]
tab_s7[["breakeven_bias"]] <- breakeven_bias(tab_s7[["n"]], tab_s7[["n_e"]], tab_s7[["sigma_t"]],
                                             tab_s7[["sigma_c"]])
tab_s7[["critical_margin"]] <- critical_margin(tab_s7[["n"]], tab_s7[["delta"]], tab_s7[["n_e"]],
                                               tab_s7[["sigma_t"]], tab_s7[["sigma_c"]])
v_sat_s7 <- mse_sat(tab_s7[["n"]], tab_s7[["n_e"]], sigma_t = tab_s7[["sigma_t"]],
                    sigma_c = tab_s7[["sigma_c"]])
tab_s7[["margin_avg"]] <- pmax(0, tab_s7[["bias_mean"]] +
                                 z_alpha_s3 * (sqrt(v_sat_s7 + tab_s7[["tau_used"]]^2) -
                                                 sqrt(v_sat_s7)))
tab_s7[["power_sat_margin_avg_averaged"]] <- power_sat(tab_s7[["n"]], tab_s7[["delta"]],
                                                       tab_s7[["n_e"]],
                                                       bias = tab_s7[["bias_mean"]],
                                                       margin = tab_s7[["margin_avg"]],
                                                       tau = tab_s7[["tau_used"]],
                                                       sigma_t = tab_s7[["sigma_t"]],
                                                       sigma_c = tab_s7[["sigma_c"]],
                                                       alpha = alpha)
tab_s7[["type1_sat"]] <- power_sat(tab_s7[["n"]], 0, tab_s7[["n_e"]],
                                   bias = tab_s7[["bias_mean"]], tau = tab_s7[["tau_used"]],
                                   sigma_t = tab_s7[["sigma_t"]], sigma_c = tab_s7[["sigma_c"]],
                                   alpha = alpha)
tab_s7[["margin_avg_above_critical"]] <- tab_s7[["margin_avg"]] > tab_s7[["critical_margin"]]
tab_s7[["tau_below_breakeven"]] <- tab_s7[["tau_used"]] < tab_s7[["breakeven_bias"]]
k <- tab_s7[["scenario"]] == "reference"
ref_s7 <- table3[match(paste(tab_s7[["id"]][k], tab_s7[["n"]][k]),
                       paste(table3[["id"]], table3[["n"]])), ]
columns_ref_s7 <- c("power_rct", "power_sat", "alpha_matched", "breakeven_bias",
                    "critical_margin", "margin_avg", "power_sat_margin_avg_averaged", "type1_sat")
assert_close(as.matrix(tab_s7[k, columns_ref_s7]), as.matrix(ref_s7[, columns_ref_s7]), 1e-12,
             "Table S7: the reference scenario reproduces Tables 3 and 4")
assert_close(tab_s7[["power_sat_margin_avg_averaged"]] > tab_s7[["power_rct"]],
             tab_s7[["tau_below_breakeven"]], 0,
             "Table S7: the SAT with m_avg has more averaged power than the RCT exactly when tau < b*")
save_csv(tab_s7, "tableS7_design_sensitivity.csv")

# ---- Figure S3: bias at which the type I error rate of the SAT reaches that of the RCT ----
# (a) Binary endpoint, exact calculation in the setting of Figure S2 (p_t = 0.55, p_c = 0.30,
# margin 0.05 for the SAT, N_E = 2 N, score tests). A bias b lowers the response probability
# of the external controls to p_c - b, while the response probability p_c of the current
# patients on the control is unchanged. The exact power of the RCT is a step function of its
# nominal level, so the RCT is matched to the SAT by the convention that its nominal level
# alpha'' is the smallest at which its exact power is at least the exact power of the SAT
# without bias. The type I error rate of the RCT is its exact rejection probability at
# p_t = p_c, and the crossing is the bias at which the exact type I error rate of the SAT, with
# p_t = p_c and external response probability p_c - b, equals it. The normal approximation of
# the crossing is m_crit, with the standard deviations evaluated at p_t and p_c, and the crossing
# is also approximated with the variance under the null hypothesis.
smallest_level <- function(power_at, target) {
  lo <- 1e-8
  hi <- 0.9
  stopifnot(power_at(hi) >= target, power_at(lo) < target)
  while (hi - lo > 1e-13) {
    mid <- (lo + hi) / 2
    if (power_at(mid) >= target) hi <- mid else lo <- mid
  }
  hi
}
fig_s3a <- do.call(rbind, lapply(n_s2, function(n) {
  power_sat_exact <- oc_binary_exact(n, 2 * n, p_t, p_c, margin_s2, alpha = alpha)
  level <- smallest_level(function(a) oc_binary_exact(n / 2, n / 2, p_t, p_c, alpha = a),
                          power_sat_exact)
  type1_rct <- oc_binary_exact(n / 2, n / 2, p_c, p_c, alpha = level)
  type1_sat <- function(b) oc_binary_exact(n, 2 * n, p_c, p_c - b, margin_s2, alpha = alpha)
  crossing <- stats::uniroot(function(b) type1_sat(b) - type1_rct, c(0, 0.29),
                             tol = 1e-12)[["root"]]
  m_crit <- critical_margin(n, p_t - p_c, 2 * n, sd_t, sd_c)
  data.frame(
    alpha = alpha, n = n, n_e = 2 * n, p_t = p_t, p_c = p_c, margin = margin_s2,
    sat_exact_power = power_sat_exact,
    alpha_matched_normal = matched_alpha(n, p_t - p_c, 2 * n, margin_s2, sd_t, sd_c,
                                         alpha)[["alpha_matched"]],
    rct_nominal_level = level,
    rct_exact_power = oc_binary_exact(n / 2, n / 2, p_t, p_c, alpha = level),
    rct_exact_type1 = type1_rct,
    crossing_exact = crossing,
    critical_margin_normal = m_crit,
    sat_exact_type1_at_mcrit = type1_sat(m_crit)
  )
}))
fig_s3a[["crossing_minus_mcrit"]] <- fig_s3a[["crossing_exact"]] -
  fig_s3a[["critical_margin_normal"]]
fig_s3a[["rct_type1_minus_alpha_matched"]] <- fig_s3a[["rct_exact_type1"]] -
  fig_s3a[["alpha_matched_normal"]]
# normal approximation of the crossing with the variance under the null hypothesis for the type I
# error rate of the SAT, in which its patients have the response probability p_c and the external
# controls p_c - b, and with the variances under the alternative hypothesis for the level of the
# RCT matched to the power of the SAT, as for the time-to-event endpoint below
crossing_normal_null_s3a <- function(n, n_e, m) {
  z_a <- qnorm(alpha, lower.tail = FALSE)
  v_alt <- p_t * (1 - p_t) / n + p_c * (1 - p_c) / n_e
  w_alt <- 2 * p_t * (1 - p_t) / n + 2 * p_c * (1 - p_c) / n
  z_matched <- z_a + (p_t - p_c) / sqrt(w_alt) - (p_t - p_c - m) / sqrt(v_alt)
  f <- function(b) {
    v_null <- p_c * (1 - p_c) / n + (p_c - b) * (1 - p_c + b) / n_e
    (b - m) / sqrt(v_null) - z_a + z_matched
  }
  stats::uniroot(f, c(0, p_c - 1e-6), tol = 1e-12)[["root"]]
}
fig_s3a[["crossing_normal_null"]] <- mapply(crossing_normal_null_s3a, fig_s3a[["n"]],
                                            fig_s3a[["n_e"]], fig_s3a[["margin"]])
# the convention gives the smallest exact power of the RCT that is at least that of the SAT,
# and the exact power of the SAT without bias is that of Figure S2
stopifnot(all(fig_s3a[["rct_exact_power"]] >= fig_s3a[["sat_exact_power"]]))
assert_close(fig_s3a[["sat_exact_power"]], fig_s2[["ext_exact_power"]], 1e-12,
             "Figure S3: the exact power of the SAT without bias is that of Figure S2")
save_csv(fig_s3a, "figS3_binary_crossing.csv")

# (b) Time-to-event endpoint, patient-level simulation in the setting of Table 3 (hazard ratio
# 0.6, N_E = 2 N, N = 30, 60, 90, 120). Survival is exponential, and every patient is followed
# for the same time, chosen so that a patient on the control has an event with probability 0.7;
# a patient on the experimental treatment then has an event with probability 1 - 0.3^0.6.
# A bias b multiplies the hazard of the external controls by exp(b). Each group is summarized
# by the logarithm of its total follow-up time divided by its number of events, the maximum
# likelihood estimate of the logarithm of the mean survival time under exponential survival,
# with variance estimated by the reciprocal of the number of events; a sample in which a group
# has no event does not lead to rejection. The RCT is matched to the SAT by the critical value
# at which its simulated power equals the simulated power of the SAT without bias, and its type
# I error rate is its simulated rejection rate at hazard ratio 1 with this critical value. The
# type I error rate of the SAT is simulated on a grid of biases with common random numbers,
# and the crossing is found by linear interpolation. The margins are 0 and 0.1. The
# approximation m_crit is given with the common event probability 0.7 of Table 3 and with the
# event probability of each group, and the crossing is also approximated with the variance under
# the null hypothesis. Simulation with 100,000 samples per size and a fixed seed.
reps_s3 <- 100000
chunk_s3 <- 10000
margins_s3 <- c(0, 0.1)
bias_grid_s3 <- round(seq(0.10, 0.30, by = 0.005), 3)
lambda_c_s3 <- -log(1 - prob_event_s7)
lambda_t_s3 <- hazard_ratio_s7 * lambda_c_s3
prob_event_t_s3 <- 1 - exp(-lambda_t_s3)
delta_s3 <- -log(hazard_ratio_s7)
z_s3 <- qnorm(alpha, lower.tail = FALSE)
summarize_group <- function(k, n, lambda, e = NULL) {
  if (is.null(e)) e <- matrix(stats::rexp(k * n), nrow = k)
  x <- e / lambda
  d <- rowSums(x <= 1)
  list(mu = ifelse(d > 0, log(rowSums(pmin(x, 1)) / pmax(d, 1)), NA_real_), d = d)
}
wald <- function(g1, g2, m) {
  ok <- g1[["d"]] > 0 & g2[["d"]] > 0
  z <- rep(-Inf, length(ok))
  z[ok] <- (g1[["mu"]][ok] - g2[["mu"]][ok] - m) / sqrt(1 / g1[["d"]][ok] + 1 / g2[["d"]][ok])
  z
}
set.seed(20261104)
fig_s3b <- do.call(rbind, lapply(sort(unique(table3[["n"]])), function(n) {
  n_e <- 2 * n
  n_half <- n / 2
  z_rct_alt <- z_rct_null <- numeric(0)
  z_sat_alt <- lapply(margins_s3, function(m) numeric(0))
  reject_sat_null <- lapply(margins_s3, function(m) numeric(length(bias_grid_s3)))
  for (chunk in seq_len(reps_s3 / chunk_s3)) {
    z_rct_alt <- c(z_rct_alt, wald(summarize_group(chunk_s3, n_half, lambda_t_s3),
                                   summarize_group(chunk_s3, n_half, lambda_c_s3), 0))
    z_rct_null <- c(z_rct_null, wald(summarize_group(chunk_s3, n_half, lambda_c_s3),
                                     summarize_group(chunk_s3, n_half, lambda_c_s3), 0))
    g_t <- summarize_group(chunk_s3, n, lambda_t_s3)
    g_e <- summarize_group(chunk_s3, n_e, lambda_c_s3)
    for (i in seq_along(margins_s3)) {
      z_sat_alt[[i]] <- c(z_sat_alt[[i]], wald(g_t, g_e, margins_s3[i]))
    }
    g_t0 <- summarize_group(chunk_s3, n, lambda_c_s3)
    e0 <- matrix(stats::rexp(chunk_s3 * n_e), nrow = chunk_s3)
    for (j in seq_along(bias_grid_s3)) {
      g_e0 <- summarize_group(chunk_s3, n_e, lambda_c_s3 * exp(bias_grid_s3[j]), e = e0)
      for (i in seq_along(margins_s3)) {
        reject_sat_null[[i]][j] <- reject_sat_null[[i]][j] +
          sum(wald(g_t0, g_e0, margins_s3[i]) > z_s3)
      }
    }
  }
  do.call(rbind, lapply(seq_along(margins_s3), function(i) {
    m <- margins_s3[i]
    power_sat_sim <- mean(z_sat_alt[[i]] > z_s3)
    crit <- stats::quantile(z_rct_alt, 1 - power_sat_sim, names = FALSE)
    type1_rct <- mean(z_rct_null > crit)
    type1_sat <- reject_sat_null[[i]] / reps_s3
    j <- which(type1_sat >= type1_rct)[1]
    stopifnot(!is.na(j), j > 1)
    crossing <- bias_grid_s3[j - 1] + (type1_rct - type1_sat[j - 1]) *
      (bias_grid_s3[j] - bias_grid_s3[j - 1]) / (type1_sat[j] - type1_sat[j - 1])
    v_groups <- 1 / (prob_event_t_s3 * n) + 1 / (prob_event_s7 * n_e)
    w_groups <- 2 / (prob_event_t_s3 * n) + 2 / (prob_event_s7 * n)
    j_m <- match(m, bias_grid_s3)
    data.frame(
      alpha = alpha, n = n, n_e = n_e, hazard_ratio = hazard_ratio_s7,
      prob_event_c = prob_event_s7, prob_event_t = prob_event_t_s3, margin = m,
      reps = reps_s3, sat_sim_power = power_sat_sim, rct_nominal_level = 1 - pnorm(crit),
      rct_sim_power = mean(z_rct_alt > crit), rct_sim_type1 = type1_rct,
      alpha_matched_groups = matched_alpha(n, delta_s3, n_e, m,
                                           endpoint_sd("survival", prob_event = prob_event_t_s3),
                                           endpoint_sd("survival", prob_event = prob_event_s7),
                                           alpha)[["alpha_matched"]],
      crossing_sim = crossing,
      critical_margin_common = critical_margin(n, delta_s3, n_e,
                                               endpoint_sd("survival", prob_event = prob_event_s7)),
      critical_margin_groups = delta_s3 * (1 - sqrt(v_groups / w_groups)),
      sat_sim_type1_at_margin = if (is.na(j_m)) NA_real_ else type1_sat[j_m]
    )
  }))
}))
assert_close(fig_s3b[["critical_margin_groups"]],
             critical_margin(fig_s3b[["n"]], delta_s3, fig_s3b[["n_e"]],
                             endpoint_sd("survival", prob_event = prob_event_t_s3),
                             endpoint_sd("survival", prob_event = prob_event_s7)), 1e-12,
             "Figure S3: critical_margin() gives m_crit with the event probability of each group")
k <- match(paste(fig_s3b[["n"]]), paste(table3[["n"]]))
assert_close(fig_s3b[["critical_margin_common"]], table3[["critical_margin"]][k], 1e-12,
             "Figure S3: with the common event probability, m_crit is that of Table 3")
# mean of the simulated crossings over the sizes, for each margin
fig_s3b[["crossing_sim_mean"]] <- stats::ave(fig_s3b[["crossing_sim"]], fig_s3b[["margin"]])
fig_s3b[["sat_power_minus_rct_power"]] <- fig_s3b[["sat_sim_power"]] - fig_s3b[["rct_sim_power"]]
# normal approximation of the crossing with the variance under the null hypothesis for the type I
# error rate of the SAT, in which its patients have the event probability of the controls and the
# external controls the event probability raised by the bias, and with the variances under the
# alternative hypothesis for the level of the RCT matched to the power of the SAT
crossing_normal_null_s3 <- function(n, n_e, m) {
  v_alt <- 1 / (prob_event_t_s3 * n) + 1 / (prob_event_s7 * n_e)
  w_alt <- 2 / (prob_event_t_s3 * n) + 2 / (prob_event_s7 * n)
  z_matched <- z_s3 + delta_s3 / sqrt(w_alt) - (delta_s3 - m) / sqrt(v_alt)
  f <- function(b) {
    prob_event_e <- 1 - exp(-lambda_c_s3 * exp(b))
    v_null <- 1 / (prob_event_s7 * n) + 1 / (prob_event_e * n_e)
    (b - m) / sqrt(v_null) - z_s3 + z_matched
  }
  stats::uniroot(f, c(0, 1), tol = 1e-12)[["root"]]
}
fig_s3b[["crossing_normal_null"]] <- mapply(crossing_normal_null_s3, fig_s3b[["n"]],
                                            fig_s3b[["n_e"]], fig_s3b[["margin"]])
save_csv(fig_s3b, "figS3_survival_crossing.csv")

elapsed <- (proc.time() - start_time)[["elapsed"]]
message(sprintf("data_generate_supplement.R finished in %.1f seconds.", elapsed))
