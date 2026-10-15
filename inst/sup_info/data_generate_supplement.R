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
tab_s3[["margin_avg"]] <- tab_s3[["bias_mean"]] + z_alpha_s3 * (sd_bias_avg_s3 - sqrt(v_sat_s3))
tab_s3[["margin_q95"]] <- tab_s3[["bias_mean"]] + qnorm(0.95) * tab_s3[["tau_used"]]
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
assert_close(tab_s3[["type1_sat_margin_avg"]], alpha, 1e-12,
             "Table S4: the type I error rate averaged over the bias equals alpha at m_avg")
assert_close(tab_s3[["power_sat_margin_avg_averaged"]],
             pnorm(tab_s3[["delta"]] / sd_bias_avg_s3 - z_alpha_s3), 1e-12,
             "Table S5: closed form of the averaged power of the SAT with m_avg")
assert_close(tab_s3[["power_sat_margin_q95_averaged"]],
             pnorm((tab_s3[["delta"]] - qnorm(0.95) * tab_s3[["tau_used"]] -
                      z_alpha_s3 * sqrt(v_sat_s3)) / sd_bias_avg_s3), 1e-12,
             "Table S5: closed form of the averaged power of the SAT with m_0.95")
assert_close(tab_s3[["power_sat_margin_avg_averaged"]] > tab_s3[["power_rct"]],
             tab_s3[["tau_below_breakeven"]], 0,
             paste("Table S5: averaged over the bias, the SAT with m_avg has more power than the",
                   "RCT exactly when tau is below b*"))
save_csv(tab_s3, "tableS3_bias_uncertainty.csv")

elapsed <- (proc.time() - start_time)[["elapsed"]]
message(sprintf("data_generate_supplement.R finished in %.1f seconds.", elapsed))
