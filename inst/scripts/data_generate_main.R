# Compute the results shown in the figures and tables of the main text and save them to
# inst/scripts/results/data/. The figures and tables are drawn from these files by
# table_and_figure_manuscript.R, so that the layout can be changed without recomputing.
#
# Settings common to all results unless stated otherwise: one-sided significance level
# alpha = 0.025, z-tests, common standard deviation sigma = 1 and 1:1 randomization in the
# randomized controlled trial (RCT). The single-arm trial (SAT) gives the experimental
# treatment to all N patients and compares them with N_E external controls; N_E = Inf is the
# comparison with a fixed threshold.
#
# Before saving, the script checks relations that hold by construction and stops if one of
# them fails.
#
# Run from the package root after devtools::load_all().
# Output: inst/scripts/results/data/*.csv

if (!exists("power_sat", mode = "function")) {
  stop("Run devtools::load_all() (or library(satrct)) before sourcing this script.",
       call. = FALSE)
}
start_time <- proc.time()
out_dir <- "inst/scripts/results/data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
alpha <- 0.025
z_alpha <- qnorm(alpha, lower.tail = FALSE)

# Stop when two sets of values that agree by construction differ by more than tol
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

# ---- Figure 1(a): break-even bias against the total sample size ----
fig1a <- expand.grid(n = 10:200, n_e = c(Inf, 200, 100, 50), KEEP.OUT.ATTRS = FALSE)
fig1a[["breakeven_bias"]] <- breakeven_bias(fig1a[["n"]], fig1a[["n_e"]])
# b* exists only when the SAT without bias has the smaller variance, that is, N < 3 N_E
# (the boundary N = 3 N_E, where b* = 0 up to rounding, is excluded from the check)
k <- fig1a[["n"]] != 3 * fig1a[["n_e"]]
assert_close(is.na(fig1a[["breakeven_bias"]][k]), fig1a[["n"]][k] > 3 * fig1a[["n_e"]][k], 0,
             "Figure 1(a): b* is missing exactly when N > 3 N_E")
save_csv(fig1a, "fig1a_breakeven_bias.csv")

# ---- Figure 1(b) and Table 1: results that depend only on the power of the RCT and N / N_E ----
# With a common standard deviation and 1:1 randomization, the power of the SAT, the matched
# level alpha', b* / delta and m_crit / delta depend on N, N_E and delta only through the power
# of the RCT and the ratio N / N_E. They are computed at a reference size n_ref, with delta set
# so that the RCT has the given power, and the computation is repeated at another reference
# size to check that the results do not depend on it.
relative_measures <- function(power_target, ratio, n_ref) {
  g <- expand.grid(power_rct = power_target, ratio = ratio, KEEP.OUT.ATTRS = FALSE)
  n_e <- ifelse(g[["ratio"]] == 0, Inf, n_ref / g[["ratio"]])
  delta <- (z_alpha + qnorm(g[["power_rct"]])) * sqrt(mse_rct(n_ref))
  ma <- matched_alpha(n_ref, delta, n_e, margin = 0, alpha = alpha)
  data.frame(
    alpha = alpha,
    n_ref = n_ref,
    ratio = g[["ratio"]],
    n_e = n_e,
    power_rct = g[["power_rct"]],
    delta = delta,
    power_rct_check = power_rct(n_ref, delta, alpha = alpha),
    power_sat = power_sat(n_ref, delta, n_e, alpha = alpha),
    alpha_matched = ma[["alpha_matched"]],
    breakeven_bias_rel = breakeven_bias(n_ref, n_e) / delta,
    critical_margin_rel = critical_margin(n_ref, delta, n_e) / delta
  )
}
check_relative <- function(x, y, label) {
  assert_close(x[["power_rct_check"]], x[["power_rct"]], 1e-12,
               paste0(label, ": the RCT has the target power"))
  cols <- c("power_sat", "alpha_matched", "breakeven_bias_rel", "critical_margin_rel")
  assert_close(as.matrix(x[cols]), as.matrix(y[cols]), 1e-10,
               paste0(label, ": the results do not depend on the reference size"))
  k <- x[["ratio"]] == 0
  assert_close(x[["alpha_matched"]][k], x[["power_rct"]][k], 1e-10,
               paste0(label, ": against a threshold, alpha' equals the power of the RCT"))
}

power_grid <- round(seq(0.10, 0.90, by = 0.01), 2)
ratio_levels <- c(0, 0.5, 1, 2)
fig1b <- relative_measures(power_grid, ratio_levels, n_ref = 100)
check_relative(fig1b, relative_measures(power_grid, ratio_levels, n_ref = 30), "Figure 1(b)")
save_csv(fig1b, "fig1b_relative.csv")

table1 <- relative_measures(c(0.2, 0.35, 0.5, 0.8), ratio_levels, n_ref = 100)
check_relative(table1, relative_measures(c(0.2, 0.35, 0.5, 0.8), ratio_levels, n_ref = 30),
               "Table 1")
save_csv(table1, "table1_relative.csv")

# ---- Figure 2: power against the total sample size ----
delta_main <- 0.5
fig2 <- compare_designs(n = 10:200, delta = delta_main, n_e = c(Inf, 100),
                        margin = delta_main * c(0, 0.25, 0.5, 0.75), alpha = alpha)
fig2[["margin_rel"]] <- fig2[["margin"]] / fig2[["delta"]]
fig2[["alpha"]] <- alpha
# Against a threshold, m_crit = delta / 2 for every N, so the SAT with margin delta / 2 has
# the power of the RCT at every N
k <- is.infinite(fig2[["n_e"]]) & fig2[["margin_rel"]] == 0.5
assert_close(fig2[["power_sat"]][k], fig2[["power_rct"]][k], 1e-12,
             paste("Figure 2: against a threshold, the SAT with margin delta / 2 has the power",
                   "of the RCT"))
save_csv(fig2[c("alpha", "n", "delta", "n_e", "margin", "margin_rel", "power_rct", "power_sat",
                "critical_margin")],
         "fig2_power.csv")

# ---- Figure 3: type I error rate of the SAT against the bias ----
n_fig3 <- 40
n_e_fig3 <- c(Inf, 80)
margin_fig3 <- delta_main * c(0, 0.15, 0.3)
fig3 <- compare_designs(n = n_fig3, delta = delta_main, n_e = n_e_fig3,
                        bias = delta_main * seq(0, 1, by = 0.005), margin = margin_fig3,
                        alpha = alpha)
fig3[["margin_rel"]] <- fig3[["margin"]] / fig3[["delta"]]
fig3[["bias_rel"]] <- fig3[["bias"]] / fig3[["delta"]]
fig3[["alpha"]] <- alpha
save_csv(fig3[c("alpha", "n", "delta", "n_e", "margin", "margin_rel", "bias", "bias_rel",
                "type1_sat")],
         "fig3_type1.csv")

# Crossing points: for every margin, the type I error rate of the SAT reaches the level
# alpha'(m) of the RCT with the same power at the same bias, m_crit
cross <- expand.grid(n_e = n_e_fig3, margin = margin_fig3, KEEP.OUT.ATTRS = FALSE)
ma <- matched_alpha(n_fig3, delta_main, cross[["n_e"]], cross[["margin"]], alpha = alpha)
fig3_cross <- data.frame(
  alpha = alpha,
  n = n_fig3,
  delta = delta_main,
  n_e = cross[["n_e"]],
  margin = cross[["margin"]],
  margin_rel = cross[["margin"]] / delta_main,
  power_rct = power_rct(n_fig3, delta_main, alpha = alpha),
  power_sat = ma[["power"]],
  alpha_matched = ma[["alpha_matched"]],
  bias_crossing = ma[["bias_crossing"]],
  critical_margin = critical_margin(n_fig3, delta_main, cross[["n_e"]]),
  breakeven_bias = breakeven_bias(n_fig3, cross[["n_e"]])
)
fig3_cross[["type1_at_crossing"]] <- power_sat(n_fig3, 0, fig3_cross[["n_e"]],
                                               fig3_cross[["bias_crossing"]],
                                               fig3_cross[["margin"]], alpha = alpha)
fig3_cross[["power_rct_matched"]] <- power_rct(n_fig3, delta_main,
                                               alpha = fig3_cross[["alpha_matched"]])
assert_close(fig3_cross[["power_rct_matched"]], fig3_cross[["power_sat"]], 1e-10,
             "Figure 3: the RCT at level alpha' has the power of the SAT")
assert_close(fig3_cross[["type1_at_crossing"]], fig3_cross[["alpha_matched"]], 1e-10,
             "Figure 3: the type I error rate of the SAT equals alpha' at the crossing")
assert_close(fig3_cross[["bias_crossing"]], fig3_cross[["critical_margin"]], 1e-12,
             "Figure 3: the crossing is at m_crit for every margin")
save_csv(fig3_cross, "fig3_crossing.csv")

# ---- Table 2: three endpoints on their own scales ----
p_c <- 0.30
p_t <- 0.55
hazard_ratio <- 0.6
prob_event <- 0.7
sd_t_bin <- endpoint_sd("binary", p = p_t)
sd_c_bin <- endpoint_sd("binary", p = p_c)
sd_surv <- endpoint_sd("survival", prob_event = prob_event)
table2 <- data.frame(
  alpha = alpha,
  endpoint = c("continuous", "binary", "binary", "survival"),
  n = c(40, 40, 40, 60),
  n_e = c(80, 80, Inf, 120),
  delta = c(delta_main, p_t - p_c, p_t - p_c, -log(hazard_ratio)),
  sigma_t = c(1, sd_t_bin, sd_t_bin, sd_surv),
  sigma_c = c(1, sd_c_bin, sd_c_bin, sd_surv),
  p_t = c(NA, p_t, p_t, NA),
  p_c = c(NA, p_c, p_c, NA),
  hazard_ratio = c(NA, NA, NA, hazard_ratio),
  prob_event = c(NA, NA, NA, prob_event),
  stringsAsFactors = FALSE
)
ma <- matched_alpha(table2[["n"]], table2[["delta"]], table2[["n_e"]], 0, table2[["sigma_t"]],
                    table2[["sigma_c"]], alpha)
table2[["power_rct"]] <- power_rct(table2[["n"]], table2[["delta"]], table2[["sigma_t"]],
                                   table2[["sigma_c"]], alpha)
table2[["power_sat"]] <- power_sat(table2[["n"]], table2[["delta"]], table2[["n_e"]],
                                   sigma_t = table2[["sigma_t"]],
                                   sigma_c = table2[["sigma_c"]], alpha = alpha)
table2[["alpha_matched"]] <- ma[["alpha_matched"]]
table2[["breakeven_bias"]] <- breakeven_bias(table2[["n"]], table2[["n_e"]],
                                             table2[["sigma_t"]], table2[["sigma_c"]])
table2[["critical_margin"]] <- critical_margin(table2[["n"]], table2[["delta"]],
                                               table2[["n_e"]], table2[["sigma_t"]],
                                               table2[["sigma_c"]])
assert_close(ma[["power"]], table2[["power_sat"]], 1e-12,
             "Table 2: matched_alpha() and power_sat() give the same power of the SAT")

# Exact rejection probabilities for the binary endpoint: score test for two proportions
# (RCT with N / 2 per group, SAT with N_E external controls) and exact binomial test against
# the threshold p_c
k_ext <- table2[["endpoint"]] == "binary" & is.finite(table2[["n_e"]])
k_thr <- table2[["endpoint"]] == "binary" & is.infinite(table2[["n_e"]])
n_bin <- table2[["n"]][k_ext]
table2[["power_rct_exact"]] <- NA_real_
table2[["power_sat_exact"]] <- NA_real_
table2[["power_rct_exact"]][table2[["endpoint"]] == "binary"] <-
  oc_binary_exact(n_bin / 2, n_bin / 2, p_t, p_c, alpha = alpha)
table2[["power_sat_exact"]][k_ext] <- oc_binary_exact(n_bin, table2[["n_e"]][k_ext], p_t, p_c,
                                                      alpha = alpha)
table2[["power_sat_exact"]][k_thr] <- oc_sat_binary_exact(table2[["n"]][k_thr], p_t, p_c,
                                                          alpha = alpha)[["prob_reject"]]
save_csv(table2, "table2_illustration.csv")

elapsed <- (proc.time() - start_time)[["elapsed"]]
message(sprintf("data_generate_main.R finished in %.1f seconds.", elapsed))
