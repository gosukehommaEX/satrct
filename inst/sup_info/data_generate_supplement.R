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

elapsed <- (proc.time() - start_time)[["elapsed"]]
message(sprintf("data_generate_supplement.R finished in %.1f seconds.", elapsed))
