# Reproduce Pocock SJ (1976). The combination of randomized and historical controls in clinical
# trials. J Chronic Dis 29: 175-188.
#
# Example 1 (pp.182-183) combines randomized (R) and historical (H) controls for the log mean
# survival, whose variance is 1 / (number of deaths). Formulae (3) and (4) are the hybrid
# estimator of mse_hybrid() with sigma = 1, n_c = deaths in R, n_e = deaths in H and
# tau = sigma_delta, the weight of the randomized controls being the default optimal weight.
# Example 2 (pp.186-187) uses formula (7), which optimal_nc() implements.
#
# Run from the package root after devtools::load_all().
# Output: inst/reproduce/results/reproduce_Pocock1976.csv

source("inst/validation/judge.R")
check_loaded()
script <- "reproduce_Pocock1976"

# ---- Example 1 (Table 2, p.182) ----
deaths_t <- 49
deaths_r <- 59
deaths_h <- 44
mean_t <- 27.1
mean_r <- 28.4
mean_h <- 23.2
example1 <- function(sd2) {
  # n is the number of treated deaths plus n_c, so that the first term of the mean squared
  # error is 1 / deaths_t, the variance of log mean survival on the new treatment
  hyb <- mse_hybrid(deaths_t + deaths_r, n_c = deaths_r, n_e = deaths_h, tau = sqrt(sd2))
  w <- hyb[["weight"]]
  log_yc <- w * log(mean_r) + (1 - w) * log(mean_h)
  list(log_yc = log_yc, var_log_mr = hyb[["mse"]] - 1 / deaths_t, sd_diff = sqrt(hyb[["mse"]]),
       diff = log_yc - log(mean_t))
}
e1 <- example1(0.01)
e1b <- example1(0.04)
res <- rbind(
  judge(script, "ex1_log_yc", "Posterior mean of log mean survival on DTIC, sigma_delta^2 = 0.01",
        "p.182", 3.28, e1[["log_yc"]], "round", digits = 2),
  judge(script, "ex1_var_log_mr", "Posterior variance of log mean survival on DTIC",
        "p.182", 0.0112, e1[["var_log_mr"]], "round", digits = 4),
  judge(script, "ex1_yc", "Combined estimate of mean survival on DTIC (weeks)",
        "p.182", 26.5, exp(e1[["log_yc"]]), "round", digits = 1),
  judge(script, "ex1_mean_diff", "Posterior mean of log(m_R / m_T)",
        "p.183", -0.02, e1[["diff"]], "round", digits = 2),
  judge(script, "ex1_sd_diff", "Posterior SD of log(m_R / m_T)",
        "p.183", 0.18, e1[["sd_diff"]], "round", digits = 2,
        note = "Printed as '0.0112 + 0.0204 = 0.18'; the square root is missing in the print"),
  judge(script, "ex1_normal_deviate", "Normal deviate", "p.183", -0.1,
        e1[["diff"]] / e1[["sd_diff"]], "round", digits = 1),
  judge(script, "ex1_yc_sd2_0.04", "Combined estimate of mean survival, sigma_delta^2 = 0.04",
        "p.183", 27.2, exp(e1b[["log_yc"]]), "round", digits = 1)
)

# ---- Example 2 (pp.186-187), N = 200 ----
# (a) Tumour response: N_H = 83, sigma = 49.7 (printed value of sqrt(45 * 55)), sigma_delta = 3 or 5
tumour <- optimal_nc(200, n_e = 83, tau = c(3, 5), sigma_t = 49.7)[["n_c"]]
# (b) Survival: N_H = 88, sigma = 1, sigma_delta = 0.05 or 0.1
survival <- optimal_nc(200, n_e = 88, tau = c(0.05, 0.1))[["n_c"]]
res <- rbind(
  res,
  judge(script, "ex2_tumour_sd3", "Optimal N_R, tumour response, sigma_delta = 3%",
        "p.186", 68.1, tumour[1], "round", digits = 1),
  judge(script, "ex2_tumour_sd5", "Optimal N_R, tumour response, sigma_delta = 5%",
        "p.187", 77, tumour[2], "round", digits = 0),
  judge(script, "ex2_survival_sd0.05", "Optimal N_R, survival, sigma_delta = 0.05",
        "p.187", 63.9, survival[1], "round", digits = 1,
        note = "The text says sigma_delta = 0.56 but the displayed formula uses 0.05, which gives 63.9"),
  judge(script, "ex2_survival_sd0.1", "Optimal N_R, survival, sigma_delta = 0.1",
        "p.187", 77, survival[2], "round", digits = 0)
)

finalize_results(res, "inst/reproduce/results")
