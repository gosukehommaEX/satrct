# Reproduce Kopp-Schneider A, Wiesenfarth M, Held L, Calderazzo S (2024). Simulating and
# reporting frequentist operating characteristics of clinical trials that borrow external
# information. Pharm Stat 23: 4-19.
#
# The power of the one-arm and two-arm normal tests without borrowing, and of the tests
# "calibrated to borrowing" (the test without borrowing at the type I error alpha_B of the
# test with borrowing), are power_sat() with n_e = Inf and power_rct() at the relevant level.
# The values of alpha_B in Section 4.1 are Monte Carlo estimates printed to three decimals,
# so the calibrated power is recomputed over the rounding interval of alpha_B (rule "range").
#
# Run from the package root after devtools::load_all().
# Output: inst/reproduce/results/reproduce_KoppSchneider2024.csv

source("inst/validation/judge.R")
check_loaded()
script <- "reproduce_KoppSchneider2024"

res <- rbind(
  # One-arm trial, n = 25, theta_1 = 0.5, unit variance, alpha = 0.025 (Section 3.2, p.7)
  judge(script, "power_one_arm", "Power of the one-arm normal test, n = 25, effect 0.5",
        "p.7", 0.705, power_sat(25, 0.5), "round", digits = 3),
  # Two-arm trial, 15 patients per group, effect 1 (Section 3.3, p.11)
  judge(script, "power_two_arm", "Power of the two-arm normal test, 15 per group, effect 1",
        "p.11", 0.78, power_rct(30, 1), "round", digits = 2)
)

calibrated <- data.frame(
  item = c("calibrated_power_fixedPP_thetaE0", "calibrated_power_fixedPP_thetaE0.5",
           "calibrated_power_EB_thetaE0", "calibrated_power_EB_thetaE0.5"),
  description = c("Calibrated power, fixed power prior, theta_E = 0",
                  "Calibrated power, fixed power prior, theta_E = 0.5",
                  "Calibrated power, empirical Bayes power prior, theta_E = 0",
                  "Calibrated power, empirical Bayes power prior, theta_E = 0.5"),
  alpha_b = c(0.017, 0.114, 0.030, 0.113),
  printed = c(0.649, 0.848, 0.730, 0.901),
  stringsAsFactors = FALSE
)
for (i in seq_len(nrow(calibrated))) {
  a <- calibrated[["alpha_b"]][i]
  res <- rbind(res, judge(
    script, calibrated[["item"]][i], calibrated[["description"]][i], "Section 4.1, p.14",
    calibrated[["printed"]][i], power_sat(25, 0.5, alpha = a), "range",
    value_low = power_sat(25, 0.5, alpha = a - 0.0005),
    value_high = power_sat(25, 0.5, alpha = a + 0.0005),
    note = sprintf("alpha_B printed as %.3f; range over [%.4f, %.4f]", a, a - 0.0005, a + 0.0005)))
}

finalize_results(res, "inst/reproduce/results")
