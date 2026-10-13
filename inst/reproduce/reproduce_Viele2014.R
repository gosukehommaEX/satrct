# Reproduce Viele K, Berry S, Neuenschwander B, et al. (2014). Use of historical control data
# for assessing treatment effects in clinical trials. Pharm Stat 13: 41-54.
#
# Setting (pp.42-43): 200 patients per group, historical control data 65/100, one-sided
# alpha = 0.025, power evaluated at p_T = p_0 + 0.12. The single-arm trial treats 200 patients
# and tests H0: p = 0.65 with the exact binomial test.
#
# The paper states that the separate and pooled analyses use the Fisher exact test, but its
# figures are reproduced by the one-sided chi-squared test with the Yates correction
# (stats::prop.test with correct = TRUE), which oc_binary_exact() computes with
# test = "pearson_cc". The only printed number is the critical value of the single-arm trial;
# the other rows check statements in the text and values read from the figures (INFO).
#
# Run from the package root after devtools::load_all().
# Output: inst/reproduce/results/reproduce_Viele2014.csv

source("inst/validation/judge.R")
check_loaded()
script <- "reproduce_Viele2014"
yates_note <- "Yates-corrected chi-squared test, which reproduces the figures (the text states Fisher)"

sat <- oc_sat_binary_exact(200, p_t = c(0.65, 0.70, 0.77), p0 = 0.65)
sat70 <- oc_sat_binary_exact(200, p_t = c(0.70, 0.75), p0 = 0.65, margin = 0.05)
separate_power <- oc_binary_exact(200, 200, p_t = 0.77, p_c = 0.65, test = "pearson_cc")
pooled <- function(p_c, delta) {
  oc_binary_exact(200, 200, p_t = p_c + delta, p_c = p_c, test = "pearson_cc", n_ext = 100,
                  y_ext = 65)
}

res <- rbind(
  judge(script, "sat_critical_value", "Single-arm trial: success with at least 144 responders",
        "p.43", 144, sat[["crit"]][1], "abs", tol = 0),
  judge(script, "sat_type1_p0_0.70", "Single-arm type I error at a true control rate of 0.70",
        "p.45", NA, sat[["prob_reject"]][2], "info",
        note = "Text: the type I error 'exceeds 20%'"),
  judge(script, "sat_power_p0_0.65", "Single-arm power at p_T = 0.77", "Figure 2, p.44", NA,
        sat[["prob_reject"]][3], "info", note = "Read from the figure: about 0.957"),
  judge(script, "sat_null_0.70_type1", "Single-arm test of H0: p = 0.70, type I error at 0.70",
        "p.45", NA, sat70[["prob_reject"]][1], "info",
        note = "Text: 'may afford type I error control up to a true current parameter of 0.7'"),
  judge(script, "sat_null_0.70_type1_at_0.75",
        "Single-arm test of H0: p = 0.70, type I error at 0.75", "p.45", NA,
        sat70[["prob_reject"]][2], "info",
        note = "Text: 'the type I error rate inflates greatly beyond whatever null value is chosen'"),
  judge(script, "separate_power_p0_0.65", "Separate analysis, power at p_0 = 0.65", "p.45", NA,
        separate_power, "info", note = paste("Text: 'around 70%';", yates_note)),
  judge(script, "pooled_type1_p0_0.80", "Pooled analysis, type I error at p_0 = 0.80", "p.45",
        NA, pooled(0.80, 0), "info",
        note = paste("Text: 'approaches 20%'; read from Figure 2: 0.193-0.194;", yates_note)),
  judge(script, "pooled_type1_p0_0.75", "Pooled analysis, type I error at p_0 = 0.75",
        "Figure 2, p.44", NA, pooled(0.75, 0), "info",
        note = paste("Read from the figure: 0.086-0.088;", yates_note)),
  judge(script, "pooled_power_p0_0.65", "Pooled analysis, power at p_0 = 0.65",
        "Figure 2, p.44", NA, pooled(0.65, 0.12), "info",
        note = paste("Read from the figure: about 0.82;", yates_note)),
  judge(script, "pooled_power_p0_0.50", "Pooled analysis, power at p_0 = 0.50",
        "Figure 2, p.44", NA, pooled(0.50, 0.12), "info",
        note = paste("Read from the figure: about 0.30;", yates_note))
)

finalize_results(res, "inst/reproduce/results")
