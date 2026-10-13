# Reproduce Tang H, Foster NR, Grothey A, et al. (2010). Comparison of error rates in single-arm
# versus randomized phase II cancer clinical trials. J Clin Oncol 28: 1936-1941.
#
# Table 1 (p.1937) lists nine single-arm rules of the form "reject the null hypothesis if at
# least c of 50 patients respond", with alpha and beta printed to two decimals. They are
# described as observed in simulations from patient data, but they equal the exact binomial
# probabilities of the rules, which oc_sat_binary_exact() computes with a given critical value.
# The other results of the paper depend on non-public patient data or on unstated settings and
# are not reproduced.
#
# Run from the package root after devtools::load_all().
# Output: inst/reproduce/results/reproduce_Tang2010.csv

source("inst/validation/judge.R")
check_loaded()
script <- "reproduce_Tang2010"

table1 <- data.frame(
  endpoint = rep(c("response", "os6", "ttp6"), each = 3),
  p0 = c(0.30, 0.35, 0.40, 0.75, 0.80, 0.85, 0.50, 0.55, 0.60),
  p1 = c(0.45, 0.50, 0.55, 0.85, 0.90, 0.95, 0.65, 0.70, 0.75),
  crit = c(19, 22, 25, 41, 44, 46, 30, 32, 35),
  alpha = c(0.14, 0.12, 0.10, 0.16, 0.10, 0.11, 0.10, 0.13, 0.10),
  beta = c(0.13, 0.16, 0.20, 0.21, 0.23, 0.10, 0.19, 0.14, 0.16)
)

res <- NULL
for (i in seq_len(nrow(table1))) {
  oc <- oc_sat_binary_exact(50, p_t = table1[["p1"]][i], p0 = table1[["p0"]][i],
                            crit = table1[["crit"]][i])
  label <- sprintf("%s_%d_v_%d_crit%d", table1[["endpoint"]][i], round(100 * table1[["p0"]][i]),
                   round(100 * table1[["p1"]][i]), table1[["crit"]][i])
  res <- rbind(
    res,
    judge(script, paste0(label, "_alpha"),
          sprintf("Alpha of the rule 'at least %d of 50', %.2f versus %.2f",
                  table1[["crit"]][i], table1[["p0"]][i], table1[["p1"]][i]),
          "Table 1, p.1937", table1[["alpha"]][i], oc[["size"]], "round", digits = 2),
    judge(script, paste0(label, "_beta"),
          sprintf("Beta of the rule 'at least %d of 50', %.2f versus %.2f",
                  table1[["crit"]][i], table1[["p0"]][i], table1[["p1"]][i]),
          "Table 1, p.1937", table1[["beta"]][i], 1 - oc[["prob_reject"]], "round", digits = 2)
  )
}

finalize_results(res, "inst/reproduce/results")
