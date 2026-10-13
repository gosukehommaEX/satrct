# Reproduce Collignon O, Schritz A, Senn SJ, Spezia R (2020).
# Stat Methods Med Res 29: 1960-1971.
#
# Equation (6) (p.1964) gives the number of patients n_c of the current trial above which a
# randomized trial is a better strategy than allocating all patients to the experimental
# treatment and comparing them with k_h historical studies of n_h patients each, when the
# between-study variance is tau_b^2. In breakeven_n() this corresponds to
#   n_e = k_h * n_h  and  tau^2 = (1 + 1 / k_h) * tau_b^2.
#
# Run from the package root after devtools::load_all().
# Output: inst/reproduce/results/reproduce_Collignon2020.csv

source("inst/validation/judge.R")
check_loaded()
script <- "reproduce_Collignon2020"

# Worked example (p.1964): sigma^2 = 1, 10 historical arms of 100 patients,
# between-study variance 0.02: "running a randomised clinical trial with 130 patients"
k_h <- 10
n_h <- 100
tau_b2 <- 0.02
res <- judge(script, "eq6_example",
             "Sample size of the RCT with the same variance (eq. 6 example)", "p.1964",
             130, breakeven_n(0, n_e = k_h * n_h, tau = sqrt((1 + 1 / k_h) * tau_b2)),
             "round", digits = 0)

# Figure 1 (p.1964): values at zero between-study variance, read from the plot
# (about 30, 300 and 300; the curve for k_h = 10, n_h = 100 starts above the plotted range)
fig <- expand.grid(k_h = c(1, 10), n_h = c(10, 100))
for (i in seq_len(nrow(fig))) {
  res <- rbind(res, judge(
    script, sprintf("fig1_intercept_kh%d_nh%d", fig[["k_h"]][i], fig[["n_h"]][i]),
    sprintf("Figure 1 at zero between-study variance, k_h = %d, n_h = %d",
            fig[["k_h"]][i], fig[["n_h"]][i]),
    "Figure 1, p.1964", NA, breakeven_n(0, n_e = fig[["k_h"]][i] * fig[["n_h"]][i]), "info",
    note = "Read from the figure: about 30, 300, 300 and above the plotted range"))
}

finalize_results(res, "inst/reproduce/results")
