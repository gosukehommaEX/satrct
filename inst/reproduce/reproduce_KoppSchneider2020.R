# Reproduce Kopp-Schneider A, Calderazzo S, Wiesenfarth M (2020). Power gains by using external
# information in clinical trials are typically not possible when requiring strict type I error
# control. Biom J 62: 361-374.
#
# Section 4 (p.365): single-arm paediatric trial with 40 patients, H0: p <= 0.2, alpha = 0.05.
# The uniformly most powerful test rejects for at least 13 responders, with actual size 0.043.
# The borrowing methods of the paper are outside the scope of the package and are not
# reproduced.
#
# Run from the package root after devtools::load_all().
# Output: inst/reproduce/results/reproduce_KoppSchneider2020.csv

source("inst/validation/judge.R")
check_loaded()
script <- "reproduce_KoppSchneider2020"

oc <- oc_sat_binary_exact(40, p_t = 0.2, p0 = 0.2, alpha = 0.05)
res <- rbind(
  judge(script, "ump_critical_value", "Critical value of the UMP test (reject if r >= 13)",
        "p.365", 13, oc[["crit"]], "abs", tol = 0),
  judge(script, "ump_size", "Actual type I error of the UMP test", "p.365", 0.043,
        oc[["size"]], "round", digits = 3)
)

finalize_results(res, "inst/reproduce/results")
