# Summarize the results of the reproduction scripts.
#
# Run from the package root after the scripts reproduce_*.R.
# Output: inst/reproduce/results/summary.csv and summary.md

source("inst/validation/judge.R")
summarize_results("inst/reproduce/results", "^reproduce_.*\\.csv$")
