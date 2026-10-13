# Summarize the results of the validation scripts, including the mutation check of the tests.
#
# Run from the package root after the scripts validate_*.R.
# Output: inst/validation/results/summary.csv and summary.md

source("inst/validation/judge.R")
summarize_results("inst/validation/results", "^(validate_|test_mutation).*\\.csv$")
