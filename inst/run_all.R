# Reproduce every result of the article from the package root.
#
# In RStudio, open satrct.Rproj, so that the working directory is the package root, and run
#   devtools::load_all()
#   source("inst/run_all.R")
# or, from a terminal in the package root,
#   Rscript -e "devtools::load_all(); source('inst/run_all.R')"
#
# The script runs, in this order, the computations and the tables and figures of the main text
# (inst/scripts), the computations of the Supporting Information (inst/sup_info), the
# reproduction of published results (inst/reproduce), the validation (inst/validation), their
# summaries, the tables and figures of the Supporting Information, which include the summary of
# the verdicts (Table S8), and the check of the numbers registered for the text
# (inst/scripts/number_check.R). Each script runs in an environment of its own, and the run stops
# at the first script that fails, including a comparison with the verdict FAIL. The versions of
# R and of the packages are written to inst/scripts/results/session_info.txt.
#
# Copying the tables and figures to a manuscript folder is optional. Set the environment
# variable SATRCT_MANUSCRIPT_DIR to that folder before sourcing this script, for example
#   Sys.setenv(SATRCT_MANUSCRIPT_DIR = "../../Manuscript")
# Without it, the copy step is skipped.
#
# The validation uses the suggested packages rpact (validate_rpact.R), survival
# (validate_simulation.R) and testthat (validate_test_mutation.R). Without rpact or survival,
# those comparisons give the verdict INFO instead of being run, so that the run is not the
# complete validation summarized in Table S8. The comparison with the independent Python
# implementation (validate_python_reference.R) uses the reference values saved in
# inst/validation/reference/grid_*.csv and does not run Python; regenerating them requires
# Python 3 with NumPy and SciPy (make_python_grid.py in that folder).

if (!exists("power_sat", mode = "function")) {
  stop("Run devtools::load_all() (or library(satrct)) before sourcing this script.",
       call. = FALSE)
}
start_all <- proc.time()
run_script <- function(path) {
  message("\n==== ", path, " ====")
  source(path, local = new.env(parent = globalenv()))
  invisible(TRUE)
}
scripts_all <- c(
  "inst/scripts/data_generate_main.R",
  "inst/scripts/table_and_figure_manuscript.R",
  "inst/sup_info/data_generate_supplement.R",
  sort(list.files("inst/reproduce", pattern = "^reproduce_.*\\.R$", full.names = TRUE)),
  "inst/reproduce/summarize_reproduction.R",
  sort(list.files("inst/validation", pattern = "^validate_.*\\.R$", full.names = TRUE)),
  "inst/validation/summarize_validation.R",
  "inst/sup_info/table_and_figure_supplement.R",
  "inst/scripts/number_check.R"
)
stopifnot(all(file.exists(scripts_all)))
for (path in scripts_all) {
  run_script(path)
}
if (nzchar(Sys.getenv("SATRCT_MANUSCRIPT_DIR"))) {
  run_script("inst/scripts/copy_results.R")
} else {
  message("\nSATRCT_MANUSCRIPT_DIR is not set, so the tables and figures are not copied.")
}
writeLines(utils::capture.output(utils::sessionInfo()), "inst/scripts/results/session_info.txt")
message(sprintf("\nrun_all.R finished in %.1f minutes; session information saved in %s.",
                (proc.time() - start_all)[["elapsed"]] / 60,
                "inst/scripts/results/session_info.txt"))
