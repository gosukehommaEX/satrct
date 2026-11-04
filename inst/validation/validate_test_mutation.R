# Sensitivity of the unit tests to a changed expected value.
#
# For each file tests/testthat/test-*.R, the file is first run unchanged, which must give no
# failure and no error (the baseline). Then the first expected value written as
# "<number>, tolerance" is multiplied by (1 + 1e-4) in a temporary copy, and the copy is run.
# The check is PASS when the test_that() block that contains the changed value fails; it is
# FAIL when the baseline fails, when that block passes or when it stops with an error (an error
# is not counted as a detection), and INFO when the file has no such expected value. This checks
# that the tests detect a changed expected value; it is not a mutation test of the functions of
# the package.
#
# Run from the package root after devtools::load_all().
# Output: inst/validation/results/test_mutation.csv

test_files <- list.files("tests/testthat", pattern = "^test-.*\\.R$", full.names = TRUE)
target <- "(-?[0-9]+(\\.[0-9]+)?(e-?[0-9]+)?)(,\\s*tolerance)"

run_file <- function(path) {
  as.data.frame(testthat::test_file(path, reporter = "silent"))
}

# Description of the test_that() block that contains the character position pos of src
block_at <- function(src, pos) {
  starts <- gregexpr("test_that\\(\"[^\"]*\"", src, perl = TRUE)[[1]]
  if (starts[1] < 0) {
    return(NA_character_)
  }
  names <- sub("\"$", "", sub("^test_that\\(\"", "", regmatches(src, list(starts))[[1]]))
  before <- which(starts < pos)
  if (length(before) == 0) NA_character_ else names[max(before)]
}

run_mutation <- function(path) {
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")
  baseline <- run_file(path)
  baseline_problems <- sum(baseline[["failed"]]) + sum(baseline[["error"]])
  hit <- regexpr(target, src, perl = TRUE)
  if (hit < 0) {
    return(data.frame(file = basename(path), block = NA_character_, original = NA_character_,
                      mutated = NA_character_, baseline_problems = baseline_problems,
                      block_failures = NA_integer_, block_errors = NA_integer_,
                      verdict = if (baseline_problems == 0) "INFO" else "FAIL"))
  }
  block <- block_at(src, hit)
  matched <- regmatches(src, hit)
  original <- sub(target, "\\1", matched, perl = TRUE)
  mutated <- format(as.numeric(original) * (1 + 1e-4), digits = 17)
  regmatches(src, hit) <- sub(target, paste0(mutated, "\\4"), matched, perl = TRUE)
  tmp <- file.path(tempdir(), basename(path))
  writeLines(src, tmp)
  results <- run_file(tmp)
  in_block <- !is.na(block) & results[["test"]] == block
  block_failures <- sum(results[["failed"]][in_block])
  block_errors <- sum(results[["error"]][in_block])
  detected <- baseline_problems == 0 && any(in_block) && block_failures > 0 && block_errors == 0
  data.frame(file = basename(path), block = block, original = original, mutated = mutated,
             baseline_problems = baseline_problems, block_failures = block_failures,
             block_errors = block_errors, verdict = if (detected) "PASS" else "FAIL")
}

mutation <- do.call(rbind, lapply(test_files, run_mutation))
dir.create("inst/validation/results", showWarnings = FALSE, recursive = TRUE)
write.csv(mutation, "inst/validation/results/test_mutation.csv", row.names = FALSE)
print(mutation)
print(table(mutation[["verdict"]]))
if (any(mutation[["verdict"]] == "FAIL")) {
  stop("The unit tests did not detect a changed expected value in ",
       paste(mutation[["file"]][mutation[["verdict"]] == "FAIL"], collapse = ", "), ".",
       call. = FALSE)
}
