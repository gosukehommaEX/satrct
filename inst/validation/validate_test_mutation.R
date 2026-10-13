# Mutation check of the unit tests.
#
# For each file tests/testthat/test-*.R, the first expected value written as
# "<number>, tolerance" is multiplied by (1 + 1e-4) in a temporary copy, and
# the copy is run. The check is PASS when the mutated copy has at least one
# failing expectation, FAIL when it passes (the test would not detect a wrong
# value), and INFO when the file has no such expected value.
#
# Run from the package root after devtools::load_all().
# Output: inst/validation/results/test_mutation.csv

test_files <- list.files("tests/testthat", pattern = "^test-.*\\.R$", full.names = TRUE)
target <- "(-?[0-9]+(\\.[0-9]+)?(e-?[0-9]+)?)(,\\s*tolerance)"

run_mutation <- function(path) {
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")
  hit <- regexpr(target, src, perl = TRUE)
  if (hit < 0) {
    return(data.frame(file = basename(path), original = NA_character_,
                      mutated = NA_character_, failures = NA_integer_, verdict = "INFO"))
  }
  matched <- regmatches(src, hit)
  original <- sub(target, "\\1", matched, perl = TRUE)
  mutated <- format(as.numeric(original) * (1 + 1e-4), digits = 17)
  regmatches(src, hit) <- sub(target, paste0(mutated, "\\4"), matched, perl = TRUE)
  tmp <- file.path(tempdir(), basename(path))
  writeLines(src, tmp)
  results <- as.data.frame(testthat::test_file(tmp, reporter = "silent"))
  failures <- sum(results[["failed"]]) + sum(results[["error"]])
  data.frame(file = basename(path), original = original, mutated = mutated,
             failures = failures, verdict = if (failures > 0) "PASS" else "FAIL")
}

mutation <- do.call(rbind, lapply(test_files, run_mutation))
dir.create("inst/validation/results", showWarnings = FALSE, recursive = TRUE)
write.csv(mutation, "inst/validation/results/test_mutation.csv", row.names = FALSE)
print(mutation)
print(table(mutation[["verdict"]]))
