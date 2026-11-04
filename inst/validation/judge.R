# Common helpers for the scripts in inst/reproduce and inst/validation.
#
# Source from the package root after devtools::load_all():
#   source("inst/validation/judge.R")
#
# Verdicts:
#   PASS      the recomputed value agrees with the reference under the stated rule
#   FAIL      it does not
#   EXPLAINED it does not, and the reason is registered in inst/validation/explained.csv
#   INFO      no pass/fail rule applies (values read from figures, qualitative statements,
#             or the accuracy of an approximation), reported for information

check_loaded <- function() {
  if (!exists("power_sat", mode = "function")) {
    stop("Run devtools::load_all() (or library(satrct)) before sourcing this script.",
         call. = FALSE)
  }
  invisible(TRUE)
}

# One row (or several, for vector arguments) of the results table.
# rule = "round": PASS if value rounded to `digits` decimals equals the reference
# rule = "abs":   PASS if |value - reference| <= tol
# rule = "rel":   PASS if |value - reference| <= tol * max(1, |reference|)
# rule = "range": PASS if the reference lies in [value_low, value_high]
# rule = "info":  verdict INFO
judge <- function(script, item, description, source, reference, value,
                  rule = c("round", "abs", "rel", "range", "info"), tol = NA_real_,
                  digits = NA_real_, value_low = NA_real_, value_high = NA_real_, note = "") {
  rule <- match.arg(rule)
  ok <- switch(rule,
    round = abs(round(value, digits) - reference) < 1e-9,
    abs = abs(value - reference) <= tol,
    rel = abs(value - reference) <= tol * pmax(1, abs(reference)),
    range = reference >= value_low - 1e-12 & reference <= value_high + 1e-12,
    info = rep(NA, max(length(value), length(reference)))
  )
  verdict <- if (rule == "info") {
    rep("INFO", length(ok))
  } else {
    ifelse(!is.na(ok) & ok, "PASS", "FAIL")
  }
  data.frame(script = script, item = item, description = description, source = source,
             reference = reference, value = value, value_low = value_low,
             value_high = value_high, rule = rule, tol = tol, digits = digits,
             verdict = verdict, note = note, stringsAsFactors = FALSE)
}

# Replace FAIL by EXPLAINED for the items registered in explained.csv, but only when the value is
# still the registered one (within the registered tolerance), so that a later change of the
# value is not hidden by an old explanation
apply_explained <- function(results, path = "inst/validation/explained.csv") {
  registry <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!all(c("script", "item", "expected_value", "tolerance", "reason") %in% names(registry))) {
    stop("explained.csv needs the columns script, item, expected_value, tolerance and reason.",
         call. = FALSE)
  }
  key_results <- paste(results[["script"]], results[["item"]], sep = "::")
  key_registry <- paste(registry[["script"]], registry[["item"]], sep = "::")
  idx <- match(key_results, key_registry)
  registered <- results[["verdict"]] == "FAIL" & !is.na(idx)
  same <- registered &
    abs(results[["value"]] - registry[["expected_value"]][idx]) <= registry[["tolerance"]][idx]
  same[is.na(same)] <- FALSE
  results[["verdict"]][same] <- "EXPLAINED"
  results[["note"]][same] <- registry[["reason"]][idx[same]]
  changed <- registered & !same
  if (any(changed)) {
    results[["note"]][changed] <- paste0(
      "Registered explanation not applied: the value differs from the registered value ",
      format(registry[["expected_value"]][idx[changed]], digits = 15))
  }
  unused <- key_registry[startsWith(key_registry, paste0(results[["script"]][1], "::")) &
                           !(key_registry %in% key_results[same])]
  if (length(unused) > 0) {
    message("Registered explanations not used (the item passes, is absent or has changed): ",
            paste(unused, collapse = ", "))
  }
  results
}

# Confirm that the comparison is not vacuous: perturb the reference of the first PASS row and
# check that the same rule then gives FAIL
self_check <- function(results) {
  idx <- which(results[["verdict"]] == "PASS" &
                 results[["rule"]] %in% c("round", "abs", "rel"))[1]
  script <- results[["script"]][1]
  if (is.na(idx)) {
    return(judge(script, "self_check", "No PASS row available to perturb", "", NA, NA, "info"))
  }
  r <- results[idx, ]
  shift <- switch(r[["rule"]],
    round = 3 * 10^(-r[["digits"]]),
    abs = 10 * r[["tol"]] + 1e-9,
    rel = 10 * r[["tol"]] * max(1, abs(r[["reference"]])) + 1e-12
  )
  perturbed <- judge(script, r[["item"]], "", "", r[["reference"]] + shift, r[["value"]],
                     r[["rule"]], tol = r[["tol"]], digits = r[["digits"]])
  detected <- perturbed[["verdict"]] == "FAIL"
  judge(script, "self_check",
        paste0("Reference of '", r[["item"]], "' shifted by ", format(shift),
               " is detected as FAIL"),
        "", 1, as.numeric(detected), "abs", tol = 0)
}

# Apply the registry, add the self-check, write the CSV, print a summary and stop if a
# comparison failed
finalize_results <- function(results, out_dir) {
  results <- apply_explained(results)
  results <- rbind(results, self_check(results))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(out_dir, paste0(results[["script"]][1], ".csv"))
  utils::write.csv(results, path, row.names = FALSE)
  print(results[, c("item", "reference", "value", "verdict")], row.names = FALSE)
  print(table(factor(results[["verdict"]], levels = c("PASS", "EXPLAINED", "FAIL", "INFO"))))
  message("Saved: ", path)
  n_fail <- sum(results[["verdict"]] == "FAIL")
  if (n_fail > 0) {
    stop(n_fail, " comparison(s) of ", results[["script"]][1], " failed; see ", path, ".",
         call. = FALSE)
  }
  invisible(results)
}

# Count the verdicts of all result files in a folder and list the rows that are not PASS.
# Writes summary.csv (counts) and summary.md (counts and non-PASS rows) to the same folder.
summarize_results <- function(dir, pattern) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0L) {
    stop("No result files found in ", dir, call. = FALSE)
  }
  levels <- c("PASS", "EXPLAINED", "FAIL", "INFO")
  tables <- lapply(files, function(f) {
    x <- utils::read.csv(f, stringsAsFactors = FALSE)
    if (!("script" %in% names(x))) {
      x[["script"]] <- sub("\\.csv$", "", basename(f))
    }
    if (!("item" %in% names(x))) {
      x[["item"]] <- if ("file" %in% names(x)) x[["file"]] else as.character(seq_len(nrow(x)))
    }
    if (!("note" %in% names(x))) {
      x[["note"]] <- ""
    }
    x[, c("script", "item", "verdict", "note")]
  })
  all_rows <- do.call(rbind, tables)
  counts <- table(all_rows[["script"]], factor(all_rows[["verdict"]], levels = levels))
  summary_table <- data.frame(script = rownames(counts), stringsAsFactors = FALSE)
  for (lv in levels) {
    summary_table[[lv]] <- as.integer(counts[, lv])
  }
  summary_table[["total"]] <- rowSums(summary_table[, levels])
  utils::write.csv(summary_table, file.path(dir, "summary.csv"), row.names = FALSE)
  md <- c("# Summary of verdicts", "",
          paste("Generated by summarize_results() from the files in", dir, "on",
                format(Sys.Date())), "",
          "| script | PASS | EXPLAINED | FAIL | INFO | total |",
          "|---|---|---|---|---|---|",
          sprintf("| %s | %d | %d | %d | %d | %d |", summary_table[["script"]],
                  summary_table[["PASS"]], summary_table[["EXPLAINED"]],
                  summary_table[["FAIL"]], summary_table[["INFO"]], summary_table[["total"]]))
  other <- all_rows[all_rows[["verdict"]] %in% c("EXPLAINED", "FAIL"), ]
  if (nrow(other) > 0L) {
    md <- c(md, "", "## Rows that are not PASS or INFO", "",
            "| script | item | verdict | note |", "|---|---|---|---|",
            sprintf("| %s | %s | %s | %s |", other[["script"]], other[["item"]],
                    other[["verdict"]], gsub("\\|", "/", other[["note"]])))
  }
  writeLines(md, file.path(dir, "summary.md"))
  print(summary_table, row.names = FALSE)
  message("Saved: ", file.path(dir, "summary.csv"), " and summary.md")
  invisible(summary_table)
}
