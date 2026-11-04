# Check the numbers written in the manuscript against the saved results.
#
# The ledger inst/scripts/number_locations.csv has one row per number written in main.tex,
# supporting_information.tex or cover_letter.tex (columns id, file, anchor, text_value,
# source_file, source_expr, rounding, type, bibkey, note). For every row with a source
# expression, the value is computed from the source file, formatted by the rounding rule and
# printed next to the value written in the manuscript. The location of the anchors in the .tex
# files is checked separately by the script tools/check_numbers.py of the manuscript folder of
# the authors, which is not part of the package and is not needed to reproduce the results.
#
# source_expr: selectors column[condition] combined with numbers, + - * / and parentheses,
# evaluated in the data frame read from source_file (path relative to the package root). Every
# selector must pick exactly one row; the values in the conditions are compared exactly.
# Rounding rules: dp:k (k decimal places), pct:k (100 times the value with k decimal places),
# int (nearest integer), sig:k (k significant digits), exact (numerical equality with the
# text) and order (power of ten written as 10^{k}, k = round(log10|value|)).
#
# A self-check first evaluates a synthetic row on table1_relative.csv with the correct text
# and with a changed text and requires PASS and FAIL. The script stops with an error if any
# row fails.
#
# Run from the package root (the package does not need to be loaded).
# Output: printed table and inst/scripts/results/number_check.csv

ledger_path <- "inst/scripts/number_locations.csv"
columns <- c("id", "file", "anchor", "text_value", "source_file", "source_expr", "rounding",
             "type", "bibkey", "note")
ledger <- utils::read.csv(ledger_path, colClasses = "character", fileEncoding = "UTF-8",
                          na.strings = character(0))
if (!identical(names(ledger), columns)) {
  stop("The ledger must have the columns ", paste(columns, collapse = ", "), ".", call. = FALSE)
}

format_fixed <- function(x, digits) {
  if (digits >= 0) {
    return(formatC(x, format = "f", digits = digits))
  }
  scaled <- formatC(x / 10^(-digits), format = "f", digits = 0)
  if (scaled %in% c("0", "-0")) scaled else paste0(scaled, strrep("0", -digits))
}
format_value <- function(x, rule) {
  parts <- strsplit(rule, ":", fixed = TRUE)[[1]]
  k <- if (length(parts) > 1) as.integer(parts[2]) else NA_integer_
  switch(parts[1],
    dp = format_fixed(x, k),
    pct = format_fixed(100 * x, k),
    int = format_fixed(x, 0),
    sig = if (x == 0) "0" else format_fixed(x, k - 1 - floor(log10(abs(x)))),
    order = paste0("10^{", round(log10(abs(x))), "}"),
    stop("Unknown rounding rule '", rule, "'.", call. = FALSE)
  )
}
normalize_text <- function(s) {
  s <- gsub("\u2212", "-", s, fixed = TRUE)
  for (token in c("{,}", "\\,", "\\%", "%", "$", "~", ",")) {
    s <- gsub(token, "", s, fixed = TRUE)
  }
  gsub("[[:space:]]+", "", s)
}
evaluate_expr <- function(expr, data) {
  value <- eval(parse(text = expr), envir = data, enclos = baseenv())
  if (length(value) != 1 || !is.numeric(value) || is.na(value)) {
    stop("'", expr, "' does not give exactly one number.", call. = FALSE)
  }
  value
}
compare_value <- function(x, text, rule) {
  if (rule == "exact") {
    y <- suppressWarnings(as.numeric(normalize_text(text)))
    ok <- !is.na(y) && abs(y - x) <= max(1e-15, 1e-12 * abs(x))
    return(list(recomputed = format(x, digits = 15), verdict = if (ok) "PASS" else "FAIL"))
  }
  shown <- format_value(x, rule)
  list(recomputed = shown,
       verdict = if (normalize_text(shown) == normalize_text(text)) "PASS" else "FAIL")
}

# Self-check
check_data <- utils::read.csv("inst/scripts/results/data/table1_relative.csv")
x_check <- evaluate_expr("alpha_matched[ratio == 0 & power_rct == 0.35]", check_data)
v_correct <- compare_value(x_check, format_value(x_check, "dp:3"), "dp:3")[["verdict"]]
v_changed <- compare_value(x_check, format_value(x_check + 0.001, "dp:3"), "dp:3")[["verdict"]]
if (v_correct != "PASS" || v_changed != "FAIL") {
  stop("Self-check failed: a changed text value was not detected.", call. = FALSE)
}
message("Self-check passed: a changed text value is reported as FAIL.")

data_cache <- list()
blank <- rep("", nrow(ledger))
results <- data.frame(id = ledger[["id"]], file = ledger[["file"]],
                      text_value = ledger[["text_value"]], recomputed = blank,
                      rounding = ledger[["rounding"]], type = ledger[["type"]], verdict = blank,
                      message = blank, stringsAsFactors = FALSE)
for (i in seq_len(nrow(ledger))) {
  row <- ledger[i, ]
  problems <- character(0)
  if (!row[["type"]] %in% c("computed", "input", "literature")) {
    problems <- c(problems, paste0("unknown type '", row[["type"]], "'"))
  }
  if (row[["type"]] == "computed" && row[["source_expr"]] == "") {
    problems <- c(problems, "computed row without source_expr")
  }
  if (row[["source_expr"]] != "") {
    res <- tryCatch({
      src <- row[["source_file"]]
      if (is.null(data_cache[[src]])) {
        data_cache[[src]] <- utils::read.csv(src, stringsAsFactors = FALSE)
      }
      compare_value(evaluate_expr(row[["source_expr"]], data_cache[[src]]), row[["text_value"]],
                    row[["rounding"]])
    }, error = function(e) list(recomputed = "", verdict = "FAIL", error = conditionMessage(e)))
    results[["recomputed"]][i] <- res[["recomputed"]]
    if (res[["verdict"]] == "FAIL") {
      problems <- c(problems, if (is.null(res[["error"]])) "value differs" else res[["error"]])
    }
  }
  results[["verdict"]][i] <- if (length(problems) > 0) "FAIL" else "PASS"
  results[["message"]][i] <- paste(problems, collapse = "; ")
}
utils::write.csv(results, "inst/scripts/results/number_check.csv", row.names = FALSE)
print(results[c("id", "file", "text_value", "recomputed", "verdict", "message")],
      row.names = FALSE)
n_fail <- sum(results[["verdict"]] == "FAIL")
if (n_fail > 0) {
  stop(n_fail, " of ", nrow(results), " ledger rows failed; see ",
       "inst/scripts/results/number_check.csv.", call. = FALSE)
}
message(sprintf("All %d ledger rows passed.", nrow(results)))
