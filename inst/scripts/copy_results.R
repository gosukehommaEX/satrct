# Copy the tables and figures of the main text and the Supporting Information to the
# manuscript folder and check that every copy is identical to its source (MD5).
#
# The manuscript folder is ../../Manuscript relative to the package root
# (49_SAT_vs_RCT/Manuscript). The tables go to Manuscript/tables and the figures to
# Manuscript/figures. Files in these folders are not edited by hand.
#
# Run from the package root after table_and_figure_manuscript.R and
# table_and_figure_supplement.R.
# Output: the copies, and the log inst/scripts/results/copy_log.csv

manuscript_dir <- file.path("..", "..", "Manuscript")
if (!dir.exists(manuscript_dir)) {
  stop("The manuscript folder ", manuscript_dir, " was not found. Run this script from the ",
       "package root.", call. = FALSE)
}

md5 <- function(path) {
  unname(tools::md5sum(path))
}

# Self-check: a copy that differs from its source by one byte is detected
tmp_src <- tempfile(fileext = ".txt")
tmp_dest <- tempfile(fileext = ".txt")
writeLines("self-check", tmp_src)
file.copy(tmp_src, tmp_dest, overwrite = TRUE)
cat("x", file = tmp_dest, append = TRUE)
if (md5(tmp_src) == md5(tmp_dest)) {
  stop("Self-check failed: a modified copy was not detected.", call. = FALSE)
}
unlink(c(tmp_src, tmp_dest))
message("Self-check passed: a copy modified by one byte is detected.")

sets <- list(
  list(from = "inst/scripts/results/tables", to = "tables", pattern = "\\.tex$"),
  list(from = "inst/scripts/results/figures", to = "figures", pattern = "\\.(eps|pdf)$"),
  list(from = "inst/sup_info/results/tables", to = "tables", pattern = "\\.tex$"),
  list(from = "inst/sup_info/results/figures", to = "figures", pattern = "\\.(eps|pdf)$")
)
copy_log <- NULL
for (s in sets) {
  files <- list.files(s[["from"]], pattern = s[["pattern"]], full.names = TRUE)
  if (length(files) == 0) {
    stop("No files to copy in ", s[["from"]], ". Run the table and figure scripts first.",
         call. = FALSE)
  }
  dest_dir <- file.path(manuscript_dir, s[["to"]])
  dir.create(dest_dir, showWarnings = FALSE)
  dest <- file.path(dest_dir, basename(files))
  copied <- file.copy(files, dest, overwrite = TRUE)
  md5_source <- md5(files)
  md5_copy <- md5(dest)
  copy_log <- rbind(copy_log, data.frame(
    source = files,
    copy = file.path("Manuscript", s[["to"]], basename(files)),
    md5_source = md5_source,
    md5_copy = md5_copy,
    copied = copied,
    identical = copied & !is.na(md5_copy) & md5_source == md5_copy,
    stringsAsFactors = FALSE
  ))
}
utils::write.csv(copy_log, "inst/scripts/results/copy_log.csv", row.names = FALSE)
print(copy_log[c("copy", "identical")], row.names = FALSE)
if (!all(copy_log[["identical"]])) {
  stop("Some copies are missing or differ from their sources; see ",
       "inst/scripts/results/copy_log.csv.", call. = FALSE)
}
message(sprintf("All %d files were copied and are identical to their sources.",
                nrow(copy_log)))
