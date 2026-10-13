#' Recycle arguments to a common length
#'
#' Internal helper. Each argument must have length one or the length of the
#' longest argument, otherwise an error is raised.
#'
#' @param ... Named vectors.
#'
#' @return A named list of vectors that all have the common length.
#' @keywords internal
#' @noRd
recycle_args <- function(...) {
  args <- list(...)
  lens <- lengths(args)
  len <- max(lens)
  if (any(lens != 1L & lens != len)) {
    stop("Vector arguments must have length one or a common length.", call. = FALSE)
  }
  lapply(args, rep_len, length.out = len)
}
