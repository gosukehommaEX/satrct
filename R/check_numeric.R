#' Check a numeric argument
#'
#' Internal helper that stops with an informative message when an argument is
#' not numeric, contains missing values or lies outside its admissible range.
#'
#' @param x Value to check.
#' @param name Name of the argument, used in the error message.
#' @param lower,upper Bounds of the admissible range.
#' @param lower_open,upper_open Logical; whether the bounds are excluded.
#' @param allow_inf Logical; whether infinite values are admissible.
#' @param scalar Logical; whether \code{x} must have length one.
#' @param integer Logical; whether \code{x} must be a whole number.
#'
#' @return \code{x}, invisibly.
#' @keywords internal
#' @noRd
check_numeric <- function(x, name, lower = -Inf, upper = Inf, lower_open = FALSE,
                          upper_open = FALSE, allow_inf = FALSE, scalar = FALSE,
                          integer = FALSE) {
  if (!is.numeric(x) || length(x) == 0L || anyNA(x)) {
    stop(sprintf("'%s' must be a non-empty numeric vector without missing values.", name),
         call. = FALSE)
  }
  if (scalar && length(x) != 1L) {
    stop(sprintf("'%s' must be a single number.", name), call. = FALSE)
  }
  if (!allow_inf && any(is.infinite(x))) {
    stop(sprintf("'%s' must be finite.", name), call. = FALSE)
  }
  bad_lower <- if (lower_open) x <= lower else x < lower
  bad_upper <- if (upper_open) x >= upper else x > upper
  if (any(bad_lower | bad_upper)) {
    stop(sprintf("'%s' must lie in the interval %s%s, %s%s.", name,
                 if (lower_open) "(" else "[", format(lower), format(upper),
                 if (upper_open) ")" else "]"),
         call. = FALSE)
  }
  if (integer) {
    finite_x <- x[is.finite(x)]
    if (any(abs(finite_x - round(finite_x)) > sqrt(.Machine$double.eps))) {
      stop(sprintf("'%s' must be a whole number.", name), call. = FALSE)
    }
  }
  invisible(x)
}
