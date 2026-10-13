#' Exact operating characteristics of a single-arm trial with a binary endpoint
#'
#' Computes the critical value and the exact rejection probabilities of the
#' one-sided exact binomial test of a single-arm trial against a fixed
#' threshold \code{p0}, optionally raised by a margin.
#'
#' @param n Number of patients in the single-arm trial. A single whole
#'   number.
#' @param p_t True response probabilities at which the rejection probability
#'   is computed. A vector with values in \code{[0, 1]}.
#' @param p0 Threshold response probability. A single number in
#'   \code{(0, 1)}.
#' @param margin Margin added to \code{p0}, so that the test is of the null
#'   hypothesis that the response probability is at most \code{p0 + margin}.
#'   Defaults to \code{0}.
#' @param alpha One-sided significance level, used when \code{crit} is
#'   \code{NULL}. Defaults to \code{0.025}.
#' @param crit Critical value: the test rejects when at least \code{crit}
#'   responses are observed. \code{NULL} (the default) uses the smallest
#'   value whose size does not exceed \code{alpha}.
#'
#' @details The test rejects when \eqn{Y \ge c}{Y >= crit}, where
#'   \eqn{Y}{Y} is the number of responses. This is the decision of
#'   \code{\link[stats]{binom.test}} with \code{p = p0 + margin} and
#'   \code{alternative = "greater"} at level \code{alpha}. The size is
#'   compared with \code{alpha} with an absolute tolerance of
#'   \code{1e-12}.
#'
#' @return A data frame with one row per value of \code{p_t} and the columns
#'   \code{p_t}, \code{crit}, \code{size} (rejection probability at
#'   \code{p0 + margin}) and \code{prob_reject} (rejection probability at
#'   \code{p_t}).
#'
#' @seealso \code{\link{oc_binary_exact}}
#'
#' @examples
#' # Single-arm trial of 200 patients against a threshold of 0.65
#' oc_sat_binary_exact(n = 200, p_t = c(0.65, 0.70, 0.75), p0 = 0.65)
#' # The same trial tested against 0.70
#' oc_sat_binary_exact(n = 200, p_t = c(0.65, 0.70, 0.75), p0 = 0.65, margin = 0.05)
#'
#' @export
oc_sat_binary_exact <- function(n, p_t, p0, margin = 0, alpha = 0.025, crit = NULL) {
  check_numeric(n, "n", lower = 1, scalar = TRUE, integer = TRUE)
  check_numeric(p_t, "p_t", lower = 0, upper = 1)
  check_numeric(p0, "p0", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE,
                scalar = TRUE)
  check_numeric(margin, "margin", scalar = TRUE)
  check_numeric(alpha, "alpha", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE,
                scalar = TRUE)
  p_null <- p0 + margin
  if (p_null <= 0 || p_null >= 1) {
    stop("'p0 + margin' must lie in the interval (0, 1).", call. = FALSE)
  }
  if (is.null(crit)) {
    candidates <- as.numeric(0:(n + 1))
    tail_prob <- pbinom(candidates - 1, n, p_null, lower.tail = FALSE)
    crit <- candidates[which(tail_prob <= alpha + 1e-12)[1L]]
  } else {
    check_numeric(crit, "crit", lower = 0, upper = n + 1, scalar = TRUE, integer = TRUE)
    crit <- as.numeric(crit)
  }
  data.frame(
    p_t = p_t,
    crit = crit,
    size = pbinom(crit - 1, n, p_null, lower.tail = FALSE),
    prob_reject = pbinom(crit - 1, n, p_t, lower.tail = FALSE)
  )
}
