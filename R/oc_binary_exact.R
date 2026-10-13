#' Exact rejection probability of a two-group comparison of proportions
#'
#' Computes the exact rejection probability of a one-sided test comparing a
#' treatment group with a control group on a binary endpoint, by summing over
#' all possible numbers of responders. The function covers the randomized
#' controlled trial (\code{n_t = n_c}), the single-arm trial compared with an
#' external control group (\code{n_t = n}, \code{n_c = n_e} and \code{p_c}
#' the true response probability of the external controls), and the pooling
#' of fixed external data into the control group.
#'
#' @param n_t Number of patients in the treatment group. A single whole
#'   number.
#' @param n_c Number of patients in the control group whose responses are
#'   random. A single whole number.
#' @param p_t,p_c True response probabilities of the treatment and control
#'   groups. Vectors with values in \code{[0, 1]}, recycled to a common
#'   length.
#' @param margin Margin of the score test: the null hypothesis is that the
#'   difference of the response probabilities, treatment minus control, is
#'   at most \code{margin}. Must be \code{0} for the Pearson tests. Defaults
#'   to \code{0}.
#' @param alpha One-sided significance level. Defaults to \code{0.025}.
#' @param test \code{"score"} (the default) for the Farrington and Manning
#'   score test, \code{"pearson"} for the Pearson chi-squared test and
#'   \code{"pearson_cc"} for the same test with the Yates continuity
#'   correction. The two Pearson tests reproduce the one-sided decisions of
#'   \code{\link[stats]{prop.test}} with \code{correct = FALSE} and
#'   \code{correct = TRUE}.
#' @param n_ext,y_ext Number of patients and of responders in fixed external
#'   control data that are pooled into the control group, as in the pooled
#'   analysis of Viele et al. (2014). Default to \code{0}.
#'
#' @details Each test rejects when its z statistic exceeds
#'   \eqn{z_{1 - \alpha}}{qnorm(1 - alpha)}. Tables in which the standard
#'   error is zero (all patients responders, or none, with \code{margin = 0})
#'   do not lead to rejection. The score test uses the restricted maximum
#'   likelihood estimates of Farrington and Manning (1990), computed from
#'   their closed-form solution.
#'
#' @return A numeric vector of rejection probabilities, one for each pair of
#'   \code{p_t} and \code{p_c}.
#'
#' @references
#' Farrington CP, Manning G. Test statistics and sample size formulae for
#' comparative binomial trials with null hypothesis of non-zero risk
#' difference or non-unity relative risk. \emph{Statistics in Medicine}
#' 1990; 9: 1447--1454.
#'
#' Viele K, Berry S, Neuenschwander B, et al. Use of historical control data
#' for assessing treatment effects in clinical trials. \emph{Pharmaceutical
#' Statistics} 2014; 13: 41--54.
#'
#' @seealso \code{\link{oc_sat_binary_exact}}
#'
#' @examples
#' # Randomized trial with 20 patients per group
#' oc_binary_exact(n_t = 20, n_c = 20, p_t = c(0.3, 0.5), p_c = 0.3)
#' # Single-arm trial of 30 patients against 60 external controls, margin 0.1
#' oc_binary_exact(n_t = 30, n_c = 60, p_t = c(0.4, 0.6), p_c = 0.3, margin = 0.1)
#'
#' @export
oc_binary_exact <- function(n_t, n_c, p_t, p_c, margin = 0, alpha = 0.025,
                            test = c("score", "pearson", "pearson_cc"), n_ext = 0, y_ext = 0) {
  test <- match.arg(test)
  check_numeric(n_t, "n_t", lower = 1, scalar = TRUE, integer = TRUE)
  check_numeric(n_c, "n_c", lower = 1, scalar = TRUE, integer = TRUE)
  check_numeric(p_t, "p_t", lower = 0, upper = 1)
  check_numeric(p_c, "p_c", lower = 0, upper = 1)
  check_numeric(margin, "margin", lower = -1, upper = 1, lower_open = TRUE, upper_open = TRUE,
                scalar = TRUE)
  check_numeric(alpha, "alpha", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE,
                scalar = TRUE)
  check_numeric(n_ext, "n_ext", lower = 0, scalar = TRUE, integer = TRUE)
  check_numeric(y_ext, "y_ext", lower = 0, upper = n_ext, scalar = TRUE, integer = TRUE)
  if (test != "score" && abs(margin) > 0) {
    stop("The Pearson tests require margin = 0.", call. = FALSE)
  }
  a <- recycle_args(p_t = p_t, p_c = p_c)
  y_t <- rep(0:n_t, times = n_c + 1)
  y_c <- rep(0:n_c, each = n_t + 1)
  z <- binary_z(y_t, n_t, y_c + y_ext, n_c + n_ext, margin, test)
  reject <- !is.na(z) & z > qnorm(alpha, lower.tail = FALSE)
  reject_matrix <- matrix(as.numeric(reject), nrow = n_t + 1, ncol = n_c + 1)
  vapply(seq_along(a[["p_t"]]), function(i) {
    prob_t <- dbinom(0:n_t, n_t, a[["p_t"]][i])
    prob_c <- dbinom(0:n_c, n_c, a[["p_c"]][i])
    sum(prob_t * (reject_matrix %*% prob_c))
  }, numeric(1))
}
