#' Bias at which a single-arm trial and an RCT have equal mean squared error
#'
#' Computes the break-even bias, the absolute bias of the single-arm
#' comparison at which the single-arm trial and the randomized controlled
#' trial (RCT) of the same total sample size have equal mean squared error.
#'
#' @param n Total number of patients in the current trial.
#' @param n_e Number of patients in the external control group of the
#'   single-arm trial. \code{Inf} (the default) means comparison with a fixed
#'   threshold.
#' @param sigma_t Standard deviation of the outcome of one patient on the
#'   experimental treatment, on the scale of the treatment effect.
#' @param sigma_c Standard deviation for one control patient, concurrent or
#'   external. Defaults to \code{sigma_t}.
#' @param r Fraction of patients allocated to the experimental treatment in
#'   the RCT. Defaults to \code{0.5}.
#'
#' @details The single-arm trial has the smaller mean squared error if and
#'   only if \eqn{b^2 + \tau^2 < b^{*2}}{bias^2 + tau^2 < b_star^2}, where
#'   \deqn{b^{*2} = \sigma_T^2 / (r n) + \sigma_C^2 / ((1 - r) n) - \sigma_T^2 / n - \sigma_C^2 / n_E.}{b_star^2 = sigma_t^2 / (r n) + sigma_c^2 / ((1 - r) n) - sigma_t^2 / n - sigma_c^2 / n_e.}
#'   With 1:1 allocation and equal standard deviations this is
#'   \eqn{\sigma \sqrt{3 / n - 1 / n_E}}{sigma sqrt(3 / n - 1 / n_e)}, the
#'   condition given in random-effects form by Collignon et al. (2020,
#'   equations 5 and 6). It also follows from equation 5 of Pocock (1976) with
#'   no randomized controls.
#'
#' @return A numeric vector of break-even biases. The value is \code{NA} when
#'   the right-hand side above is negative, that is, when the single-arm trial
#'   has the larger mean squared error even without bias. Vector arguments are
#'   recycled to a common length.
#'
#' @references
#' Pocock SJ. The combination of randomized and historical controls in
#' clinical trials. \emph{Journal of Chronic Diseases} 1976; 29: 175--188.
#'
#' Collignon O, Schritz A, Senn SJ, Spezia R. Clustered allocation as a way of
#' understanding historical controls: components of variation and regulatory
#' considerations. \emph{Statistical Methods in Medical Research} 2020; 29:
#' 1960--1971.
#'
#' @seealso \code{\link{breakeven_n}}, \code{\link{mse_rct}},
#'   \code{\link{mse_sat}}
#'
#' @examples
#' breakeven_bias(n = 40, n_e = 100)
#' # The tolerable bias shrinks as the trial grows
#' breakeven_bias(n = c(20, 40, 80, 160), n_e = Inf)
#'
#' @export
breakeven_bias <- function(n, n_e = Inf, sigma_t = 1, sigma_c = sigma_t, r = 0.5) {
  check_numeric(n, "n", lower = 0, lower_open = TRUE)
  check_numeric(n_e, "n_e", lower = 0, lower_open = TRUE, allow_inf = TRUE)
  check_numeric(sigma_t, "sigma_t", lower = 0, lower_open = TRUE)
  check_numeric(sigma_c, "sigma_c", lower = 0, lower_open = TRUE)
  check_numeric(r, "r", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  a <- recycle_args(n = n, n_e = n_e, sigma_t = sigma_t, sigma_c = sigma_c, r = r)
  d <- mse_rct(a[["n"]], a[["sigma_t"]], a[["sigma_c"]], a[["r"]]) -
    mse_sat(a[["n"]], a[["n_e"]], 0, 0, a[["sigma_t"]], a[["sigma_c"]])
  ifelse(d >= 0, sqrt(pmax(d, 0)), NA_real_)
}
