#' Sample size at which a single-arm trial and an RCT have equal mean squared error
#'
#' Computes the total sample size of the current trial at which a single-arm
#' trial with a given bias and the randomized controlled trial (RCT) of the
#' same size have equal mean squared error. Below this size the single-arm
#' trial has the smaller mean squared error, and above it the RCT does.
#'
#' @param bias Fixed bias of the single-arm comparison, defined as the
#'   expectation of the estimated treatment effect minus the true effect.
#' @param n_e Number of patients in the external control group. \code{Inf}
#'   (the default) means comparison with a fixed threshold.
#' @param tau Standard deviation of a random bias with mean zero. Defaults to
#'   \code{0}.
#' @param sigma_t Standard deviation of the outcome of one patient on the
#'   experimental treatment, on the scale of the treatment effect.
#' @param sigma_c Standard deviation for one control patient. Defaults to
#'   \code{sigma_t}.
#' @param r Fraction of patients allocated to the experimental treatment in
#'   the RCT. Defaults to \code{0.5}.
#'
#' @details The break-even sample size is
#'   \deqn{n^* = (\sigma_T^2 (1 / r - 1) + \sigma_C^2 / (1 - r)) / (b^2 + \tau^2 + \sigma_C^2 / n_E).}{n_star = (sigma_t^2 (1 / r - 1) + sigma_c^2 / (1 - r)) / (bias^2 + tau^2 + sigma_c^2 / n_e).}
#'   With 1:1 allocation and equal standard deviations the numerator is
#'   \eqn{3 \sigma^2}{3 sigma^2}. Setting \code{tau^2} to
#'   \eqn{(1 + 1 / k_h)}{(1 + 1 / k_h)} times the between-study variance and
#'   \code{n_e} to the total number of patients in \eqn{k_h}{k_h} historical
#'   studies gives equation 6 of Collignon et al. (2020).
#'
#' @return A numeric vector of sample sizes. The value is \code{Inf} when
#'   \code{bias = 0}, \code{tau = 0} and \code{n_e = Inf}. Vector arguments
#'   are recycled to a common length.
#'
#' @references Collignon O, Schritz A, Senn SJ, Spezia R. Clustered
#'   allocation as a way of understanding historical controls: components of
#'   variation and regulatory considerations. \emph{Statistical Methods in
#'   Medical Research} 2020; 29: 1960--1971.
#'
#' @seealso \code{\link{breakeven_bias}}
#'
#' @examples
#' breakeven_n(bias = 0.2, n_e = 100)
#' # Example of Collignon et al. (2020): 10 historical studies of 100 patients
#' # with between-study variance 0.02
#' breakeven_n(bias = 0, n_e = 1000, tau = sqrt((1 + 1 / 10) * 0.02))
#'
#' @export
breakeven_n <- function(bias, n_e = Inf, tau = 0, sigma_t = 1, sigma_c = sigma_t, r = 0.5) {
  check_numeric(bias, "bias")
  check_numeric(n_e, "n_e", lower = 0, lower_open = TRUE, allow_inf = TRUE)
  check_numeric(tau, "tau", lower = 0)
  check_numeric(sigma_t, "sigma_t", lower = 0, lower_open = TRUE)
  check_numeric(sigma_c, "sigma_c", lower = 0, lower_open = TRUE)
  check_numeric(r, "r", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  a <- recycle_args(bias = bias, n_e = n_e, tau = tau, sigma_t = sigma_t, sigma_c = sigma_c,
                    r = r)
  numerator <- a[["sigma_t"]]^2 * (1 / a[["r"]] - 1) + a[["sigma_c"]]^2 / (1 - a[["r"]])
  denominator <- a[["bias"]]^2 + a[["tau"]]^2 + a[["sigma_c"]]^2 / a[["n_e"]]
  numerator / denominator
}
