#' Mean squared error of a single-arm trial with an external control
#'
#' Computes the mean squared error of the estimated treatment effect when all
#' \code{n} patients of the current trial receive the experimental treatment
#' and are compared with an external control group of size \code{n_e}.
#'
#' @param n Total number of patients in the current trial, all on the
#'   experimental treatment.
#' @param n_e Number of patients in the external control group. \code{Inf}
#'   (the default) means comparison with a fixed threshold known without
#'   sampling error.
#' @param bias Fixed bias of the comparison, defined as the expectation of the
#'   estimated treatment effect minus the true effect. A positive value
#'   overstates the benefit of the experimental treatment.
#' @param tau Standard deviation of a random bias with mean zero, as in Pocock
#'   (1976). Defaults to \code{0}.
#' @param sigma_t Standard deviation of the outcome of one patient on the
#'   experimental treatment, on the scale of the treatment effect.
#' @param sigma_c Standard deviation for one external control patient.
#'   Defaults to \code{sigma_t}.
#'
#' @details The mean squared error is
#'   \deqn{\sigma_T^2 / n + \sigma_C^2 / n_E + b^2 + \tau^2.}{sigma_t^2 / n + sigma_c^2 / n_e + bias^2 + tau^2.}
#'
#' @return A numeric vector. Vector arguments are recycled to a common
#'   length.
#'
#' @references Pocock SJ. The combination of randomized and historical
#'   controls in clinical trials. \emph{Journal of Chronic Diseases} 1976; 29:
#'   175--188.
#'
#' @seealso \code{\link{mse_rct}}, \code{\link{breakeven_bias}}
#'
#' @examples
#' mse_sat(n = 40, n_e = 100, bias = 0.1)
#' mse_sat(n = 40, n_e = Inf, bias = 0.1, tau = 0.05)
#'
#' @export
mse_sat <- function(n, n_e = Inf, bias = 0, tau = 0, sigma_t = 1, sigma_c = sigma_t) {
  check_numeric(n, "n", lower = 0, lower_open = TRUE)
  check_numeric(n_e, "n_e", lower = 0, lower_open = TRUE, allow_inf = TRUE)
  check_numeric(bias, "bias")
  check_numeric(tau, "tau", lower = 0)
  check_numeric(sigma_t, "sigma_t", lower = 0, lower_open = TRUE)
  check_numeric(sigma_c, "sigma_c", lower = 0, lower_open = TRUE)
  a <- recycle_args(n = n, n_e = n_e, bias = bias, tau = tau, sigma_t = sigma_t,
                    sigma_c = sigma_c)
  a[["sigma_t"]]^2 / a[["n"]] + a[["sigma_c"]]^2 / a[["n_e"]] + a[["bias"]]^2 + a[["tau"]]^2
}
