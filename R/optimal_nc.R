#' Optimal number of concurrent controls in a hybrid design
#'
#' Computes the number of concurrent controls that minimizes the mean squared
#' error of the hybrid design of \code{\link{mse_hybrid}} for a fixed total
#' sample size, when the control mean combines concurrent and external
#' controls with the optimal weight.
#'
#' @param n Total number of patients in the current trial.
#' @param n_e Number of patients in the external control group. \code{Inf}
#'   means a fixed threshold.
#' @param bias Fixed bias of the external control assumed for the
#'   calculation. Defaults to \code{0}.
#' @param tau Standard deviation of a random bias with mean zero. Defaults to
#'   \code{0}.
#' @param sigma_t Standard deviation of the outcome of one patient on the
#'   experimental treatment, on the scale of the treatment effect.
#' @param sigma_c Standard deviation for one control patient, concurrent or
#'   external. Defaults to \code{sigma_t}.
#'
#' @details With \eqn{A = \sigma_C^2 / n_E + b^2 + \tau^2}{A = sigma_c^2 / n_e + bias^2 + tau^2},
#'   the optimal number of concurrent controls is
#'   \deqn{n_C = \sigma_C / (\sigma_C + \sigma_T) (n - \sigma_C \sigma_T / A),}{n_c = sigma_c / (sigma_c + sigma_t) (n - sigma_c sigma_t / A),}
#'   truncated at zero. This is equation 6 of Pocock (1976) with the prior
#'   variance of the bias set to \code{bias^2 + tau^2}; with equal standard
#'   deviations it reduces to his equation 7,
#'   \eqn{(n - n_{eff}) / 2}{(n - n_eff) / 2}, where
#'   \eqn{n_{eff} = \sigma_C^2 / A}{n_eff = sigma_c^2 / A} is the effective
#'   number of external controls. The single-arm trial is optimal among the
#'   hybrid designs when the formula gives a value of zero or less.
#'
#' @return A data frame with one row per recycled set of arguments and the
#'   columns \code{n_c} (optimal number of concurrent controls, not rounded),
#'   \code{n_eff} (effective number of external controls) and \code{mse}
#'   (mean squared error at the optimum).
#'
#' @references Pocock SJ. The combination of randomized and historical
#'   controls in clinical trials. \emph{Journal of Chronic Diseases} 1976; 29:
#'   175--188.
#'
#' @seealso \code{\link{mse_hybrid}}
#'
#' @examples
#' # Example 2 of Pocock (1976): tumour response with 83 historical controls
#' optimal_nc(n = 200, n_e = 83, tau = 3, sigma_t = sqrt(45 * 55))
#' # The single-arm trial is optimal when the external control is precise
#' optimal_nc(n = 50, n_e = 200, bias = 0.1)
#'
#' @export
optimal_nc <- function(n, n_e, bias = 0, tau = 0, sigma_t = 1, sigma_c = sigma_t) {
  check_numeric(n, "n", lower = 0, lower_open = TRUE)
  check_numeric(n_e, "n_e", lower = 0, lower_open = TRUE, allow_inf = TRUE)
  check_numeric(bias, "bias")
  check_numeric(tau, "tau", lower = 0)
  check_numeric(sigma_t, "sigma_t", lower = 0, lower_open = TRUE)
  check_numeric(sigma_c, "sigma_c", lower = 0, lower_open = TRUE)
  a <- recycle_args(n = n, n_e = n_e, bias = bias, tau = tau, sigma_t = sigma_t,
                    sigma_c = sigma_c)
  ext <- a[["sigma_c"]]^2 / a[["n_e"]] + a[["bias"]]^2 + a[["tau"]]^2
  share <- a[["sigma_c"]] / (a[["sigma_c"]] + a[["sigma_t"]])
  n_c <- pmax(share * (a[["n"]] - a[["sigma_c"]] * a[["sigma_t"]] / ext), 0)
  n_eff <- a[["sigma_c"]]^2 / ext
  mse <- mse_hybrid(a[["n"]], n_c, a[["n_e"]], a[["bias"]], a[["tau"]], a[["sigma_t"]],
                    a[["sigma_c"]])[["mse"]]
  data.frame(n_c = n_c, n_eff = n_eff, mse = mse)
}
