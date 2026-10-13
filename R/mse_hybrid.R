#' Mean squared error of a hybrid design with concurrent and external controls
#'
#' Computes the mean squared error of the estimated treatment effect when
#' \code{n - n_c} patients of the current trial receive the experimental
#' treatment, \code{n_c} are randomized to a concurrent control group, and
#' the control mean is estimated by a weighted average of the concurrent and
#' external control means.
#'
#' @param n Total number of patients in the current trial.
#' @param n_c Number of concurrent controls, with \code{0 <= n_c < n}.
#'   \code{n_c = 0} gives the single-arm trial.
#' @param n_e Number of patients in the external control group. \code{Inf}
#'   means a fixed threshold.
#' @param bias Fixed bias of the external control, on the scale of the
#'   treatment effect. Defaults to \code{0}.
#' @param tau Standard deviation of a random bias with mean zero. Defaults to
#'   \code{0}.
#' @param sigma_t Standard deviation of the outcome of one patient on the
#'   experimental treatment, on the scale of the treatment effect.
#' @param sigma_c Standard deviation for one control patient, concurrent or
#'   external. Defaults to \code{sigma_t}.
#' @param weight Weight \eqn{w}{w} of the concurrent control mean, in
#'   \code{[0, 1]}. \code{NULL} (the default) uses the weight that minimizes
#'   the mean squared error for the assumed \code{bias} and \code{tau}, which
#'   is the weight of Pocock (1976) with prior variance
#'   \code{bias^2 + tau^2}. A positive weight requires \code{n_c > 0}.
#'
#' @details Write \eqn{A = \sigma_C^2 / n_E + b^2 + \tau^2}{A = sigma_c^2 / n_e + bias^2 + tau^2}
#'   and \eqn{B = \sigma_C^2 / n_C}{B = sigma_c^2 / n_c}. The mean squared
#'   error is
#'   \deqn{\sigma_T^2 / (n - n_C) + w^2 B + (1 - w)^2 A,}{sigma_t^2 / (n - n_c) + w^2 B + (1 - w)^2 A,}
#'   and the default weight is \eqn{A / (A + B)}{A / (A + B)}. The design
#'   reduces to the single-arm trial of \code{\link{mse_sat}} when
#'   \code{n_c = 0}, and to an RCT that ignores the external controls when
#'   \code{weight = 1}.
#'
#' @return A data frame with one row per recycled set of arguments and the
#'   columns \code{weight} and \code{mse}.
#'
#' @references Pocock SJ. The combination of randomized and historical
#'   controls in clinical trials. \emph{Journal of Chronic Diseases} 1976; 29:
#'   175--188.
#'
#' @seealso \code{\link{optimal_nc}}, \code{\link{mse_sat}},
#'   \code{\link{mse_rct}}
#'
#' @examples
#' mse_hybrid(n = 100, n_c = 20, n_e = 200, bias = 0.1)
#' mse_hybrid(n = 100, n_c = c(0, 20, 50), n_e = 200, bias = 0.1)
#'
#' @export
mse_hybrid <- function(n, n_c, n_e, bias = 0, tau = 0, sigma_t = 1, sigma_c = sigma_t,
                       weight = NULL) {
  check_numeric(n, "n", lower = 0, lower_open = TRUE)
  check_numeric(n_c, "n_c", lower = 0)
  check_numeric(n_e, "n_e", lower = 0, lower_open = TRUE, allow_inf = TRUE)
  check_numeric(bias, "bias")
  check_numeric(tau, "tau", lower = 0)
  check_numeric(sigma_t, "sigma_t", lower = 0, lower_open = TRUE)
  check_numeric(sigma_c, "sigma_c", lower = 0, lower_open = TRUE)
  if (!is.null(weight)) {
    check_numeric(weight, "weight", lower = 0, upper = 1)
  }
  a <- recycle_args(n = n, n_c = n_c, n_e = n_e, bias = bias, tau = tau, sigma_t = sigma_t,
                    sigma_c = sigma_c, weight = if (is.null(weight)) NA_real_ else weight)
  if (any(a[["n_c"]] >= a[["n"]])) {
    stop("'n_c' must be smaller than 'n'.", call. = FALSE)
  }
  ext <- a[["sigma_c"]]^2 / a[["n_e"]] + a[["bias"]]^2 + a[["tau"]]^2
  conc <- ifelse(a[["n_c"]] > 0, a[["sigma_c"]]^2 / a[["n_c"]], Inf)
  if (is.null(weight)) {
    w <- ext / (conc + ext)
  } else {
    w <- a[["weight"]]
    if (any(a[["n_c"]] <= 0 & w > 0)) {
      stop("A positive 'weight' requires 'n_c' > 0.", call. = FALSE)
    }
  }
  conc_term <- ifelse(a[["n_c"]] > 0, w^2 * conc, 0)
  mse <- a[["sigma_t"]]^2 / (a[["n"]] - a[["n_c"]]) + conc_term + (1 - w)^2 * ext
  data.frame(weight = w, mse = mse)
}
