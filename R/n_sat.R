#' Sample size of a single-arm trial with a bias margin
#'
#' Computes the number of patients on the experimental treatment at which a
#' single-arm trial compared with an external control group of fixed size
#' \code{n_e} reaches the target power (see \code{\link{power_sat}}).
#'
#' @param delta True treatment effect assumed for the calculation.
#' @param power Target power. Must exceed \code{alpha}. Defaults to
#'   \code{0.8}.
#' @param n_e Number of patients in the external control group. \code{Inf}
#'   (the default) means comparison with a fixed threshold.
#' @param bias Fixed bias of the comparison assumed for the calculation.
#'   Defaults to \code{0}.
#' @param margin Bias margin of the test. Defaults to \code{0}.
#' @param sigma_t Standard deviation of the outcome of one patient on the
#'   experimental treatment, on the scale of the treatment effect.
#' @param sigma_c Standard deviation for one external control patient.
#'   Defaults to \code{sigma_t}.
#' @param alpha One-sided significance level. Defaults to \code{0.025}.
#' @param method Either \code{"z"} (the default) or \code{"t"}; see
#'   \code{\link{power_sat}}.
#'
#' @details Write \eqn{d = \delta + b - m}{d = delta + bias - margin}. With
#'   \code{method = "z"} the sample size is
#'   \deqn{\sigma_T^2 / ((d / (z_{1 - \alpha} + z_{1 - \beta}))^2 - \sigma_C^2 / n_E),}{sigma_t^2 / ((d / (qnorm(1 - alpha) + qnorm(power)))^2 - sigma_c^2 / n_e),}
#'   where \eqn{1 - \beta}{power} is the target power. Because the external
#'   control group has a fixed size, the power cannot exceed
#'   \eqn{\Phi(d / (\sigma_C / \sqrt{n_E}) - z_{1 - \alpha})}{pnorm(d / (sigma_c / sqrt(n_e)) - qnorm(1 - alpha))}
#'   however many patients are treated. When the target power is not below
#'   this limit, or when \eqn{d \le 0}{d <= 0}, the function returns
#'   \code{Inf}. With \code{method = "t"} the sample size is found with
#'   \code{\link[stats]{uniroot}}. The result is not rounded.
#'
#' @return A numeric vector of sample sizes, as continuous planning values to
#'   be rounded up to whole numbers of patients. Vector arguments are
#'   recycled to a common length.
#'
#' @seealso \code{\link{power_sat}}, \code{\link{n_rct}}
#'
#' @examples
#' n_sat(delta = 0.5)
#' n_sat(delta = 0.5, n_e = 100, margin = 0.1)
#' # Not attainable with only 20 external controls
#' n_sat(delta = 0.5, n_e = 20)
#'
#' @export
n_sat <- function(delta, power = 0.8, n_e = Inf, bias = 0, margin = 0, sigma_t = 1,
                  sigma_c = sigma_t, alpha = 0.025, method = c("z", "t")) {
  method <- match.arg(method)
  check_numeric(delta, "delta")
  check_numeric(power, "power", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  check_numeric(n_e, "n_e", lower = 0, lower_open = TRUE, allow_inf = TRUE)
  check_numeric(bias, "bias")
  check_numeric(margin, "margin")
  check_numeric(sigma_t, "sigma_t", lower = 0, lower_open = TRUE)
  check_numeric(sigma_c, "sigma_c", lower = 0, lower_open = TRUE)
  check_numeric(alpha, "alpha", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  a <- recycle_args(delta = delta, power = power, n_e = n_e, bias = bias, margin = margin,
                    sigma_t = sigma_t, sigma_c = sigma_c, alpha = alpha)
  if (any(a[["power"]] <= a[["alpha"]])) {
    stop("'power' must exceed 'alpha'.", call. = FALSE)
  }
  if (method == "t" &&
      any(abs(a[["sigma_t"]] - a[["sigma_c"]]) > sqrt(.Machine$double.eps) * a[["sigma_t"]])) {
    stop("method = \"t\" requires sigma_t == sigma_c.", call. = FALSE)
  }
  d <- a[["delta"]] + a[["bias"]] - a[["margin"]]
  z_alpha <- qnorm(a[["alpha"]], lower.tail = FALSE)
  z_sum <- z_alpha + qnorm(a[["power"]])
  # Power that is approached as the number of treated patients grows
  power_limit <- ifelse(is.infinite(a[["n_e"]]), ifelse(d > 0, 1, 0),
                        pnorm(d / (a[["sigma_c"]] / sqrt(a[["n_e"]])) - z_alpha))
  attainable <- d > 0 & power_limit > a[["power"]]
  denominator <- (d / z_sum)^2 - a[["sigma_c"]]^2 / a[["n_e"]]
  n_z <- ifelse(attainable, a[["sigma_t"]]^2 / denominator, Inf)
  if (method == "z") {
    return(n_z)
  }
  vapply(seq_along(n_z), function(i) {
    if (!attainable[i]) {
      return(Inf)
    }
    f <- function(n) {
      power_sat(n, a[["delta"]][i], a[["n_e"]][i], a[["bias"]][i], a[["margin"]][i], 0,
                a[["sigma_t"]][i], a[["sigma_c"]][i], a[["alpha"]][i], method = "t") -
        a[["power"]][i]
    }
    df_bound <- if (is.infinite(a[["n_e"]][i])) 1 else 2 - a[["n_e"]][i]
    lower <- max(n_z[i], df_bound + 1e-6, 1e-6)
    uniroot(f, lower = lower, upper = 2 * lower + 10, extendInt = "upX",
            tol = 1e-10)$root
  }, numeric(1))
}
