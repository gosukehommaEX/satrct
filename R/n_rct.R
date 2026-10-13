#' Sample size of a randomized controlled trial
#'
#' Computes the total sample size at which a randomized controlled trial
#' (RCT) reaches the target power (see \code{\link{power_rct}}).
#'
#' @param delta True treatment effect assumed for the calculation. Must be
#'   positive.
#' @param power Target power. Must exceed \code{alpha}. Defaults to
#'   \code{0.8}.
#' @param sigma_t Standard deviation of the outcome of one patient on the
#'   experimental treatment, on the scale of the treatment effect.
#' @param sigma_c Standard deviation for one control patient. Defaults to
#'   \code{sigma_t}.
#' @param alpha One-sided significance level. Defaults to \code{0.025}.
#' @param r Fraction of patients allocated to the experimental treatment.
#'   Defaults to \code{0.5}.
#' @param method Either \code{"z"} (the default) or \code{"t"}; see
#'   \code{\link{power_rct}}.
#'
#' @details With \code{method = "z"} the sample size is
#'   \deqn{(z_{1 - \alpha} + z_{1 - \beta})^2 (\sigma_T^2 / r + \sigma_C^2 / (1 - r)) / \delta^2,}{(qnorm(1 - alpha) + qnorm(power))^2 (sigma_t^2 / r + sigma_c^2 / (1 - r)) / delta^2,}
#'   where \eqn{1 - \beta}{power} is the target power. With \code{method = "t"} it is
#'   found with \code{\link[stats]{uniroot}}. The result is not rounded, so
#'   that it can be compared across designs; round it up as needed, for
#'   example to an even number for 1:1 allocation.
#'
#' @return A numeric vector of total sample sizes. Vector arguments are
#'   recycled to a common length.
#'
#' @seealso \code{\link{power_rct}}, \code{\link{n_sat}}
#'
#' @examples
#' n_rct(delta = 0.5)
#' n_rct(delta = 0.5, method = "t")
#'
#' @export
n_rct <- function(delta, power = 0.8, sigma_t = 1, sigma_c = sigma_t, alpha = 0.025, r = 0.5,
                  method = c("z", "t")) {
  method <- match.arg(method)
  check_numeric(delta, "delta", lower = 0, lower_open = TRUE)
  check_numeric(power, "power", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  check_numeric(sigma_t, "sigma_t", lower = 0, lower_open = TRUE)
  check_numeric(sigma_c, "sigma_c", lower = 0, lower_open = TRUE)
  check_numeric(alpha, "alpha", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  check_numeric(r, "r", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  a <- recycle_args(delta = delta, power = power, sigma_t = sigma_t, sigma_c = sigma_c,
                    alpha = alpha, r = r)
  if (any(a[["power"]] <= a[["alpha"]])) {
    stop("'power' must exceed 'alpha'.", call. = FALSE)
  }
  k <- a[["sigma_t"]]^2 / a[["r"]] + a[["sigma_c"]]^2 / (1 - a[["r"]])
  z_sum <- qnorm(a[["alpha"]], lower.tail = FALSE) + qnorm(a[["power"]])
  n_z <- z_sum^2 * k / a[["delta"]]^2
  if (method == "z") {
    return(n_z)
  }
  if (any(abs(a[["sigma_t"]] - a[["sigma_c"]]) > sqrt(.Machine$double.eps) * a[["sigma_t"]])) {
    stop("method = \"t\" requires sigma_t == sigma_c.", call. = FALSE)
  }
  vapply(seq_along(n_z), function(i) {
    f <- function(n) {
      power_rct(n, a[["delta"]][i], a[["sigma_t"]][i], a[["sigma_c"]][i], a[["alpha"]][i],
                a[["r"]][i], method = "t") - a[["power"]][i]
    }
    lower <- max(n_z[i], 2 + 1e-6)
    uniroot(f, lower = lower, upper = 2 * lower + 10, extendInt = "upX",
            tol = 1e-10)$root
  }, numeric(1))
}
