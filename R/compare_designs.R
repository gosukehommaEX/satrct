#' Compare a single-arm trial and an RCT over a grid of settings
#'
#' Evaluates the main quantities of the package for every combination of the
#' supplied sample sizes, effects, external control sizes, biases, margins
#' and random-bias standard deviations, using the z-test throughout.
#'
#' @param n Total numbers of patients in the current trial.
#' @param delta True treatment effects assumed for power.
#' @param n_e Numbers of patients in the external control group. \code{Inf}
#'   (the default) means comparison with a fixed threshold.
#' @param bias Fixed biases of the single-arm comparison. Defaults to
#'   \code{0}.
#' @param margin Bias margins of the single-arm test. Defaults to \code{0}.
#' @param tau Standard deviations of a random bias with mean zero. Defaults
#'   to \code{0}.
#' @param sigma_t Standard deviation of the outcome of one patient on the
#'   experimental treatment, on the scale of the treatment effect. A single
#'   number.
#' @param sigma_c Standard deviation for one control patient. A single
#'   number. Defaults to \code{sigma_t}.
#' @param alpha One-sided significance level. A single number. Defaults to
#'   \code{0.025}.
#' @param r Fraction of patients allocated to the experimental treatment in
#'   the RCT. A single number. Defaults to \code{0.5}.
#'
#' @return A data frame with one row for each combination of \code{n},
#'   \code{delta}, \code{n_e}, \code{bias}, \code{margin} and \code{tau}
#'   (formed by \code{\link[base]{expand.grid}}), these six columns, and
#'   \describe{
#'     \item{\code{mse_rct}, \code{mse_sat}}{Mean squared errors from
#'       \code{\link{mse_rct}} and \code{\link{mse_sat}}.}
#'     \item{\code{breakeven_bias}}{Break-even bias from
#'       \code{\link{breakeven_bias}}.}
#'     \item{\code{power_rct}, \code{power_sat}}{Power from
#'       \code{\link{power_rct}} and \code{\link{power_sat}}.}
#'     \item{\code{type1_sat}}{Type I error of the single-arm trial, that is,
#'       \code{\link{power_sat}} with \code{delta = 0}.}
#'     \item{\code{critical_margin}}{Critical margin from
#'       \code{\link{critical_margin}}.}
#'     \item{\code{alpha_matched}}{Matched significance level from
#'       \code{\link{matched_alpha}}.}
#'   }
#'
#' @seealso \code{\link{power_sat}}, \code{\link{matched_alpha}}
#'
#' @examples
#' compare_designs(n = c(20, 40), delta = 0.6, n_e = 100, bias = c(0, 0.1),
#'                 margin = 0.1)
#'
#' @export
compare_designs <- function(n, delta, n_e = Inf, bias = 0, margin = 0, tau = 0, sigma_t = 1,
                            sigma_c = sigma_t, alpha = 0.025, r = 0.5) {
  check_numeric(n, "n", lower = 0, lower_open = TRUE)
  check_numeric(delta, "delta")
  check_numeric(n_e, "n_e", lower = 0, lower_open = TRUE, allow_inf = TRUE)
  check_numeric(bias, "bias")
  check_numeric(margin, "margin")
  check_numeric(tau, "tau", lower = 0)
  check_numeric(sigma_t, "sigma_t", lower = 0, lower_open = TRUE, scalar = TRUE)
  check_numeric(sigma_c, "sigma_c", lower = 0, lower_open = TRUE, scalar = TRUE)
  check_numeric(alpha, "alpha", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE,
                scalar = TRUE)
  check_numeric(r, "r", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE,
                scalar = TRUE)
  grid <- expand.grid(n = n, delta = delta, n_e = n_e, bias = bias, margin = margin, tau = tau,
                      KEEP.OUT.ATTRS = FALSE)
  g_n <- grid[["n"]]
  g_delta <- grid[["delta"]]
  g_n_e <- grid[["n_e"]]
  g_bias <- grid[["bias"]]
  g_margin <- grid[["margin"]]
  g_tau <- grid[["tau"]]
  grid[["mse_rct"]] <- mse_rct(g_n, sigma_t, sigma_c, r)
  grid[["mse_sat"]] <- mse_sat(g_n, g_n_e, g_bias, g_tau, sigma_t, sigma_c)
  grid[["breakeven_bias"]] <- breakeven_bias(g_n, g_n_e, sigma_t, sigma_c, r)
  grid[["power_rct"]] <- power_rct(g_n, g_delta, sigma_t, sigma_c, alpha, r)
  grid[["power_sat"]] <- power_sat(g_n, g_delta, g_n_e, g_bias, g_margin, g_tau, sigma_t,
                                   sigma_c, alpha)
  grid[["type1_sat"]] <- power_sat(g_n, 0, g_n_e, g_bias, g_margin, g_tau, sigma_t, sigma_c,
                                   alpha)
  grid[["critical_margin"]] <- critical_margin(g_n, g_delta, g_n_e, sigma_t, sigma_c, r)
  grid[["alpha_matched"]] <- matched_alpha(g_n, g_delta, g_n_e, g_margin, sigma_t, sigma_c,
                                           alpha, r)[["alpha_matched"]]
  grid
}
