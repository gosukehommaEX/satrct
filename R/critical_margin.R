#' Margin at which a single-arm trial and an RCT have equal power
#'
#' Computes the bias margin at which a single-arm trial with an external
#' control (see \code{\link{power_sat}}) and the randomized controlled trial
#' (RCT) of the same total sample size have equal power when the single-arm
#' comparison is unbiased and both use the same significance level.
#'
#' @param n Total number of patients in the current trial.
#' @param delta True treatment effect assumed for the power calculation.
#' @param n_e Number of patients in the external control group. \code{Inf}
#'   (the default) means comparison with a fixed threshold.
#' @param sigma_t Standard deviation of the outcome of one patient on the
#'   experimental treatment, on the scale of the treatment effect.
#' @param sigma_c Standard deviation for one control patient. Defaults to
#'   \code{sigma_t}.
#' @param r Fraction of patients allocated to the experimental treatment in
#'   the RCT. Defaults to \code{0.5}.
#'
#' @details The critical margin is
#'   \eqn{m_c = \delta (1 - \sqrt{V_S / V_R})}{m_c = delta (1 - sqrt(V_S / V_R))},
#'   where \eqn{V_R}{V_R} and \eqn{V_S}{V_S} are the variances given by
#'   \code{\link{mse_rct}} and by \code{\link{mse_sat}} with no bias. With 1:1
#'   allocation and equal standard deviations it is
#'   \eqn{\delta (1 - \sqrt{1 + n / n_E} / 2)}{delta (1 - sqrt(1 + n / n_e) / 2)}.
#'   A single-arm trial with a margin smaller than \eqn{m_c}{m_c} has higher
#'   power than the RCT at zero bias. The critical margin does not depend on
#'   the significance level, and depends on \code{n} and \code{n_e} only
#'   through their ratio. It is negative when \eqn{V_S > V_R}{V_S > V_R}. It is
#'   also the bias at which the type I error of the single-arm trial reaches
#'   the significance level returned by \code{\link{matched_alpha}}.
#'
#' @return A numeric vector of critical margins. Vector arguments are recycled
#'   to a common length.
#'
#' @seealso \code{\link{matched_alpha}}, \code{\link{power_sat}}
#'
#' @examples
#' critical_margin(n = 40, delta = 0.8, n_e = 100)
#' critical_margin(n = 40, delta = 0.8, n_e = Inf)
#'
#' @export
critical_margin <- function(n, delta, n_e = Inf, sigma_t = 1, sigma_c = sigma_t, r = 0.5) {
  check_numeric(n, "n", lower = 0, lower_open = TRUE)
  check_numeric(delta, "delta")
  check_numeric(n_e, "n_e", lower = 0, lower_open = TRUE, allow_inf = TRUE)
  check_numeric(sigma_t, "sigma_t", lower = 0, lower_open = TRUE)
  check_numeric(sigma_c, "sigma_c", lower = 0, lower_open = TRUE)
  check_numeric(r, "r", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  a <- recycle_args(n = n, delta = delta, n_e = n_e, sigma_t = sigma_t, sigma_c = sigma_c,
                    r = r)
  v_r <- mse_rct(a[["n"]], a[["sigma_t"]], a[["sigma_c"]], a[["r"]])
  v_s <- mse_sat(a[["n"]], a[["n_e"]], 0, 0, a[["sigma_t"]], a[["sigma_c"]])
  a[["delta"]] * (1 - sqrt(v_s / v_r))
}
