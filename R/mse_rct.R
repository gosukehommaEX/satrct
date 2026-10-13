#' Mean squared error of a randomized controlled trial
#'
#' Computes the mean squared error, equal to the variance, of the estimated
#' treatment effect in a randomized controlled trial (RCT) with total sample
#' size \code{n}, of which a fraction \code{r} is allocated to the
#' experimental treatment.
#'
#' @param n Total number of patients in the current trial.
#' @param sigma_t Standard deviation of the outcome of one patient on the
#'   experimental treatment, on the scale of the treatment effect. See
#'   \code{\link{endpoint_sd}} for binary and time-to-event endpoints.
#' @param sigma_c Standard deviation for one control patient. Defaults to
#'   \code{sigma_t}.
#' @param r Fraction of patients allocated to the experimental treatment. The
#'   default \code{0.5} gives 1:1 allocation.
#'
#' @details The mean squared error is
#'   \deqn{\sigma_T^2 / (r n) + \sigma_C^2 / ((1 - r) n).}{sigma_t^2 / (r n) + sigma_c^2 / ((1 - r) n).}
#'   Group sizes are treated as continuous, so \code{r * n} need not be a
#'   whole number.
#'
#' @return A numeric vector. Vector arguments are recycled to a common
#'   length.
#'
#' @seealso \code{\link{mse_sat}}, \code{\link{breakeven_bias}}
#'
#' @examples
#' mse_rct(n = 40)
#' mse_rct(n = 30, sigma_t = 1.2, sigma_c = 0.8, r = 2 / 3)
#'
#' @export
mse_rct <- function(n, sigma_t = 1, sigma_c = sigma_t, r = 0.5) {
  check_numeric(n, "n", lower = 0, lower_open = TRUE)
  check_numeric(sigma_t, "sigma_t", lower = 0, lower_open = TRUE)
  check_numeric(sigma_c, "sigma_c", lower = 0, lower_open = TRUE)
  check_numeric(r, "r", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
  a <- recycle_args(n = n, sigma_t = sigma_t, sigma_c = sigma_c, r = r)
  a[["sigma_t"]]^2 / (a[["r"]] * a[["n"]]) + a[["sigma_c"]]^2 / ((1 - a[["r"]]) * a[["n"]])
}
