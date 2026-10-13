#' Standard deviation of one patient on the scale of the treatment effect
#'
#' Converts the parameters of a continuous, binary or time-to-event endpoint
#' into the standard deviation of one patient that the normal-approximation
#' functions of the package use as \code{sigma_t} or \code{sigma_c}.
#'
#' @param type One of \code{"continuous"}, \code{"binary"} and
#'   \code{"survival"}.
#' @param sigma Standard deviation of the outcome, for
#'   \code{type = "continuous"}.
#' @param p Response probability, for \code{type = "binary"}, with values in
#'   \code{(0, 1)}.
#' @param prob_event Probability that a patient has an event during the
#'   trial, for \code{type = "survival"}, with values in \code{(0, 1]}.
#'
#' @details For a continuous endpoint the treatment effect is a difference in
#'   means and the standard deviation is \code{sigma}. For a binary endpoint
#'   the effect is a difference in response probabilities and the standard
#'   deviation is \eqn{\sqrt{p (1 - p)}}{sqrt(p (1 - p))}, evaluated at the
#'   planning value of \code{p}. For a time-to-event endpoint the effect is
#'   the negative log hazard ratio, whose variance is approximately the sum of
#'   the reciprocals of the numbers of events in the two groups, so the
#'   standard deviation per patient is
#'   \eqn{1 / \sqrt{\psi}}{1 / sqrt(prob_event)}, where \eqn{\psi}{prob_event}
#'   is the event probability. These are normal approximations; for small
#'   binary trials see \code{\link{oc_binary_exact}} and
#'   \code{\link{oc_sat_binary_exact}}.
#'
#' @return A numeric vector of standard deviations.
#'
#' @seealso \code{\link{power_sat}}, \code{\link{power_rct}}
#'
#' @examples
#' endpoint_sd("binary", p = c(0.2, 0.3))
#' endpoint_sd("survival", prob_event = 0.64)
#' # Power of an RCT with a binary endpoint, response 0.5 versus 0.3
#' power_rct(n = 200, delta = 0.2, sigma_t = endpoint_sd("binary", p = 0.5),
#'           sigma_c = endpoint_sd("binary", p = 0.3))
#'
#' @export
endpoint_sd <- function(type = c("continuous", "binary", "survival"), sigma = NULL, p = NULL,
                        prob_event = NULL) {
  type <- match.arg(type)
  if (type == "continuous") {
    if (is.null(sigma)) {
      stop("'sigma' is required for type = \"continuous\".", call. = FALSE)
    }
    check_numeric(sigma, "sigma", lower = 0, lower_open = TRUE)
    return(sigma)
  }
  if (type == "binary") {
    if (is.null(p)) {
      stop("'p' is required for type = \"binary\".", call. = FALSE)
    }
    check_numeric(p, "p", lower = 0, upper = 1, lower_open = TRUE, upper_open = TRUE)
    return(sqrt(p * (1 - p)))
  }
  if (is.null(prob_event)) {
    stop("'prob_event' is required for type = \"survival\".", call. = FALSE)
  }
  check_numeric(prob_event, "prob_event", lower = 0, upper = 1, lower_open = TRUE)
  1 / sqrt(prob_event)
}
