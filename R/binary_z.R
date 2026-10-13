#' One-sided z statistics for the difference of two proportions
#'
#' Internal helper for \code{\link{oc_binary_exact}}. Computes, for vectors of
#' success counts, the z statistic of the Farrington and Manning score test of
#' the null hypothesis \code{p1 - p2 <= margin}, or the signed square root of
#' the Pearson chi-squared statistic with or without the Yates continuity
#' correction, as in \code{\link[stats]{prop.test}}.
#'
#' @param y1,y2 Numbers of successes in groups 1 and 2.
#' @param n1,n2 Group sizes.
#' @param margin Margin for the score test. Must be \code{0} for the Pearson
#'   tests.
#' @param test One of \code{"score"}, \code{"pearson"} and
#'   \code{"pearson_cc"}.
#'
#' @return A numeric vector of z statistics, with \code{NA} where the standard
#'   error is zero.
#' @keywords internal
#' @noRd
binary_z <- function(y1, n1, y2, n2, margin, test) {
  p1 <- y1 / n1
  p2 <- y2 / n2
  if (test == "score") {
    # Restricted maximum likelihood estimates under p1 - p2 = margin
    # (Farrington and Manning, 1990), from the closed-form cubic solution
    theta <- n2 / n1
    ca <- 1 + theta
    cb <- -(1 + theta + p1 + theta * p2 + margin * (theta + 2))
    cc <- margin^2 + margin * (2 * p1 + theta + 1) + p1 + theta * p2
    cd <- -p1 * margin * (1 + margin)
    v <- cb^3 / (27 * ca^3) - cb * cc / (6 * ca^2) + cd / (2 * ca)
    u <- sign(v) * sqrt(pmax(cb^2 / (9 * ca^2) - cc / (3 * ca), 0))
    ratio <- ifelse(abs(u) > 0, pmin(pmax(v / u^3, -1), 1), 0)
    w <- (pi + acos(ratio)) / 3
    p1_r <- pmin(pmax(2 * u * cos(w) - cb / (3 * ca), 0), 1)
    p2_r <- pmin(pmax(p1_r - margin, 0), 1)
    se <- sqrt(p1_r * (1 - p1_r) / n1 + p2_r * (1 - p2_r) / n2)
    z <- (p1 - p2 - margin) / se
  } else {
    s <- 1 / n1 + 1 / n2
    p_pool <- (y1 + y2) / (n1 + n2)
    diff <- p1 - p2
    yates <- if (test == "pearson_cc") pmin(0.5, abs(diff) / s) else 0
    z <- sign(diff) * (abs(diff) - yates * s) / sqrt(p_pool * (1 - p_pool) * s)
  }
  z[!is.finite(z)] <- NA_real_
  z
}
