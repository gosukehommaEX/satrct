#' satrct: Single-Arm Trials Versus Randomized Trials of the Same Size
#'
#' The package compares two designs that enrol the same total number of
#' patients in the current trial. The first is a randomized controlled trial
#' (RCT). The second is a single-arm trial (SAT) in which all patients receive
#' the experimental treatment and are compared with an external control group
#' of size \code{n_e}, or with a fixed threshold when \code{n_e = Inf}. The
#' comparison with the external control may be biased by an amount
#' \code{bias}, defined as the expectation of the estimated treatment effect
#' minus the true effect.
#'
#' The main functions are:
#' \itemize{
#'   \item \code{\link{mse_rct}}, \code{\link{mse_sat}},
#'     \code{\link{breakeven_bias}} and \code{\link{breakeven_n}} for the mean
#'     squared error of the two designs and the bias, or sample size, at which
#'     they are equal.
#'   \item \code{\link{power_rct}}, \code{\link{power_sat}},
#'     \code{\link{n_rct}} and \code{\link{n_sat}} for power, type I error and
#'     sample size, where the single-arm comparison may use a prespecified bias
#'     margin.
#'   \item \code{\link{critical_margin}} and \code{\link{matched_alpha}} for
#'     the margin at which the two designs have equal power and the relaxed
#'     significance level at which an RCT matches the power of a single-arm
#'     trial with a margin.
#'   \item \code{\link{mse_hybrid}} and \code{\link{optimal_nc}} for hybrid
#'     designs that combine concurrent and external controls.
#'   \item \code{\link{oc_sat_binary_exact}} and \code{\link{oc_binary_exact}}
#'     for exact operating characteristics with binary endpoints.
#'   \item \code{\link{compare_designs}} and \code{\link{endpoint_sd}} as
#'     helpers.
#' }
#'
#' All tests are one-sided, and a larger treatment effect favours the
#' experimental treatment.
#'
#' @references
#' Pocock SJ. The combination of randomized and historical controls in
#' clinical trials. \emph{Journal of Chronic Diseases} 1976; 29: 175--188.
#'
#' Collignon O, Schritz A, Senn SJ, Spezia R. Clustered allocation as a way of
#' understanding historical controls: components of variation and regulatory
#' considerations. \emph{Statistical Methods in Medical Research} 2020; 29:
#' 1960--1971.
#'
#' @keywords internal
#' @importFrom stats dbinom pbinom pnorm pt qnorm qt uniroot
"_PACKAGE"
