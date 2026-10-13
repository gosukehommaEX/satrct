# Compare power_rct() and power_sat() with rpact (an independent implementation in another
# package).
#
# A single-arm trial compared with n_e external controls is a two-group comparison with
# allocation ratio n / n_e, null value equal to the margin and true difference delta + bias, so
# rpact::getPowerMeans() with groups = 2, thetaH0 = margin and allocationRatioPlanned = n / n_e
# computes the same power. Against a fixed threshold (n_e = Inf) it is the one-group test with
# groups = 1. normalApproximation = TRUE gives the z-test and FALSE the t-test. For a
# time-to-event endpoint, power_rct() with endpoint_sd("survival") is compared with
# rpact::getPowerSurvival() (Schoenfeld) for a given number of events.
#
# Run from the package root after devtools::load_all().
# Output: inst/validation/results/validate_rpact.csv

source("inst/validation/judge.R")
check_loaded()
script <- "validate_rpact"

if (!requireNamespace("rpact", quietly = TRUE)) {
  message("rpact is not installed: the comparison is not run and the result is INFO.")
  res <- judge(script, "rpact_unavailable", "rpact is not installed", "", NA, NA, "info")
} else {
  message("rpact ", as.character(utils::packageVersion("rpact")), " is used.")
  rpact_power <- function(x) {
    p <- x[["overallReject"]]
    if (is.null(p)) {
      stop("The field 'overallReject' was not found; available fields: ",
           paste(names(x), collapse = ", "), call. = FALSE)
    }
    as.numeric(p)
  }
  sigma <- 1.2
  res <- NULL

  # ---- single-arm trial with an external control or a threshold ----
  grid <- expand.grid(n = c(20, 40), n_e = c(40, 120, Inf), delta = c(0.3, 0.6),
                      bias = c(0, 0.1), margin = c(0, 0.1), method = c("z", "t"),
                      stringsAsFactors = FALSE)
  ours <- numeric(nrow(grid))
  theirs <- numeric(nrow(grid))
  for (i in seq_len(nrow(grid))) {
    x <- grid[i, ]
    ours[i] <- power_sat(x[["n"]], x[["delta"]], x[["n_e"]], x[["bias"]], x[["margin"]],
                         sigma_t = sigma, method = x[["method"]])
    if (is.infinite(x[["n_e"]])) {
      fit <- rpact::getPowerMeans(groups = 1, thetaH0 = x[["margin"]],
                                  alternative = x[["delta"]] + x[["bias"]], stDev = sigma,
                                  maxNumberOfSubjects = x[["n"]],
                                  normalApproximation = x[["method"]] == "z",
                                  alpha = 0.025, sided = 1)
    } else {
      fit <- rpact::getPowerMeans(groups = 2, thetaH0 = x[["margin"]],
                                  alternative = x[["delta"]] + x[["bias"]], stDev = sigma,
                                  maxNumberOfSubjects = x[["n"]] + x[["n_e"]],
                                  allocationRatioPlanned = x[["n"]] / x[["n_e"]],
                                  normalApproximation = x[["method"]] == "z",
                                  alpha = 0.025, sided = 1)
    }
    theirs[i] <- rpact_power(fit)
  }
  for (m in c("z", "t")) {
    k <- grid[["method"]] == m
    res <- rbind(res, judge(
      script, paste0("power_sat_", m),
      sprintf("power_sat(method = \"%s\") against rpact::getPowerMeans()", m),
      sprintf("%d settings", sum(k)), 0, max(abs(ours[k] - theirs[k])), "abs", tol = 1e-8,
      note = "largest absolute difference"))
  }

  # ---- randomized controlled trial ----
  grid <- expand.grid(n = c(20, 60), delta = c(0.4, 0.8), r = c(0.5, 2 / 3),
                      method = c("z", "t"), stringsAsFactors = FALSE)
  ours <- numeric(nrow(grid))
  theirs <- numeric(nrow(grid))
  for (i in seq_len(nrow(grid))) {
    x <- grid[i, ]
    ours[i] <- power_rct(x[["n"]], x[["delta"]], sigma_t = sigma, r = x[["r"]],
                         method = x[["method"]])
    fit <- rpact::getPowerMeans(groups = 2, thetaH0 = 0, alternative = x[["delta"]],
                                stDev = sigma, maxNumberOfSubjects = x[["n"]],
                                allocationRatioPlanned = x[["r"]] / (1 - x[["r"]]),
                                normalApproximation = x[["method"]] == "z",
                                alpha = 0.025, sided = 1)
    theirs[i] <- rpact_power(fit)
  }
  for (m in c("z", "t")) {
    k <- grid[["method"]] == m
    res <- rbind(res, judge(
      script, paste0("power_rct_", m),
      sprintf("power_rct(method = \"%s\") against rpact::getPowerMeans()", m),
      sprintf("%d settings", sum(k)), 0, max(abs(ours[k] - theirs[k])), "abs", tol = 1e-8,
      note = "largest absolute difference"))
  }

  # ---- time-to-event endpoint, 1:1 RCT with a given number of events ----
  grid <- expand.grid(events = c(40, 90, 200), hazard_ratio = c(0.6, 0.75),
                      stringsAsFactors = FALSE)
  prob_event <- 0.6
  ours <- numeric(nrow(grid))
  theirs <- numeric(nrow(grid))
  for (i in seq_len(nrow(grid))) {
    x <- grid[i, ]
    ours[i] <- power_rct(x[["events"]] / prob_event, -log(x[["hazard_ratio"]]),
                         sigma_t = endpoint_sd("survival", prob_event = prob_event))
    fit <- rpact::getPowerSurvival(hazardRatio = x[["hazard_ratio"]], lambda2 = 0.05,
                                   maxNumberOfEvents = x[["events"]],
                                   maxNumberOfSubjects = ceiling(x[["events"]] / prob_event),
                                   directionUpper = FALSE, alpha = 0.025, sided = 1)
    theirs[i] <- rpact_power(fit)
  }
  res <- rbind(res, judge(
    script, "power_rct_survival",
    "power_rct() with endpoint_sd(\"survival\") against rpact::getPowerSurvival() (Schoenfeld)",
    sprintf("%d settings", nrow(grid)), 0, max(abs(ours - theirs)), "abs", tol = 1e-8,
    note = "largest absolute difference"))
}

finalize_results(res, "inst/validation/results")
