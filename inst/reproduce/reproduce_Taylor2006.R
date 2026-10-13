# Reproduce Table 2 (p.345) of Taylor JMG, Braun TM, Li Z (2006). Comparing an experimental
# agent to a standard agent: relative merits of a one-arm or randomized two-arm Phase II design.
# Clin Trials 3: 335-348.
#
# This is the closest precedent of a comparison of the two designs at the same total sample
# size. The calculation is specific to the model of the paper and uses base R only; it checks
# our reading of the paper rather than functions of the package.
#
# Model (pp.337-338, Table 1): control rate at institution i, p0i ~ U(p0 - omega, p0 + omega);
# multiplicative effect d_i ~ U(1 + mu - phi, 1 + mu + phi) with p1i = p0i * d_i; historical
# guess p* ~ U(p0 - theta, p0 + theta). Table 2: M = 5 studies of n = 40 patients, phi = 0.05,
# proceed to Phase III if T > 0.05, with
#   one-arm: T1 = mean(Y_m / n) - p*,  two-arm: T2 = mean((Y1_m - Y0_m) / (n / 2)).
# Three conventions are not stated in the paper and are needed to reproduce it:
#   (1) the M one-arm studies share one historical guess p*,
#   (2) in the two-arm design a tie (T2 = 0.05) counts one half,
#   (3) E{p1i} = p0 + 0.10 means mu = 0.10 / p0.
# Probabilities are computed exactly, by Gauss-Legendre quadrature over the uniform
# distributions and exact binomial sums. The paper's values are Monte Carlo estimates printed to
# two decimals; a difference of at most 0.01 (rounding plus simulation error) is a PASS.
#
# Run from the package root.
# Output: inst/reproduce/results/reproduce_Taylor2006.csv

source("inst/validation/judge.R")
script <- "reproduce_Taylor2006"

n_study <- 40
m_study <- 5
phi <- 0.05
threshold <- 0.05

# Gauss-Legendre nodes and weights on [-1, 1] (Golub-Welsch algorithm)
gauss_legendre <- function(k) {
  i <- seq_len(k - 1)
  b <- i / sqrt(4 * i^2 - 1)
  jacobi <- matrix(0, k, k)
  jacobi[cbind(i, i + 1)] <- b
  jacobi[cbind(i + 1, i)] <- b
  e <- eigen(jacobi, symmetric = TRUE)
  list(x = e[["values"]], w = 2 * e[["vectors"]][1, ]^2)
}
gl <- gauss_legendre(48)

unif_nodes <- function(center, half) {
  list(x = center + half * gl[["x"]], w = gl[["w"]] / 2)
}

# Quadrature nodes for (p0i, p1i) and their weights
rate_nodes <- function(p0, omega, mu) {
  a <- unif_nodes(p0, omega)
  b <- unif_nodes(1 + mu, phi)
  na <- length(a[["x"]])
  nb <- length(b[["x"]])
  p_control <- rep(a[["x"]], each = nb)
  p_new <- p_control * rep(b[["x"]], times = na)
  list(p_control = pmin(pmax(p_control, 0), 1), p_new = pmin(pmax(p_new, 0), 1),
       w = rep(a[["w"]], each = nb) * rep(b[["w"]], times = na))
}

# Exact convolution of two probability vectors
convolve_pmf <- function(a, b) {
  out <- numeric(length(a) + length(b) - 1)
  for (i in seq_along(a)) {
    idx <- i:(i + length(b) - 1)
    out[idx] <- out[idx] + a[i] * b
  }
  out
}
convolve_power <- function(f, m) {
  out <- 1
  for (j in seq_len(m)) {
    out <- convolve_pmf(out, f)
  }
  out
}

prob_one_arm <- function(p0, omega, mu, theta) {
  nodes <- rate_nodes(p0, omega, mu)
  dens <- vapply(nodes[["p_new"]], function(p) stats::dbinom(0:n_study, n_study, p),
                 numeric(n_study + 1))
  f <- as.vector(dens %*% nodes[["w"]])
  f_sum <- convolve_power(f, m_study)
  s <- 0:(m_study * n_study)
  # T1 > threshold  <=>  p* < s / (M n) - threshold
  x <- s / (m_study * n_study) - threshold
  sum(f_sum * pmin(pmax((x - (p0 - theta)) / (2 * theta), 0), 1))
}

prob_two_arm <- function(p0, omega, mu) {
  h <- n_study / 2
  nodes <- rate_nodes(p0, omega, mu)
  f_new <- vapply(nodes[["p_new"]], function(p) stats::dbinom(0:h, h, p), numeric(h + 1))
  f_control <- vapply(nodes[["p_control"]], function(p) stats::dbinom(0:h, h, p),
                      numeric(h + 1))
  joint <- f_new %*% (nodes[["w"]] * t(f_control))
  diff_index <- as.vector(outer(0:h, 0:h, "-"))
  g <- as.numeric(tapply(as.vector(joint), factor(diff_index, levels = -h:h), sum))
  g_sum <- convolve_power(g, m_study)
  d <- seq(-m_study * h, m_study * h)
  # T2 > threshold  <=>  sum of differences > threshold * M * n / 2; a tie counts one half
  cut <- threshold * m_study * n_study / 2
  sum(g_sum[d > cut + 1e-9]) + 0.5 * sum(g_sum[abs(d - cut) < 1e-9])
}

table2 <- data.frame(
  sub = rep(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "d", "d", "d"), each = 2),
  p0 = rep(c(0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30), each = 2),
  theta = rep(c(0.025, 0.025, 0.025, 0.010, 0.025, 0.040, 0.050, 0.050, 0.050, 0.020, 0.050,
                0.080), each = 2),
  omega = rep(c(0.02, 0.05, 0.08, 0.05, 0.05, 0.05, 0.04, 0.10, 0.16, 0.10, 0.10, 0.10), each = 2),
  effect = rep(c("p0", "p0+0.10"), times = 12),
  one_arm = c(0.03, 0.94, 0.04, 0.89, 0.07, 0.83, 0.03, 0.91, 0.04, 0.89, 0.07, 0.87,
              0.14, 0.85, 0.17, 0.81, 0.20, 0.76, 0.12, 0.84, 0.16, 0.81, 0.22, 0.76),
  two_arm = c(0.12, 0.84, 0.12, 0.84, 0.12, 0.83, 0.12, 0.83, 0.12, 0.84, 0.12, 0.84,
              0.22, 0.77, 0.22, 0.77, 0.22, 0.77, 0.22, 0.77, 0.22, 0.77, 0.22, 0.77),
  stringsAsFactors = FALSE
)

res <- NULL
for (i in seq_len(nrow(table2))) {
  p0 <- table2[["p0"]][i]
  mu <- if (table2[["effect"]][i] == "p0") 0 else 0.10 / p0
  label <- sprintf("(%s) p0 = %.2f, theta = %.3f, omega = %.2f, E{p1} = %s", table2[["sub"]][i],
                   p0, table2[["theta"]][i], table2[["omega"]][i], table2[["effect"]][i])
  item <- sprintf("%s_p0_%.2f_theta_%.3f_omega_%.2f_%s", table2[["sub"]][i], p0,
                  table2[["theta"]][i], table2[["omega"]][i], table2[["effect"]][i])
  res <- rbind(
    res,
    judge(script, paste0(item, "_one_arm"), paste("One-arm,", label), "Table 2, p.345",
          table2[["one_arm"]][i], prob_one_arm(p0, table2[["omega"]][i], mu, table2[["theta"]][i]),
          "abs", tol = 0.01),
    judge(script, paste0(item, "_two_arm"), paste("Two-arm,", label), "Table 2, p.345",
          table2[["two_arm"]][i], prob_two_arm(p0, table2[["omega"]][i], mu), "abs", tol = 0.01)
  )
}

finalize_results(res, "inst/reproduce/results")
