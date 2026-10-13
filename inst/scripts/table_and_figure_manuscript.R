# Draw the figures and write the tables of the main text from the results saved by
# data_generate_main.R.
#
# Figures: Figure<k>.pdf and Figure<k>.eps. Tables: table<k>_<content>.tex, each a complete
# table environment with caption, label and note; the settings quoted in the captions and
# notes are taken from the saved results. The figure captions are written in main.tex.
#
# Run from the package root (the package does not need to be loaded).
# Output: inst/scripts/results/figures/ and inst/scripts/results/tables/

source("inst/scripts/figure_table_helpers.R")
data_dir <- "inst/scripts/results/data"
fig_dir <- "inst/scripts/results/figures"
tab_dir <- "inst/scripts/results/tables"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
read_data <- function(name) {
  read_results(data_dir, name, "inst/scripts/data_generate_main.R")
}

# ---- Figure 1: break-even bias ----
fig1a <- read_data("fig1a_breakeven_bias.csv")
fig1b <- read_data("fig1b_relative.csv")
label_ne <- function(v) {
  if (is.infinite(v)) quote(italic(N)[E] == infinity) else bquote(italic(N)[E] == .(v))
}
label_ratio <- function(v) {
  bquote(italic(N) / italic(N)[E] == .(v))
}
draw_fig1 <- function() {
  start_panels()
  # (a) b* / sigma against N
  levels_ne <- unique(fig1a[["n_e"]])
  graphics::plot(NA, xlim = c(0, 200), ylim = c(0, 0.6), xaxs = "i", yaxs = "i",
                 xlab = expression("Total sample size" ~ italic(N)),
                 ylab = expression("Break-even bias" ~ italic(b)^"*" / sigma))
  for (i in seq_along(levels_ne)) {
    d <- fig1a[fig1a[["n_e"]] == levels_ne[i], ]
    graphics::lines(d[["n"]], d[["breakeven_bias"]], col = pal[i], lty = lty_series[i],
                    lwd = 1.8)
  }
  graphics::legend("topright", legend = as.expression(lapply(levels_ne, label_ne)),
                   col = pal[seq_along(levels_ne)], lty = lty_series[seq_along(levels_ne)],
                   lwd = 1.8, bty = "n", seg.len = 3)
  panel_title(quote(bold("(a)")))
  # (b) b* / delta against the power of the RCT
  levels_ratio <- unique(fig1b[["ratio"]])
  graphics::plot(NA, xlim = c(0.1, 0.9), ylim = c(0, 1.4), xaxs = "i", yaxs = "i",
                 xlab = "Power of the RCT",
                 ylab = expression("Break-even bias" ~ italic(b)^"*" / delta))
  for (i in seq_along(levels_ratio)) {
    d <- fig1b[fig1b[["ratio"]] == levels_ratio[i], ]
    graphics::lines(d[["power_rct"]], d[["breakeven_bias_rel"]], col = pal[i],
                    lty = lty_series[i], lwd = 1.8)
  }
  graphics::legend("topright", legend = as.expression(lapply(levels_ratio, label_ratio)),
                   col = pal[seq_along(levels_ratio)],
                   lty = lty_series[seq_along(levels_ratio)], lwd = 1.8, bty = "n",
                   seg.len = 3)
  panel_title(quote(bold("(b)")))
}
save_figure(fig_dir, "Figure1", draw_fig1)

# ---- Figure 2: power against the total sample size ----
fig2 <- read_data("fig2_power.csv")
draw_fig2 <- function() {
  start_panels(legend_lines = 3.2)
  levels_ne <- unique(fig2[["n_e"]])
  levels_m <- sort(unique(fig2[["margin_rel"]]))
  for (j in seq_along(levels_ne)) {
    d_ne <- fig2[fig2[["n_e"]] == levels_ne[j], ]
    graphics::plot(NA, xlim = c(0, 200), ylim = c(0, 1), xaxs = "i", yaxs = "i",
                   xlab = expression("Total sample size" ~ italic(N)), ylab = "Power")
    rct <- d_ne[d_ne[["margin_rel"]] == levels_m[1], ]
    graphics::lines(rct[["n"]], rct[["power_rct"]], col = "black", lwd = 2.6)
    for (i in seq_along(levels_m)) {
      d <- d_ne[d_ne[["margin_rel"]] == levels_m[i], ]
      graphics::lines(d[["n"]], d[["power_sat"]], col = pal[i], lty = lty_with_ref[i],
                      lwd = 1.8)
    }
    panel_title(title_ne(c("(a)", "(b)")[j], levels_ne[j]))
  }
  labels <- c(list("RCT"),
              lapply(levels_m, function(v) bquote("SAT," ~ italic(m) / delta == .(v))))
  bottom_legend(labels, col = c("black", pal[seq_along(levels_m)]),
                lty = c(1, lty_with_ref[seq_along(levels_m)]),
                lwd = c(2.6, rep(1.8, length(levels_m))), ncol = 3)
}
save_figure(fig_dir, "Figure2", draw_fig2)

# ---- Figure 3: type I error rate of the SAT against the bias ----
fig3 <- read_data("fig3_type1.csv")
fig3_cross <- read_data("fig3_crossing.csv")
draw_fig3 <- function() {
  start_panels(legend_lines = 3.2)
  levels_ne <- unique(fig3[["n_e"]])
  levels_m <- sort(unique(fig3[["margin_rel"]]))
  for (j in seq_along(levels_ne)) {
    d_ne <- fig3[fig3[["n_e"]] == levels_ne[j], ]
    c_ne <- fig3_cross[fig3_cross[["n_e"]] == levels_ne[j], ]
    graphics::plot(NA, xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i",
                   xlab = expression("Bias" ~ italic(b) / delta), ylab = "Type I error rate")
    m_crit <- c_ne[["critical_margin"]][1] / c_ne[["delta"]][1]
    b_star <- c_ne[["breakeven_bias"]][1] / c_ne[["delta"]][1]
    graphics::abline(v = m_crit, col = "grey40", lwd = 1)
    graphics::abline(v = b_star, col = "grey40", lty = 3, lwd = 1)
    for (i in seq_along(levels_m)) {
      d <- d_ne[d_ne[["margin_rel"]] == levels_m[i], ]
      cm <- c_ne[abs(c_ne[["margin_rel"]] - levels_m[i]) < 1e-9, ]
      graphics::abline(h = cm[["alpha_matched"]], col = pal[i], lty = 2, lwd = 1.5)
      graphics::lines(d[["bias_rel"]], d[["type1_sat"]], col = pal[i], lty = 1, lwd = 1.8)
      graphics::points(cm[["bias_crossing"]] / cm[["delta"]], cm[["alpha_matched"]], pch = 21,
                       bg = pal[i], col = "black", cex = 1.1)
    }
    graphics::text(m_crit, 0.96, expression(italic(m)[crit]), pos = 2, offset = 0.3)
    graphics::text(b_star, 0.96, expression(italic(b)^"*"), pos = 4, offset = 0.3)
    panel_title(title_ne(c("(a)", "(b)")[j], levels_ne[j]))
  }
  labels <- c(lapply(levels_m, function(v) bquote(italic(m) / delta == .(v))),
              list("Single-arm trial", quote("RCT at level" ~ alpha * "'")))
  bottom_legend(labels, col = c(pal[seq_along(levels_m)], "black", "black"),
                lty = c(rep(1, length(levels_m)), 1, 2),
                lwd = c(rep(1.8, length(levels_m)), 1.8, 1.5), ncol = 3)
}
save_figure(fig_dir, "Figure3", draw_fig3)

# ---- Table 1: results that depend only on the power of the RCT and N / N_E ----
tab1 <- read_data("table1_relative.csv")
alpha1 <- unique(tab1[["alpha"]])
stopifnot(length(alpha1) == 1)
ratios <- unique(tab1[["ratio"]])
body1 <- character(0)
for (i in seq_along(ratios)) {
  d <- tab1[tab1[["ratio"]] == ratios[i], ]
  d <- d[order(d[["power_rct"]]), ]
  # m_crit / delta does not depend on the power of the RCT
  stopifnot(max(abs(d[["critical_margin_rel"]] - d[["critical_margin_rel"]][1])) < 1e-12)
  for (j in seq_len(nrow(d))) {
    lead <- if (j == 1) {
      c(paste0("$", fmt_setting(ratios[i]), "$"), fmt_num(d[["critical_margin_rel"]][1]))
    } else {
      c("", "")
    }
    cells <- c(lead, sprintf("%.2f", d[["power_rct"]][j]), fmt_prob(d[["power_sat"]][j]),
               fmt_prob(d[["alpha_matched"]][j]), fmt_num(d[["breakeven_bias_rel"]][j]))
    body1 <- c(body1, paste0(paste(cells, collapse = " & "), " \\\\"))
  }
  if (i < length(ratios)) {
    body1 <- c(body1, "\\addlinespace")
  }
}
caption1 <- paste0(
  "Power of a single-arm trial (SAT) without margin and with an unbiased external control, ",
  "one-sided level $\\alpha'$ at which a randomized controlled trial (RCT) of the same total ",
  "size $N$ has the same power, critical margin $m_{\\mathrm{crit}}$ and break-even bias ",
  "$b^{*}$ for the mean squared error, by the power of the RCT at one-sided $\\alpha = ",
  fmt_setting(alpha1), "$ and the ratio of $N$ to the number of external controls $N_E$.")
note1 <- paste0(
  "With a common standard deviation and 1:1 randomization in the RCT, these quantities ",
  "depend on $N$, $N_E$ and the treatment effect $\\delta$ only through the power of the RCT ",
  "and $N/N_E$, and $m_{\\mathrm{crit}}/\\delta$ does not depend on the power of the RCT. ",
  "$N/N_E = 0$ is the comparison with a fixed threshold.")
write_table(tab_dir, "table1_relative",
            table_lines(caption1, "tab:relative", "cccccc",
                        paste0("$N/N_E$ & $m_{\\mathrm{crit}}/\\delta$ & Power of RCT & ",
                               "Power of SAT & $\\alpha'$ & $b^{*}/\\delta$ \\\\"),
                        body1, note1))

# ---- Table 2: three endpoints on their own scales ----
tab2 <- read_data("table2_illustration.csv")
alpha2 <- unique(tab2[["alpha"]])
stopifnot(length(alpha2) == 1)
endpoint_label <- c(continuous = "Continuous", binary = "Binary", survival = "Time to event")
with_exact <- function(x, exact) {
  ifelse(is.na(exact), fmt_prob(x), paste0(fmt_prob(x), " (", fmt_prob(exact), ")"))
}
body2 <- character(0)
for (i in seq_len(nrow(tab2))) {
  row <- tab2[i, ]
  effect <- switch(row[["endpoint"]],
    continuous = paste0("$\\delta = ", fmt_setting(row[["delta"]] / row[["sigma_t"]]),
                        "\\sigma$"),
    binary = sprintf("%.2f vs %.2f", row[["p_t"]], row[["p_c"]]),
    survival = paste0("HR ", fmt_setting(row[["hazard_ratio"]]))
  )
  n_e <- if (is.infinite(row[["n_e"]])) "$\\infty$" else fmt_setting(row[["n_e"]])
  cells <- c(endpoint_label[[row[["endpoint"]]]], effect, fmt_setting(row[["n"]]), n_e,
             with_exact(row[["power_rct"]], row[["power_rct_exact"]]),
             with_exact(row[["power_sat"]], row[["power_sat_exact"]]),
             fmt_prob(row[["alpha_matched"]]), fmt_num(row[["breakeven_bias"]]),
             fmt_num(row[["critical_margin"]]))
  body2 <- c(body2, paste0(paste(cells, collapse = " & "), " \\\\"))
}
threshold <- tab2[["p_c"]][tab2[["endpoint"]] == "binary" & is.infinite(tab2[["n_e"]])]
prob_event <- tab2[["prob_event"]][tab2[["endpoint"]] == "survival"]
stopifnot(length(threshold) == 1, length(prob_event) == 1)
caption2 <- paste0(
  "Operating characteristics of a randomized controlled trial (RCT) with 1:1 randomization ",
  "and a single-arm trial (SAT) of the same total size $N$ for three endpoints, with ",
  "one-sided $\\alpha = ", fmt_setting(alpha2), "$. The SAT is tested without margin against ",
  "an unbiased external control of $N_E$ patients or against a fixed threshold ",
  "($N_E = \\infty$). $\\alpha'$ is the one-sided level at which the RCT has the power of the ",
  "SAT, $b^{*}$ the break-even bias for the mean squared error and $m_{\\mathrm{crit}}$ the ",
  "critical margin.")
note2 <- paste0(
  "Values without parentheses use the normal approximation. The effect is the difference in ",
  "means (standard deviation $\\sigma$), the difference in proportions or the negative log ",
  "hazard ratio, and $b^{*}$ and $m_{\\mathrm{crit}}$ are on the same scale. Values in ",
  "parentheses are exact rejection probabilities of the score test for two proportions (RCT ",
  "with $N/2$ patients per group, SAT against external controls) and of the exact binomial ",
  "test against the threshold ", sprintf("%.2f", threshold), ". For the time-to-event ",
  "endpoint, the probability of an event is ", fmt_setting(prob_event), " for every patient.")
write_table(tab_dir, "table2_illustration",
            table_lines(caption2, "tab:illustration", "llccccccc",
                        paste0("Endpoint & Effect & $N$ & $N_E$ & Power of RCT & ",
                               "Power of SAT & $\\alpha'$ & $b^{*}$ & ",
                               "$m_{\\mathrm{crit}}$ \\\\"),
                        body2, note2, size = "\\small"))
