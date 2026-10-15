# Draw the figures and write the tables of the main text from the results saved by
# data_generate_main.R.
#
# Figures: Figure<k>.pdf and Figure<k>.eps. Tables: table<k>_<content>.tex, each a complete
# table environment with caption, label and note; Table 4 is drawn from the results of Table 3.
# The settings quoted in the captions and notes are taken from the saved results. The figure
# captions are written in main.tex.
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
  if (is.infinite(v)) quote(italic(N)[italic(E)] ~ "=" ~ infinity) else
    bquote(italic(N)[italic(E)] ~ "=" ~ .(v))
}
label_ratio <- function(v) {
  bquote(italic(N) / italic(N)[italic(E)] ~ "=" ~ .(v))
}
draw_fig1 <- function() {
  start_panels()
  # (a) b* / sigma against N
  levels_ne <- unique(fig1a[["n_e"]])
  graphics::plot(NA, xlim = c(0, 200), ylim = c(0, 0.6), xaxs = "i", yaxs = "i",
                 xlab = expression("Total sample size" ~ italic(N)),
                 ylab = expression("Break-even bias" ~ italic(b) * "*" / sigma))
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
                 ylab = expression("Break-even bias" ~ italic(b) * "*" / delta))
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
              lapply(levels_m, function(v) bquote("SAT," ~ italic(m) / delta ~ "=" ~ .(v))))
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
    graphics::text(b_star, 0.96, expression(italic(b) * "*"), pos = 4, offset = 0.3)
    panel_title(title_ne(c("(a)", "(b)")[j], levels_ne[j]))
  }
  labels <- c(lapply(levels_m, function(v) bquote(italic(m) / delta ~ "=" ~ .(v))),
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
  "hazard ratio, and $b^{*}$ and $m_{\\mathrm{crit}}$ are on the same scale (in units of ",
  "$\\sigma$ for the continuous endpoint). Values in ",
  "parentheses are exact rejection probabilities of the score test for two proportions (RCT ",
  "with $N/2$ patients per group, SAT against external controls) and of the exact binomial ",
  "test against the threshold ", sprintf("%.2f", threshold), ". For the time-to-event ",
  "endpoint, the probability of an event is ", fmt_setting(prob_event), " for every patient.")
write_table(tab_dir, "table2_illustration",
            table_lines(caption2, "tab:illustration", "llccccccc",
                        paste0("Endpoint & Effect & $N$ & $N_E$ & Power of RCT & ",
                               "Power of SAT & $\\alpha'$ & $b^{*}$ & ",
                               "$m_{\\mathrm{crit}}$ \\\\"),
                        body2, note2, size = c("\\small", "\\setlength{\\tabcolsep}{4pt}")))

# ---- Table 3: type I error rate of the SAT under published estimates of the bias ----
tab3 <- read_data("table3_empirical_bias.csv")
alpha3 <- unique(tab3[["alpha"]])
hr3 <- unique(tab3[["hazard_ratio"]])
prob_event3 <- unique(tab3[["prob_event"]])
stopifnot(length(alpha3) == 1, length(hr3) == 1, length(prob_event3) == 1,
          all(tab3[["n_e"]] == 2 * tab3[["n"]]),
          all(tab3[["subset"]] %in% c("All", "Outlier removed")))
sizes3 <- sort(unique(tab3[["n"]]))
design3 <- tab3[!duplicated(tab3[["n"]]), ]
design3 <- design3[order(design3[["n"]]), ]
n_col3 <- 6 + length(sizes3)
design_line <- function(label, values) {
  paste0("\\multicolumn{6}{l}{", label, "} & ", paste(values, collapse = " & "), " \\\\")
}
body3 <- c(
  paste0("\\multicolumn{", n_col3, "}{l}{\\textit{(a) Designs}} \\\\"),
  design_line("Power of RCT", fmt_prob(design3[["power_rct"]])),
  design_line("Power of SAT", fmt_prob(design3[["power_sat"]])),
  design_line("$\\alpha'$", fmt_prob(design3[["alpha_matched"]])),
  design_line("$b^{*}$", fmt_num(design3[["breakeven_bias"]])),
  design_line("$m_{\\mathrm{crit}}$", fmt_num(design3[["critical_margin"]])),
  "\\addlinespace",
  paste0("\\multicolumn{", n_col3, "}{l}{\\textit{(b) Type I error rate of the SAT under ",
         "published estimates of the bias}} \\\\")
)
# Source, endpoint, number of comparisons and analysis of a row of Table 3(b); the source is
# written only in its first row
label_cells3 <- function(first, previous_key) {
  source_cell <- if (first[["bibkey"]] == previous_key) {
    ""
  } else {
    paste0(first[["author"]], "~\\cite{", first[["bibkey"]], "}")
  }
  c(source_cell, first[["endpoint"]], fmt_setting(first[["n_comparisons"]]),
    analysis_cell(first))
}
analysis_cell <- function(first) {
  if (first[["subset"]] == "Outlier removed") {
    paste0(first[["analysis"]], "$^{\\ddagger}$")
  } else {
    first[["analysis"]]
  }
}
previous_key <- ""
for (s in sort(unique(tab3[["id"]]))) {
  d <- tab3[tab3[["id"]] == s, ]
  d <- d[order(d[["n"]]), ]
  stopifnot(nrow(d) == length(sizes3), all(d[["n"]] == sizes3))
  first <- d[1, ]
  type1 <- fmt_prob(d[["type1_sat"]])
  type1 <- ifelse(d[["above_alpha_matched"]], paste0(type1, "$^{\\dagger}$"), type1)
  tau_cell <- if (is.na(first[["tau"]])) "--" else fmt_num(first[["tau"]])
  cells <- c(label_cells3(first, previous_key), fmt_num(first[["bias_mean"]]), tau_cell, type1)
  previous_key <- first[["bibkey"]]
  body3 <- c(body3, paste0(paste(cells, collapse = " & "), " \\\\"))
}
header3 <- c(
  paste0("Source & Endpoint & $K$ & Analysis & $\\bar{b}$ & $\\tau$ & \\multicolumn{",
         length(sizes3), "}{c}{$N$} \\\\"),
  paste0("\\cmidrule(lr){7-", n_col3, "}"),
  paste0(" & & & & & & ", paste(fmt_setting(sizes3), collapse = " & "), " \\\\")
)
caption3 <- paste0(
  "Single-arm trial (SAT) with an external control of $N_E = 2N$ patients and randomized ",
  "controlled trial (RCT) with 1:1 randomization for a time-to-event endpoint with hazard ",
  "ratio ", fmt_setting(hr3), ", by the total size $N$, with one-sided $\\alpha = ",
  fmt_setting(alpha3), "$. (a) Power of the RCT and of the SAT without bias, level ",
  "$\\alpha'$ at which the RCT has the power of the SAT, break-even bias $b^{*}$ and critical ",
  "margin $m_{\\mathrm{crit}}$. (b) Type I error rate of the SAT without margin, averaged over ",
  "a normal distribution of the bias with the published mean $\\bar{b}$ and between-study ",
  "standard deviation $\\tau$ of the bias of external controls.")
abbreviations3 <- paste0(
  "OS, overall survival; PFS, progression-free survival (real-world progression-free ",
  "survival for the external controls); QBA, quantitative bias analysis.")
outlier_note3 <- paste0(
  "Analyses marked with $\\ddagger$ exclude the study that the authors identified as an ",
  "outlier (study 5 of Incerti et al.\\ and Bahmane et al.\\ and the NILE trial of Gupta et ",
  "al.).")
note3 <- paste0(
  "The bias is the log hazard ratio estimated with the randomized control minus the log ",
  "hazard ratio estimated with the external control, so that positive values favour the ",
  "experimental treatment; $b^{*}$, $m_{\\mathrm{crit}}$, $\\bar{b}$ and $\\tau$ are on this ",
  "scale. Incerti et al.\\ and Bahmane et al.\\ report posterior medians of the hazard ratio ",
  "of the internal versus the external control and of the between-study standard deviation, ",
  "and $\\bar{b}$ is minus the logarithm of the hazard ratio. Gupta et al.\\ report the mean ",
  "difference in log hazard ratios over the trials without a between-study standard ",
  "deviation, and $\\tau = 0$ is used. Type I error rates are computed from $\\bar{b}$ before ",
  "rounding. $K$ is the number of comparisons. Type I error rates above $\\alpha'$ in the ",
  "same column are marked with $\\dagger$. ", outlier_note3, " ",
  "Section~\\ref{S-sec:bias_uncertainty} of the Supporting Information repeats (b) with the ",
  "upper limits of the credible intervals and with the trial-level differences of Gupta et ",
  "al. The probability of an event is ", fmt_setting(prob_event3), " for every patient. ",
  abbreviations3)
write_table(tab_dir, "table3_empirical_bias",
            table_lines(caption3, "tab:empirical",
                        paste0("llcl", "cc", strrep("c", length(sizes3))),
                        header3, body3, note3,
                        size = c("\\small", "\\setlength{\\tabcolsep}{2.5pt}")))

# ---- Table 4: bias margins under the published estimates of the bias ----
# The weighted analyses of Table 3, with the source written in a row of its own. (a) gives the
# margins and (b) the power of the SAT with each margin averaged over the distribution of the
# bias, for the estimates with a between-study standard deviation, beside the power of the RCT.
# m_crit and m_0.95 do not depend on N in this setting (N_E = 2 N). Margins above m_crit and
# averaged powers below the power of the RCT are marked. Gupta et al. report no between-study
# standard deviation, so m_0.95 and (b) are not shown for their estimates.
stopifnot(diff(range(tab3[["critical_margin"]])) < 1e-12)
m_crit4 <- tab3[["critical_margin"]][1]
mark_above_crit <- function(x) {
  ifelse(x > m_crit4, paste0(fmt_num(x), "$^{\\S}$"), fmt_num(x))
}
mark_below_rct <- function(x, power_rct) {
  ifelse(x < power_rct, paste0(fmt_prob(x), "$^{\\P}$"), fmt_prob(x))
}
n_col4 <- 6 + length(sizes3)
source_line4 <- function(first) {
  paste0("\\multicolumn{", n_col4, "}{l}{", first[["author"]], "~\\cite{", first[["bibkey"]],
         "}} \\\\")
}
ids4 <- sort(unique(tab3[["id"]][tab3[["analysis"]] != "Unadjusted"]))
body4 <- paste0("\\multicolumn{", n_col4, "}{l}{\\textit{(a) Bias margins, with ",
                "$m_{\\mathrm{avg}}$ by $N$}} \\\\")
previous_key <- ""
for (s in ids4) {
  d <- tab3[tab3[["id"]] == s, ]
  d <- d[order(d[["n"]]), ]
  stopifnot(nrow(d) == length(sizes3), all(d[["n"]] == sizes3),
            diff(range(d[["margin_q95"]])) < 1e-12)
  first <- d[1, ]
  if (first[["bibkey"]] != previous_key) {
    body4 <- c(body4, if (previous_key != "") "\\addlinespace", source_line4(first))
  }
  previous_key <- first[["bibkey"]]
  tau_cell <- if (is.na(first[["tau"]])) "--" else fmt_num(first[["tau"]])
  q95_cell <- if (is.na(first[["tau"]])) "--" else mark_above_crit(first[["margin_q95"]])
  cells <- c(first[["endpoint"]], fmt_setting(first[["n_comparisons"]]), analysis_cell(first),
             fmt_num(first[["bias_mean"]]), tau_cell, q95_cell,
             mark_above_crit(d[["margin_avg"]]))
  body4 <- c(body4, paste0(paste(cells, collapse = " & "), " \\\\"))
}
body4 <- c(body4, "\\addlinespace",
           paste0("\\multicolumn{", n_col4, "}{l}{\\textit{(b) Power by $N$, averaged over ",
                  "the distribution of the bias}} \\\\"),
           design_line("RCT", fmt_prob(design3[["power_rct"]])))
margin_label4 <- c(avg = "$m_{\\mathrm{avg}}$", q95 = "$m_{0.95}$")
previous_key <- ""
for (s in ids4) {
  d <- tab3[tab3[["id"]] == s, ]
  d <- d[order(d[["n"]]), ]
  first <- d[1, ]
  if (is.na(first[["tau"]])) {
    next
  }
  stopifnot(max(abs(d[["power_rct"]] - design3[["power_rct"]])) < 1e-12)
  if (first[["bibkey"]] != previous_key) {
    body4 <- c(body4, if (previous_key != "") "\\addlinespace", source_line4(first))
  }
  previous_key <- first[["bibkey"]]
  for (v in names(margin_label4)) {
    lead <- if (v == "avg") {
      c(first[["endpoint"]], fmt_setting(first[["n_comparisons"]]), analysis_cell(first))
    } else {
      c("", "", "")
    }
    cells <- c(lead, paste0("\\multicolumn{3}{l}{SAT with ", margin_label4[[v]], "}"),
               mark_below_rct(d[[paste0("power_sat_margin_", v, "_averaged")]],
                              d[["power_rct"]]))
    body4 <- c(body4, paste0(paste(cells, collapse = " & "), " \\\\"))
  }
}
header4 <- c(
  paste0("Endpoint & $K$ & Analysis & $\\bar{b}$ & $\\tau$ & $m_{0.95}$ & \\multicolumn{",
         length(sizes3), "}{c}{$N$} \\\\"),
  paste0("\\cmidrule(lr){7-", n_col4, "}"),
  paste0(" & & & & & & ", paste(fmt_setting(sizes3), collapse = " & "), " \\\\")
)
caption4 <- paste0(
  "Bias margins for the single-arm trial (SAT) of Table~\\ref{tab:empirical} under the ",
  "published estimates of the bias from the weighted analyses (a), and power of the SAT with ",
  "each margin, averaged over the distribution of the bias, and of the randomized controlled ",
  "trial (RCT) with 1:1 randomization at one-sided $\\alpha = ", fmt_setting(alpha3), "$ (b).")
note4 <- paste0(
  "With the margin $m_{\\mathrm{avg}}$, the type I error rate averaged over a normal ",
  "distribution of the bias with mean $\\bar{b}$ and standard deviation $\\tau$ equals ",
  "$\\alpha$. The margin $m_{0.95} = \\bar{b} + z_{0.95}\\tau$ is the 95th percentile of this ",
  "distribution, so that the type I error rate given the bias is at most $\\alpha$ in 95\\% of ",
  "trials. The critical margin is $m_{\\mathrm{crit}} = ", fmt_num(m_crit4), "$ at every $N$ ",
  "(Table~\\ref{tab:empirical}(a)), and margins above it are marked with $\\S$; with such a ",
  "margin, the SAT has less power than the RCT when the external control is unbiased. ",
  "Averaged powers below the power of the RCT in the same column are marked with $\\P$. ",
  "Gupta et al.\\ report no between-study standard deviation, so that $m_{0.95}$ and (b) are ",
  "not shown for their estimates and $m_{\\mathrm{avg}}$ equals $\\bar{b}$; ",
  "Section~\\ref{S-sec:bias_uncertainty} of the Supporting Information uses the standard ",
  "deviation of the trial-level differences instead. The margins are computed from $\\bar{b}$ ",
  "before rounding. $K$, $\\ddagger$ and the abbreviations are as in ",
  "Table~\\ref{tab:empirical}.")
write_table(tab_dir, "table4_bias_margins",
            table_lines(caption4, "tab:margins",
                        paste0("lcl", "ccc", strrep("c", length(sizes3))),
                        header4, body4, note4,
                        size = c("\\small", "\\setlength{\\tabcolsep}{3pt}")))
