# Draw the figures and write the tables of the Supporting Information from the results saved
# by data_generate_supplement.R, except Table S6, which summarizes the verdicts of the scripts
# in inst/reproduce and inst/validation.
#
# Figures: FigureS<k>.pdf and FigureS<k>.eps. Tables: tableS<k>_<content>.tex, each a complete
# table environment with caption, label and note; the settings quoted in the captions and
# notes are taken from the saved results. The figure captions are written in
# supporting_information.tex.
#
# Run from the package root (the package does not need to be loaded).
# Output: inst/sup_info/results/figures/ and inst/sup_info/results/tables/

source("inst/scripts/figure_table_helpers.R")
data_dir <- "inst/sup_info/results/data"
fig_dir <- "inst/sup_info/results/figures"
tab_dir <- "inst/sup_info/results/tables"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
read_data <- function(name) {
  read_results(data_dir, name, "inst/sup_info/data_generate_supplement.R")
}

# ---- Figure S1: hybrid design ----
fig_s1 <- read_data("figS1_hybrid.csv")
draw_fig_s1 <- function() {
  start_panels(legend_lines = 2.2)
  b_star <- fig_s1[["breakeven_bias"]][1]
  # (a) mean squared error against the bias
  graphics::plot(NA, xlim = c(0, 0.6), ylim = c(0, 0.15), xaxs = "i", yaxs = "i",
                 xlab = expression("Bias" ~ italic(b) / sigma), ylab = "Mean squared error")
  graphics::abline(v = b_star, col = "grey40", lty = 3)
  graphics::lines(fig_s1[["bias"]], fig_s1[["mse_rct"]], col = "black", lwd = 2.6)
  graphics::lines(fig_s1[["bias"]], fig_s1[["mse_sat"]], col = pal[1], lty = lty_with_ref[1],
                  lwd = 1.8)
  graphics::lines(fig_s1[["bias"]], fig_s1[["mse_hybrid"]], col = pal[2],
                  lty = lty_with_ref[2], lwd = 1.8)
  graphics::text(b_star, 0.142, expression(italic(b) * "*"), pos = 4, offset = 0.3)
  panel_title(quote(bold("(a)")))
  # (b) optimal number of concurrent controls against the bias
  graphics::plot(NA, xlim = c(0, 0.6), ylim = c(0, 0.55), xaxs = "i",
                 xlab = expression("Bias" ~ italic(b) / sigma),
                 ylab = expression("Concurrent controls" ~ italic(N)[italic(C)] / italic(N)))
  graphics::abline(h = 0.5, col = "black", lwd = 2.6)
  graphics::abline(h = 0, col = pal[1], lty = lty_with_ref[1], lwd = 1.8)
  graphics::lines(fig_s1[["bias"]], fig_s1[["n_c_opt"]] / fig_s1[["n"]], col = pal[2],
                  lty = lty_with_ref[2], lwd = 1.8)
  panel_title(quote(bold("(b)")))
  bottom_legend(list("RCT", "SAT", "Optimal hybrid"),
                col = c("black", pal[1], pal[2]), lty = c(1, lty_with_ref[1:2]),
                lwd = c(2.6, 1.8, 1.8), ncol = 3)
}
save_figure(fig_dir, "FigureS1", draw_fig_s1)

# ---- Figure S2: exact operating characteristics for a binary endpoint ----
fig_s2 <- read_data("figS2_binary.csv")
designs <- c("rct", "ext", "thr")
col_design <- c("black", pal[1], pal[2])
draw_fig_s2 <- function() {
  start_panels(legend_lines = 3.2)
  # (a) power, exact and normal approximation
  graphics::plot(NA, xlim = c(20, 100), ylim = c(0, 1), xaxs = "i", yaxs = "i",
                 xlab = expression("Total sample size" ~ italic(N)), ylab = "Power")
  for (i in seq_along(designs)) {
    graphics::lines(fig_s2[["n"]], fig_s2[[paste0(designs[i], "_exact_power")]],
                    col = col_design[i], lty = 1, lwd = 1.6)
    graphics::lines(fig_s2[["n"]], fig_s2[[paste0(designs[i], "_normal_power")]],
                    col = col_design[i], lty = 2, lwd = 1.6)
  }
  panel_title(quote(bold("(a)")))
  # (b) exact type I error rate at the boundary of the null hypothesis
  type1 <- unlist(fig_s2[paste0(designs, "_exact_type1")])
  graphics::plot(NA, xlim = c(20, 100), ylim = c(0, max(0.05, 1.1 * max(type1))),
                 xaxs = "i", yaxs = "i", xlab = expression("Total sample size" ~ italic(N)),
                 ylab = "Type I error rate")
  graphics::abline(h = unique(fig_s2[["alpha"]]), col = "grey40", lty = 3)
  for (i in seq_along(designs)) {
    graphics::lines(fig_s2[["n"]], fig_s2[[paste0(designs[i], "_exact_type1")]],
                    col = col_design[i], lty = 1, lwd = 1.6)
  }
  panel_title(quote(bold("(b)")))
  # The designs are told apart by colour and the calculations by line type, shown in grey
  labels <- list("RCT", "SAT, external control", "SAT, threshold", "Exact",
                 "Normal approximation")
  bottom_legend(labels, col = c(col_design, "grey50", "grey50"), lty = c(1, 1, 1, 1, 2),
                lwd = rep(1.6, 5), ncol = 3)
}
save_figure(fig_dir, "FigureS2", draw_fig_s2)

# ---- Table S1: z-test and t-test ----
tab_s1 <- read_data("tableS1_z_t.csv")
alpha_s1 <- unique(tab_s1[["alpha"]])
stopifnot(length(alpha_s1) == 1)
tab_s1 <- tab_s1[order(tab_s1[["n"]], tab_s1[["delta"]]), ]
body_s1 <- character(0)
for (i in seq_len(nrow(tab_s1))) {
  row <- tab_s1[i, ]
  first <- i == 1 || row[["n"]] != tab_s1[["n"]][i - 1]
  if (first && i > 1) {
    body_s1 <- c(body_s1, "\\addlinespace")
  }
  cells <- c(if (first) fmt_setting(row[["n"]]) else "", fmt_setting(row[["delta"]]),
             fmt_prob(unlist(row[c("power_rct_z", "power_rct_t", "power_thr_z",
                                   "power_thr_t", "power_ext_z", "power_ext_t")])))
  body_s1 <- c(body_s1, paste0(paste(cells, collapse = " & "), " \\\\"))
}
caption_s1 <- paste0(
  "Power of a randomized controlled trial (RCT) with 1:1 randomization and of a single-arm ",
  "trial (SAT) of the same total size $N$, without margin and with an unbiased external ",
  "control, computed with the $z$-test and the $t$-test, with one-sided $\\alpha = ",
  fmt_setting(alpha_s1), "$.")
note_s1 <- paste0(
  "The $t$-test has $N - 2$ degrees of freedom in the RCT, $N - 1$ against a fixed threshold ",
  "and $N + N_E - 2$ against $N_E$ external controls, and assumes a common standard deviation ",
  "$\\sigma$.")
header_s1 <- c(
  paste0(" & & \\multicolumn{2}{c}{RCT} & \\multicolumn{2}{c}{SAT, threshold} & ",
         "\\multicolumn{2}{c}{SAT, $N_E = 2N$} \\\\"),
  "\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}\\cmidrule(lr){7-8}",
  "$N$ & $\\delta/\\sigma$ & $z$ & $t$ & $z$ & $t$ & $z$ & $t$ \\\\")
write_table(tab_dir, "tableS1_z_t",
            table_lines(caption_s1, "tab:z_t", "cccccccc", header_s1, body_s1, note_s1))

# ---- Table S2: unequal allocation and unequal standard deviations ----
tab_s2 <- read_data("tableS2_sensitivity.csv")
alpha_s2 <- unique(tab_s2[["alpha"]])
n_s2 <- unique(tab_s2[["n"]])
delta_s2 <- unique(tab_s2[["delta"]])
sigma_t_s2 <- unique(tab_s2[["sigma_t"]])
stopifnot(length(alpha_s2) == 1, length(n_s2) == 1, length(delta_s2) == 1,
          length(sigma_t_s2) == 1)
fmt_allocation <- function(r) {
  if (abs(r - 1 / 2) < 1e-9) {
    return("1:1")
  }
  if (abs(r - 2 / 3) < 1e-9) {
    return("2:1")
  }
  sprintf("%.2f", r)
}
body_s2 <- character(0)
for (i in seq_len(nrow(tab_s2))) {
  row <- tab_s2[i, ]
  first_ne <- i == 1 || row[["n_e"]] != tab_s2[["n_e"]][i - 1]
  first_r <- first_ne || abs(row[["r"]] - tab_s2[["r"]][i - 1]) > 1e-9
  if (first_ne && i > 1) {
    body_s2 <- c(body_s2, "\\addlinespace")
  }
  n_e <- if (is.infinite(row[["n_e"]])) "$\\infty$" else fmt_setting(row[["n_e"]])
  cells <- c(if (first_ne) n_e else "", if (first_r) fmt_allocation(row[["r"]]) else "",
             fmt_setting(row[["sd_ratio"]]), fmt_prob(row[["power_rct"]]),
             fmt_prob(row[["power_sat"]]), fmt_prob(row[["alpha_matched"]]),
             fmt_num(row[["breakeven_bias_rel"]]), fmt_num(row[["critical_margin_rel"]]))
  body_s2 <- c(body_s2, paste0(paste(cells, collapse = " & "), " \\\\"))
}
caption_s2 <- paste0(
  "Power of a randomized controlled trial (RCT) and of a single-arm trial (SAT) without ",
  "margin and with an unbiased external control, one-sided level $\\alpha'$ at which the RCT ",
  "has the power of the SAT, break-even bias $b^{*}$ and critical margin $m_{\\mathrm{crit}}$ ",
  "for a total size $N = ", fmt_setting(n_s2), "$ and an effect $\\delta = ",
  fmt_setting(delta_s2 / sigma_t_s2), "\\sigma_T$, by the number of external controls $N_E$, ",
  "the allocation in the RCT and the ratio of the standard deviations, with one-sided ",
  "$\\alpha = ", fmt_setting(alpha_s2), "$.")
note_s2 <- paste0(
  "Allocation is the ratio of the numbers of patients randomized to the experimental and ",
  "control treatments in the RCT. $\\sigma_T$ and $\\sigma_C$ are the standard deviations in ",
  "the experimental group and in the control group (concurrent or external). ",
  "$N_E = \\infty$ is the comparison with a fixed threshold, for which $\\sigma_C$ affects ",
  "only the RCT.")
write_table(tab_dir, "tableS2_sensitivity",
            table_lines(caption_s2, "tab:sensitivity", "cccccccc",
                        paste0("$N_E$ & Allocation & $\\sigma_C/\\sigma_T$ & Power of RCT & ",
                               "Power of SAT & $\\alpha'$ & $b^{*}/\\delta$ & ",
                               "$m_{\\mathrm{crit}}/\\delta$ \\\\"),
                        body_s2, note_s2))

# ---- Table S3: uncertainty in the published estimates of the bias ----
tab_s3 <- read_data("tableS3_bias_uncertainty.csv")
alpha_s3 <- unique(tab_s3[["alpha"]])
hr_s3 <- unique(tab_s3[["hazard_ratio"]])
stopifnot(length(alpha_s3) == 1, length(hr_s3) == 1, all(tab_s3[["n_e"]] == 2 * tab_s3[["n"]]),
          all(tab_s3[["subset"]] %in% c("All", "Outlier removed")))
sizes_s3 <- sort(unique(tab_s3[["n"]]))
design_s3 <- tab_s3[!duplicated(tab_s3[["n"]]), ]
design_s3 <- design_s3[order(design_s3[["n"]]), ]
variation_label <- c(mean_upper = "Upper $\\bar{b}$", tau_upper = "Upper $\\tau$",
                     ivw_mean = "IVW mean", trial_sd = "Trial SD")
stopifnot(all(tab_s3[["variation"]] %in% names(variation_label)))
# Negative values are written with a minus sign rather than a hyphen
fmt_signed <- function(x) {
  sub("^-", "$-$", fmt_num(x))
}
n_col_s3 <- 6 + length(sizes_s3)
body_s3 <- paste0("\\multicolumn{6}{l}{$\\alpha'$} & ",
                  paste(fmt_prob(design_s3[["alpha_matched"]]), collapse = " & "), " \\\\")
previous_source <- ""
previous_analysis <- ""
for (s in sort(unique(tab_s3[["id"]]))) {
  d <- tab_s3[tab_s3[["id"]] == s, ]
  d <- d[order(d[["n"]]), ]
  stopifnot(nrow(d) == length(sizes_s3), all(d[["n"]] == sizes_s3))
  first <- d[1, ]
  type1 <- fmt_prob(d[["type1_sat"]])
  type1 <- ifelse(d[["above_alpha_matched"]], paste0(type1, "$^{\\dagger}$"), type1)
  # Each source starts with a row that names it, and the endpoint, number of comparisons and
  # analysis are written in the first row of each analysis
  if (first[["bibkey"]] != previous_source) {
    body_s3 <- c(body_s3, "\\addlinespace",
                 paste0("\\multicolumn{", n_col_s3, "}{l}{", first[["author"]], "~\\cite{",
                        first[["bibkey"]], "}} \\\\"))
  }
  analysis_key <- paste(first[["bibkey"]], first[["endpoint"]], first[["analysis"]],
                        first[["subset"]])
  analysis_cells <- if (analysis_key == previous_analysis) {
    c("", "", "")
  } else {
    c(first[["endpoint"]], fmt_setting(first[["n_comparisons"]]),
      if (first[["subset"]] == "Outlier removed") {
        paste0(first[["analysis"]], "$^{\\ddagger}$")
      } else {
        first[["analysis"]]
      })
  }
  previous_source <- first[["bibkey"]]
  previous_analysis <- analysis_key
  tau_cell <- if (is.na(first[["tau"]])) "--" else fmt_num(first[["tau"]])
  cells <- c(analysis_cells, variation_label[[first[["variation"]]]],
             fmt_signed(first[["bias_mean"]]), tau_cell, type1)
  body_s3 <- c(body_s3, paste0(paste(cells, collapse = " & "), " \\\\"))
}
header_s3 <- c(
  paste0("Endpoint & $K$ & Analysis & Variation & $\\bar{b}$ & $\\tau$ & ",
         "\\multicolumn{", length(sizes_s3), "}{c}{$N$} \\\\"),
  paste0("\\cmidrule(lr){7-", n_col_s3, "}"),
  paste0(" & & & & & & ", paste(fmt_setting(sizes_s3), collapse = " & "), " \\\\")
)
caption_s3 <- paste0(
  "Type I error rate of a single-arm trial (SAT) without margin when the published estimates ",
  "of the bias of external controls in Table~\\ref{M-tab:empirical} of the main article ",
  "are ",
  "replaced by alternative values that reflect their uncertainty, for a time-to-event endpoint ",
  "with hazard ratio ",
  fmt_setting(hr_s3), " and an external control of $N_E = 2N$ patients, by the total size $N$, ",
  "with one-sided $\\alpha = ", fmt_setting(alpha_s3), "$.")
note_s3 <- paste0(
  "Upper $\\bar{b}$ replaces the posterior median of the mean bias by the upper limit of its ",
  "95\\% credible interval, and upper $\\tau$ replaces the posterior median of the ",
  "between-study standard deviation $\\tau$ by the upper limit of its 95\\% credible interval; ",
  "the other parameter is kept at its posterior median. IVW mean is the difference in ",
  "inverse variance-weighted log hazard ratios that Gupta et al.\\ report beside the ",
  "arithmetic mean of the trial-level differences, used with $\\tau = 0$. Trial SD uses the ",
  "published mean with $\\tau$ equal to ",
  "the sample standard deviation of the trial-level differences, which includes the sampling ",
  "error of each trial and therefore tends to overstate the between-trial variation. The ",
  "type I error ",
  "rate is averaged over a normal distribution of the bias with mean $\\bar{b}$ and standard ",
  "deviation $\\tau$. $\\alpha'$ is the level at which the randomized controlled trial with 1:1 ",
  "randomization has the power of the SAT without bias. Type I error rates above $\\alpha'$ in ",
  "the same column are marked with $\\dagger$. Analyses marked with $\\ddagger$ exclude the ",
  "study that the authors identified as an outlier. $K$ is the number of comparisons. IVW, ",
  "inverse variance-weighted; OS, overall survival; PFS, progression-free survival; QBA, ",
  "quantitative bias analysis; SD, standard deviation.")
write_table(tab_dir, "tableS3_bias_uncertainty",
            table_lines(caption_s3, "tab:bias_uncertainty",
                        paste0("lcllcc", strrep("c", length(sizes_s3))),
                        header_s3, body_s3, note_s3,
                        size = c("\\small", "\\setlength{\\tabcolsep}{3pt}")))

# ---- Tables S4 and S5: bias margins and averaged power under the variations of Table S3 ----
# The rows are those of Table S3. m_crit and m_0.95 do not depend on N in this setting
# (N_E = 2 N). Margins above m_crit (Table S4) and averaged powers below the power of the RCT
# (Table S5) are marked; m_0.95 and tau are not shown for the variations without a between-study
# standard deviation.
stopifnot(diff(range(tab_s3[["critical_margin"]])) < 1e-12,
          all(c("margin_avg", "margin_q95", "power_sat_margin_avg_averaged", "power_rct") %in%
                names(tab_s3)))
m_crit_s4 <- tab_s3[["critical_margin"]][1]
mark_above_crit_s4 <- function(x) {
  ifelse(x > m_crit_s4, paste0(fmt_signed(x), "$^{\\S}$"), fmt_signed(x))
}
mark_below_rct_s5 <- function(x, power_rct) {
  ifelse(x < power_rct, paste0(fmt_prob(x), "$^{\\P}$"), fmt_prob(x))
}
# Rows of Tables S4 and S5, one per variation in the order of Table S3. Each source starts with
# a row that names it, and the endpoint, number of comparisons and analysis are written in the
# first row of each analysis; cells(d) gives the other cells of the row of the variation d.
rows_by_variation <- function(n_col, cells) {
  out <- character(0)
  previous_source <- ""
  previous_analysis <- ""
  for (s in sort(unique(tab_s3[["id"]]))) {
    d <- tab_s3[tab_s3[["id"]] == s, ]
    d <- d[order(d[["n"]]), ]
    stopifnot(nrow(d) == length(sizes_s3), all(d[["n"]] == sizes_s3))
    first <- d[1, ]
    if (first[["bibkey"]] != previous_source) {
      out <- c(out, if (length(out) > 0) "\\addlinespace",
               paste0("\\multicolumn{", n_col, "}{l}{", first[["author"]], "~\\cite{",
                      first[["bibkey"]], "}} \\\\"))
    }
    analysis_key <- paste(first[["bibkey"]], first[["endpoint"]], first[["analysis"]],
                          first[["subset"]])
    analysis_cells <- if (analysis_key == previous_analysis) {
      c("", "", "")
    } else {
      c(first[["endpoint"]], fmt_setting(first[["n_comparisons"]]),
        if (first[["subset"]] == "Outlier removed") {
          paste0(first[["analysis"]], "$^{\\ddagger}$")
        } else {
          first[["analysis"]]
        })
    }
    previous_source <- first[["bibkey"]]
    previous_analysis <- analysis_key
    row_cells <- c(analysis_cells, variation_label[[first[["variation"]]]], cells(d))
    out <- c(out, paste0(paste(row_cells, collapse = " & "), " \\\\"))
  }
  out
}
abbreviations_s4 <- paste0(
  "Analyses marked with $\\ddagger$ exclude the study that the authors identified as an ",
  "outlier. $K$ is the number of comparisons. IVW, inverse variance-weighted; OS, overall ",
  "survival; PFS, progression-free survival; QBA, quantitative bias analysis; SD, standard ",
  "deviation.")
settings_s4 <- paste0(
  "when the published estimates of the bias are replaced by the values of ",
  "Table~\\ref{tab:bias_uncertainty}, for a time-to-event endpoint with hazard ratio ",
  fmt_setting(hr_s3), " and an external control of $N_E = 2N$ patients, by the total size $N$, ",
  "with one-sided $\\alpha = ", fmt_setting(alpha_s3), "$.")

# Table S4: margins
cells_s4 <- function(d) {
  stopifnot(diff(range(d[["margin_q95"]])) < 1e-12)
  q95_cell <- if (is.na(d[["tau"]][1])) "--" else mark_above_crit_s4(d[["margin_q95"]][1])
  c(q95_cell, mark_above_crit_s4(d[["margin_avg"]]))
}
n_col_s4 <- 5 + length(sizes_s3)
body_s4 <- rows_by_variation(n_col_s4, cells_s4)
header_s4 <- c(
  paste0("Endpoint & $K$ & Analysis & Variation & $m_{0.95}$ & \\multicolumn{",
         length(sizes_s3), "}{c}{$m_{\\mathrm{avg}}$ by $N$} \\\\"),
  paste0("\\cmidrule(lr){6-", n_col_s4, "}"),
  paste0(" & & & & & ", paste(fmt_setting(sizes_s3), collapse = " & "), " \\\\")
)
caption_s4 <- paste0(
  "Bias margins $m_{\\mathrm{avg}}$ and $m_{0.95}$ of Table~\\ref{M-tab:margins} of the main ",
  "article ", settings_s4)
note_s4 <- paste0(
  "With $m_{\\mathrm{avg}}$, the type I error rate averaged over a normal distribution of the ",
  "bias with mean $\\bar{b}$ and standard deviation $\\tau$ equals $\\alpha$, and ",
  "$m_{0.95} = \\bar{b} + z_{0.95}\\tau$ is the 95th percentile of this distribution. The ",
  "critical margin is $m_{\\mathrm{crit}} = ", fmt_num(m_crit_s4), "$ at every $N$, and ",
  "margins above it are marked with $\\S$. The rows are those of ",
  "Table~\\ref{tab:bias_uncertainty}, which gives $\\bar{b}$ and $\\tau$. IVW mean is used ",
  "with $\\tau = 0$, so that $m_{0.95}$ is not shown and $m_{\\mathrm{avg}}$ equals ",
  "$\\bar{b}$. ", abbreviations_s4)
write_table(tab_dir, "tableS4_margin_uncertainty",
            table_lines(caption_s4, "tab:margin_uncertainty",
                        paste0("lcllc", strrep("c", length(sizes_s3))),
                        header_s4, body_s4, note_s4,
                        size = c("\\small", "\\setlength{\\tabcolsep}{3pt}")))

# Table S5: power averaged over the distribution of the bias
cells_s5 <- function(d) {
  stopifnot(max(abs(d[["power_rct"]] - design_s3[["power_rct"]])) < 1e-12)
  tau_cell <- if (is.na(d[["tau"]][1])) "--" else fmt_num(d[["tau"]][1])
  c(tau_cell, mark_below_rct_s5(d[["power_sat_margin_avg_averaged"]], d[["power_rct"]]))
}
n_col_s5 <- 5 + length(sizes_s3)
body_s5 <- c(paste0("\\multicolumn{5}{l}{Power of RCT} & ",
                    paste(fmt_prob(design_s3[["power_rct"]]), collapse = " & "), " \\\\"),
             "\\addlinespace", rows_by_variation(n_col_s5, cells_s5))
header_s5 <- c(
  paste0("Endpoint & $K$ & Analysis & Variation & $\\tau$ & \\multicolumn{", length(sizes_s3),
         "}{c}{$N$} \\\\"),
  paste0("\\cmidrule(lr){6-", n_col_s5, "}"),
  paste0(" & & & & & ", paste(fmt_setting(sizes_s3), collapse = " & "), " \\\\")
)
caption_s5 <- paste0(
  "Power of the single-arm trial (SAT) with the margin $m_{\\mathrm{avg}}$ of ",
  "Table~\\ref{tab:margin_uncertainty}, averaged over the distribution of the bias, and of the ",
  "randomized controlled trial (RCT) with 1:1 randomization, ", settings_s4)
note_s5 <- paste0(
  "The power of the SAT is averaged over a normal distribution of the bias with mean ",
  "$\\bar{b}$ and standard deviation $\\tau$. It does not depend on $\\bar{b}$, so that the ",
  "rows with the upper limit of $\\bar{b}$ repeat Table~\\ref{M-tab:margins}(b) of the main ",
  "article, and it exceeds the power of the RCT exactly when $\\tau$ is below the break-even ",
  "bias $b^{*}$ (Table~\\ref{M-tab:empirical}(a) of the main article). Powers below the power ",
  "of the RCT in the same column are marked with $\\P$. IVW mean is used with $\\tau = 0$. ",
  abbreviations_s4)
write_table(tab_dir, "tableS5_power_uncertainty",
            table_lines(caption_s5, "tab:power_uncertainty",
                        paste0("lcllc", strrep("c", length(sizes_s3))),
                        header_s5, body_s5, note_s5,
                        size = c("\\small", "\\setlength{\\tabcolsep}{3pt}")))

# ---- Table S6: reproduction of published results and validation ----
# The numbers of verdicts are read from the summaries written by
# inst/reproduce/summarize_reproduction.R and inst/validation/summarize_validation.R, the
# description of each script from inst/sup_info/validation_descriptions.csv, the reason for each
# EXPLAINED verdict from the results file of its script and the version of the package from
# DESCRIPTION. The section stops if the scripts in the summaries and in the descriptions differ,
# if the verdicts do not add up to the totals, if any comparison failed or if an EXPLAINED
# verdict has no reason.
verdict_names <- c("PASS", "EXPLAINED", "FAIL", "INFO")
results_dirs_s6 <- c(Reproduction = "inst/reproduce/results",
                     Validation = "inst/validation/results")
summary_s6 <- do.call(rbind, lapply(names(results_dirs_s6), function(g) {
  path <- file.path(results_dirs_s6[[g]], "summary.csv")
  if (!file.exists(path)) {
    stop("Missing ", path, ". Run the summary scripts in inst/reproduce and inst/validation ",
         "first.", call. = FALSE)
  }
  s <- utils::read.csv(path, stringsAsFactors = FALSE)
  s[["group"]] <- g
  s
}))
desc_s6 <- utils::read.csv("inst/sup_info/validation_descriptions.csv",
                           colClasses = "character")
if (!identical(names(desc_s6), c("group", "script", "comparison", "bibkey")) ||
    !all(c("script", verdict_names, "total") %in% names(summary_s6))) {
  stop("Unexpected columns in the summaries or in validation_descriptions.csv.", call. = FALSE)
}
key_desc <- paste(desc_s6[["group"]], desc_s6[["script"]])
key_summary <- paste(summary_s6[["group"]], summary_s6[["script"]])
if (anyDuplicated(key_desc) > 0 || !setequal(key_desc, key_summary)) {
  stop("The scripts in the summaries and in inst/sup_info/validation_descriptions.csv differ.",
       call. = FALSE)
}
if (any(rowSums(summary_s6[verdict_names]) != summary_s6[["total"]])) {
  stop("The verdicts in the summaries do not add up to their totals.", call. = FALSE)
}
if (sum(summary_s6[["FAIL"]]) > 0) {
  stop("Some comparisons failed, so Table S6 is not written.", call. = FALSE)
}
# Escape the special characters of LaTeX in the names and reasons taken from the results; stop
# for characters that cannot be escaped this way
tex_escape <- function(x) {
  unsupported <- c("\\", "{", "}", "~", "^")
  for (ch in unsupported) {
    if (any(grepl(ch, x, fixed = TRUE))) {
      stop("Unsupported character '", ch, "' in a name or reason for Table S6.", call. = FALSE)
    }
  }
  gsub("([_%&#$])", "\\\\\\1", x)
}
explained_s6 <- character(0)
for (i in which(summary_s6[["EXPLAINED"]] > 0)) {
  script <- summary_s6[["script"]][i]
  res <- utils::read.csv(file.path(results_dirs_s6[[summary_s6[["group"]][i]]],
                                   paste0(script, ".csv")),
                         stringsAsFactors = FALSE)
  reasons <- res[["note"]][res[["verdict"]] == "EXPLAINED"]
  if (length(reasons) != summary_s6[["EXPLAINED"]][i] || any(is.na(reasons) | reasons == "")) {
    stop("An EXPLAINED verdict of ", script, " has no reason.", call. = FALSE)
  }
  explained_s6 <- c(explained_s6, paste0("EXPLAINED in \\texttt{", tex_escape(script), "}. ",
                                         tex_escape(reasons)))
}
version_s6 <- read.dcf("DESCRIPTION", fields = "Version")[1, "Version"]
group_label_s6 <- c(
  Reproduction = "(a) Reproduction of published results (folder \\texttt{inst/reproduce})",
  Validation = "(b) Validation (folder \\texttt{inst/validation})")
n_col_s6 <- 2 + length(verdict_names)
body_s6 <- character(0)
for (g in names(group_label_s6)) {
  if (length(body_s6) > 0) {
    body_s6 <- c(body_s6, "\\addlinespace")
  }
  body_s6 <- c(body_s6, paste0("\\multicolumn{", n_col_s6, "}{l}{\\textit{",
                               group_label_s6[[g]], "}} \\tabularnewline"))
  d <- desc_s6[desc_s6[["group"]] == g, ]
  for (j in seq_len(nrow(d))) {
    s <- summary_s6[key_summary == paste(g, d[["script"]][j]), ]
    comparison <- if (d[["bibkey"]][j] == "") {
      d[["comparison"]][j]
    } else {
      paste0(d[["comparison"]][j], "~\\cite{", d[["bibkey"]][j], "}")
    }
    # the files are reproduce_<name>.R and validate_<name>.R; the table shows <name>
    name <- sub("^(reproduce|validate)_", "", d[["script"]][j])
    cells <- c(paste0("\\texttt{", tex_escape(name), "}"), paste0("\\raggedright ", comparison),
               as.character(unlist(s[verdict_names])))
    body_s6 <- c(body_s6, paste0(paste(cells, collapse = " & "), " \\tabularnewline"))
  }
}
caption_s6 <- paste0(
  "Verdicts of the scripts that reproduce published results with the \\texttt{R} package ",
  "\\texttt{satrct} (version ", version_s6, ") and validate it, by script.")
note_s6 <- paste0(
  "PASS, the recomputed value agrees with the reference under the stated rule; EXPLAINED, it ",
  "does not, and the reason is identified and recorded; FAIL, it does not; INFO, no pass or fail ",
  "rule applies (values read from figures, qualitative statements or the accuracy of an ",
  "approximation). The scripts are the files \\texttt{reproduce\\_}\\textit{name}\\texttt{.R} in ",
  "\\texttt{inst/reproduce} and \\texttt{validate\\_}\\textit{name}\\texttt{.R} in ",
  "\\texttt{inst/validation}.")
if (length(explained_s6) > 0) {
  note_s6 <- paste(note_s6, paste(explained_s6, collapse = " "))
}
# The comparison column is a paragraph column, so the rows end with \tabularnewline, which
# \raggedright does not redefine
write_table(tab_dir, "tableS6_validation", c(
  "\\begin{table}[!htb]",
  "\\centering",
  "\\small",
  paste0("\\caption{", caption_s6, "}"),
  "\\label{tab:validation}",
  "\\footnotesize",
  "\\setlength{\\tabcolsep}{4pt}",
  paste0("\\begin{tabular}{lp{6.4cm}", strrep("c", length(verdict_names)), "}"),
  "\\toprule",
  paste0("Script & Comparison & ", paste(verdict_names, collapse = " & "), " \\tabularnewline"),
  "\\midrule",
  body_s6,
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\smallskip",
  "\\begin{minipage}{0.95\\linewidth}",
  "\\footnotesize",
  note_s6,
  "\\end{minipage}",
  "\\end{table}"))
