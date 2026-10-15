# Common settings and helpers for table_and_figure_manuscript.R and
# inst/sup_info/table_and_figure_supplement.R. Source from the package root.

# Okabe-Ito colours for up to four series. Black is kept for the RCT, and every series is also
# distinguished by line type and named in a legend, so that the figures can be read in grey
# scale.
pal <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")
lty_series <- c(1, 2, 4, 5)
# line types of series drawn together with a solid black reference line
lty_with_ref <- c(2, 4, 5, 6)

read_results <- function(dir, name, script) {
  path <- file.path(dir, name)
  if (!file.exists(path)) {
    stop("Missing ", path, ". Run ", script, " first.", call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}

# Save a figure as PDF and EPS (vector graphics, Helvetica, no transparency) by calling draw()
# once for each device
save_figure <- function(dir, name, draw, width = 7, height = 3.8) {
  open_draw_close <- function(open) {
    open()
    on.exit(grDevices::dev.off())
    draw()
  }
  open_draw_close(function() {
    grDevices::pdf(file.path(dir, paste0(name, ".pdf")), width = width, height = height,
                   family = "Helvetica", pointsize = 10, useDingbats = FALSE)
  })
  open_draw_close(function() {
    grDevices::postscript(file.path(dir, paste0(name, ".eps")), width = width,
                          height = height, family = "Helvetica", pointsize = 10,
                          horizontal = FALSE, onefile = FALSE, paper = "special")
  })
  message("Saved ", file.path(dir, name), ".pdf and .eps")
}

# Two panels side by side, with legend_lines lines below them for a common legend
start_panels <- function(legend_lines = 0) {
  graphics::par(mfrow = c(1, 2), oma = c(legend_lines, 0, 0, 0), mar = c(3.8, 4.2, 2, 0.8),
                mgp = c(2.4, 0.7, 0), las = 1, tcl = -0.35)
}

panel_title <- function(expr) {
  graphics::mtext(expr, side = 3, line = 0.6, adj = 0)
}

# Panel title naming the external control: a fixed threshold (n_e = Inf) or n_e patients.
# In plotmath labels, "=" is written as a string so that it is set in Helvetica with a space on
# each side, and the asterisk of b* is set at full size (a superscript asterisk is too small).
title_ne <- function(label, n_e) {
  if (is.infinite(n_e)) {
    bquote(bold(.(label)) ~~ "Threshold" ~ (italic(N)[italic(E)] ~ "=" ~ infinity))
  } else {
    bquote(bold(.(label)) ~~ "External control" ~ (italic(N)[italic(E)] ~ "=" ~ .(n_e)))
  }
}

# Common legend below the panels. The labels (a list of character strings and plotmath calls)
# are given row by row and laid out in ncol columns.
bottom_legend <- function(labels, col, lty, lwd, ncol) {
  k <- length(labels)
  n_row <- ceiling(k / ncol)
  pad <- n_row * ncol - k
  labels <- c(labels, rep(list(""), pad))
  col <- c(col, rep("white", pad))
  lty <- c(lty, rep(0, pad))
  lwd <- c(lwd, rep(1, pad))
  ord <- as.vector(matrix(seq_len(n_row * ncol), n_row, ncol, byrow = TRUE))
  graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  graphics::plot.new()
  graphics::legend("bottom", legend = as.expression(labels[ord]), col = col[ord],
                   lty = lty[ord], lwd = lwd[ord], ncol = ncol, bty = "n", xpd = NA,
                   seg.len = 3, x.intersp = 0.7, inset = 0.01)
}

# Number formats for the tables. fmt_prob() prints probabilities that round to 1 as ">0.999".
fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
}
fmt_prob <- function(x, digits = 3) {
  out <- fmt_num(x, digits)
  big <- !is.na(x) & x >= 1 - 0.5 * 10^(-digits)
  out[big] <- paste0("$>$", formatC(1 - 10^(-digits), format = "f", digits = digits))
  out
}
# Settings quoted in captions and notes, without trailing zeros
fmt_setting <- function(x) {
  format(x, trim = TRUE, drop0trailing = TRUE)
}

write_table <- function(dir, name, lines) {
  path <- file.path(dir, paste0(name, ".tex"))
  writeLines(lines, path)
  message("Saved ", path)
}

# Table environment with caption, label, tabular body and a note below the table
table_lines <- function(caption, label, align, header, body, note, size = NULL) {
  c("\\begin{table}[!htb]",
    "\\centering",
    size,
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    header,
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}",
    "\\par\\smallskip",
    "\\begin{minipage}{0.95\\linewidth}",
    "\\footnotesize",
    note,
    "\\end{minipage}",
    "\\end{table}")
}
