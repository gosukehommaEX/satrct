# Scripts for the tables and figures of the article

The scripts in `inst/scripts/` (main text) and `inst/sup_info/` (Supporting Information)
compute all numerical results, tables and figures of the article. Computation and drawing are
separate: the `data_generate_*.R` scripts save the results as CSV files, and the
`table_and_figure_*.R` scripts read them and write the tables and figures, so that the layout
can be changed without recomputing.

## Order of execution

Open `satrct.Rproj` in RStudio, so that the working directory is the package root, and run

```r
devtools::load_all()
source("inst/run_all.R")
```

`inst/run_all.R` runs, each in an environment of its own and in this order,
`data_generate_main.R`, `table_and_figure_manuscript.R`, `data_generate_supplement.R`, the
scripts in `inst/reproduce` and `inst/validation` with their summaries,
`table_and_figure_supplement.R` (Table S8 summarizes the verdicts) and `number_check.R`, and
stops at the first failure. It writes the versions of R and of the packages to
`results/session_info.txt`. The copy into the manuscript folder of the authors is optional:

```r
Sys.setenv(SATRCT_MANUSCRIPT_DIR = "../../Manuscript")
source("inst/run_all.R")
```

| Script | Time | Output |
|---|---|---|
| `data_generate_main.R` | < 10 s | `inst/scripts/results/data/*.csv` |
| `table_and_figure_manuscript.R` | < 10 s | `inst/scripts/results/figures/Figure1`--`4` (`.pdf`, `.eps`), `inst/scripts/results/tables/table1_relative.tex`, `table2_illustration.tex`, `table3_empirical_bias.tex`, `table4_bias_margins.tex` |
| `data_generate_supplement.R` | a few minutes | `inst/sup_info/results/data/*.csv` |
| `table_and_figure_supplement.R` | < 10 s | `inst/sup_info/results/figures/FigureS1`--`S3` (`.pdf`, `.eps`), `inst/sup_info/results/tables/tableS1_z_t.tex`, `tableS2_sensitivity.tex`, `tableS3_bias_uncertainty.tex`, `tableS4_margin_uncertainty.tex`, `tableS5_power_uncertainty.tex`, `tableS6_fixed_margin.tex`, `tableS7_design_sensitivity.tex`, `tableS8_validation.tex` |
| `copy_results.R` | < 10 s | copies in `<folder>/tables` and `<folder>/figures` of the folder `SATRCT_MANUSCRIPT_DIR` (default `../../Manuscript`), log `inst/scripts/results/copy_log.csv` |

Most of the time of `data_generate_supplement.R` is taken by the patient-level simulation of
Figure S3(b), with 100,000 samples for each of four sizes; the other scripts of this table take
less than 10 seconds each. The simulation uses the fixed seed `set.seed(20261104)`, so its
results are reproduced with the same version of R. All other calculations use closed forms or
the enumeration of binomial outcomes. The `data_generate_*.R` scripts
print the elapsed time and stop if a relation that holds by construction fails (for example,
that the type I error rate of the single-arm trial equals the matched level alpha' at the
bias m_crit for every margin).

## Contents

| Result | Data | Settings |
|---|---|---|
| Figure 1 | `fig1a_breakeven_bias.csv`, `fig1b_relative.csv` | (a) b* against N for N_E = Inf, 200, 100, 50; (b) b* / delta against the power of the RCT for N / N_E = 0, 0.5, 1, 2 |
| Figure 2 | `fig2_power.csv` | power against N for delta = 0.5 sigma, margins m / delta = 0, 0.25, 0.5, 0.75 and N_E = Inf, 100 |
| Figure 3 | `fig3_type1.csv`, `fig3_crossing.csv` | type I error rate against the bias for N = 40, delta = 0.5 sigma, m / delta = 0, 0.15, 0.3 and N_E = Inf, 80 |
| Figure 4 | `fig4_fixed_margin.csv` | margins m_avg and m_0.95 of Table 4 for the estimate of Incerti et al. with all 14 studies at N = 60, kept fixed: (a) type I error rate given the bias, (b) type I error rate averaged over a distribution of the bias whose mean or standard deviation differs from the planning values |
| Table 1 | `table1_relative.csv` | power of the RCT 0.2, 0.35, 0.5, 0.8 and N / N_E = 0, 0.5, 1, 2 |
| Table 2 | `table2_illustration.csv` | continuous, binary (against external controls and a threshold) and time-to-event endpoints |
| Table 3 | `table3_empirical_bias.csv` | time-to-event endpoint with hazard ratio 0.6, N = 30, 60, 90, 120 and N_E = 2 N, under the published estimates of the bias of external controls in `empirical_bias_sources.csv` |
| Table 4 | `table3_empirical_bias.csv` | (a) bias margins m_avg and m_0.95 for a random bias under the weighted estimates of Table 3; (b) power of the single-arm trial with each margin, averaged over the distribution of the bias |
| Figure S1 | `figS1_hybrid.csv` | hybrid design with N = 40 and N_E = 100 |
| Figure S2 | `figS2_binary.csv` | exact and approximate power and exact type I error rate, 0.55 vs 0.30, margin 0.05, N = 20 to 100 |
| Figure S3 | `figS3_binary_crossing.csv`, `figS3_survival_crossing.csv` | bias at which the type I error rate of the single-arm trial reaches that of the matched RCT: (a) binary endpoint of Figure S2, exact, and its normal approximation with the variance under the null hypothesis; (b) time-to-event endpoint of Table 3 with margins 0 and 0.1, simulated, and its normal approximation with the variance under the null hypothesis |
| Table S1 | `tableS1_z_t.csv` | z-test and t-test for N = 10, 20, 40 and delta / sigma = 0.5, 1 |
| Table S2 | `tableS2_sensitivity.csv` | allocation 1:1 and 2:1 and sigma_C / sigma_T = 0.5, 1, 2 |
| Table S3 | `tableS3_bias_uncertainty.csv` | the settings of Table 3 (read from `table3_empirical_bias.csv`, so `data_generate_main.R` is run first) with the limits of the credible intervals in `inst/sup_info/bias_uncertainty_sources.csv` and the trial-level differences of Gupta et al. in `inst/sup_info/gupta_trial_differences.csv` |
| Table S4 | `tableS3_bias_uncertainty.csv` | bias margins m_avg and m_0.95 under the variations of Table S3 |
| Table S5 | `tableS3_bias_uncertainty.csv` | power of the single-arm trial with m_avg, averaged over the distribution of the bias, under the variations of Table S3 |
| Table S6 | `tableS6_fixed_margin.csv` | margins of Table 4 kept fixed under the posterior medians, the upper limits of the credible intervals of the mean bias and of tau, and both (table for N = 60; the file has all four sizes) |
| Table S7 | `tableS7_design_sensitivity.csv` | effective numbers of external controls N and N / 2 and event probability 1 - 0.3^0.6 on the experimental treatment |
| Table S8 | `inst/reproduce/results/summary.csv`, `inst/validation/results/summary.csv`, `inst/sup_info/validation_descriptions.csv` | numbers of verdicts of the reproduction and validation scripts; rerun the summary scripts in `inst/reproduce` and `inst/validation` when their results change |

The figures are vector graphics (PDF and EPS) drawn with base R graphics, so the scripts need
no package other than satrct.

The published estimates of the bias of external controls used for Table 3 are entered in
`empirical_bias_sources.csv`, one row per estimate with its location in the source. The bias
is the log hazard ratio estimated with the randomized control minus the log hazard ratio
estimated with the external control (column `measure`: `hr_internal_vs_external` for a reported
hazard ratio of the internal versus the external control, `log_hr_difference` for a reported
difference in log hazard ratios). The column `subset` marks the estimates that exclude a study
identified as an outlier by the authors of the source. The inputs of Table S3 are entered in the
same way in `inst/sup_info/bias_uncertainty_sources.csv` (column `variation`) and
`inst/sup_info/gupta_trial_differences.csv` (one row per trial, with the column `outlier`).

## Numbers in the text

Every number written in the manuscript is registered in `number_locations.csv` (one row per
number, with the string of the .tex file that contains it, the results file and the expression
that give its value, and the rounding rule). `number_check.R` recomputes every registered value
from the saved results and stops if a value differs from the text:

```r
source("inst/scripts/number_check.R")
```

Its output is written to `results/number_check.csv`. The location of the strings in the .tex
files is checked by the script `tools/check_numbers.py` of the manuscript folder of the authors,
which is not part of the package and is not needed to reproduce the results.
