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
source("inst/scripts/data_generate_main.R")
source("inst/scripts/table_and_figure_manuscript.R")
source("inst/sup_info/data_generate_supplement.R")
source("inst/sup_info/table_and_figure_supplement.R")
source("inst/scripts/copy_results.R")
```

| Script | Time | Output |
|---|---|---|
| `data_generate_main.R` | < 10 s | `inst/scripts/results/data/*.csv` |
| `table_and_figure_manuscript.R` | < 10 s | `inst/scripts/results/figures/Figure1`--`3` (`.pdf`, `.eps`), `inst/scripts/results/tables/table1_relative.tex`, `table2_illustration.tex` |
| `data_generate_supplement.R` | < 10 s | `inst/sup_info/results/data/*.csv` |
| `table_and_figure_supplement.R` | < 10 s | `inst/sup_info/results/figures/FigureS1`--`S2` (`.pdf`, `.eps`), `inst/sup_info/results/tables/tableS1_z_t.tex`, `tableS2_sensitivity.tex` |
| `copy_results.R` | < 10 s | copies in `Manuscript/tables` and `Manuscript/figures`, log `inst/scripts/results/copy_log.csv` |

The times were measured on the author's computer; the five scripts together take about 30
seconds. All calculations use closed forms or the enumeration of binomial outcomes, without
simulation, so the results do not depend on a random seed. The `data_generate_*.R` scripts
print the elapsed time and stop if a relation that holds by construction fails (for example,
that the type I error rate of the single-arm trial equals the matched level alpha' at the
bias m_crit for every margin).

## Contents

| Result | Data | Settings |
|---|---|---|
| Figure 1 | `fig1a_breakeven_bias.csv`, `fig1b_relative.csv` | (a) b* against N for N_E = Inf, 200, 100, 50; (b) b* / delta against the power of the RCT for N / N_E = 0, 0.5, 1, 2 |
| Figure 2 | `fig2_power.csv` | power against N for delta = 0.5 sigma, margins m / delta = 0, 0.25, 0.5, 0.75 and N_E = Inf, 100 |
| Figure 3 | `fig3_type1.csv`, `fig3_crossing.csv` | type I error rate against the bias for N = 40, delta = 0.5 sigma, m / delta = 0, 0.15, 0.3 and N_E = Inf, 80 |
| Table 1 | `table1_relative.csv` | power of the RCT 0.2, 0.35, 0.5, 0.8 and N / N_E = 0, 0.5, 1, 2 |
| Table 2 | `table2_illustration.csv` | continuous, binary (against external controls and a threshold) and time-to-event endpoints |
| Figure S1 | `figS1_hybrid.csv` | hybrid design with N = 40 and N_E = 100 |
| Figure S2 | `figS2_binary.csv` | exact and approximate power and exact type I error rate, 0.55 vs 0.30, margin 0.05, N = 20 to 100 |
| Table S1 | `tableS1_z_t.csv` | z-test and t-test for N = 10, 20, 40 and delta / sigma = 0.5, 1 |
| Table S2 | `tableS2_sensitivity.csv` | allocation 1:1 and 2:1 and sigma_C / sigma_T = 0.5, 1, 2 |

The figures are vector graphics (PDF and EPS) drawn with base R graphics, so the scripts need
no package other than satrct.
