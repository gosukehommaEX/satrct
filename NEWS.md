# satrct 0.1.0

* Initial version.
* After a review by a co-author, before the first release:
  - `inst/scripts/` gives Figure 4 (bias margins fixed before the trial),
    and `inst/sup_info/` gives Figure S3 (bias at which the type I error
    rates cross for binary and time-to-event endpoints) and Tables S6 and S7
    (fixed margins, effective number of external controls and unequal event
    probabilities). Negative margins are replaced by zero in Tables S4 and
    S5.
  - `inst/run_all.R` runs every script of the article in order, records the
    session information and copies the outputs to a manuscript folder only
    when `SATRCT_MANUSCRIPT_DIR` is set.
  - The validation scripts stop when a comparison fails, a registered
    explanation applies only to the registered value, and the check of the
    unit tests requires an unchanged baseline and a failure of the block that
    contains the altered value.
  - The documentation states that `n_rct()` and `n_sat()` return continuous
    planning values, and the vignette covers a random bias and fixed
    margins.
* Normal-approximation comparison of a single-arm trial with an external
  control and an RCT of the same size: `mse_rct()`, `mse_sat()`,
  `breakeven_bias()`, `breakeven_n()`, `power_rct()`, `power_sat()`,
  `n_rct()`, `n_sat()`, `critical_margin()`, `matched_alpha()` and
  `compare_designs()`, with the z-test and the t-test.
* Hybrid designs with concurrent and external controls: `mse_hybrid()` and
  `optimal_nc()`.
* Exact operating characteristics for binary endpoints:
  `oc_sat_binary_exact()` and `oc_binary_exact()`.
* `endpoint_sd()` for binary and time-to-event endpoints.
* `inst/reproduce/` reproduces published results of Collignon et al. (2020),
  Pocock (1976), Kopp-Schneider et al. (2020, 2024), Tang et al. (2010),
  Viele et al. (2014) and Taylor et al. (2006).
* `inst/validation/` compares the functions with an independent Python
  implementation, with rpact and with patient-level simulation.
* `inst/scripts/` and `inst/sup_info/` compute the tables and figures of the
  main text and the Supporting Information of the accompanying article.
