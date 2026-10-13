# satrct 0.1.0

* Initial version.
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
