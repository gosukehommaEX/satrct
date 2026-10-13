
<!-- README.md is generated from README.Rmd. Please edit that file -->

# satrct

satrct compares a single-arm trial, in which all patients receive the
experimental treatment and are compared with an external control group
or a fixed threshold, with a randomized controlled trial (RCT) that
enrols the same total number of patients. It provides

- the break-even bias at which the two designs have equal mean squared
  error (Pocock, 1976; Collignon et al., 2020) and the corresponding
  sample size,
- the power and type I error of a single-arm comparison with a
  prespecified bias margin, and the sample sizes of both designs,
- the margin at which the two designs have equal power, and the relaxed
  significance level at which an RCT matches the power of a single-arm
  trial with a margin,
- hybrid designs that combine concurrent and external controls (Pocock,
  1976), and
- exact operating characteristics for binary endpoints.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("gosukehommaEX/satrct")
```

## Example

``` r
library(satrct)

# Bias at which a single-arm trial of 40 patients with 100 external controls
# and a 1:1 RCT of 40 patients have equal mean squared error (unit SD)
breakeven_bias(n = 40, n_e = 100)
#> [1] 0.254951

# Type I error of the single-arm test with a bias margin of 0.1
power_sat(n = 40, delta = 0, n_e = 100, bias = c(0, 0.1, 0.2), margin = 0.1)
#> [1] 0.006306977 0.025000000 0.077014809

# Relaxed level at which the RCT matches the power of the single-arm trial
matched_alpha(n = 40, delta = 0.6, n_e = 100, margin = 0.1)
#>   alpha_matched     power bias_crossing
#> 1     0.1180644 0.7619683     0.2450352
```

The package version used in this README is 0.1.0.

## References

Pocock SJ. The combination of randomized and historical controls in
clinical trials. *Journal of Chronic Diseases* 1976; 29: 175–188.

Collignon O, Schritz A, Senn SJ, Spezia R. Clustered allocation as a way
of understanding historical controls: components of variation and
regulatory considerations. *Statistical Methods in Medical Research*
2020; 29: 1960–1971.
