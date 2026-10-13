"""Reference values on grids of settings, computed with the independent Python implementation in
satrct_reference.py, for inst/validation/validate_python_reference.R.

Run from this folder:  python3 make_python_grid.py
Output: grid_mse.csv, grid_power.csv, grid_margin.csv, grid_n.csv, grid_hybrid.csv,
        grid_sat_binary.csv, grid_binary.csv (Inf is written as "Inf")
"""
import csv
import itertools

import numpy as np
from scipy import stats

import satrct_reference as ref

INF = np.inf
SIGMAS = [(1.0, 1.0), (1.5, 0.8)]


def fmt(x):
    if isinstance(x, str):
        return x
    x = float(x)
    if np.isinf(x):
        return "Inf" if x > 0 else "-Inf"
    if np.isnan(x):
        return "NA"
    return repr(x)


def write(path, header, rows):
    with open(path, "w", newline="") as f:
        w = csv.writer(f, lineterminator="\r\n")
        w.writerow(header)
        for r in rows:
            w.writerow([fmt(v) for v in r])
    print(path, len(rows), "rows")


# ---- mean squared error, break-even bias and sample size ----
rows = []
for n, n_e, bias, tau, (st, sc), r in itertools.product(
        [10, 40, 150], [15, 100, INF], [-0.1, 0.0, 0.25], [0.0, 0.1], SIGMAS, [0.5, 2 / 3]):
    rows.append([n, n_e, bias, tau, st, sc, r,
                 ref.mse_rct(n, st, sc, r), ref.mse_sat(n, n_e, bias, tau, st, sc),
                 ref.breakeven_bias(n, n_e, st, sc, r), ref.breakeven_n(bias, n_e, tau, st, sc, r)])
write("grid_mse.csv", ["n", "n_e", "bias", "tau", "sigma_t", "sigma_c", "r", "mse_rct", "mse_sat",
                       "breakeven_bias", "breakeven_n"], rows)

# ---- power and type I error ----
rows = []
for n, delta, n_e, bias, margin, tau, (st, sc), alpha, r, method in itertools.product(
        [10, 30, 80], [0.0, 0.3, 0.8], [20, 100, INF], [-0.1, 0.0, 0.2], [0.0, 0.15], [0.0, 0.1],
        SIGMAS, [0.025, 0.1], [0.5, 2 / 3], ["z", "t"]):
    if method == "t" and (st != sc or tau > 0):
        continue
    rows.append([n, delta, n_e, bias, margin, tau, st, sc, alpha, r, method,
                 ref.power_rct(n, delta, st, sc, alpha, r, method),
                 ref.power_sat(n, delta, n_e, bias, margin, tau, st, sc, alpha, method)])
write("grid_power.csv", ["n", "delta", "n_e", "bias", "margin", "tau", "sigma_t", "sigma_c",
                         "alpha", "r", "method", "power_rct", "power_sat"], rows)

# ---- critical margin and matched significance level ----
# Settings in which the power or the matched level is within 1e-4 of one are skipped, because
# the numerical root finding of the reference implementation loses accuracy there.
rows = []
skipped = 0
for n, delta, n_e, margin, (st, sc), alpha, r in itertools.product(
        [10, 40, 100], [0.2, 0.4, 0.6], [30, 200, INF], [0.0, 0.1, 0.3], SIGMAS, [0.025, 0.05],
        [0.5, 2 / 3]):
    pw = ref.power_sat(n, delta, n_e, 0.0, margin, 0.0, st, sc, alpha)
    if pw > 0.9999:
        skipped += 1
        continue
    a_m, pw, b_x = ref.matched_alpha(n, delta, n_e, margin, st, sc, alpha, r)
    if a_m > 0.9999:
        skipped += 1
        continue
    rows.append([n, delta, n_e, margin, st, sc, alpha, r,
                 ref.critical_margin(n, delta, n_e, st, sc, r), a_m, pw, b_x])
print("grid_margin: skipped", skipped, "settings near power one")
write("grid_margin.csv", ["n", "delta", "n_e", "margin", "sigma_t", "sigma_c", "alpha", "r",
                          "critical_margin", "alpha_matched", "power", "bias_crossing"], rows)

# ---- sample sizes ----
rows = []
for delta, power, n_e, bias, margin, (st, sc), alpha, r, method in itertools.product(
        [0.3, 0.8], [0.8, 0.9], [50, 200, INF], [0.0, 0.05], [0.0, 0.1], SIGMAS, [0.025, 0.05],
        [0.5, 2 / 3], ["z", "t"]):
    if method == "t" and st != sc:
        continue
    rows.append([delta, power, n_e, bias, margin, st, sc, alpha, r, method,
                 ref.n_rct(delta, power, st, sc, alpha, r, method),
                 ref.n_sat(delta, power, n_e, bias, margin, st, sc, alpha, method)])
write("grid_n.csv", ["delta", "power", "n_e", "bias", "margin", "sigma_t", "sigma_c", "alpha", "r",
                     "method", "n_rct", "n_sat"], rows)

# ---- hybrid designs ----
rows = []
for n, frac, n_e, bias, tau, (st, sc) in itertools.product(
        [60, 200], [0.0, 0.1, 0.3, 0.5], [40, 300, INF], [0.0, 0.1], [0.0, 0.05], SIGMAS):
    n_c = frac * n
    w, mse = ref.mse_hybrid(n, n_c, n_e, bias, tau, st, sc)
    nc_opt, n_eff, mse_opt = ref.optimal_nc(n, n_e, bias, tau, st, sc)
    sd_bias = np.sqrt(bias**2 + tau**2)
    nc_eq6 = max(0.0, ref.pocock_eq6(n, n_e, sd_bias, sc, st)) if (bias > 0 or tau > 0 or not np.isinf(n_e)) else 0.0
    rows.append([n, n_c, n_e, bias, tau, st, sc, w, mse, nc_eq6, nc_opt, n_eff, mse_opt])
write("grid_hybrid.csv", ["n", "n_c", "n_e", "bias", "tau", "sigma_t", "sigma_c", "weight", "mse",
                          "n_c_opt_eq6", "n_c_opt_numeric", "n_eff", "mse_opt"], rows)

# ---- exact single-arm binomial test ----
rows = []
for n, p0, margin, alpha in itertools.product([15, 40, 120], [0.2, 0.5], [0.0, 0.1], [0.025, 0.1]):
    for p_t in [0.1, 0.4, 0.75]:
        crit, size, prob = ref.oc_sat_binary_exact(n, p_t, p0, margin, alpha)
        rows.append([n, p_t, p0, margin, alpha, crit, size, float(prob)])
write("grid_sat_binary.csv", ["n", "p_t", "p0", "margin", "alpha", "crit", "size", "prob_reject"],
      rows)

# ---- exact two-group binary comparisons ----
rows = []
z_cache = {}


def rejection_matrix(n_t, n_c, margin, alpha, test, n_ext, y_ext):
    key = (n_t, n_c, margin, alpha, test, n_ext, y_ext)
    if key not in z_cache:
        z_crit = stats.norm.ppf(1 - alpha)
        rej = np.zeros((n_t + 1, n_c + 1))
        for yt in range(n_t + 1):
            for yc in range(n_c + 1):
                if test == "score":
                    z = ref.z_score_fm(yt, n_t, yc + y_ext, n_c + n_ext, margin)
                else:
                    z = ref.z_pearson(yt, n_t, yc + y_ext, n_c + n_ext, test == "pearson_cc")
                rej[yt, yc] = 1.0 if (np.isfinite(z) and z > z_crit) else 0.0
        z_cache[key] = rej
    return z_cache[key]


for (n_t, n_c), margin, test, (n_ext, y_ext), alpha in itertools.product(
        [(10, 10), (12, 25)], [0.0, 0.1, -0.1], ["score", "pearson", "pearson_cc"],
        [(0, 0), (20, 7)], [0.025, 0.1]):
    if test != "score" and margin != 0:
        continue
    rej = rejection_matrix(n_t, n_c, margin, alpha, test, n_ext, y_ext)
    for p_t, p_c in [(0.3, 0.3), (0.6, 0.3), (0.45, 0.2)]:
        prob = float(stats.binom.pmf(np.arange(n_t + 1), n_t, p_t) @ rej @
                     stats.binom.pmf(np.arange(n_c + 1), n_c, p_c))
        rows.append([n_t, n_c, p_t, p_c, margin, alpha, test, n_ext, y_ext, prob])
write("grid_binary.csv", ["n_t", "n_c", "p_t", "p_c", "margin", "alpha", "test", "n_ext", "y_ext",
                          "prob_reject"], rows)
