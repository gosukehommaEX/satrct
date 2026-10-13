"""Compute the expected values written into tests/testthat with the independent
Python implementation in satrct_reference.py.

Run from this folder:  python3 make_test_values.py > test_values.txt
"""
import numpy as np

import satrct_reference as ref

INF = np.inf


def show(label, value):
    if isinstance(value, (tuple, list)):
        print(label, "=", ", ".join(repr(float(v)) for v in value))
    else:
        print(label, "=", repr(float(value)))


print("# mse_rct")
show("mse_rct(40)", ref.mse_rct(40))
show("mse_rct(30, 1.2, 0.8, 2/3)", ref.mse_rct(30, 1.2, 0.8, 2 / 3))

print("# mse_sat")
show("mse_sat(40, 100, 0.1, 0.05)", ref.mse_sat(40, 100, 0.1, 0.05))
show("mse_sat(30, Inf, -0.2, 0, 1.2, 0.8)", ref.mse_sat(30, INF, -0.2, 0, 1.2, 0.8))

print("# breakeven_bias")
show("breakeven_bias(40, 100)", ref.breakeven_bias(40, 100))
show("breakeven_bias(40, Inf)", ref.breakeven_bias(40, INF))
show("breakeven_bias(40, 10)", ref.breakeven_bias(40, 10))
show("breakeven_bias(30, 60, 1.2, 0.8, 2/3)", ref.breakeven_bias(30, 60, 1.2, 0.8, 2 / 3))
show("breakeven_bias(20, 200, 2)", ref.breakeven_bias(20, 200, 2.0))

print("# breakeven_n")
show("breakeven_n(0.2, 100)", ref.breakeven_n(0.2, 100))
show("breakeven_n(0, 1000, sqrt(0.022))  [Collignon 2020 example]", ref.breakeven_n(0.0, 1000, np.sqrt(0.022)))
show("breakeven_n(0.3, Inf, 0.1, 1.2, 0.8, 2/3)", ref.breakeven_n(0.3, INF, 0.1, 1.2, 0.8, 2 / 3))

print("# power_rct")
show("power_rct(40, 0.5)", ref.power_rct(40, 0.5))
show("power_rct(30, 0.8, 1.2, 0.8, 0.05, 2/3)", ref.power_rct(30, 0.8, 1.2, 0.8, 0.05, 2 / 3))
show("power_rct(40, 0.5, method=t)", ref.power_rct(40, 0.5, method="t"))
show("power_rct(30, 1.5, 2, alpha=0.05, r=2/3, method=t)", ref.power_rct(30, 1.5, 2.0, 2.0, 0.05, 2 / 3, "t"))
show("power_rct(30, 1, 1)  [Kopp-Schneider 2024 two-arm, 15 per group]", ref.power_rct(30, 1.0))

print("# power_sat")
show("power_sat(40, 0.5, 120, 0.1, 0.1)", ref.power_sat(40, 0.5, 120, 0.1, 0.1))
show("power_sat(40, 0, 120, 0.15, 0.1)", ref.power_sat(40, 0.0, 120, 0.15, 0.1))
show("power_sat(40, 0.5, 120, 0, 0.1, tau=0.1)", ref.power_sat(40, 0.5, 120, 0.0, 0.1, 0.1))
show("power_sat(30, 0.8, Inf, 0, 0, 0, 1.2, 0.8, 0.05)", ref.power_sat(30, 0.8, INF, 0, 0, 0, 1.2, 0.8, 0.05))
show("power_sat(40, 0.5, 120, 0.1, 0.1, method=t)", ref.power_sat(40, 0.5, 120, 0.1, 0.1, method="t"))
show("power_sat(25, 0.5, Inf, method=t)", ref.power_sat(25, 0.5, INF, method="t"))
show("power_sat(25, 0.5)  [Kopp-Schneider 2024 one-arm]", ref.power_sat(25, 0.5))
show("power_sat(25, 0.5, alpha=0.017)  [KS2024 calibrated]", ref.power_sat(25, 0.5, alpha=0.017))
show("power_sat(40, 0, 120, 0.05, 0.1, tau=0.1)", ref.power_sat(40, 0.0, 120, 0.05, 0.1, 0.1))
show("power_sat(40, 0.5, 120, 0.05, 0.1, tau=0.1)", ref.power_sat(40, 0.5, 120, 0.05, 0.1, 0.1))

print("# critical_margin")
show("critical_margin(40, 0.8, 100)", ref.critical_margin(40, 0.8, 100))
show("critical_margin(40, 0.8, Inf)", ref.critical_margin(40, 0.8, INF))
show("critical_margin(30, 0.6, 50, 1.2, 0.8, 2/3)", ref.critical_margin(30, 0.6, 50, 1.2, 0.8, 2 / 3))
show("critical_margin(40, 0.8, 10)", ref.critical_margin(40, 0.8, 10))

print("# matched_alpha  (alpha_matched, power, bias_crossing)")
show("matched_alpha(40, 0.8, 100, 0.1)", ref.matched_alpha(40, 0.8, 100, 0.1))
show("matched_alpha(60, 0.6, 200, 0.0)", ref.matched_alpha(60, 0.6, 200, 0.0))
show("matched_alpha(30, 0.6, 50, 0.05, 1.2, 0.8, 0.05, 2/3)", ref.matched_alpha(30, 0.6, 50, 0.05, 1.2, 0.8, 0.05, 2 / 3))

print("# n_rct")
show("n_rct(0.5)", ref.n_rct(0.5))
show("n_rct(0.8, 0.9, 1.2, 0.8, 0.05, 2/3)", ref.n_rct(0.8, 0.9, 1.2, 0.8, 0.05, 2 / 3))
show("n_rct(0.5, method=t)", ref.n_rct(0.5, method="t"))

print("# n_sat")
show("n_sat(0.5)", ref.n_sat(0.5))
show("n_sat(0.5, 0.8, 100)", ref.n_sat(0.5, 0.8, 100))
show("n_sat(0.5, 0.8, 100, 0.05, 0.1)", ref.n_sat(0.5, 0.8, 100, 0.05, 0.1))
show("n_sat(0.5, 0.8, 20)", ref.n_sat(0.5, 0.8, 20))
show("n_sat(0.5, method=t)", ref.n_sat(0.5, method="t"))
show("n_sat(0.5, 0.8, 100, method=t)", ref.n_sat(0.5, 0.8, 100, method="t"))

print("# mse_hybrid  (weight, mse)")
show("mse_hybrid(100, 20, 200, 0.1)", ref.mse_hybrid(100, 20, 200, 0.1))
show("mse_hybrid(100, 20, 200, 0.1, weight=0.5)", ref.mse_hybrid(100, 20, 200, 0.1, weight=0.5))
show("mse_hybrid(60, 15, 100, 0.05, 0.1, 1.2, 0.8)", ref.mse_hybrid(60, 15, 100, 0.05, 0.1, 1.2, 0.8))

print("# optimal_nc  (n_c, n_eff, mse)")
s_tr = np.sqrt(45 * 55)
show("optimal_nc(200, 83, 0, 3, sqrt(45*55))  [Pocock 68.1]", ref.optimal_nc(200, 83, 0, 3, s_tr))
show("optimal_nc(200, 83, 0, 5, sqrt(45*55))  [Pocock 77]", ref.optimal_nc(200, 83, 0, 5, s_tr))
show("optimal_nc(200, 88, 0, 0.05, 1)  [Pocock 63.9]", ref.optimal_nc(200, 88, 0, 0.05, 1.0))
show("optimal_nc(200, 88, 0, 0.1, 1)  [Pocock 77]", ref.optimal_nc(200, 88, 0, 0.1, 1.0))
show("pocock_eq7 for the four cases", [ref.pocock_eq7(200, 83, 3, s_tr), ref.pocock_eq7(200, 83, 5, s_tr),
                                       ref.pocock_eq7(200, 88, 0.05, 1), ref.pocock_eq7(200, 88, 0.1, 1)])
show("optimal_nc(60, 100, 0.05, 0.1, 1.2, 0.8)", ref.optimal_nc(60, 100, 0.05, 0.1, 1.2, 0.8))
show("pocock_eq6(60, 100, sqrt(0.05^2 + 0.1^2), sigma_r=0.8, sigma_t=1.2)",
     ref.pocock_eq6(60, 100, np.sqrt(0.05**2 + 0.1**2), 0.8, 1.2))
show("mse_hybrid at pocock_eq6 n_c (weight, mse)",
     ref.mse_hybrid(60, ref.pocock_eq6(60, 100, np.sqrt(0.05**2 + 0.1**2), 0.8, 1.2), 100, 0.05, 0.1, 1.2, 0.8))
show("optimal_nc(50, 200, 0.1)  [SAT optimal]", ref.optimal_nc(50, 200, 0.1))

print("# oc_sat_binary_exact  (crit, size, prob)")
show("Viele: n=200, p0=0.65, p_t=0.70", ref.oc_sat_binary_exact(200, 0.70, 0.65))
show("Viele: n=200, p0=0.65, margin=0.05, p_t=c(0.70, 0.75)",
     [*ref.oc_sat_binary_exact(200, 0.70, 0.65, 0.05)[:2], *ref.oc_sat_binary_exact(200, [0.70, 0.75], 0.65, 0.05)[2]])
show("KS2020: n=40, p0=0.2, alpha=0.05, p_t=0.3", ref.oc_sat_binary_exact(40, 0.3, 0.2, 0.0, 0.05))
show("Tang: n=50, crit=19, p0=0.30, p_t=0.45", ref.oc_sat_binary_exact(50, 0.45, 0.30, crit=19))
show("Tang: n=50, crit=46, p0=0.85, p_t=0.95", ref.oc_sat_binary_exact(50, 0.95, 0.85, crit=46))

print("# oc_binary_exact")
show("score: n_t=20, n_c=20, p_t=0.5, p_c=0.3, margin=0", ref.oc_binary_exact(20, 20, 0.5, 0.3, 0.0, 0.025, "score"))
show("pearson: n_t=20, n_c=20, p_t=0.5, p_c=0.3", ref.oc_binary_exact(20, 20, 0.5, 0.3, 0.0, 0.025, "pearson"))
show("pearson_cc: n_t=20, n_c=20, p_t=0.5, p_c=0.3", ref.oc_binary_exact(20, 20, 0.5, 0.3, 0.0, 0.025, "pearson_cc"))
show("score: n_t=30, n_c=60, p_t=0.4, p_c=0.3, margin=0.1", ref.oc_binary_exact(30, 60, 0.4, 0.3, 0.1, 0.025, "score"))
show("score: n_t=30, n_c=60, p_t=0.6, p_c=0.3, margin=0.1", ref.oc_binary_exact(30, 60, 0.6, 0.3, 0.1, 0.025, "score"))
show("score: n_t=25, n_c=40, p_t=0.35, p_c=0.4, margin=-0.05, alpha=0.05",
     ref.oc_binary_exact(25, 40, 0.35, 0.40, -0.05, 0.05, "score"))
show("Viele pooled, pearson_cc: 200/200 + 65/100, p=0.80", ref.oc_binary_exact(200, 200, 0.80, 0.80, 0.0, 0.025, "pearson_cc", 100, 65))
show("Viele separate, pearson_cc: 200/200, p_c=0.65, p_t=0.77", ref.oc_binary_exact(200, 200, 0.77, 0.65, 0.0, 0.025, "pearson_cc"))

print("# endpoint_sd")
show("endpoint_sd(binary, p=0.3)", ref.endpoint_sd("binary", p=0.3))
show("endpoint_sd(survival, prob_event=0.64)", ref.endpoint_sd("survival", prob_event=0.64))
