"""Independent Python reference implementation of the satrct calculations.

This module is written separately from the R code of the package and is used to
compute the expected values of the unit tests (make_test_values.py) and, in
Phase 3, the reference values of inst/validation. It uses numpy and scipy only.

Conventions (same as the package): one-sided tests, larger effect is better,
bias b = E[estimate of the single-arm design] - delta.
"""
import numpy as np
from scipy import optimize, stats


# ---------------------------------------------------------------------------
# Normal approximation
# ---------------------------------------------------------------------------

def var_rct(n, sigma_t=1.0, sigma_c=None, r=0.5):
    sigma_c = sigma_t if sigma_c is None else sigma_c
    return sigma_t**2 / (r * n) + sigma_c**2 / ((1.0 - r) * n)


def var_sat(n, n_e=np.inf, sigma_t=1.0, sigma_c=None):
    sigma_c = sigma_t if sigma_c is None else sigma_c
    return sigma_t**2 / n + (0.0 if np.isinf(n_e) else sigma_c**2 / n_e)


def mse_rct(n, sigma_t=1.0, sigma_c=None, r=0.5):
    return var_rct(n, sigma_t, sigma_c, r)


def mse_sat(n, n_e=np.inf, bias=0.0, tau=0.0, sigma_t=1.0, sigma_c=None):
    return var_sat(n, n_e, sigma_t, sigma_c) + bias**2 + tau**2


def breakeven_bias(n, n_e=np.inf, sigma_t=1.0, sigma_c=None, r=0.5):
    d = var_rct(n, sigma_t, sigma_c, r) - var_sat(n, n_e, sigma_t, sigma_c)
    return np.sqrt(d) if d >= 0 else np.nan


def breakeven_n(bias, n_e=np.inf, tau=0.0, sigma_t=1.0, sigma_c=None, r=0.5):
    # Solve var_rct(n) = var_sat(n) + bias^2 + tau^2 numerically (independent of the closed form)
    sigma_c = sigma_t if sigma_c is None else sigma_c

    def f(n):
        return var_rct(n, sigma_t, sigma_c, r) - mse_sat(n, n_e, bias, tau, sigma_t, sigma_c)

    if bias == 0 and tau == 0 and np.isinf(n_e):
        return np.inf
    return optimize.brentq(f, 1e-8, 1e12, xtol=1e-14, rtol=1e-14, maxiter=1000)


def power_rct(n, delta, sigma_t=1.0, sigma_c=None, alpha=0.025, r=0.5, method="z"):
    sigma_c = sigma_t if sigma_c is None else sigma_c
    se = np.sqrt(var_rct(n, sigma_t, sigma_c, r))
    if method == "z":
        return stats.norm.cdf(delta / se - stats.norm.ppf(1 - alpha))
    df = n - 2
    crit = stats.t.ppf(1 - alpha, df)
    return stats.nct.sf(crit, df, delta / se)


def power_sat(n, delta, n_e=np.inf, bias=0.0, margin=0.0, tau=0.0, sigma_t=1.0,
              sigma_c=None, alpha=0.025, method="z"):
    sigma_c = sigma_t if sigma_c is None else sigma_c
    v = var_sat(n, n_e, sigma_t, sigma_c)
    if method == "z":
        # P(delta_hat > margin + z * sqrt(v)), delta_hat ~ N(delta + bias, v + tau^2)
        return stats.norm.sf(margin + stats.norm.ppf(1 - alpha) * np.sqrt(v),
                             loc=delta + bias, scale=np.sqrt(v + tau**2))
    df = n - 1 if np.isinf(n_e) else n + n_e - 2
    crit = stats.t.ppf(1 - alpha, df)
    return stats.nct.sf(crit, df, (delta + bias - margin) / np.sqrt(v))


def critical_margin(n, delta, n_e=np.inf, sigma_t=1.0, sigma_c=None, r=0.5):
    # Solve power_sat(margin, bias = 0) = power_rct numerically on the probit scale
    target = stats.norm.ppf(power_rct(n, delta, sigma_t, sigma_c, 0.025, r))
    f = lambda m: stats.norm.ppf(power_sat(n, delta, n_e, 0.0, m, 0.0, sigma_t, sigma_c, 0.025)) - target
    return optimize.brentq(f, delta - 2, delta + 2, xtol=1e-14, rtol=1e-14, maxiter=1000)


def matched_alpha(n, delta, n_e=np.inf, margin=0.0, sigma_t=1.0, sigma_c=None, alpha=0.025, r=0.5):
    pw = power_sat(n, delta, n_e, 0.0, margin, 0.0, sigma_t, sigma_c, alpha)
    # alpha' such that power_rct(alpha') = pw, found numerically
    g = lambda a: power_rct(n, delta, sigma_t, sigma_c, a, r) - pw
    a_m = optimize.brentq(g, 1e-15, 1 - 1e-15, xtol=1e-16, rtol=1e-14, maxiter=1000)
    # bias at which the type I error of the single-arm design equals alpha'
    h = lambda b: power_sat(n, 0.0, n_e, b, margin, 0.0, sigma_t, sigma_c, alpha) - a_m
    b_x = optimize.brentq(h, -1e3, 1e3, xtol=1e-14, rtol=1e-14, maxiter=1000)
    return a_m, pw, b_x


def n_rct(delta, power=0.8, sigma_t=1.0, sigma_c=None, alpha=0.025, r=0.5, method="z"):
    f = lambda n: power_rct(n, delta, sigma_t, sigma_c, alpha, r, method) - power
    lo = 2 + 1e-9 if method == "t" else 1e-9
    return optimize.brentq(f, lo, 1e7, xtol=1e-12, rtol=1e-14, maxiter=2000)


def n_sat(delta, power=0.8, n_e=np.inf, bias=0.0, margin=0.0, sigma_t=1.0, sigma_c=None,
          alpha=0.025, method="z"):
    sigma_c = sigma_t if sigma_c is None else sigma_c
    d = delta + bias - margin
    lim_se = 0.0 if np.isinf(n_e) else sigma_c / np.sqrt(n_e)
    if d <= 0:
        return np.inf
    if lim_se > 0 and stats.norm.cdf(d / lim_se - stats.norm.ppf(1 - alpha)) <= power:
        return np.inf
    f = lambda n: power_sat(n, delta, n_e, bias, margin, 0.0, sigma_t, sigma_c, alpha, method) - power
    if method == "t":
        lo = 1 + 1e-9 if np.isinf(n_e) else max(1e-9, 2 - n_e + 1e-9)
    else:
        lo = 1e-9
    return optimize.brentq(f, lo, 1e9, xtol=1e-12, rtol=1e-14, maxiter=2000)


# ---------------------------------------------------------------------------
# Hybrid design (Pocock 1976)
# ---------------------------------------------------------------------------

def mse_hybrid(n, n_c, n_e, bias=0.0, tau=0.0, sigma_t=1.0, sigma_c=None, weight=None):
    sigma_c = sigma_t if sigma_c is None else sigma_c
    a = (0.0 if np.isinf(n_e) else sigma_c**2 / n_e) + bias**2 + tau**2

    def mse_w(w):
        term_c = 0.0 if n_c == 0 else w**2 * sigma_c**2 / n_c
        return sigma_t**2 / (n - n_c) + term_c + (1 - w)**2 * a

    if weight is None:
        if n_c == 0:
            w = 0.0
        else:
            # numerical minimisation over w in [0, 1] (independent of the closed form)
            res = optimize.minimize_scalar(mse_w, bounds=(0.0, 1.0), method="bounded",
                                           options={"xatol": 1e-14})
            w = res.x
    else:
        w = weight
    return w, mse_w(w)


def optimal_nc(n, n_e, bias=0.0, tau=0.0, sigma_t=1.0, sigma_c=None):
    # numerical minimisation over n_c of the hybrid MSE with the optimal weight
    sigma_c = sigma_t if sigma_c is None else sigma_c
    f = lambda nc: mse_hybrid(n, nc, n_e, bias, tau, sigma_t, sigma_c)[1] if nc > 0 else \
        mse_sat(n, n_e, bias, tau, sigma_t, sigma_c)
    res = optimize.minimize_scalar(f, bounds=(0.0, n * 0.999), method="bounded",
                                   options={"xatol": 1e-10})
    nc = res.x
    if f(0.0) <= f(nc):
        nc = 0.0
    a = (0.0 if np.isinf(n_e) else sigma_c**2 / n_e) + bias**2 + tau**2
    n_eff = np.inf if a == 0 else sigma_c**2 / a
    return nc, n_eff, f(nc)


def pocock_eq7(n, n_h, sd_delta, sigma):
    """Pocock (1976) eq. (7), printed formula."""
    return 0.5 * (n - n_h / (1 + sd_delta**2 * n_h / sigma**2))


def pocock_eq6(n, n_h, sd_delta, sigma_r, sigma_t, sigma_h=None):
    """Pocock (1976) eq. (6), printed formula (sigma_H defaults to sigma_R)."""
    sigma_h = sigma_r if sigma_h is None else sigma_h
    return sigma_r / (sigma_r + sigma_t) * (n - sigma_r * sigma_t / (sigma_h**2 / n_h + sd_delta**2))


# ---------------------------------------------------------------------------
# Exact binary computations
# ---------------------------------------------------------------------------

def oc_sat_binary_exact(n, p_t, p0, margin=0.0, alpha=0.025, crit=None):
    p_null = p0 + margin
    y = np.arange(n + 2)  # candidate critical values 0..n+1
    tail = np.array([stats.binom.sf(c - 1, n, p_null) for c in y])
    if crit is None:
        crit = int(y[np.where(tail <= alpha + 1e-12)[0][0]])
    size = stats.binom.sf(crit - 1, n, p_null)
    prob = stats.binom.sf(crit - 1, n, np.asarray(p_t))
    return crit, size, prob


def _fm_restricted_mle_numeric(y1, n1, y2, n2, d0):
    """Restricted MLE of (p1, p2) under p1 - p2 = d0 by direct numerical maximisation."""
    lo = max(0.0, d0) + 1e-13
    hi = min(1.0, 1.0 + d0) - 1e-13

    def negll(p1):
        p2 = p1 - d0
        ll = 0.0
        for y, n, p in ((y1, n1, p1), (y2, n2, p2)):
            if y > 0:
                ll += y * np.log(p)
            if n - y > 0:
                ll += (n - y) * np.log1p(-p)
        return -ll

    grid = np.linspace(lo, hi, 2001)
    vals = np.array([negll(g) for g in grid])
    k = int(np.argmin(vals))
    a = grid[max(k - 1, 0)]
    b = grid[min(k + 1, len(grid) - 1)]
    res = optimize.minimize_scalar(negll, bounds=(a, b), method="bounded", options={"xatol": 1e-15})
    p1 = res.x
    return p1, p1 - d0


def z_score_fm(y1, n1, y2, n2, d0):
    p1h, p2h = y1 / n1, y2 / n2
    p1, p2 = _fm_restricted_mle_numeric(y1, n1, y2, n2, d0)
    v = p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2
    if v <= 0:
        return np.nan
    return (p1h - p2h - d0) / np.sqrt(v)


def z_pearson(y1, n1, y2, n2, correct=False):
    """One-sided z statistic of stats::prop.test(c(y1, y2), c(n1, n2)) written from its definition."""
    x = np.array([[y1, n1 - y1], [y2, n2 - y2]], dtype=float)
    nn = np.array([n1, n2], dtype=float)
    p = (y1 + y2) / (n1 + n2)
    e = np.column_stack([nn * p, nn * (1 - p)])
    if np.any(e == 0):
        return np.nan
    delta = y1 / n1 - y2 / n2
    yates = min(0.5, abs(delta) / (1 / n1 + 1 / n2)) if correct else 0.0
    stat = np.sum((np.abs(x - e) - yates)**2 / e)
    return np.sign(delta) * np.sqrt(stat)


def oc_binary_exact(n_t, n_c, p_t, p_c, margin=0.0, alpha=0.025, test="score", n_ext=0, y_ext=0):
    z_crit = stats.norm.ppf(1 - alpha)
    rej = np.zeros((n_t + 1, n_c + 1))
    for yt in range(n_t + 1):
        for yc in range(n_c + 1):
            yc2, nc2 = yc + y_ext, n_c + n_ext
            if test == "score":
                z = z_score_fm(yt, n_t, yc2, nc2, margin)
            elif test == "pearson":
                z = z_pearson(yt, n_t, yc2, nc2, False)
            else:
                z = z_pearson(yt, n_t, yc2, nc2, True)
            rej[yt, yc] = 1.0 if (np.isfinite(z) and z > z_crit) else 0.0
    pt_ = stats.binom.pmf(np.arange(n_t + 1), n_t, p_t)
    pc_ = stats.binom.pmf(np.arange(n_c + 1), n_c, p_c)
    return float(pt_ @ rej @ pc_)


def endpoint_sd(kind, sigma=None, p=None, prob_event=None):
    if kind == "continuous":
        return sigma
    if kind == "binary":
        return np.sqrt(p * (1 - p))
    return 1 / np.sqrt(prob_event)
