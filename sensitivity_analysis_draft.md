# Sensitivity Analysis: Code Review, Interpretation, and Draft Text

Prepared for the Green Central Banks project. This document has five parts: (1) a plain-language guide to what the HonestDiD analysis does; (2) how to read your three plots and the breakdown output; (3) a line-by-line code review verified against the HonestDiD package source on GitHub; (4) draft text for `manuscriptv1.qmd`; (5) draft text for `manuscript_sup_materialsv1.qmd`, plus optional analyses that would strengthen the section.

---

## Part 1. What the sensitivity analysis does, in plain language

Your Callaway & Sant'Anna estimates rest on one untestable promise: **if NGFS members had never joined, their emissions would have moved in parallel with never-treated countries.** That is the parallel trends assumption. You can check whether the two groups moved in parallel *before* treatment (your pre-trend estimates), but you can never check it *after* treatment, because the untreated version of a treated country no longer exists. In Stata terms: this is the assumption behind every `didregress` or `xtdidregress` you have ever run, and it is always taken on faith for the post-period.

Rambachan and Roth (2023) reframe the question. Instead of asking "does parallel trends hold?" (unanswerable), they ask: **"suppose parallel trends fails by some bounded amount — is my conclusion still standing?"** Their method recomputes the 95% confidence interval under progressively worse hypothetical violations. Each version of "how wrong could it be" is controlled by a dial:

- **Relative magnitudes restriction ($\bar{M}$, "Mbar").** The dial compares the future to the past: post-treatment violations of parallel trends are allowed to be up to $\bar{M}$ times the *largest* violation observed anywhere in the pre-treatment period. $\bar{M} = 0$ means parallel trends holds exactly after treatment. $\bar{M} = 1$ means the post-period may misbehave as badly as the worst pre-period year. $\bar{M} = 2$ means twice as badly.
- **Smoothness restriction ($M$).** The dial controls curvature instead: the treated group is allowed to be on a *different trend* than the controls, but that differential trend cannot bend by more than $M$ (in log points) from one year to the next. $M = 0$ allows a perfectly straight differential trend (a linear drift); larger $M$ allows the drift to accelerate.

As the dial increases, the robust confidence interval widens, because you are admitting more ways the estimate could be confounded. The **breakdown value** is the dial setting at which the interval first touches zero — i.e., the largest violation your conclusion can absorb before it is no longer statistically distinguishable from no effect. A large breakdown value means a sturdy result; a breakdown near zero means the result leans heavily on parallel trends being close to exact.

(For your toolkit: the same method exists in Stata as the `honestdid` module by Cáceres Bravo, which wraps the identical R code.)

## Part 2. Reading your output

### The two printed sentences

> `Overall average ATT survives relative-magnitude violations up to Mbar = 0.1284 ...`

Your headline CS estimate — the average effect across post-treatment years 0 through 6, $-0.088$ (≈ 8.8% lower emissions) — stays statistically significant as long as post-treatment parallel-trends violations are no larger than **about 13% of the single largest pre-treatment deviation**. Beyond that, the robust CI includes zero.

> `ATT(0) survives relative-magnitude violations up to Mbar = 0 ...`

This sentence is technically produced by the code but is **misleading as written**. It does not mean ATT(0) breaks down at $\bar{M}=0$; it means ATT(0) — the effect in the year of joining, roughly $-0.008$ — **was never statistically significant in the first place** (its original 95% CI already includes zero, visible as the blue bar crossing the zero line in your plots). There is nothing for the sensitivity analysis to "break." This is not bad news: your own event study says the effect takes about two years to appear, so a null on-impact effect is exactly what your theory predicts. Part 3 suggests a code fix so the printed message distinguishes these cases.

### The three plots

**`honest_did_relmag_avg.png` (overall ATT, relative magnitudes) — the plot that matters most.** The blue bar is the ordinary CS confidence interval (approx. $[-0.13, -0.05]$): entirely below zero, so significant under exact parallel trends. The red bars show the robust CI as $\bar{M}$ increases in steps of 0.5. Already at $\bar{M} = 0.5$ the interval (approx. $[-0.38, +0.20]$) includes zero, and it keeps widening. The exact crossing point is the $\bar{M} = 0.128$ from the printed output. Note the red bar at $\bar{M}=0$ nearly coincides with the blue bar — that is the sanity check that the machinery reproduces the original inference when no violation is allowed.

**`honest_did_relmag.png` (ATT(0), relative magnitudes).** Every bar, including the blue original, crosses zero. The plot documents that the on-impact effect is a precisely-estimated near-zero, consistent with delayed policy transmission — not that the sensitivity analysis "fails."

**`honest_did_smooth.png` (ATT(0), smoothness).** Same story under the smoothness dial: all bars cross zero because the target itself is null. One subtlety worth knowing: at $M = 0$ the red bar is *shifted upward* relative to the blue bar rather than identical to it. That is expected — $M = 0$ under the smoothness restriction is not "no violation"; it allows a linear differential trend fitted to your pre-period and extrapolated forward, which shifts the identified effect. The two restrictions answer different questions and $\bar{M}=0$, not $M=0$, corresponds to exact parallel trends.

### So is the result a fluke, or is it bulletproof?

Neither — and that is the honest, defensible position:

- **Not a fluke:** under exact parallel trends the overall effect is clearly significant, its direction and rough magnitude agree across TWFE, CS with two comparison groups, and Sun–Abraham, and the dynamic profile (null on impact, growing through year 6) matches the institutional mechanism rather than a spurious level shift.
- **Not bulletproof:** a breakdown at $\bar{M} \approx 0.13$ is well below the $\bar{M} = 1$ benchmark that Rambachan and Roth treat as the natural reference point. Statistical significance depends on post-treatment violations being small relative to the worst pre-treatment deviation.
- **The mitigating context you can legitimately invoke:** the relative-magnitudes benchmark is the *maximum* deviation over a pre-period of up to ten event-time years. Your own manuscript notes that the large pre-treatment deviations are concentrated in 2013–2015 and at distant event times, which are estimated from fewer cohorts and are least informative about the counterfactual near treatment; deviations in the two to three years immediately before joining are small and insignificant. Benchmarking against the single worst distant year is therefore a demanding stress test, and 13% of a large number can still be a nontrivial absolute violation. Say this — but do not lean on it so hard that it reads as explaining the number away.

## Part 3. Code review (verified against HonestDiD source, GitHub master / v0.2.8)

**Bottom line: the chunk is doing what it claims.** I checked every construction against `R/honest_did.R` and `R/sensitivityresults.R` in the package repository. Specific confirmations:

1. `HonestDiD:::honest_did.AGGTEobj` is the package's own vignette-recommended method; pulling it via `:::` is correct given the missing S3 registration.
2. Your covariance for the average-ATT target, `crossprod(inf_keep) / n_avg^2`, matches the package's `V <- t(es_inf_func) %*% es_inf_func / n / n` exactly, and dropping the reference column before the crossproduct is algebraically identical to the package's dropping of the reference row/column afterward.
3. Pre/post counting is right: with $e = -1$ removed, `sum(egt_k < 0)` equals the package's `sum(egt < -1)`; `l_vec = rep(1/7, 7)` targets the equally-weighted average of ATT(0)–ATT(6), which is what `aggte(type = "dynamic")` reports as `overall.att` — and you verified the reproduction numerically.
4. The grid-widening fix is legitimate: `grid.lb`/`grid.ub` pass straight through `createSensitivityResults_relativeMagnitudes()` into `computeConditionalCS_DeltaRM()`, replacing the ±20-SE default that was clipping. Your plots show no flat-topped bars at large $\bar{M}$, and the widest interval (≈ ±1.25 at $\bar{M}=2$ for the overall ATT) sits comfortably inside your ±(50 × half-width) bound. The `method = "Conditional"` choice for smoothness and default `C-LF` for relative magnitudes both match the legends in your saved plots.
5. The root-finding for the exact breakdown is sound, including the sign flip for negative estimates (tracking `ub` instead of `lb`), the bracketing between adjacent grid points, and the guard conditions.

**Issues to fix or note (none invalidates the results):**

1. **The misleading ATT(0) message (the one real problem).** `find_breakdown_mbar()` returns `mbar_lo` (= 0) when the CI already includes zero at the bottom of the bracket, so the printout says ATT(0) "survives up to Mbar = 0" when the truthful statement is "not significant even under exact parallel trends." Replace the two `cat(sprintf(...))` calls with a reporter that distinguishes the three cases:

    ```r
    report_breakdown <- function(label, exact, orig_ci, mbar_max) {
      orig_sig <- (orig_ci$lb > 0) | (orig_ci$ub < 0)
      if (!orig_sig) {
        cat(sprintf(
          "%s is not statistically significant even under exact parallel trends (original 95%% CI includes zero); the breakdown value is not informative for this target.\n",
          label))
      } else if (is.na(exact)) {
        cat(sprintf(
          "%s survives relative-magnitude violations beyond the largest Mbar tested (%s).\n",
          label, mbar_max))
      } else {
        cat(sprintf(
          "%s survives relative-magnitude violations up to Mbar = %s before the robust 95%% CI includes zero.\n",
          label, round(exact, 4)))
      }
    }

    report_breakdown("ATT(0)", breakdown_relmag_exact, hd_relmag$orig_ci, 2)
    report_breakdown("Overall average ATT", breakdown_relmag_avg_exact, hd_relmag_avg$orig_ci, 2)
    ```

2. **Fragile reference-period exclusion.** `keep <- which(!is.na(cs_dynamic$se.egt))` happens to drop only $e = -1$ today, but if any other event time ever returned an NA standard error (e.g., after a data revision leaves an event time with a single cohort), the pre/post counts and `l_vec` would silently misalign. Mirror the package and make the intent explicit:

    ```r
    keep <- which(cs_dynamic$egt != -1)
    stopifnot(sum(is.na(cs_dynamic$se.egt[keep])) == 0)
    ```

3. **Your global `warning: false` hides exactly the warnings your comments discuss.** The YAML execute options suppress HonestDiD's "CI is open at one of the endpoints" and CVXR's "solution may be inaccurate" — the two diagnostics your grid-widening and Conditional-method choices were designed to address. Add `#| warning: true` to this one chunk so a future data update that re-triggers clipping is visible rather than silently swallowed.

4. **The smoothness call does not widen its grid.** `hd_smooth` uses the default grid bounds. Your plot shows no clipping (the bars keep growing through $M = 0.15$), so it is fine today, but with `warning: true` (fix 3) you would be alerted if that changes.

5. **Two trivial comment corrections.** The `honest_did()` wrapper's default `gridPoints` is 100, not 1000 (1000 is the default of the underlying `createSensitivityResults_relativeMagnitudes()`); and the default `Mbarvec` is `seq(0, 2, length.out = 10)` (hence the ~0.22 spacing you noted).

6. **Precision caveat, not a bug.** `uniroot(tol = 1e-4)` finds the crossing on a CI that is itself computed on a θ-grid with step ≈ `2 * grid_bound / 2000` ≈ 0.0007 log points, so the fourth decimal of 0.1284 is not meaningful. Report $\bar{M} \approx 0.13$ in text; the supplement can show 0.128.

7. **Plot caption error.** All three captions say "Shaded region: robust 95% confidence interval," but `createSensitivityPlot*()` draws error bars, not a shaded ribbon. Change to "Error bars: robust 95% confidence intervals." Also consider relabeling the legend for reviewers: "C-LF"/"Conditional" → "Robust 95% CI" and "Original" → "Original 95% CI" (e.g., via `scale_color_manual(labels = ...)` — but note the package hardcodes colors, so relabel only).

## Part 4. Draft text for `manuscriptv1.qmd`

Insert after the paragraph ending "...not a clean structural break from a perfectly flat baseline" (the paragraph discussing @fig-cs-event), since it flows directly from the parallel-trends acknowledgement:

> Because the pre-treatment estimates are not uniformly null, I assess how much the conclusions depend on the parallel trends assumption itself using the partial identification approach of Rambachan and Roth [-@rambachan2023]. This method recomputes confidence intervals that remain valid when post-treatment violations of parallel trends are permitted up to a chosen multiple ($\bar{M}$) of the largest violation observed in the pre-treatment period. The overall average post-treatment effect on total emissions ($-8.8\%$) remains statistically distinguishable from zero for violations up to $\bar{M} \approx 0.13$---that is, provided post-treatment confounding does not exceed roughly 13\% of the largest pre-treatment deviation, which occurs at distant event times estimated from few cohorts (see Supplementary Materials). The on-impact effect, ATT(0), is statistically indistinguishable from zero irrespective of the assumption, consistent with the two-to-three-year policy transmission lag visible in @fig-cs-event. The sensitivity analysis thus indicates that the estimated emissions reduction is not an artifact of the estimator, but its statistical significance does presuppose that post-treatment departures from parallel trends are modest relative to the worst pre-treatment deviation---an important qualification given the pre-2015 divergence among early joiners.

If you need a one-sentence version for space-constrained submission (Science's main text is tight):

> A formal sensitivity analysis [-@rambachan2023] shows the overall effect withstands modest violations of parallel trends ($\bar{M} \approx 0.13$) but not violations comparable in size to the largest pre-treatment deviation (Supplementary Materials, Figs. S_X–S_Y).

Remember to add the `rambachan2023` entry to `references.bib` (your existing callout already flags this):

```
@article{rambachan2023,
  author  = {Rambachan, Ashesh and Roth, Jonathan},
  title   = {A More Credible Approach to Parallel Trends},
  journal = {Review of Economic Studies},
  year    = {2023},
  volume  = {90},
  number  = {5},
  pages   = {2555--2591},
  doi     = {10.1093/restud/rdad018}
}
```

## Part 5. Draft text for `manuscript_sup_materialsv1.qmd`

Replace the current single paragraph under "### HonestDiD Sensitivity Analysis" with the following. Numbers marked `[CHECK]` should be filled from `hd_relmag_avg$robust_ci` / `hd_relmag$orig_ci` on your next render — I read them off the saved plots and they are approximate.

> #### Rationale
>
> The Callaway and Sant'Anna [-@callaway2021] estimates in the main text identify the effect of NGFS membership under the assumption that, absent treatment, members' emissions would have evolved in parallel with those of never-treated countries. Pre-treatment estimates provide indirect evidence on this assumption, and as reported in the main text, they are not uniformly null: deviations concentrated among early joiners in 2013--2015 reject exact parallel pre-trends, while the estimates in the two to three years immediately preceding treatment are small and statistically indistinguishable from zero. Rather than treating parallel trends as an all-or-nothing premise, I follow Rambachan and Roth [-@rambachan2023] and report confidence intervals that remain valid under bounded violations of the assumption, implemented with the `HonestDiD` package applied to the dynamic event-study aggregation of the doubly robust estimator (never-treated comparison group, universal base period).
>
> #### Restrictions and targets
>
> I consider the two restrictions proposed by Rambachan and Roth [-@rambachan2023]. The *relative magnitudes* restriction $\Delta^{RM}(\bar{M})$ requires the period-to-period violation of parallel trends after treatment to be no larger than $\bar{M}$ times the largest violation observed between consecutive pre-treatment periods; $\bar{M} = 0$ imposes exact post-treatment parallel trends, and $\bar{M} = 1$ allows post-treatment confounds as large as the worst pre-treatment deviation. The *smoothness* restriction $\Delta^{SD}(M)$ instead allows the treated group to follow a differential linear trend whose slope may change by at most $M$ per period. For each restriction, the *breakdown value* is the largest $\bar{M}$ (or $M$) at which the robust 95\% confidence interval still excludes zero. I apply these restrictions to two targets: the instantaneous effect ATT(0) and the equally weighted average of ATT(0) through ATT(6), which equals the overall dynamic ATT of $-0.088$ reported in the main text.
>
> #### Results
>
> For the overall average effect, the robust confidence interval under exact post-treatment parallel trends ($\bar{M} = 0$) is $[-0.13, -0.05]$ `[CHECK]`, nearly identical to the conventional interval, confirming that the robust procedure reproduces the original inference when no violation is allowed (Fig. S_X). As $\bar{M}$ increases the interval widens, and the exact breakdown value---located by root-finding between grid points---is $\bar{M} = 0.128$. The overall emissions effect therefore remains statistically distinguishable from zero provided post-treatment violations of parallel trends do not exceed roughly 13\% of the largest pre-treatment deviation. Two considerations inform the interpretation of this threshold. First, the benchmark is the single largest deviation over a pre-treatment window extending up to ten event-time years, and the large deviations occur at distant event times (2013--2015), which are estimated from fewer cohorts and are least informative about counterfactual trends near treatment; deviations in the years immediately preceding membership are small. Thirteen percent of the worst distant-year deviation is accordingly a nontrivial absolute allowance. Second, the threshold nonetheless falls well short of $\bar{M} = 1$, the reference point at which post-treatment confounding as severe as the worst pre-treatment year would be tolerated. The appropriate conclusion is that the estimated effect is not an artifact of exact-parallel-trends bookkeeping, but its statistical significance requires that unobserved post-treatment confounding be modest relative to the largest historical deviation. I therefore present the effect as robust to modest violations rather than as insensitive to the identifying assumption.
>
> For ATT(0), the conventional confidence interval already includes zero ($[-0.02, 0.01]$ `[CHECK]`; Figs. S_Y and S_Z), so the sensitivity analysis is not informative about a breakdown: there is no significant on-impact effect to overturn under either restriction. This is consistent with the dynamic profile in the main text, in which emissions reductions emerge approximately two years after membership and deepen through year six, as expected for policies that operate through supervisory rule-making, disclosure requirements, and gradual capital reallocation rather than through immediate mandates. Under the smoothness restriction, the interval at $M = 0$ is shifted relative to the conventional interval because $M = 0$ permits a linear differential trend extrapolated from the pre-period rather than no violation; this shift does not alter the substantive conclusion.
>
> #### Implementation notes
>
> Three implementation details depart from package defaults, in each case to correct a numerical problem rather than to alter the estimand. First, because `honest_did()` targets a single event-time coefficient, the average-ATT analysis constructs the coefficient vector, its covariance matrix (from the estimator's influence functions, using the package's own sandwich formula), and an equal-weight vector over post-treatment periods directly, and passes them to `createSensitivityResults_relativeMagnitudes()`; the construction reproduces the reported overall ATT exactly. Second, the default search grid for the robust interval (±20 standard errors) is too narrow at $\bar{M} \geq 1$ for these data, causing silent truncation at the grid edge; the grid was widened to ±50 times the original half-width, with 2,000 grid points, and no interval reaches the widened bound. Third, the smoothness analysis uses the conditional (ARP) test rather than the default FLCI because the convex solver used by the FLCI intermittently reported inaccurate solutions on this event study; the conditional test avoids the solver entirely. Breakdown values are refined beyond the $\bar{M}$ grid by bisection (`uniroot`) between the last grid point excluding zero and the first including it.

### Optional additions that would strengthen the section

These are suggestions, not drafted claims — run them first; do not cite numbers you have not seen:

1. **Smoothness restriction for the overall average ATT.** You already built `betahat_avg`, `sigma_avg`, and `l_vec_avg`; one extra call gives the smoothness counterpart of your headline analysis, making the two restrictions symmetric across targets:

    ```r
    hd_smooth_avg <- list(
      orig_ci   = hd_relmag_avg$orig_ci,
      robust_ci = createSensitivityResults(
        betahat        = betahat_avg,
        sigma          = sigma_avg,
        numPrePeriods  = numPrePeriods_avg,
        numPostPeriods = numPostPeriods_avg,
        l_vec          = l_vec_avg,
        method         = "Conditional"
      )
    )
    ```

2. **Sensitivity at a later event time,** e.g. `honest_did(cs_dynamic, e = 3, ...)`. ATT(3) is where your theory says the effect lives; if it has a nonzero breakdown value, that is a better-targeted robustness claim than ATT(0). (Event times up to `e = 6` are available.)

3. **A benchmark-sensitivity check on the pre-window.** Because the RM benchmark is driven by distant pre-periods, re-estimating the event study with a balanced or truncated event window (`aggte(..., balance_e = ...)` or `min_e`) and re-running HonestDiD would show whether the breakdown value is being set by 2013-era noise. If the breakdown rises materially with a shorter pre-window, report both; if it does not, you have pre-empted the obvious reviewer probe either way.

4. **Sign restriction (optional, more assertive).** If you are willing to argue that any selection story implies members were *already trending down* (i.e., bias is toward finding reductions), `biasDirection = "negative"` in the RM call imposes that sign and typically tightens the intervals. Only do this if you can defend the direction theoretically — it is a stronger assumption, and reviewers will ask.

### Two consistency flags outside the sensitivity section

- The main text says the panel covers 2013–2023 ($N = 1{,}595$), but the Discussion's limitations paragraph says "the statistical analysis ends in 2021," and the supplement describes EDGAR extraction for 2001–2021. Reconcile before submission.
- The supplement's descriptive-statistics caption says $N = 931$ while the main text says $N = 1{,}595$; if the difference is listwise deletion on specific variables, say so where the smaller N first appears.
