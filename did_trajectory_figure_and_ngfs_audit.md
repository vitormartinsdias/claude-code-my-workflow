# DiD Trajectory Figure (schematic style): Diagnosis, Fixed Code, and `ngfs_member` Audit

Two deliverables: (1) a corrected, ready-to-paste chunk for the `did_normalized_trajectories` figure, whose data logic I verified by running the identical computation on `gcb_clean_data.rds` (a preview of the resulting figure is saved as `did_normalized_trajectories_preview.png` in this folder); (2) an audit of every `ngfs_member` usage in the revised `manuscriptv1.qmd`, verified against the actual data.

---

## Part 1. Why the current figure misbehaves

The caption of the current `did_normalized_trajectories.png` reveals two design problems, and both are visible in the plot:

**Problem 1: single "median cohort" alignment for the never-treated group.** Never-treated countries have no entry year, so the current code maps them onto one pseudo-clock (the median treatment cohort, baselined to 2016–2018). Consequences: (a) the control line's event-time range is whatever 2013–2023 happens to span on that one clock — hence the abrupt ending at +3 while members run to +4; (b) the visible kinks (the hump at −1, the dip at 0) are artifacts of one arbitrary calendar mapping, since every wiggle of the world economy in specific years lands on specific event times for the entire control group at once.

**Problem 2: changing composition across event time for members.** With data covering 2013–2023, event time +4 exists only for the 2017–2020 cohorts, while −6 exists only for the 2019+ cohorts. A line that averages whoever happens to be observed at each event time mixes different sets of countries at different x-positions, so the line can bend (or end) purely because the sample changed — the same "forbidden comparison" spirit that motivated your move from TWFE to Callaway & Sant'Anna, resurfacing in a descriptive plot.

## Part 2. The fix, and what it produces

Three design changes, all verified against your data:

1. **Balanced window, fixed composition.** Restrict to the 2017–2019 cohorts (8 + 11 + 19 = 38 countries), all of which are observed over the full −4…+4 window. The set of countries is then *identical at every point on the x-axis* — the code below asserts this with `stopifnot()`.
2. **Cohort-clock alignment for never-treated countries.** Each of the 65 never-treated countries is aligned to *each* of the three cohort clocks (2017, 2018, 2019), baselined exactly as that cohort's members are; the three aligned series are then averaged within country (weighted by cohort size: 8/11/19) before group means are taken. No arbitrary single clock, no truncation: the control line spans the same −4…+4 as members, and standard errors are computed over countries (65), not over duplicated alignments.
3. **Common origin at entry.** Both groups keep the "own 3-year pre-entry average" baseline, so both lines sit near 0% just before entry — the visual anchor of the classic schematic.

Running this on `gcb_clean_data.rds` gives (log-point deviations, converted to % in the plot):

| event time | Members (n=38) | Never-treated (n=65) |
|---:|---:|---:|
| −4 | −0.4% | −3.8% |
| −1 | +0.7% | +1.8% |
| 0 | +0.5% | +3.6% |
| +2 | −3.3% | +4.4% |
| +4 | −4.8% | +8.3% |

The picture mirrors the schematic: the two groups drift upward together pre-entry, then members bend down while never-treated countries keep rising. Both lines now run the full window with no abrupt endings and no kinks. One honest feature to keep in mind when writing about it: the never-treated line rises somewhat *faster* than members in the pre-period (the lines cross around −2), consistent with the "modest pre-existing convergence" you already acknowledge in the manuscript — do not describe the pre-period as perfectly parallel.

## Part 3. Ready-to-paste chunk

Self-contained: it derives entry years from `ngfs_member` directly (correctly treating it as the absorbing 0→1 indicator), so it can be placed anywhere after `further-cleaning` and does not depend on `first_treat` or `ngfs_entry` from other chunks. Requires only tidyverse (already loaded in `setup`).

````
```{r}
#| label: did-schematic-trajectories

# Observed (model-free) DiD visualization in the spirit of the classic
# two-line schematic: NGFS members vs. never-treated countries, each
# country normalized to its own pre-entry baseline.
#
# Design notes:
# * Balanced composition. Event-time averages mix cohorts: with data
#   2013-2023, event time +4 exists only for cohorts <= 2019 and -4 only
#   for cohorts >= 2017. Restricting to the 2017-2019 cohorts (n = 38)
#   over the window -4..+4 keeps the set of countries identical at every
#   x-position, so lines cannot bend or end abruptly for compositional
#   reasons (asserted below).
# * Never-treated alignment. Never-treated countries have no entry year,
#   so each is aligned to every cohort clock (2017, 2018, 2019),
#   baselined identically to that cohort's members; each country's three
#   aligned series are then averaged (cohort-size weighted) within
#   country before group means are taken. This replaces the previous
#   single "median cohort" alignment, which truncated the control line
#   and produced kinks tied to one arbitrary calendar mapping. SEs are
#   computed across countries (one observation per country per event
#   time), never across duplicated alignments.
# * Baseline = own 3-year pre-entry average (event times -3..-1), so
#   both lines sit near 0% just before entry, as in the schematic.

traj_cohorts <- c(2017, 2018, 2019)
traj_window  <- -4:4

# Entry year derived directly from ngfs_member (absorbing 0 -> 1
# indicator: e.g., France is 0 before 2017 and 1 from 2017 onward),
# NA = never treated within the panel.
traj_entry <- gcb_clean_data |>
  group_by(country) |>
  summarise(
    entry = if (any(ngfs_member == 1)) min(year[ngfs_member == 1]) else NA_integer_,
    .groups = "drop"
  )

cohort_sizes <- traj_entry |>
  filter(entry %in% traj_cohorts) |>
  count(entry, name = "w")

traj_data <- gcb_clean_data |>
  select(country, year, log_ed_ghg_emissions) |>
  left_join(traj_entry, by = "country")

# --- NGFS members (2017-2019 cohorts) ---
traj_members <- traj_data |>
  filter(entry %in% traj_cohorts) |>
  mutate(e = year - entry) |>
  group_by(country) |>
  mutate(base = mean(log_ed_ghg_emissions[e >= -3 & e <= -1])) |>
  ungroup() |>
  filter(e %in% traj_window) |>
  transmute(country, e, dev = log_ed_ghg_emissions - base,
            group = "NGFS Members")

# --- Never-treated, aligned to each cohort clock, then collapsed to one
# --- observation per country x event time ---
traj_never <- map_dfr(traj_cohorts, function(g) {
  traj_data |>
    filter(is.na(entry)) |>
    mutate(e = year - g) |>
    group_by(country) |>
    mutate(base = mean(log_ed_ghg_emissions[e >= -3 & e <= -1])) |>
    ungroup() |>
    filter(e %in% traj_window) |>
    transmute(country, e, dev = log_ed_ghg_emissions - base,
              w = cohort_sizes$w[cohort_sizes$entry == g])
}) |>
  group_by(country, e) |>
  summarise(dev = weighted.mean(dev, w), .groups = "drop") |>
  mutate(group = "Never-Treated")

# --- Group means, SEs, % conversion ---
traj_plot_data <- bind_rows(traj_members, traj_never) |>
  group_by(group, e) |>
  summarise(
    n_countries = n(),
    m  = mean(dev),
    se = sd(dev) / sqrt(n_countries),
    .groups = "drop"
  ) |>
  mutate(
    pct      = 100 * (exp(m) - 1),
    pct_low  = 100 * (exp(m - se) - 1),
    pct_high = 100 * (exp(m + se) - 1)
  )

# Composition check: each group must have a constant country count at
# every event time (38 members, 65 never-treated). Fails loudly if the
# window/cohort choice ever goes out of sync with the data.
stopifnot(
  traj_plot_data |>
    summarise(ok = n_distinct(n_countries) == 1, .by = group) |>
    pull(ok)
)

did_traj_plot <- ggplot(
  traj_plot_data,
  aes(x = e, y = pct, color = group, linetype = group,
      ymin = pct_low, ymax = pct_high)
) +
  annotate("rect", xmin = -Inf, xmax = -0.5, ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "gray50") +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.4) +
  geom_vline(xintercept = -0.5, linetype = "dashed",
             color = "black", linewidth = 0.6) +
  geom_ribbon(aes(fill = group), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  annotate("text", x = min(traj_window), y = Inf, label = "Pre-Treatment",
           hjust = 0, vjust = 1.5, fontface = "italic",
           color = "gray40", size = 4) +
  annotate("text", x = 0.2, y = Inf, label = "Post-Treatment",
           hjust = 0, vjust = 1.5, fontface = "italic",
           color = "gray40", size = 4) +
  scale_color_manual(
    values = c("Never-Treated" = "gray55", "NGFS Members" = "black")
  ) +
  scale_fill_manual(
    values = c("Never-Treated" = "gray55", "NGFS Members" = "black"),
    guide = "none"
  ) +
  scale_linetype_manual(
    values = c("Never-Treated" = "dashed", "NGFS Members" = "solid")
  ) +
  scale_x_continuous(breaks = traj_window) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    plot.caption     = element_text(hjust = 0.5, size = 9, color = "gray30")
  ) +
  labs(
    title = "Emission Trajectories Before and After NGFS Membership",
    x = "Years relative to NGFS entry (event time)",
    y = "% deviation from own pre-entry baseline",
    caption = paste0(
      "Total GHG emissions; each country normalized to its own pre-entry baseline ",
      "(average of the three years before joining).\n",
      "Sample: 2017–2019 entry cohorts (n = 38) and all never-treated countries ",
      "(n = 65), the set observed over the full −4 to +4 window,\n",
      "so group composition is identical at every event time. Never-treated countries ",
      "are aligned to each cohort clock, baselined identically,\n",
      "and averaged (cohort-size weighted) within country. Lines = group means; ",
      "bands = ±1 SE. Descriptive, model-free companion to the\n",
      "event-study estimates; see the Callaway & Sant'Anna figures for ",
      "model-adjusted effects."
    )
  )

did_traj_plot

ggsave("did_normalized_trajectories.png", did_traj_plot,
       width = 10, height = 6.5, dpi = 300, bg = "white")

```
````

**Sentence for the main text** (e.g., right before or after the @fig-cs-event discussion), adjust to taste:

> The observed data mirror this pattern without any modeling: relative to their own pre-entry baselines, the 2017--2019 entry cohorts and never-treated countries drift upward together before treatment, after which members' emissions bend downward (about $-5\%$ by year four) while never-treated countries' emissions continue rising (about $+8\%$), a divergence in the raw trajectories consistent with the doubly robust estimates.

**Two honest-writing cautions.** First, the pre-period lines are similar but not identical — never-treated countries rise slightly faster and the lines cross around event time −2 — so describe the pre-period as "broadly similar drift," not "parallel." Second, this figure is descriptive: it involves no covariates, no reweighting, and drops the 2020–2023 cohorts (42 countries) to keep composition fixed; say explicitly that it is a raw-data companion to, not a substitute for, the CS estimates.

## Part 4. `ngfs_member` audit (revised `manuscriptv1.qmd`)

I checked the variable in the data itself and every place the manuscript uses it. **Verdict: it is used correctly everywhere.** In `gcb_clean_data.rds`, `ngfs_member` is a factor with levels "0"/"1", it is absorbing (no country ever reverts 1→0), France is 0 for 2013–2016 and 1 from 2017 on, and the panel is 145 countries × 11 years (2013–2023), N = 1,595, with cohorts 2017:8, 2018:11, 2019:19, 2020:19, 2021:10, 2022:8, 2023:5 and 65 never-treated.

Chunk-by-chunk:

- **`map` (ngfs_entry) and `did` (first_treat):** both derive entry as `min(year[ngfs_member == 1])`, which is exactly right for an absorbing indicator (the factor-vs-numeric comparison `ngfs_member == 1` works because R matches the level label). `first_treat = 0` for never-treated matches the `did` package convention. The two chunks duplicate the same derivation under two names — harmless, but consider deriving once and reusing.
- **TWFE chunks (`femlist1–3`):** entering the absorbing dummy directly is the standard generalized DiD specification; the coefficient compares country-years after entry with country-years before entry and with never-members, within country and year. Correct given the coding.
- **`fecoefplot` (`filter(term == "ngfs_member1")`):** correct *because* the variable is a factor (fixest names the coefficient `ngfs_member1`). This is the one brittle spot: if the variable ever became numeric upstream, the term would be `ngfs_member`, the filter would silently return zero rows, and the figure would be empty. Add a guard near the top of `femlist1`: `stopifnot(is.factor(gcb_clean_data$ngfs_member))`.
- **`marginal-effects`, `combined-marginsplot`, `predictions-plot*` (marginaleffects):** `variables = "ngfs_member"` on a factor yields the 1-vs-0 contrast, i.e., the effect of becoming a member — correct. The percent conversions (`100*(exp(estimate)-1)` in the plot chunks; `(1-exp(estimate))*100` as "reduction" in the prose) are mutually consistent.
- **CS chunks (`group-time-att`, `cs-plot-*`):** treatment enters only through `first_treat`, derived correctly as above; `control_group = "nevertreated"` relies on the 0 convention, satisfied.
- **`mediation-analysis`:** `as.numeric(as.character(ngfs_member))` is the correct factor→numeric conversion (plain `as.numeric()` on a factor would return level indices 1/2 — the classic trap, avoided here), and `treat_val`/`control_val` are computed from the converted variable.
- **Leftover:** the commented-out `obs-boxplot` chunk references `dngfs_member`, a stale variable name. It cannot run (it is inside a comment block), but delete it or fix the name if you ever revive it.

One data note to be aware of, not an error: 2022 and 2023 joiners (13 countries) have zero or one post-treatment year and contribute almost nothing to post-treatment estimates, while never appearing in the trajectory figure above. Their inclusion in the CS estimation is still correct — the estimator handles them — this is only to preempt a reviewer asking why cohort-level estimates for 2022–2023 are absent or noisy.
