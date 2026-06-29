library(dplyr)
library(readr)
library(lubridate)
library(here)

jdr <- read_csv(here("data/processed/jury_data_transformed.csv"))

# ── 1. Statewide mean pc_sent_for_sel by year ────────────────────────────────
# Get a sense of scale — where do 2017 and 2025 sit relative to all years?
jdr |>
  group_by(end_year) |>
  summarize(
    n_courts      = n(),
    mean_pct      = mean(pc_sent_for_sel, na.rm = TRUE),
    median_pct    = median(pc_sent_for_sel, na.rm = TRUE),
    n_na          = sum(is.na(pc_sent_for_sel)),
    n_over1       = sum(pc_sent_for_sel > 1, na.rm = TRUE),
    n_under0      = sum(pc_sent_for_sel < 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(end_year) |>
  print(n = Inf)

# ── 2. Court-level detail for 2017 and 2025 ──────────────────────────────────
# Show every court's components so we can spot the bad actors
components <- c(
  "county", "end_year",
  "rel_challenge", "rel_hardship", "rel_perempt", "jurors_sworn", "not_reached",
  "tqa", "oncall",
  "pc_sent_for_sel"
)

bad_years <- jdr |>
  filter(end_year %in% c(2017, 2025)) |>
  select(all_of(components)) |>
  mutate(
    numerator   = rel_challenge + rel_hardship + rel_perempt + jurors_sworn + not_reached,
    denominator = tqa - oncall,
    flag_over1  = pc_sent_for_sel > 1,
    flag_under0 = pc_sent_for_sel < 0,
    flag_na     = is.na(pc_sent_for_sel),
    flag_zero_denom = denominator == 0 | is.na(denominator)
  ) |>
  arrange(end_year, county)

cat("\n── All courts in 2017 and 2025 ──────────────────────────────────────\n")
print(bad_years, n = Inf)

# ── 3. Flag anything suspicious ──────────────────────────────────────────────
cat("\n── Flagged rows (>1, <0, NA, or zero denominator) ──────────────────\n")
bad_years |>
  filter(flag_over1 | flag_under0 | flag_na | flag_zero_denom) |>
  print(n = Inf)

# ── 4. Distribution of pc_sent_for_sel in problem years vs. the rest ─────────
cat("\n── Summary stats: 2017 ──\n")
jdr |> filter(end_year == 2017) |> pull(pc_sent_for_sel) |> summary() |> print()

cat("\n── Summary stats: 2025 ──\n")
jdr |> filter(end_year == 2025) |> pull(pc_sent_for_sel) |> summary() |> print()

cat("\n── Summary stats: all other years ──\n")
jdr |> filter(!end_year %in% c(2017, 2025)) |> pull(pc_sent_for_sel) |> summary() |> print()

# ── 5. Check raw component NAs by year ───────────────────────────────────────
# If a whole column went missing in the raw sheet it will show up here
cat("\n── NA counts per component by year ──────────────────────────────────\n")
jdr |>
  group_by(end_year) |>
  summarize(
    across(
      c(rel_challenge, rel_hardship, rel_perempt, jurors_sworn, not_reached, tqa, oncall),
      ~ sum(is.na(.x)),
      .names = "na_{.col}"
    ),
    .groups = "drop"
  ) |>
  filter(end_year %in% c(2017, 2025) | if_any(starts_with("na_"), ~ .x > 0)) |>
  arrange(end_year) |>
  print(n = Inf)

# ── 6. Negative denominator: courts where oncall > tqa ───────────────────────
# This is the only way pc_sent_for_sel goes deeply negative
cat("\n── Courts with oncall >= tqa (negative/zero denominator) ───────────\n")
jdr |>
  filter(end_year %in% c(2017, 2025)) |>
  select(county, end_year, tqa, oncall, pc_sent_for_sel) |>
  mutate(denominator = tqa - oncall) |>
  filter(denominator <= 0 | is.na(denominator)) |>
  arrange(end_year, denominator) |>
  print(n = Inf)

# ── 7. All negative pc_sent_for_sel in 2017 and 2025, ranked by extremity ────
cat("\n── All negative pc_sent_for_sel values in 2017 and 2025 ────────────\n")
jdr |>
  filter(end_year %in% c(2017, 2025), pc_sent_for_sel < 0) |>
  select(county, end_year, tqa, oncall, pc_sent_for_sel) |>
  mutate(
    denominator = tqa - oncall,
    oncall_over_tqa = oncall - tqa
  ) |>
  arrange(pc_sent_for_sel) |>
  print(n = Inf)

# ── 8. San Francisco across all years — is 2017 a one-off? ───────────────────
cat("\n── San Francisco: tqa, oncall, denominator, pc_sent_for_sel by year ─\n")
jdr |>
  filter(county == "San Francisco") |>
  select(end_year, tqa, oncall, rel_hardship, pc_sent_for_sel) |>
  mutate(denominator = tqa - oncall) |>
  arrange(end_year) |>
  print(n = Inf)

jdr |>
  filter(county == "San Francisco") |>
  select(end_year, tqa, oncall, rel_hardship, pc_sent_for_sel) |>
  mutate(denominator = tqa - oncall) |>
  arrange(end_year) |>
  print(n = Inf)

weird_utilization <- jdr |>
  filter(end_year == 2025) |>
  filter(
    !between(pc_told_to_report, 0, 1) |
    !between(pc_sent_for_sel, 0, 1) |
    !between(pc_panel_used, 0, 1)
  ) |>
  mutate(
    flag_told_to_report = !between(pc_told_to_report, 0, 1),
    flag_sent_for_sel = !between(pc_sent_for_sel, 0, 1),
    flag_panel_used = !between(pc_panel_used, 0, 1)
  ) |>
  select(county, pc_told_to_report, flag_told_to_report, pc_sent_for_sel, flag_sent_for_sel, pc_panel_used, flag_panel_used) |>
  arrange(desc(flag_told_to_report), desc(flag_sent_for_sel), desc(flag_panel_used))
  View(weird_utilization)

jdr |>
  filter(end_year == 2025) |>
  filter(
    !between(juror_yield, 0, 1)
  ) |>
  mutate(
    flag_juror_yield = !between(juror_yield, 0, 1)
  ) |>
  select(county, juror_yield, flag_juror_yield)

colnames(jdr)
