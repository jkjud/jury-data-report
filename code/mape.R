jdr_last_five |>
  select(year = end_year, ends_with("prop")) |>
  group_by(year) |>
  # Calculate MAPE for each proportion
  summarize(
    pa = mean(abs(potentially_available_prop - 1), na.rm = TRUE),
    exc = mean(abs(excused_prop - 1), na.rm = TRUE),
    dsq = mean(abs(disqual_prop - 1), na.rm = TRUE),
    tqa = mean(abs(tqa_prop - 1), na.rm = TRUE),
    srv = mean(abs(serving_prop - 1), na.rm = TRUE),
    inc = mean(abs(incourt_prop - 1), na.rm = TRUE),
    prmpt = mean(abs(perempt_prop - 1), na.rm = TRUE),
    ond = mean(abs(one_day_prop - 1), na.rm = TRUE),
    jdy = mean(abs(jdays_prop - 1), na.rm = TRUE),
    pnl = mean(abs(panel_prop - 1), na.rm = TRUE),
    c_pnl = mean(abs(criminal_panels_prop - 1), na.rm = TRUE),
    c_swo = mean(abs(criminal_sworn_prop - 1), na.rm = TRUE)
  ) |>
  arrange(desc(year)) |>
  # Calculate year-over-year changes (in percentage points)
  mutate(across(
    pa:c_swo,
    list(chg = ~ (.x - lead(.x)) * 100), # Convert to percentage points
    .names = "{.col}_{.fn}"
  )) |>
  gt(rowname_col = "year") |>
  # Format MAPE columns as percentages
  fmt_percent(
    columns = c(
      pa,
      exc,
      dsq,
      tqa,
      srv,
      inc,
      prmpt,
      ond,
      jdy,
      pnl,
      c_pnl,
      c_swo
    ),
    decimals = 1
  ) |>
  # Format change columns with +/- signs
  fmt_number(
    columns = ends_with("_chg"),
    decimals = 1,
    force_sign = TRUE
  ) |>
  # Add column spanners to group MAPE and Change columns
  tab_spanner(
    label = md("**Potentially Available**"),
    columns = c(pa, pa_chg)
  ) |>
  tab_spanner(
    label = md("**Excused**"),
    columns = c(exc, exc_chg)
  ) |>
  tab_spanner(
    label = md("**Disqualified**"),
    columns = c(dsq, dsq_chg)
  ) |>
  tab_spanner(
    label = md("**TQA**"),
    columns = c(tqa, tqa_chg)
  ) |>
  tab_spanner(
    label = md("**Serving**"),
    columns = c(srv, srv_chg)
  ) |>
  tab_spanner(
    label = md("**In Court**"),
    columns = c(inc, inc_chg)
  ) |>
  tab_spanner(
    label = md("**Perempt**"),
    columns = c(prmpt, prmpt_chg)
  ) |>
  tab_spanner(
    label = md("**One Day**"),
    columns = c(ond, ond_chg)
  ) |>
  tab_spanner(
    label = md("**Jury Days**"),
    columns = c(jdy, jdy_chg)
  ) |>
  tab_spanner(
    label = md("**Panel**"),
    columns = c(pnl, pnl_chg)
  ) |>
  tab_spanner(
    label = md("**Criminal Panel**"),
    columns = c(c_pnl, c_pnl_chg)
  ) |>
  tab_spanner(
    label = md("**Criminal Sworn**"),
    columns = c(c_swo, c_swo_chg)
  ) |>
  cols_label(
    pa = "MAPE",
    pa_chg = "Δ pp",
    exc = "MAPE",
    exc_chg = "Δ pp",
    dsq = "MAPE",
    dsq_chg = "Δ pp",
    tqa = "MAPE",
    tqa_chg = "Δ pp",
    srv = "MAPE",
    srv_chg = "Δ pp",
    inc = "MAPE",
    inc_chg = "Δ pp",
    prmpt = "MAPE",
    prmpt_chg = "Δ pp",
    ond = "MAPE",
    ond_chg = "Δ pp",
    jdy = "MAPE",
    jdy_chg = "Δ pp",
    pnl = "MAPE",
    pnl_chg = "Δ pp",
    c_pnl = "MAPE",
    c_pnl_chg = "Δ pp",
    c_swo = "MAPE",
    c_swo_chg = "Δ pp"
  )
