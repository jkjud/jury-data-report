

jdr <- read_csv("./data/processed/jury_data_transformed.csv")
this_year <- year(today())
jdr_last_five <- jdr |>
  filter(year(end_date) >= this_year - 5)

jdr_last_ten <- jdr |>
  filter(year(end_date) >= this_year - 10)

summary_by_year <- jdr_last_ten |>
  group_by(end_year) |>
  summarize(
    oncall = sum(oncall, na.rm = TRUE),
    tqa = sum(tqa, na.rm = TRUE)
  ) |>
  mutate(pc_told_to_report = (tqa - oncall) / tqa)

scale_factor <- max(summary_by_year$oncall, summary_by_year$tqa)

summary_by_year |>
  mutate(pc_told_to_report_scaled = pc_told_to_report * scale_factor) |>
  pivot_longer(
    cols = c(oncall, tqa, pc_told_to_report_scaled),
    names_to = "type",
    values_to = "count"
  ) |>
  ggplot(aes(x = end_year, y = count, color = type)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "% Told to Report",
      labels = scales::percent
    )
  ) +
  labs(
    x = "Year",
    y = "Count",
    color = NULL
  )