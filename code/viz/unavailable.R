unavailable <- jdr_last_five |>
  group_by(year = end_year) |>
  summarize(
    ex   = sum(excused,       na.rm = TRUE) + sum(postin,   na.rm = TRUE),
    disq = sum(disqual,       na.rm = TRUE),
    dism = sum(dismiss_peace, na.rm = TRUE) + sum(dismiss_dead, na.rm = TRUE),
    undel= sum(undel,         na.rm = TRUE),
    fta  = sum(fta,           na.rm = TRUE),
    pos  = sum(postout,       na.rm = TRUE),
    s    = sum(summons, na.rm = T) + sum(postin, na.rm = T)
  ) |>
  arrange(desc(year))

  unavailable_plot <- unavailable |>
  gt(rowname_col = "year") |>
  fmt(
    columns = c(ex, disq, dism, undel, fta, pos),
    fns = function(x) {
      dplyr::case_when(
        x >= 1e6 ~ paste0(round(x / 1e6, 2), "M"),   # 1.23M
        x >= 1e3 ~ paste0(round(x / 1e3,  0), "K"),   # 123K
        TRUE     ~ scales::comma(x, accuracy = 1)      # <1,000: plain integer
      )
    }
  ) |>
  cols_label(
    ex    = "Excused",
    disq  = "Disqualified",
    undel = "Undelivered",
    fta   = "Failure to Appear",
    dism  = "Dismissed",
    pos   = "Postponed Out"
  )

###### plot #######
unavailable_long <- unavailable |>
  pivot_longer(
    cols      = c(ex, disq, dism, undel, fta, pos),
    # s is excluded, stays as a column
    names_to  = "component",
    values_to = "count"
  ) |>
  mutate(
    component = recode(component,
                       ex    = "Excused",
                       disq  = "Disqualified",
                       dism  = "Dismissed",
                       undel = "Undelivered",
                       fta   = "Failure to Appear",
                       pos   = "Postponed Out"
    )
  ) |>
  group_by(component) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    rate  = count / s,                   # s rides along since it wasn't pivoted
    index = rate / rate[year == 2021]
  ) |>
  ungroup() |>
  mutate(
    line_color = if_else(
      component == "Failure to Appear",
      "#2c2c2c",
      "#cccccc"
    )
  )


ggplot(unavailable_long, aes(x = year, y = index)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_line(aes(group = 1, color = line_color), linewidth = 0.8) +
  geom_point(aes(color = line_color), size = 2) +
  # geom_text(
  #   aes(label = paste0(scales::number(index, accuracy = 0.01), "x")),
  #   vjust = -0.8, size = 2.5
  # ) +
  scale_color_identity() +
  facet_wrap(~ component) +
  scale_y_continuous(
    labels = function(x) paste0(scales::number(x, accuracy = 0.01), "x"),
    limits = c(
      min(unavailable_long$index, na.rm = TRUE) * 0.95,
      max(unavailable_long$index, na.rm = TRUE) * 1.05
    )
  ) +
  scale_x_continuous(
    breaks = unique(unavailable_long$year),
    labels = function(x) paste0("'", substr(x, 3, 4))
  ) +
  labs(
    title    = "Change in Unavailable Juror Subcategories from FY '21 Baseline",
    subtitle = "1.0 = FY2021 baseline · values above 1.0 indicate growth from baseline",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title         = element_text(hjust = 0, margin = margin(b = 4)),
    plot.subtitle      = element_text(color = "grey30", margin = margin(b = 4)),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave(
  here::here("visuals/unavailable1.png")
       , width = 6.5
       , height = 4
       #, units = "in"
       , dpi = 300
      # , scale = .9
)