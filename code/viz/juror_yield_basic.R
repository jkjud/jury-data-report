
# Build yield data for column chart

build_jury_yield_data <- function(
  data,
  years_back = 10,
  year_col = "end_year",
  units = 1e6
) {
  df_filtered <- data |>
    filter(year(end_date) >= year(today()) - years_back)

  jury_yield <- df_filtered |>
    group_by(year = .data[[year_col]]) |>
    summarize(
      s          = sum(summons, na.rm = TRUE) + sum(postin, na.rm = TRUE),
      qualified  = sum(tqa, na.rm = TRUE),
      unavailable = sum(unavailable, na.rm = TRUE),
      .pot_avail = sum(potentially_available, na.rm = TRUE),
      yield_pct  = round(qualified / .pot_avail * 100, 2)
    )

  df <- jury_yield |>
    arrange(desc(year)) |>
    transmute(
      year,
      qualified   = round(qualified / units, 2),
      unavailable = round(unavailable / units, 2),
      yield_pct   = yield_pct
    )

  df_long <- df |>
    pivot_longer(
      c(qualified, unavailable),
      names_to  = "segment",
      values_to = "millions"
    ) |>
    mutate(
      segment   = factor(segment, levels = c("unavailable", "qualified")),
      yield_pct = round(yield_pct, 2)
    )

  list(df = df, df_long = df_long)
}

yield_data <- build_jury_yield_data(jdr)
yield_table <- yield_data$df
yield_long <- yield_data$df_long


################# Basic Juror Yield #################

jury_yield <- jdr_last_ten |>
  group_by(year = end_year) |>
  summarize(
    s                = sum(summons, na.rm = TRUE) + sum(postin, na.rm = TRUE),
    qualified        = sum(tqa, na.rm = TRUE),
    unavailable      = sum(unavailable, na.rm = TRUE),
    .pot_avail       = sum(potentially_available, na.rm = TRUE),
    yield_pct        = round(qualified / .pot_avail * 100, 2)
  )

df <- jury_yield |>
  arrange(desc(year)) |>
  transmute(
    year,
    qualified   = round(qualified / 1e6, 2),
    unavailable = round(unavailable / 1e6, 2),
    yield_pct   = yield_pct
  )

df_long <- df |>
  pivot_longer(
    c(qualified, unavailable),
    names_to = "segment",
    values_to = "millions"
  ) |>
  mutate(
    segment = factor(segment, levels = c("unavailable", "qualified"))
  )

scale_factor <- 12 / 39

# X-axis label formatter
fy_labels <- function(x) paste0("FY '", substr(x, 3, 4))
yield_long <- yield_long |>
  mutate(
    yield_pct = round(yield_pct, 2)
  )

# Basic version with simple styling
jury_yield_1 <- ggplot() +
  geom_col(
    data = yield_long,
    aes(
      x = factor(year),
      y = millions,
      fill = segment
    ),
    color = "black",
    linewidth = 0.3,
    position = "stack",
    width = 0.6
  ) +
  geom_line(
    data = yield_table,
    aes(
      x = factor(year),
      y = yield_pct * scale_factor,
      group = 1
    ),
    color = "grey40",
    linewidth = 0.6
  ) +
  geom_point(
    data = yield_table,
    aes(
      x = factor(year),
      y = yield_pct * scale_factor
    ),
    color = "grey40",
    size = 2
  ) +
  geom_text(
    data = yield_table,
    aes(
      x = factor(year),
      y = yield_pct * scale_factor,
      label = paste0(round(yield_pct, 0), "%")
    ),
    vjust = -1.2,
    color = "grey40",
    size = 3
  ) +
  scale_y_continuous(
    name = "Summoned Jurors (Millions)",
    labels = function(x) ifelse(x > 12, "", paste0(x, "M")),
    breaks = seq(0, 18, by = 3),
    limits = c(0, 19),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_x_discrete(labels = fy_labels) +
  scale_fill_manual(
    values = c(
      unavailable = "white",
      qualified = "grey70"
    ),
    labels = c(
      unavailable = "Unavailable",
      qualified = "Available"
    )
  ) +
  labs(
    title = "Juror Yield by Year",
    subtitle = "California statewide jury summons outcomes, FY2021–FY2025",
    x = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(
      size = 12,
      face = "bold",
      hjust = 0,
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 10,
      color = "grey50",
      hjust = 0,
      margin = margin(b = 12)
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = 12, r = 12, b = 8, l = 12)
  )

jury_yield_1

###################################################################

################ Version 2: yield % with red line on top #################
#  remove title/subtitle, adjust y-axis limits

scale_factor <- 5.5 / 39

fy_labels <- function(x) paste0("FY '", substr(x, 3, 4))
df_long <- df_long |>
  mutate(
    yield_pct = round(yield_pct, 2)
  )

jury_yield_2 <- ggplot() +
  geom_col(
    data = yield_long,
    aes(
      x = factor(year),
      y = millions,
      fill = segment
    ),
    color = "black",
    linewidth = 0.3,
    position = "stack",
    width = 0.6
  ) +
  geom_line(
    data = yield_table,
    aes(
      x = factor(year),
      y = yield_pct * scale_factor,
      group = 1,
      color = "Yield %"
    ),
    linewidth = 0.6
  ) +
  geom_point(
    data = yield_table,
    aes(
      x = factor(year),
      y = yield_pct * scale_factor,
      color = "Yield %"
    ),
    size = 2
  ) +
  geom_text(
    data = yield_table,
    aes(
      x = factor(year),
      y = yield_pct * scale_factor,
      label = paste0(round(yield_pct, 0), "%")
    ),
    vjust = -0.8,
    hjust = 0.5,
    color = "red",
    size = 2.3
  ) +
  scale_y_continuous(
    name = "Total Summoned Jurors",
    labels = function(x) ifelse(x > 13.5, "", paste0(x, "M")),
    breaks = seq(0, 13.5, by = 2),
    limits = c(0, 13.5),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_x_discrete(labels = fy_labels) +
  scale_fill_manual(
    values = c(
      unavailable = "grey90",
      qualified = "grey50"
    ),
    labels = c(
      unavailable = "Unavailable",
      qualified = "Available"
    )
  ) +
  scale_color_manual(
    values = c("Yield %" = "red")
  ) +
  labs(
    x = NULL,
    fill = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    axis.title.y = element_text(size = 8),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 7),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = 12, r = 12, b = 8, l = 12),
    axis.text = element_text(size = 6)
  )

jury_yield_2

########################################################################

############### Version 3: yield % with red line inline #################

scale_factor <- 13.5 / 100

fy_labels <- function(x) paste0("FY '", substr(x, 3, 4))
df_long <- df_long |>
  mutate(
    yield_pct = round(yield_pct, 2)
  )

jury_yield_3 <- ggplot() +
  geom_col(
    data = df_long,
    aes(
      x = factor(year),
      y = millions,
      fill = segment
    ),
    color = "black",
    linewidth = 0.3,
    position = "stack",
    width = 0.6
  ) +
  geom_line(
    data = yield_table,
    aes(
      x = factor(year),
      y = yield_pct * scale_factor,
      group = 1,
      color = "Yield %"
    ),
    linewidth = 0.6
  ) +
  geom_point(
    data = yield_table,
    aes(
      x = factor(year),
      y = yield_pct * scale_factor,
      color = "Yield %"
    ),
    size = 2
  ) +
  geom_text(
    data = yield_table,
    aes(
      x = factor(year),
      y = yield_pct * scale_factor,
      label = paste0(round(yield_pct, 0), "%")
    ),
    vjust = -0.8,
    hjust = 0.5,
    color = "red",
    size = 2.3
  ) +
  scale_y_continuous(
    name = "Total Summoned Jurors",
    labels = function(x) ifelse(x > 13.5, "", paste0(x, "M")),
    breaks = seq(0, 13.5, by = 2),
    limits = c(0, 13.5),
    expand = expansion(mult = c(0, 0)),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Jury Yield %",
      labels = function(x) paste0(round(x), "%"),
      breaks = seq(0, 100, by = 15)
    )
  ) +
  scale_x_discrete(labels = fy_labels) +
  scale_fill_manual(
    values = c(
      unavailable = "grey90",
      qualified = "grey50"
    ),
    labels = c(
      unavailable = "Unavailable",
      qualified = "Available"
    )
  ) +
  scale_color_manual(
    values = c("Yield %" = "red")
  ) +
  labs(
    x = NULL,
    fill = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    axis.title.y = element_text(size = 8),
    axis.title.y.right = element_text(size = 8, color = "red"),
    axis.text.y.right = element_text(size = 6, color = "red"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 7),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = 12, r = 12, b = 8, l = 12),
    axis.text = element_text(size = 6)
  )

jury_yield_3

########################################################################

############### Version 4: yield % floating above bars #################
# Same as v3 but line floats above bars: extend limits beyond labeled region
# and tune scale_factor so yield values map into that unlabeled upper zone.

scale_factor <- 13.5 / 39  # maps ~39% yield to 13.5M (top of labeled area)

jury_yield_4 <- ggplot() +
  geom_col(
    data = df_long,
    aes(
      x = factor(year),
      y = millions,
      fill = segment
    ),
    color = "black",
    linewidth = 0.3,
    position = "stack",
    width = 0.6
  ) +
  geom_line(
    data = yield_table,
    aes(
      x = factor(year),
      y = yield_pct * scale_factor,
      group = 1,
      color = "Yield %"
    ),
    linewidth = 0.6
  ) +
  geom_point(
    data = yield_table,
    aes(
      x = factor(year),
      y = yield_pct * scale_factor,
      color = "Yield %"
    ),
    size = 2
  ) +
  geom_text(
    data = yield_table,
    aes(
      x = factor(year),
      y = yield_pct * scale_factor,
      label = paste0(round(yield_pct, 0), "%")
    ),
    vjust = -0.8,
    hjust = 0.5,
    color = "red",
    size = 2.3
  ) +
  scale_y_continuous(
    name = "Total Summoned Jurors",
    labels = function(x) ifelse(x > 13.5, "", paste0(x, "M")),
    breaks = seq(0, 13.5, by = 2),
    limits = c(0, 20),
    expand = expansion(mult = c(0, 0)),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Jury Yield %",
      labels = function(x) paste0(round(x), "%"),
      breaks = seq(0, 100, by = 15)
    )
  ) +
  scale_x_discrete(labels = fy_labels) +
  scale_fill_manual(
    values = c(
      unavailable = "grey90",
      qualified = "grey50"
    ),
    labels = c(
      unavailable = "Unavailable",
      qualified = "Available"
    )
  ) +
  scale_color_manual(
    values = c("Yield %" = "red")
  ) +
  labs(
    x = NULL,
    fill = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    axis.title.y = element_text(size = 8),
    axis.title.y.right = element_text(size = 8, color = "red"),
    axis.text.y.right = element_text(size = 6, color = "red"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 7),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = 12, r = 12, b = 8, l = 12),
    axis.text = element_text(size = 6)
  )

jury_yield_4
