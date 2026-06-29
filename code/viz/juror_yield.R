
jury_yield <- jdr_last_ten |>
  group_by(year = end_year) |>
  summarize(
    # Raw sums (used directly or as building blocks for ratios)
    s              = sum(summons, na.rm = TRUE) + sum(postin, na.rm = TRUE),
    qualified            = sum(tqa, na.rm = TRUE),
    unavailable              = sum(unavailable, na.rm = TRUE),
    # Components for weighted ratios
    .pot_avail     = sum(potentially_available, na.rm = TRUE),
    yield_pct      = round(qualified / .pot_avail * 100, 2)
  )

df <- jury_yield |>
  arrange(desc(year)) |>
  transmute(
    year,
    qualified   = round(qualified / 1e6, 2),
    unavailable = round(unavailable   / 1e6, 2),
    yield_pct = yield_pct
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

# ── X-axis label formatter ────────────────────────────────────────────────────
fy_labels <- function(x) paste0("FY '", substr(x, 3, 4))
df_long <- df_long |>
  mutate(
    yield_pct = round(yield_pct, 2)
  )

jury_yield_plot <- ggplot() +
  geom_col(
    data  = df_long,
    aes(x = factor(year), y = millions, fill = segment),
    width = 0.6
  ) +
  # geom_text(
  #   data     = df_long,
  #   aes(
  #     x      = factor(year),
  #     y      = millions,
  #     label  = paste0(round(millions, 1), "M"),
  #     color  = segment
  #   ),
  #   position = position_stack(vjust = 0.5),
  #   fontface = "bold", size = 3, family = "inter"
  # ) +
  geom_line(
    data = df,
    aes(x = factor(year),
        y = yield_pct * scale_factor,
        group = 1,
        color = "Juror Yield"),
    linewidth = .5
  ) +
  geom_point(
    data = df,
    aes(
      x = factor(year),
      y = round(yield_pct * scale_factor, 2),
      color = "Juror Yield"
    ),
    size = 2
  ) +
  geom_text(
    data = df,
    aes(x = factor(year),
        y = yield_pct * scale_factor,
        label = paste0(round(yield_pct, 0), "%")),
    vjust = -1, color = "#CD9D46", size = 3, fontface = "bold", family = "inter"
  ) +
  scale_y_continuous(
    name   = "Summoned Jurors",
    labels = function(x) ifelse(x > 12, "", paste0(x, "M")),
    breaks = seq(0, 18, by = 4),
    limits = c(0, 18),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_x_discrete(labels = fy_labels) +
  scale_fill_manual(
    values = c(qualified = "#2172b0", unavailable = "#c2540e"),
    labels = c(qualified = "Available", unavailable = "Unavailable")
  ) +
  scale_color_manual(
    values = c("Juror Yield" = "#CD9D46"
               , qualified = "white"
               , unavailable = "white"),
    breaks = "Juror Yield",
    labels = c("Juror Yield" = "Juror Yield")
  ) +
  guides(
    fill  = guide_legend(order = 2),
    color = guide_legend(order = 1)
  ) +
  labs(
    title    = "Juror Yield hovers near 45% as counts of
                Available and Unavailable jurors stabilize",
    subtitle = "California statewide jury summons outcomes, FY2021–FY2025",
    x        = NULL,
    fill     = NULL,
    color    = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    text               = element_text(family = "inter"),
    plot.title         = element_text(
      family     = "lora",
      size       = 10,
      lineheight = 1.4,
      hjust      = 0,
      margin     = margin(r = 4, b = 4)
    ),
    plot.subtitle      = element_text(
      family = "lora", size = 8,
      color  = "grey30", margin = margin(b = 4)
    ),
    axis.text          = element_text(family = "inter"),
    axis.title         = element_text(family = "inter"),
    axis.title.y.right = element_text(color = "black"),
    axis.text.y.right  = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    legend.position    = "right",
    legend.direction   = "vertical",
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(t = 16, r = 12, b = 8, l = 12)
  )

jury_yield_plot

showtext_opts(dpi = 300)

ggsave("visuals/jury_yield3.png"
  , width = 6.5
  , height = 4
  , units = "in"
  , dpi = 300
  #, scale = .9
)
