# ── Bar chart (current year) ──────────────────────────────────────────────────

panel_current <- panel_types |>
  filter(year == max(year)) |>
  pivot_longer(cols = -year, names_to = "type", values_to = "count") |>
  filter(type != "crim") |>
  mutate(type = factor(type, levels = c("fel", "msdo", "civil", "other"), labels = c("Felony", "Misdemeanor", "Civil", "Other")))

ggplot(panel_current, aes(x = reorder(type, count), y = count, fill = type)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = dplyr::case_when(
      count >= 1e6 ~ paste0(round(count / 1e6, 1), "m"),
      count >= 1e3 ~ paste0(round(count / 1e3, 1), "k"),
      TRUE ~ as.character(count)
    )),
    hjust = -0.3,
    size = 3
  ) +
  scale_y_continuous(
    labels = function(x) {
      dplyr::case_when(
        x >= 1e6 ~ paste0(round(x / 1e6, 1), "m"),
        x >= 1e3 ~ paste0(round(x / 1e3,  0), "k"),
        TRUE     ~ as.character(x)
      )
    },
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_fill_manual(
    values = c("fel" = "#808080", "msdo" = "#808080",
               "civil" = "#808080", "other" = "#808080"),
    labels = c("fel" = "Felony", "msdo" = "Misdemeanor",
               "civil" = "Civil", "other" = "Other")
  ) +
  labs(
    title    = "Panel Distribution by Case Type",
    subtitle = paste0("California statewide, FY", substr(max(panel_current$year), 3, 4)),
    x        = NULL,
    y        = NULL,
    fill     = NULL
  ) +
  coord_flip() +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(hjust = 0, margin = margin(r = 4, b = 4)),
    plot.subtitle    = element_text(color = "grey30", margin = margin(b = 4)),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "none",
    plot.margin        = margin(t = 16, r = 12, b = 8, l = 12)
  )

ggsave(here("img/panel_types_bar.png"),
       width = 6.5, height = 4, units = "in", dpi = 300)

# ── Alternative: Stacked Criminal (Felony + MSDO) vs Civil vs Other ──────────

panel_stacked <- panel_types |>
  filter(year == max(year)) |>
  pivot_longer(cols = c(fel, msdo, civil, other), names_to = "type", values_to = "count") |>
  mutate(
    category = case_when(
      type %in% c("fel", "msdo") ~ "Criminal",
      type == "civil" ~ "Civil",
      type == "other" ~ "Other"
    ),
    type = factor(type, levels = c("msdo", "fel", "civil", "other"))
  ) |>
  mutate(category = factor(category, levels = c("Other", "Civil", "Criminal"))) |>
  group_by(category) |>
  mutate(
    cumsum = cumsum(count),
    label_y = cumsum - count / 2,
    format_text = dplyr::case_when(
      count >= 1e6 ~ paste0(round(count / 1e6, 1), "m"),
      count >= 1e3 ~ paste0(round(count / 1e3, 1), "k"),
      TRUE ~ as.character(round(count))
    )
  ) |>
  ungroup() |>
  mutate(
    type_label = recode(type, fel = "Felony", msdo = "Misdemeanor", civil = "Civil", other = "Other"),
    label_text_criminal = paste0(type_label, "\n", format_text),
    label_hjust = if_else(category == "Criminal", 0.5, -0.1),
    label_vjust = if_else(category == "Criminal", 0.5, 0.5)
  ) |>
  group_by(category) |>
  mutate(
    category_total = sum(count),
    category_total_text = dplyr::case_when(
      category_total >= 1e6 ~ paste0(round(category_total / 1e6, 1), "m"),
      category_total >= 1e3 ~ paste0(round(category_total / 1e3, 1), "k"),
      TRUE ~ as.character(round(category_total))
    )
  ) |>
  ungroup()

ggplot(panel_stacked, aes(x = category, y = count, fill = type, pattern = type)) +
  geom_col_pattern(
    width = 0.6,
    pattern_density = 0.15,
    pattern_spacing = 0.02,
    pattern_angle = 45,
    color = "#606060",
    linewidth = 0.3
  ) +
  geom_label(
    aes(y = label_y, label = label_text_criminal),
    size = 2.5,
    color = "black",
    fontface = "bold",
    fill = "white",
    label.padding = unit(0.2, "lines"),
    label.size = 0,
    data = panel_stacked |> filter(category == "Criminal")
  ) +
  geom_label(
    aes(y = cumsum, label = format_text, hjust = label_hjust),
    size = 3,
    color = "black",
    fill = "white",
    label.padding = unit(0.15, "lines"),
    label.size = 0,
    data = panel_stacked |> filter(category != "Criminal")
  ) +
  geom_label(
    aes(y = category_total, label = category_total_text),
    size = 2.8,
    color = "black",
    fill = "white",
    label.padding = unit(0.15, "lines"),
    label.size = 0,
    hjust = -0.1,
    data = panel_stacked |> filter(category == "Criminal") |> slice(1)
  ) +
  scale_y_continuous(
    labels = function(x) {
      dplyr::case_when(
        x >= 1e6 ~ paste0(round(x / 1e6, 1), "m"),
        x >= 1e3 ~ paste0(round(x / 1e3,  0), "k"),
        TRUE     ~ as.character(x)
      )
    },
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_manual(
    values = c("fel" = "#a0a0a0", "msdo" = NA, "civil" = "#a0a0a0", "other" = "#a0a0a0"),
    na.value = NA
  ) +
  scale_pattern_manual(
    values = c("fel" = "none", "msdo" = "stripe", "civil" = "none", "other" = "none"),
    guide = "none"
  ) +
  scale_pattern_color_manual(
    values = c("fel" = NA, "msdo" = "#a0a0a0", "civil" = NA, "other" = NA),
    guide = "none"
  ) +
  labs(
    title    = "Panel Distribution: Criminal Breakdown vs Civil and Other",
    subtitle = paste0("California statewide, FY", substr(max(panel_stacked$year), 3, 4)),
    x        = NULL,
    y        = NULL
  ) +
  coord_flip() +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(hjust = 0, margin = margin(r = 4, b = 4)),
    plot.subtitle    = element_text(color = "grey30", margin = margin(b = 4)),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position  = "none",
    plot.margin      = margin(t = 16, r = 12, b = 8, l = 12)
  )

ggsave(here("img/panel_types_stacked.png"),
       width = 6.5, height = 4, units = "in", dpi = 300)
