library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggpattern)
library(lubridate)
library(here)
library(gt)
library(scales)
library(ggtext)
library(sysfonts)
library(showtext)
library(camcorder)

source(here("code/fonts.R"))

showtext::showtext_opts(dpi = 300)
camcorder::gg_record(
  dir = "img",
  dpi = 300,
  width = 6.5,
  height = 4,
  units = "in"
)

jdr <- read_csv("./data/processed/jury_data_transformed.csv")
this_year <- year(today())

jdr_last_five <- jdr |>
  filter(year(end_date) >= this_year - 5)

jdr_last_ten <- jdr |>
  filter(year(end_date) >= this_year - 10)

# ==============================================================================
# 1. UTILIZATION PIPELINE: % Told to Report, Sent for Selection, Panel Used
# ==============================================================================

pipeline <- jdr_last_five |>
  group_by(year = end_year) |>
  summarize(
    told_to_report = mean(pc_told_to_report, na.rm = TRUE),
    sent_for_sel   = mean(pc_sent_for_sel, na.rm = TRUE),
    panel_used     = mean(pc_panel_used, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(year))

pipeline_gt <- pipeline |>
  mutate(across(told_to_report:panel_used, ~scales::percent(., accuracy = 0.1))) |>
  gt(rowname_col = "year") |>
  cols_label(
    told_to_report = "Told to Report",
    sent_for_sel   = "Sent for Selection",
    panel_used     = "Panel Used"
  )

# ── Plot ──────────────────────────────────────────────────────────────────────

pipeline_long <- pipeline |>
  pivot_longer(
    cols      = c(told_to_report, sent_for_sel, panel_used),
    names_to  = "stage",
    values_to = "percent"
  ) |>
  mutate(
    stage = recode(stage,
                   told_to_report = "Told to Report",
                   sent_for_sel   = "Sent for Selection",
                   panel_used     = "Panel Used")
  )

ggplot(pipeline_long, aes(x = factor(year), y = percent, fill = stage)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_discrete(labels = function(x) paste0("FY '", substr(x, 3, 4))) +
  scale_fill_manual(
    values = c("Told to Report" = "#404040",
               "Sent for Selection" = "#808080",
               "Panel Used" = "#c0c0c0")
  ) +
  labs(
    title    = "Utilization Pipeline: Jurors Progress Through Selection",
    subtitle = "Percent at each stage of jury selection process, FY2021–FY2025",
    x        = NULL,
    y        = NULL,
    fill     = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(hjust = 0, margin = margin(r = 4, b = 4)),
    plot.subtitle    = element_text(color = "grey30", margin = margin(b = 4)),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom"
  )

ggsave(here("img/utilization_pipeline.png"),
       width = 6.5, height = 4, units = "in", dpi = 300)

# ==============================================================================
# 2. PANEL TYPES: Count by type + Faceted trends over time
# ==============================================================================

panel_types <- jdr_last_ten |>
  group_by(year = end_year) |>
  summarize(
    crim   = sum(panels_crim,   na.rm = TRUE),
    fel    = sum(panels_fel,    na.rm = TRUE),
    msdo   = sum(panels_msdo,   na.rm = TRUE),
    civil  = sum(panels_civil,  na.rm = TRUE),
    other  = sum(panels_other,  na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(year))

panel_types_gt <- panel_types |>
  gt(rowname_col = "year") |>
  fmt_number(columns = c(crim, fel, msdo, civil, other), suffixing = TRUE) |>
  cols_label(
    crim  = "Criminal",
    fel   = "Felony",
    msdo  = "Misdemeanor",
    civil = "Civil",
    other = "Other"
  )

# ── Faceted line chart (trends over time) ─────────────────────────────────────

panel_long <- panel_types |>
  pivot_longer(
    cols      = c(crim, fel, msdo, civil, other),
    names_to  = "type",
    values_to = "count"
  ) |>
  mutate(
    type = recode(type,
                  crim  = "Criminal",
                  fel   = "Felony",
                  msdo  = "Misdemeanor",
                  civil = "Civil",
                  other = "Other")
  ) |>
  arrange(year)

ggplot(panel_long, aes(x = year, y = count)) +
  geom_line(aes(group = type), color = "#404040", linewidth = 0.8) +
  geom_point(color = "#404040", size = 2) +
  facet_wrap(~ type) +
  scale_y_continuous(
    labels = function(x) {
      dplyr::case_when(
        x >= 1e6 ~ paste0(round(x / 1e6, 1), "m"),
        x >= 1e3 ~ paste0(round(x / 1e3, 0), "k"),
        TRUE     ~ as.character(x)
      )
    },
    limits = c(0, 12000)
  ) +
  scale_x_continuous(
    breaks = unique(panel_long$year),
    labels = function(x) paste0("'", substr(x, 3, 4))
  ) +
  labs(
    title    = "Panel Trends by Case Type",
    subtitle = "Raw counts, FY2016–present",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(hjust = 0, margin = margin(r = 4, b = 4)),
    plot.subtitle    = element_text(color = "grey30", margin = margin(b = 4)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(here("img/panel_types_trends.png"),
       width = 6.5, height = 4, units = "in", dpi = 300)

# ==============================================================================
# 3. SWORN TYPES: Count by type + Faceted trends over time
# ==============================================================================

sworn_types <- jdr_last_five |>
  group_by(year = end_year) |>
  summarize(
    crim   = sum(sworn_crim,   na.rm = TRUE),
    fel    = sum(sworn_fel,    na.rm = TRUE),
    msdo   = sum(sworn_msdo,   na.rm = TRUE),
    civil  = sum(sworn_civil,  na.rm = TRUE),
    other  = sum(sworn_other,  na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(year))

sworn_types_gt <- sworn_types |>
  gt(rowname_col = "year") |>
  fmt_number(columns = c(crim, fel, msdo, civil, other), suffixing = TRUE) |>
  cols_label(
    crim  = "Criminal",
    fel   = "Felony",
    msdo  = "MSDO",
    civil = "Civil",
    other = "Other"
  )

# ── Bar chart (current year) ──────────────────────────────────────────────────

sworn_current <- sworn_types |>
  filter(year == max(year)) |>
  pivot_longer(cols = -year, names_to = "type", values_to = "count") |>
  mutate(type = factor(type, levels = c("crim", "fel", "msdo", "civil", "other")))

ggplot(sworn_current, aes(x = reorder(type, -count), y = count, fill = type)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = scales::comma(count, accuracy = 1)),
    vjust = -0.3,
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
    values = c("crim" = "#404040", "fel" = "#606060", "msdo" = "#808080",
               "civil" = "#a0a0a0", "other" = "#c0c0c0"),
    labels = c("crim" = "Criminal", "fel" = "Felony", "msdo" = "MSDO",
               "civil" = "Civil", "other" = "Other")
  ) +
  labs(
    title    = "Juries Sworn by Case Type",
    subtitle = paste0("California statewide, FY", substr(max(sworn_current$year), 3, 4)),
    x        = NULL,
    y        = NULL,
    fill     = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(hjust = 0, margin = margin(r = 4, b = 4)),
    plot.subtitle    = element_text(color = "grey30", margin = margin(b = 4)),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "none",
    plot.margin        = margin(t = 16, r = 12, b = 8, l = 12)
  )

ggsave(here("img/sworn_types_bar.png"),
       width = 6.5, height = 4, units = "in", dpi = 300)

# ── Faceted line chart (trends over time) ─────────────────────────────────────

sworn_long <- sworn_types |>
  pivot_longer(
    cols      = c(crim, fel, msdo, civil, other),
    names_to  = "type",
    values_to = "count"
  ) |>
  mutate(
    type = recode(type,
                  crim  = "Criminal",
                  fel   = "Felony",
                  msdo  = "MSDO",
                  civil = "Civil",
                  other = "Other")
  ) |>
  group_by(type) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    index = count / count[year == 2021]
  ) |>
  ungroup()

ggplot(sworn_long, aes(x = year, y = index)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_line(aes(group = 1), color = "#404040", linewidth = 0.8) +
  geom_point(color = "#404040", size = 2) +
  facet_wrap(~ type) +
  scale_y_continuous(
    labels = function(x) paste0(scales::number(x, accuracy = 0.01), "x"),
    limits = c(
      min(sworn_long$index, na.rm = TRUE) * 0.95,
      max(sworn_long$index, na.rm = TRUE) * 1.05
    )
  ) +
  scale_x_continuous(
    breaks = unique(sworn_long$year),
    labels = function(x) paste0("'", substr(x, 3, 4))
  ) +
  labs(
    title    = "Juries Sworn Trends by Case Type",
    subtitle = "1.0 = FY2021 baseline · values above 1.0 indicate growth from baseline",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(hjust = 0, margin = margin(r = 4, b = 4)),
    plot.subtitle    = element_text(color = "grey30", margin = margin(b = 4)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(here("img/sworn_types_trends.png"),
       width = 6.5, height = 4, units = "in", dpi = 300)

# ==============================================================================
# 4. ONE-DAY SERVICE BREAKDOWN: In-Person vs Tel/Web
# ==============================================================================

oneday_breakdown <- jdr_last_five |>
  group_by(year = end_year) |>
  summarize(
    inperson = sum(oneday_inperson, na.rm = TRUE),
    telweb   = sum(oneday_telweb,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    total = inperson + telweb
  ) |>
  arrange(desc(year))

oneday_breakdown_gt <- oneday_breakdown |>
  select(-total) |>
  gt(rowname_col = "year") |>
  fmt_number(columns = c(inperson, telweb), suffixing = TRUE) |>
  cols_label(
    inperson = "In-Person",
    telweb   = "Telephone/Web"
  )

# ── Bar chart (current year) ──────────────────────────────────────────────────

oneday_current <- oneday_breakdown |>
  filter(year == max(year)) |>
  pivot_longer(cols = c(inperson, telweb), names_to = "modality", values_to = "count") |>
  mutate(modality = factor(modality, levels = c("inperson", "telweb")))

ggplot(oneday_current, aes(x = modality, y = count, fill = modality)) +
  geom_col(width = 0.5) +
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
  scale_x_discrete(labels = c("inperson" = "In-Person", "telweb" = "Telephone/Web")) +
  scale_fill_manual(
    values = c("inperson" = "#606060", "telweb" = "#a0a0a0"),
    guide = "none"
  ) +
  labs(
    title    = "One-Day Service by Modality",
    subtitle = paste0("California statewide, FY", substr(max(oneday_current$year), 3, 4)),
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(hjust = 0, margin = margin(r = 4, b = 4)),
    plot.subtitle    = element_text(color = "grey30", margin = margin(b = 4)),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(t = 16, r = 12, b = 8, l = 12)
  )

ggsave(here("img/oneday_breakdown_bar.png"),
       width = 6.5, height = 4, units = "in", dpi = 300)

# ── Faceted line chart (trends over time) ─────────────────────────────────────

oneday_long <- oneday_breakdown |>
  pivot_longer(
    cols      = c(inperson, telweb),
    names_to  = "modality",
    values_to = "count"
  ) |>
  mutate(
    modality = recode(modality,
                      inperson = "In-Person",
                      telweb   = "Telephone/Web")
  ) |>
  group_by(modality) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    index = count / count[year == 2021]
  ) |>
  ungroup()

ggplot(oneday_long, aes(x = year, y = index)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_line(aes(group = 1), color = "#404040", linewidth = 0.8) +
  geom_point(color = "#404040", size = 2) +
  facet_wrap(~ modality) +
  scale_y_continuous(
    labels = function(x) paste0(scales::number(x, accuracy = 0.01), "x"),
    limits = c(
      min(oneday_long$index, na.rm = TRUE) * 0.95,
      max(oneday_long$index, na.rm = TRUE) * 1.05
    )
  ) +
  scale_x_continuous(
    breaks = unique(oneday_long$year),
    labels = function(x) paste0("'", substr(x, 3, 4))
  ) +
  labs(
    title    = "One-Day Service Trends by Modality",
    subtitle = "1.0 = FY2021 baseline · values above 1.0 indicate growth from baseline",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(hjust = 0, margin = margin(r = 4, b = 4)),
    plot.subtitle    = element_text(color = "grey30", margin = margin(b = 4)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(here("img/oneday_breakdown_trends.png"),
       width = 6.5, height = 4, units = "in", dpi = 300)

# ==============================================================================
# 5. RELEASE REASONS BREAKDOWN
# ==============================================================================

released <- jdr_last_five |>
  group_by(year = end_year) |>
  summarize(
    challenge   = sum(rel_challenge,      na.rm = TRUE),
    hardship    = sum(rel_hardship,       na.rm = TRUE),
    perempt     = sum(rel_perempt,        na.rm = TRUE),
    defendant   = sum(rel_defendant_pc,   na.rm = TRUE),
    plaintiff   = sum(rel_plaintiff_pc,   na.rm = TRUE),
    not_reached = sum(not_reached,        na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(year))

released_gt <- released |>
  gt(rowname_col = "year") |>
  fmt_number(columns = c(challenge, hardship, perempt, defendant, plaintiff, not_reached),
             suffixing = TRUE) |>
  cols_label(
    challenge   = "For Cause",
    hardship    = "Hardship",
    perempt     = "Peremptory",
    defendant   = "Defendant PC",
    plaintiff   = "Plaintiff PC",
    not_reached = "Not Reached"
  )

# ── Bar chart (current year) ──────────────────────────────────────────────────

released_current <- released |>
  filter(year == max(year)) |>
  pivot_longer(cols = -year, names_to = "reason", values_to = "count") |>
  mutate(reason = factor(reason,
                         levels = c("challenge", "hardship", "perempt", "defendant", "plaintiff", "not_reached")))

ggplot(released_current, aes(x = reorder(reason, count), y = count, fill = reason)) +
  geom_col(width = 0.6) +
  coord_flip() +
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
  scale_x_discrete(labels = c("challenge" = "For Cause", "hardship" = "Hardship",
                              "perempt" = "Peremptory", "defendant" = "Defendant PC",
                              "plaintiff" = "Plaintiff PC", "not_reached" = "Not Reached")) +
  scale_fill_manual(
    values = c("challenge" = "#b8b8b8", "hardship" = "#b8b8b8", "perempt" = "#b8b8b8",
               "defendant" = "#b8b8b8", "plaintiff" = "#b8b8b8", "not_reached" = "#b8b8b8"),
    guide = "none"
  ) +
  labs(
    title    = "Juror Release Reasons Distribution",
    subtitle = paste0("California statewide, FY", substr(max(released_current$year), 3, 4)),
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(hjust = 0, margin = margin(r = 4, b = 4)),
    plot.subtitle    = element_text(color = "grey30", margin = margin(b = 4)),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(t = 16, r = 12, b = 8, l = 12)
  )

ggsave(here("img/released_reasons_bar.png"),
       width = 6.5, height = 4, units = "in", dpi = 300)

# ── Faceted line chart (trends over time) ─────────────────────────────────────

released_long <- released |>
  pivot_longer(
    cols      = c(challenge, hardship, perempt, defendant, plaintiff, not_reached),
    names_to  = "reason",
    values_to = "count"
  ) |>
  mutate(
    reason = recode(reason,
                    challenge   = "For Cause",
                    hardship    = "Hardship",
                    perempt     = "Peremptory",
                    defendant   = "Defendant PC",
                    plaintiff   = "Plaintiff PC",
                    not_reached = "Not Reached")
  ) |>
  group_by(reason) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    index = count / count[year == 2021]
  ) |>
  ungroup()

ggplot(released_long, aes(x = year, y = index)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_line(aes(group = 1, color = reason), linewidth = 0.8) +
  geom_point(aes(color = reason), size = 2) +
  facet_wrap(~ reason) +
  scale_y_continuous(
    labels = function(x) paste0(scales::number(x, accuracy = 0.01), "x"),
    limits = c(
      min(released_long$index, na.rm = TRUE) * 0.95,
      max(released_long$index, na.rm = TRUE) * 1.05
    )
  ) +
  scale_x_continuous(
    breaks = unique(released_long$year),
    labels = function(x) paste0("'", substr(x, 3, 4))
  ) +
  scale_color_manual(
    values = c("For Cause" = "#404040", "Hardship" = "#606060", "Peremptory" = "#808080",
               "Defendant PC" = "#a0a0a0", "Plaintiff PC" = "#b8b8b8", "Not Reached" = "#d0d0d0"),
    guide = "none"
  ) +
  labs(
    title    = "Release Reason Trends",
    subtitle = "1.0 = FY2021 baseline · values above 1.0 indicate growth from baseline",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(hjust = 0, margin = margin(r = 4, b = 4)),
    plot.subtitle    = element_text(color = "grey30", margin = margin(b = 4)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(here("img/released_reasons_trends.png"),
       width = 6.5, height = 4, units = "in", dpi = 300)
