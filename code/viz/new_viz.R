library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(here)
library(gt)
library(scales)
library(ggtext)
library(sysfonts)
library(showtext)
library(camcorder)
library(plotly)

source(here("code/fonts.R"))

showtext::showtext_opts(dpi = 300)
camcorder::gg_record(
  dir = "img"
  , dpi = 300
  , width = 6.5
  , height = 4
  , units = "in"
)

jdr <- read_csv("./data/processed/jury_data_transformed.csv")
this_year <- year(today())
jdr_last_five <- jdr |>
  filter(year(end_date) >= this_year - 5)

jdr_last_ten <- jdr |>
  filter(year(end_date) >= this_year - 10)

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

################## One-Day Service ############################################

oneday_2025 <- jdr_last_ten |>
  filter(end_year == 2025) |>
  summarize(
    oneday  = sum(oneday,  na.rm = TRUE),
    serving = sum(serving, na.rm = TRUE)
  ) |>
  mutate(prop = round(oneday / serving * 100, 1))

donut_data <- tibble(
  segment = factor(c("One-Day", "Other"), levels = c("Other", "One-Day")),
  value   = c(oneday_2025$prop, 100 - oneday_2025$prop)
)

oneday_plot <- ggplot(donut_data, aes(x = 2, y = value, fill = segment)) +
  geom_col(width = 1, color = "white", linewidth = 0.5) +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +
  annotate(
    "text", x = 0.5, y = 0,
    label    = paste0(oneday_2025$prop, "%"),
    size     = 11, fontface = "bold", family = "inter", color = "#2172b0"
  ) +
  annotate(
    "text", x = 0.5, y = 0,
    label    = "\n\nserved\none day",
    size     = 3.5, family = "inter", color = "grey40", lineheight = 1.3
  ) +
  scale_fill_manual(
    values = c("One-Day" = "#2172b0", "Other" = "#d9e6f2"),
    labels = c(
      "One-Day" = "One-day service",
      "Other" = "Multi-day / not released"
    )
  ) +
  labs(
    title    = "Most serving jurors complete their obligation in one day",
    subtitle = "California statewide, FY2025",
    fill     = NULL
  ) +
  theme_void(base_size = 11) +
  theme(
    text             = element_text(family = "inter"),
    plot.title       = element_text(
      family = "lora", size = 10, lineheight = 1.4,
      hjust = 0.5, margin = margin(b = 4)
    ),
    plot.subtitle    = element_text(
      family = "lora", size = 8, color = "grey30",
      hjust = 0.5, margin = margin(b = 8)
    ),
    legend.position  = "bottom",
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(t = 16, r = 12, b = 8, l = 12)
  )

oneday_plot

################## Unavailable #################################################

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
    ex    = md("**Excused**"),
    disq  = md("**Disqualified**"),
    undel = md("**Undelivered**"),
    fta   = md("**Failure to Appear**"),
    dism  = md("**Dismissed**"),
    pos   = md("**Postponed Out**")
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
      "#c2540e",
      "grey55"
    )
  )


ggplot(unavailable_long, aes(x = year, y = index)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_line(aes(group = 1, color = line_color), linewidth = 0.8) +
  geom_point(aes(color = line_color), size = 2) +
  # geom_text(
  #   aes(label = paste0(scales::number(index, accuracy = 0.01), "x")),
  #   vjust = -0.8, size = 2.5, family = "inter"
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
    title    = "Unavailable Juror Subcategories",
    subtitle = "1.0 = FY2021 baseline · values above 1.0 indicate growth from baseline",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(
      size       = 12,
      lineheight = 1.4,
      hjust      = 0,
      margin     = margin(r = 4, b = 4)
      ),
    plot.subtitle      = element_text(size = 10, color = "grey30", margin = margin(b = 4)),
    strip.text         = element_text(size = 9),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA)
  )

ggsave(
  here::here("visuals/unavailable1.png")
       , width = 6.5
       , height = 4
       #, units = "in"
       , dpi = 300
      # , scale = .9
)

################################################################################

############## Juror Utilization Box Plot by Year ##############################
ju_box_data <- jdr_last_five |>
  filter(end_year %in% 2021:2025) |>
  select(county, year = end_year, juror_utilization) |>
  mutate(year = factor(year))

ju_utilization_boxplot <- ggplot(ju_box_data,
aes(x = year, y = juror_utilization)
) +
  geom_boxplot(
    fill      = "#2172b0",
    color     = "grey30",
    alpha     = 0.7,
    outlier.shape  = 16,
    outlier.size   = 1.5,
    outlier.color  = "#c2540e",
    outlier.alpha  = 0.7,
    width     = 0.5
  ) +
  scale_x_discrete(labels = function(x) paste0("FY '", substr(x, 3, 4))) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title    = "Juror Utilization Distribution by Year",
    subtitle = "Each point represents a county · FY2021–FY2025",
    x        = NULL,
    y        = "Juror Utilization"
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
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.margin        = margin(t = 16, r = 12, b = 8, l = 12)
  )

ju_utilization_boxplot

ggsave(here("visuals", "juror_utilization_boxplot.png"),
       width  = 6.5,
       height = 4,
       units  = "in",
       dpi    = 300)
###############################################################################


cols <- c("TRUE" = "#4E668A", "FALSE" = "darkgrey")

boxplot_jms <- jms_transformed |>
  mutate(jsi = jms_system == "JSI") |>
  ggplot(aes(forcats::fct_reorder(jms_system, summons_postin_prop, median), summons_postin_prop, color = jsi)) +
  geom_hline(yintercept = 1, linetype = "dotted", alpha = 0.5) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(width = 0.35) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, labels = scales::label_percent()) +
  labs(
    title = "Relative to other systems, it's also fairly accurate",
    subtitle = "with a median accuracy of 93.66%."
  ) +
  theme_minimal() +
  coord_flip() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  scale_color_manual(values = cols)


cols <- c("TRUE" = "#2172b0", "FALSE" = "darkgrey")

ju_utilization_boxplot <- ju_box_data |>
  mutate(is_recent = year == "2025") |>
  ggplot(aes(forcats::fct_reorder(year, juror_utilization, median), juror_utilization, color = is_recent)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(width = 0.35) +
  scale_x_discrete(
    name   = NULL,
    labels = function(x) paste0("FY '", substr(x, 3, 4))
  ) +
  scale_y_continuous(
    name   = NULL,
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  scale_color_manual(values = cols) +
  coord_flip() +
  labs(
    title    = "Juror Utilization Distribution by Year",
    subtitle = "Each point represents a county · FY2021–FY2025"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    text             = element_text(family = "inter"),
    plot.title       = element_text(
      family     = "lora",
      size       = 10,
      lineheight = 1.4,
      hjust      = 0,
      margin     = margin(r = 4, b = 4)
    ),
    plot.subtitle    = element_text(
      family = "lora", size = 8,
      color  = "grey30", margin = margin(b = 4)
    ),
    axis.text        = element_text(family = "inter"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = "none",
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin      = margin(t = 16, r = 12, b = 8, l = 12)
  )

############################# By Cluster ######################
ju_box_data <- jdr_last_five |>
  filter(end_year == 2025) |>
  select(county, cluster, juror_utilization) |>
  mutate(
    cluster    = factor(cluster),
    cluster    = forcats::fct_reorder(cluster, juror_utilization, median, .na_rm = TRUE)
  )

ju_utilization_boxplot <- ju_box_data |>
  ggplot(aes(cluster, juror_utilization)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(width = 0.35) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(
    name   = NULL,
    labels = scales::label_percent(accuracy = 1),
    limits = c(0, 0.5),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  scale_color_manual(values = cols) +
 # coord_flip() +
  labs(
    title    = "Inter-quartile range of Juror Utilization by Cluster",
    subtitle = "FY2025"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    text             = element_text(family = "inter"),
    plot.title       = element_text(
      family     = "lora",
      size       = 10,
      lineheight = 1.4,
      hjust      = 0,
      margin     = margin(r = 4, b = 4)
    ),
    plot.subtitle    = element_text(
      family = "lora", size = 8,
      color  = "grey30", margin = margin(b = 4)
    ),
    axis.text        = element_text(family = "inter"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = "none",
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin      = margin(t = 16, r = 12, b = 8, l = 12)
  )

ju_plotly <- plotly::ggplotly(ju_utilization_boxplot)


####################################################################
ju_box_data <- jdr_last_five |>
  filter(end_year == 2025) |>
  select(county, cluster, pc_told_to_report, pc_sent_for_sel, pc_panel_used) |>
  mutate(cluster = factor(cluster)) |>
  pivot_longer(
    cols      = c(pc_told_to_report, pc_sent_for_sel, pc_panel_used),
    names_to  = "metric",
    values_to = "value"
  ) |>
  mutate(
    cluster = forcats::fct_reorder(cluster, value, median, .na_rm = TRUE),
    metric  = factor(metric, 
                     levels = c("pc_told_to_report", "pc_sent_for_sel", "pc_panel_used"),
                     labels = c("Told to Report", "Sent for Selection", "Panel Used"))
  )

ju_utilization_boxplot <- ju_box_data |>
  ggplot(aes(cluster, value)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(width = 0.35, color = "grey30") +
  facet_wrap(~ metric, ncol = 3) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(
    name   = NULL,
    labels = scales::label_percent(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title    = "Juror Utilization Components by Court Cluster",
    subtitle = "FY2025"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    text             = element_text(family = "inter"),
    plot.title       = element_text(family = "lora", size = 10, hjust = 0,
                                    margin = margin(r = 4, b = 4)),
    plot.subtitle    = element_text(family = "lora", size = 8,
                                    color = "grey30", margin = margin(b = 4)),
    strip.text       = element_text(family = "lora", size = 9, face = "bold"),
    axis.text        = element_text(family = "inter"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = "none",
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", 
                                     color = NA),
    plot.margin      = margin(t = 16, r = 12, b = 8, l = 12)
  )

################## Excused ###########################################################

jdr_last_five |>
  filter(
    end_year == 2025
  ) |>
  select(excused_phys, excused_fin, excused_care, excused_trans,
         excused_12m, excused_other) |>
  pivot_longer(cols = everything(), names_to = "excuse_type",
               values_to = "count") |>
  group_by(excuse_type) |>
  summarize(total = sum(count, na.rm = TRUE)) |>
  arrange(desc(total)) |>
  mutate(
    excuse_type = recode(excuse_type,
                         excused_phys   = "Physical/Mental Condition",
                         excused_fin    = "Financial Hardship",
                         excused_care   = "Caregiving Responsibilities",
                         excused_trans   = "Transportation Issues",
                         excused_12m     = "12-Month Period",
                         excused_other   = "Other Reasons"
                    )
  ) |>
  ggplot(aes(x = reorder(excuse_type, total), y = total)) +
  geom_col(width = 0.6) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) {
      dplyr::case_when(
        x >= 1e6 ~ paste0(round(x / 1e6, 1), "M"),
        x >= 1e3 ~ paste0(round(x / 1e3,  0), "K"),
        TRUE     ~ scales::comma(x, accuracy = 1)
      )
    },
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Excused Juror Reasons",
    subtitle = "California statewide totals, FY2025",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    text             = element_text(family = "inter"),
    plot.title       = element_text(
      family     = "lora",
      size       = 10,
      lineheight = 1.4,
      hjust      = 0,
      margin     = margin(r = 4, b = 4)
    ),
    plot.subtitle    = element_text(
      family = "lora", size = 8,
      color  = "grey30", margin = margin(b = 4)
    ),
    axis.text        = element_text(family = "inter"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin      = margin(t = 16, r = 12, b = 8, l = 12)
  )

# ── Excused subcategory trend (small multiples) ───────────────────────────────

# excused_trend <- jdr_last_five |>
#   group_by(year = end_year) |>
#   summarize(
#     excused_phys  = sum(excused_phys,  na.rm = TRUE),
#     excused_fin   = sum(excused_fin,   na.rm = TRUE),
#     excused_care  = sum(excused_care,  na.rm = TRUE),
#     excused_trans = sum(excused_trans, na.rm = TRUE),
#     excused_12m   = sum(excused_12m,   na.rm = TRUE),
#     excused_other = sum(excused_other, na.rm = TRUE),
#     s             = sum(summons, na.rm = TRUE) + sum(postin, na.rm = TRUE)
#   ) |>
#   pivot_longer(
#     cols      = c(excused_phys, excused_fin, excused_care,
#                   excused_trans, excused_12m, excused_other),
#     names_to  = "excuse_type",
#     values_to = "count"
#   ) |>
#   mutate(
#     excuse_type = recode(excuse_type,
#                          excused_phys  = "Physical/Mental Condition",
#                          excused_fin   = "Financial Hardship",
#                          excused_care  = "Caregiving Responsibilities",
#                          excused_trans = "Transportation Issues",
#                          excused_12m   = "12-Month Period",
#                          excused_other = "Other Reasons"
#     )
#   ) |>
#   group_by(excuse_type) |>
#   arrange(year, .by_group = TRUE) |>
#   mutate(
#     rate  = count / s,
#     index = rate / rate[year == 2021]
#   ) |>
#   ungroup()
# 
# ggplot(excused_trend, aes(x = year, y = index)) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.4) +
#   geom_line(aes(group = 1), color = "grey55", linewidth = 0.8) +
#   geom_point(color = "grey55", size = 2) +
#   scale_y_continuous(
#     labels = function(x) paste0(scales::number(x, accuracy = 0.01), "x")
#   ) +
#   scale_x_continuous(
#     breaks = unique(excused_trend$year),
#     labels = function(x) paste0("'", substr(x, 3, 4))
#   ) +
#   facet_wrap(~ excuse_type, scales = "free_y") +
#   labs(
#     title    = "Excused Juror Reason Trends",
#     subtitle = "1.0 = FY2021 baseline · values above 1.0 indicate growth from baseline",
#     x = NULL, y = NULL
#   ) +
#   theme_minimal(base_size = 10) +
#   theme(
#     plot.title    = element_text(
#       size       = 12,
#       lineheight = 1.4,
#       hjust      = 0,
#       margin     = margin(r = 4, b = 4)
#     ),
#     plot.subtitle      = element_text(size = 10, color = "grey30", margin = margin(b = 4)),
#     strip.text         = element_text(size = 9),
#     panel.grid.major   = element_blank(),
#     panel.grid.minor   = element_blank(),
#     plot.background    = element_rect(fill = "white", color = NA),
#     panel.background   = element_rect(fill = "white", color = NA)
#   )

############################## Disqualified ###########################################

jdr_last_five |>
  filter(
    end_year == 2025
  ) |>
  select(disqual_citizen, disqual_18y, disqual_nores, disqual_nocal,
       disqual_noenglish, disqual_conserv, disqual_fel) |>
  pivot_longer(cols = everything(), names_to = "disq_type",
               values_to = "count") |>
  group_by(disq_type) |>
  summarize(total = sum(count, na.rm = TRUE)) |>
  arrange(desc(total)) |>
  mutate(
    disq_type = recode(disq_type,
                       disqual_citizen   = "Not a U.S. Citizen",
                       disqual_18y       = "Under 18 Years Old",
                       disqual_nores     = "Not a County Resident",
                       disqual_nocal     = "Not a California Resident",
                       disqual_noenglish = "Insufficient English",
                       disqual_conserv   = "Under Conservatorship",
                       disqual_fel       = "Felony Conviction"
                  )
  ) |>
  ggplot(aes(x = reorder(disq_type, total), y = total)) +
  geom_col(width = 0.6) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) {
      dplyr::case_when(
        x >= 1e6 ~ paste0(round(x / 1e6, 1), "M"),
        x >= 1e3 ~ paste0(round(x / 1e3,  0), "K"),
        TRUE     ~ scales::comma(x, accuracy = 1)
      )
    },
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Disqualified Juror Reasons",
    subtitle = "California statewide totals, FY2025",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    text             = element_text(family = "inter"),
    plot.title       = element_text(
      family     = "lora",
      size       = 10,
      lineheight = 1.4,
      hjust      = 0,
      margin     = margin(r = 4, b = 4)
    ),
    plot.subtitle    = element_text(
      family = "lora", size = 8,
      color  = "grey30", margin = margin(b = 4)
    ),
    axis.text          = element_text(family = "inter"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.margin        = margin(t = 16, r = 12, b = 8, l = 12)
  )

# ── Disqualified subcategory trend (small multiples) ─────────────────────────

# disq_long <- jdr_last_five |>
#   select(end_year, disqual_citizen, disqual_18y, disqual_nores, disqual_nocal,
#          disqual_noenglish, disqual_conserv, disqual_fel) |>
#   pivot_longer(
#     cols      = -end_year,
#     names_to  = "disq_type",
#     values_to = "count"
#   ) |>
#   # === THIS IS THE MISSING STEP ===
#   group_by(end_year, disq_type) |>
#   summarize(count = sum(count, na.rm = TRUE), .groups = "drop") |>
#   # =================================
# mutate(
#   disq_type = recode(disq_type,
#                      disqual_citizen   = "Not a U.S. Citizen",
#                      disqual_18y       = "Under 18",
#                      disqual_nores     = "Not County Resident",
#                      disqual_nocal     = "Not CA Resident",
#                      disqual_noenglish = "Insufficient English",
#                      disqual_conserv   = "Under Conservatorship",
#                      disqual_fel       = "Felony Conviction")
# ) |>
#   group_by(disq_type) |>
#   arrange(end_year, .by_group = TRUE) |>
#   mutate(
#     total = sum(count, na.rm = TRUE),
#     index = count / count[end_year == 2021]
#   ) |>
#   ungroup() |>
#   mutate(line_color = "grey55")
# 
# ggplot(disq_long, aes(x = end_year, y = index)) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.4) +
#   geom_line(aes(group = 1, color = line_color), linewidth = 0.8) +
#   geom_point(aes(color = line_color), size = 2) +
#   scale_color_identity() +
#   facet_wrap(~ reorder(disq_type, -total), scales = "free_y") +
#   scale_y_continuous(
#     labels = function(x) paste0(scales::number(x, accuracy = 0.01), "x")
#   ) +
#   scale_x_continuous(
#     breaks = unique(disq_long$end_year),
#     labels = function(x) paste0("'", substr(x, 3, 4))
#   ) +
#   labs(
#     title    = "Disqualified Juror Subcategories",
#     subtitle = "1.0 = FY2021 baseline · values above 1.0 indicate growth from baseline",
#     x = NULL, y = NULL
#   ) +
#   theme_minimal(base_size = 10) +
#   theme(
#     plot.title         = element_text(size = 12, lineheight = 1.4, hjust = 0,
#                                       margin = margin(r = 4, b = 4)),
#     plot.subtitle      = element_text(size = 10, color = "grey30", margin = margin(b = 4)),
#     strip.text         = element_text(size = 9),
#     panel.grid.major   = element_blank(),
#     panel.grid.minor   = element_blank(),
#     plot.background    = element_rect(fill = "white", color = NA),
#     panel.background   = element_rect(fill = "white", color = NA)
#   )


