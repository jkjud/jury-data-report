
#| label: setup
#| include: false

library(tidyverse)
library(gt)
library(scales)
library(ggtext)
library(camcorder)
library(here)

source(here("code/fonts.R"))

# Summons
# Unavailable
# Yield
# Jurors Sworn
# Cases
# Utilization
# Oneday
# Postponement

jdr <- read_csv("./data/processed/jury_data_transformed.csv")

jdr |>
  filter(county == "Imperial" & reporting_period == 2024) |>
  select(juror_yield, juror_utilization)

this_year <- year(today())

jdr_last_five <- jdr|>
  filter(year(end_date) >= this_year - 5)

  jdr_last_ten <- jdr|>
  filter(year(end_date) >= this_year - 10)

last_two_years <- jdr_last_five |> 
  pull(end_year) |> 
  unique() |> 
  sort() |> 
  tail(2)

current_year <- last_two_years[2]
prior_year <- last_two_years[1]

camcorder::gg_record(
  dir = 'visuals',
  width = 6.5,
  height = 4,
  dpi = 300,
  bg = 'white'
)

############## jury yield ################################
jury_yield <- jdr_last_five |>
  group_by(year = end_year) |>
  summarize(
    # Raw sums (used directly or as building blocks for ratios)
    s              = sum(summons, na.rm = TRUE) + sum(postin, na.rm = TRUE),
    tqa            = sum(tqa, na.rm = TRUE),
    u              = sum(unavailable, na.rm = TRUE),
    # Components for weighted ratios
    .pot_avail     = sum(potentially_available, na.rm = TRUE)
  ) 

jury_yield_gt <- jury_yield |>
  mutate(
    jy     = tqa / .pot_avail
  ) |>
  select(-starts_with(".")) |>     # drop the helper columns
  mutate(
    jy     = scales::percent(jy),
  ) |>
  arrange(desc(year)) |>
  gt(rowname_col = "year") |>
  fmt_number(
    columns = c(s, tqa, u),
    suffixing = TRUE
  ) |>
  cols_label(
    s       = md("**Summons**"),
    tqa     = md("**Qualified & Available**"),
    u     = md("**Unavailable**"),
    jy      = md("**Juror Yield**")
  )

gtsave(jury_yield_gt, "tables/jury_yield.docx")

## Plot ##
# ── Data ─────────────────────────────────────────────────────────────────────
df <- tibble(
  year        = c(2025, 2024, 2023, 2022, 2021),
  qualified   = c(5.16, 5.20, 5.07, 5.00, 3.98),
  unavailable = c(6.49, 6.35, 6.39, 5.63, 4.62),
  yield_pct   = c(44.09, 45.04, 42.99, 47.05, 46.28)
)

scale_factor <- 12 / 38

df_long <- df |>
  pivot_longer(c(qualified, unavailable), names_to = "segment", values_to = "millions")|>
  mutate(segment = factor(segment, levels = c("unavailable", "qualified")))

# ── X-axis label formatter ────────────────────────────────────────────────────
fy_labels <- function(x) paste0("FY '", substr(x, 3, 4))
df_long <- df_long |>
  mutate(
    yield_pct = round(yield_pct, 2)
  )

# ── Plot ──────────────────────────────────────────────────────────────────────
jury_yield_plot <- ggplot() +
  geom_col(
    data  = df_long,
    aes(x = factor(year), y = millions, fill = segment),
    width = 0.6
  ) +
  geom_text(
    data     = df_long,
    aes(
      x      = factor(year),
      y      = millions,
      label  = paste0(millions, "M"),
      color  = segment
    ),
    position = position_stack(vjust = 0.5),
    fontface = "bold", size = 3, family = "inter"
  ) +
  geom_line(
    data = df,
    aes(x = factor(year), y = yield_pct * scale_factor, group = 1, color = "Juror Yield"),
    linewidth = .5
  ) +
  geom_point(
    data = df,
    aes(x = factor(year), y = round(yield_pct * scale_factor, 2), color = "Juror Yield"),
    size = 2
  ) +
  geom_text(
    data = df,
    aes(x = factor(year), y = yield_pct * scale_factor, label = paste0(yield_pct, "%")),
    vjust = -1, color = "#CD9D46", size = 3, fontface = "bold", family = "inter"
  ) +
  scale_y_continuous(
    name   = "Jurors",
    labels = label_number(suffix = "M"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_x_discrete(labels = fy_labels) +
  scale_fill_manual(
    values = c(qualified = "#2172b0", unavailable = "#c2540e"),
    labels = c(qualified = "Available", unavailable = "Unavailable")
  ) +
  scale_color_manual(
    values = c("Juror Yield" = "#CD9D46", qualified = "white", unavailable = "white"),
    breaks = "Juror Yield",
    labels = c("Juror Yield" = "Juror Yield")
  ) +
  guides(
    fill  = guide_legend(order = 2),
    color = guide_legend(order = 1)
  ) +
  labs(
    title    = "Juror Yield hovers near 45% as Available and Unavailable stabilize",
    subtitle = "California statewide jury summons outcomes, FY2021–FY2025",
    x        = NULL,
    fill     = NULL,
    color    = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    text               = element_text(family = "inter"),
    plot.title         = element_markdown(
      family     = "lora",
      size       = 10,
      lineheight = 1.4,
      hjust      = 0,
      margin     = margin(r = 4, b = 4)
    ),
    plot.subtitle      = element_markdown(
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
#########################################################

############## unavailable ##############################
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

gtsave(unavailable, "tables/unavailable.docx")

###### plot #######
unavailable_long <- unavailable |>
  pivot_longer(
    cols      = c(ex, disq, dism, undel, fta, pos),  # s is excluded, stays as a column
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
  mutate(line_color = if_else(component == "Failure to Appear", "#c2540e", "grey55"))


ggplot(unavailable_long, aes(x = year, y = index)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_line(aes(group = 1, color = line_color), linewidth = 0.8) +
  geom_point(aes(color = line_color), size = 2) +
  geom_text(
    aes(label = paste0(scales::number(index, accuracy = 0.01), "x")),
    vjust = -0.8, size = 2.5, family = "inter"
  ) +
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
    text       = element_text(family = "inter"),
    plot.title = element_markdown(
      family     = "lora",
      size       = 12,
      lineheight = 1.4,
      hjust      = 0,
      margin     = margin(r = 4, b = 4)
      ),
    plot.subtitle      = element_text(family = "lora", size = 10,
                                      color = "grey30", margin = margin(b = 4)),
    strip.text = element_text(family = "lora", face = "bold", size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),  # outer margin area
    panel.background = element_rect(fill = "white", color = NA), 
  )

ggsave(
  here::here("visuals/unavailable1.png")
       , width = 6.5
       , height = 4
       #, units = "in"
       , dpi = 300
      # , scale = .9
)

width = 12,
height = 12 * 9 / 16,
dpi = 300,

#######################################################


############ Wrong way of calculating metrics (weighted by court-specific ratios) ########
jdr_last_five |> 
  group_by(end_year) |>
  summarize(
            ju = mean(juror_utilization, na.rm = T),
            pc_ttr = mean(pc_told_to_report, na.rm = T),
            pc_sfs = mean(pc_sent_for_sel, na.rm = T),
            pc_pu = mean(pc_panel_used, na.rm = T)
    )
###########################################################################################


############ Right way (summarizing statewide totals first, then calculating ratios)#######
jdr_last_five |>
  group_by(end_year) |>
  summarize(
    # Juror utilization components
    total_tqa        = sum(tqa, na.rm = TRUE),
    total_oncall     = sum(oncall, na.rm = TRUE),
    total_incourt    = sum(rel_challenge + rel_hardship + rel_perempt + jurors_sworn, na.rm = TRUE),
    total_not_reached = sum(not_reached, na.rm = TRUE),
    total_pot_avail  = sum(potentially_available, na.rm = TRUE)
  ) |>
  mutate(
    pc_told_to_report = (total_tqa - total_oncall) / total_tqa,
    pc_sent_for_sel   = (total_incourt + total_not_reached) / (total_tqa - total_oncall),
    pc_panel_used     = total_incourt / (total_incourt + total_not_reached),
    ju                = pc_told_to_report * pc_sent_for_sel * pc_panel_used
  ) |>
  select(end_year, ju, pc_told_to_report, pc_sent_for_sel, pc_panel_used)
##############################################################################################
 

jdr_last_five |>
  select(year = end_year, summons, unavailable, juror_yield, jurors_sworn, panel_cases, juror_utilization, oneday) |>
  group_by(year) |>
  summarize(
    s = sum(summons, na.rm = T),
    u = sum(unavailable, na.rm = T),
    js = scales::label_comma()(sum(jurors_sworn, na.rm = T)),
    pc = scales::label_comma()(sum(panel_cases, na.rm = T)),
    jy = scales::percent(mean(juror_yield, na.rm = T)),
    ju = scales::percent(mean(juror_utilization, na.rm = T)),
    od = sum(oneday, na.rm = T)
  ) |>
  arrange(desc(year)) |>
  gt(rowname_col = "year") |>
  fmt_number(
    columns = c(s, u, od),
    suffixing = T
  ) |>
  cols_label(
    s = md("**Summons**"),
    u = md("**Unavailable**"),
    jy = md("**Juror Yield**"),
    js = md("**Jurors Sworn**"),
    pc = md("**Cases**"),
    ju = md("**Juror Utilization**"),
    od = md("**One Day**")
  )
################# Correct table for Utilization and components ######
jdr_last_five |>
  group_by(year = end_year) |>
  summarize(
    total_tqa         = sum(tqa, na.rm = TRUE),
    total_oncall      = sum(oncall, na.rm = TRUE),
    total_incourt     = sum(rel_challenge + rel_hardship + rel_perempt + jurors_sworn, na.rm = TRUE),
    total_not_reached = sum(not_reached, na.rm = TRUE),
    total_pot_avail   = sum(potentially_available, na.rm = TRUE),
    total_tqa_raw     = sum(tqa, na.rm = TRUE)
  ) |>
  mutate(
    pc_told_to_report = (total_tqa - total_oncall) / total_tqa,
    pc_sent_for_sel   = (total_incourt + total_not_reached) / (total_tqa - total_oncall),
    pc_panel_used     = total_incourt / (total_incourt + total_not_reached),
    ju                = pc_told_to_report * pc_sent_for_sel * pc_panel_used
  ) |>
  mutate(
    pc_told_to_report = scales::percent(pc_told_to_report),
    pc_sent_for_sel   = scales::percent(pc_sent_for_sel),
    pc_panel_used     = scales::percent(pc_panel_used),
    ju                = scales::percent(ju)
  ) |>
  select(year, pc_told_to_report, pc_sent_for_sel, pc_panel_used, ju) |>
  arrange(desc(year)) |>
  gt(rowname_col = "year") |>
  cols_label(
    pc_told_to_report = md("**% Told to Report**"),
    pc_sent_for_sel   = md("**% Sent for Selection**"),
    pc_panel_used     = md("**% Panel Used**"),
    ju                = md("**Juror Utilization**")
  )
############################################################


############## All metrics ################################
jdr_last_five |>
  group_by(year = end_year) |>
  summarize(
    # Raw sums (used directly or as building blocks for ratios)
    s              = sum(summons, na.rm = TRUE) + sum(postin, na.rm = TRUE),
    tqa            = sum(tqa, na.rm = TRUE),
    ip             = sum(inperson, na.rm = TRUE),
    od             = sum(oneday, na.rm = TRUE),
    sw             = sum(jurors_sworn, na.rm = TRUE),
    fta_abs        = sum(fta, na.rm = TRUE),
    
    # Components for weighted ratios
    .pot_avail     = sum(potentially_available, na.rm = TRUE),
    .oncall        = sum(oncall, na.rm = TRUE),
    .postin        = sum(postin, na.rm = TRUE),
    .postout       = sum(postout, na.rm = TRUE),
    .fta           = sum(fta, na.rm = TRUE),
    .incourt       = sum(rel_challenge + rel_hardship + rel_perempt + jurors_sworn, na.rm = TRUE),
    .not_reached   = sum(not_reached, na.rm = TRUE)
  ) |>
  mutate(
    jy     = tqa / .pot_avail,
    ttr    = (tqa - .oncall) / tqa,
    sfs    = (.incourt + .not_reached) / (tqa - .oncall),
    pu     = .incourt / (.incourt + .not_reached),
    ju     = ttr * sfs * pu,
    ppr    = paste0("1:", round(.postin / .postout, 2)),
    fta_pc = .fta / .pot_avail
  ) |>
  select(-starts_with(".")) |>     # drop the helper columns
  mutate(
    jy     = scales::percent(jy),
    ju     = scales::percent(ju),
    ttr    = scales::percent(ttr),
    sfs    = scales::percent(sfs),
    pu     = scales::percent(pu),
    fta_pc = scales::percent(fta_pc)
  ) |>
  arrange(desc(year)) |>
  gt(rowname_col = "year") |>
  fmt_number(
    columns = c(s, tqa, ip, od, sw, fta_abs),
    suffixing = TRUE
  ) |>
  cols_label(
    s       = md("**Summons**"),
    tqa     = md("**Qualified & Available**"),
    od      = md("**One Day**"),
    ip      = md("**In Person**"),
    sw      = md("**Jurors Sworn**"),
    jy      = md("**Juror Yield**"),
    ju      = md("**Juror Utilization**"),
    ppr     = md("**Postponement Ratio**"),
    ttr     = md("**Told To Report**"),
    sfs     = md("**Sent For Selection**"),
    pu      = md("**Panel Used**"),
    fta_abs = md("**FTA**"),
    fta_pc  = md("**% FTA**")
  )

jdr_last_five |>
  group_by(year = end_year) |>
  summarize(
    s  = sum(summons, na.rm = TRUE) + sum(postin, na.rm = TRUE),
    tqa = sum(tqa, na.rm = TRUE),
    sw  = sum(jurors_sworn, na.rm = TRUE)
  ) |>
  arrange(desc(year)) |>
  gt(rowname_col = "year") |>
  fmt_number(
    columns = c(s, tqa, sw),
    suffixing = TRUE
  ) |>
  cols_label(
    s   = md("**Summons**"),
    tqa = md("**Qualified & Available**"),
    sw  = md("**Jurors Sworn**")
  )
#######################################################

jdr_last_five |>
 # select(year = end_year, summons, unavailable, juror_yield, jurors_sworn, panel_cases, juror_utilization, oneday) |>
  group_by(year = end_year) |>
  summarize(
    s = sum(summons, na.rm = T),
    tqa = sum(tqa, na.rm = T),
    ip = sum(inperson, na.rm = T),
    sw = sum(jurors_sworn, na.rm = T),
    jy = scales::percent(mean(juror_yield, na.rm = T)),
    ju = scales::percent(mean(juror_utilization, na.rm = T)),
    ppr = paste0("1:", round(sum(postin, na.rm = T) / sum(postout, na.rm = T), 2)),
    ttr = scales::percent(mean(pc_told_to_report, na.rm = T)),
    sfs = scales::percent(mean(pc_sent_for_sel, na.rm = T)),
    pu = scales::percent(mean(pc_panel_used, na.rm = T)),
    fta_abs = sum(fta, na.rm = T),
    fta_pc = scales::percent(mean((fta / potentially_available), na.rm = T))
  ) |>
  arrange(desc(year)) |>
  gt(rowname_col = "year") |>
  fmt_number(
    columns = c(s, tqa, ip, sw, fta_abs),
    suffixing = T
  ) |>
  cols_label(
    s = md("**Summons**"),
    tqa = md("**Qualified & Available**"),
    ip = md("**In Person**"),
    sw = md("**Jurors Sworn**"),
    jy = md("**Juror Yield**"),
    ju = md("**Juror Utilization**"),
    ppr = md("**Postponement Ratio**"),
    ttr = md("**Told To Report**"),
    sfs = md("**Sent For Selection**"),
    pu = md("**Panel Used**"),
    fta_abs = md("**FTA**"),
    fta_pc = md("**%FTA**")
  )

jdr_last_five |>
  select(year = end_year, ends_with("prop")) |>
  group_by(year) |>
  summarize(
    pa = scales::percent(mean(potentially_available_prop, na.rm = T)),
    exc = scales::percent(mean(excused_prop, na.rm = T)),
    dsq = scales::percent(mean(disqual_prop, na.rm = T)),
    tqa = scales::percent(mean(tqa_prop, na.rm = T)),
    srv = scales::percent(mean(serving_prop, na.rm = T)),
    inc = scales::percent(mean(incourt_prop, na.rm = T)),
    prmpt = scales::percent(mean(perempt_prop, na.rm = T)),
    ond = scales::percent(mean(one_day_prop, na.rm = T)),
    jdy = scales::percent(mean(jdays_prop, na.rm = T)),
    pnl = scales::percent(mean(panel_prop, na.rm = T)),
    c_pnl = scales::percent(mean(criminal_panels_prop, na.rm = T)),
    c_swo = scales::percent(mean(criminal_sworn_prop, na.rm = T))
  ) |>
  arrange(desc(year)) |>
  gt(rowname_col = "year") |>
  # fmt_number(
  #   columns = c(s, u, od),
  #   suffixing = T
  # )|>
  cols_label(
    pa = md("**Potentially Available**"),
    exc = md("**Excused**"),
    dsq = md("**Disqualified**"),
    tqa = md("**TQA**"),
    srv = md("**Serving**"),
    inc = md("**In Court**"),
    prmpt = md("**Perempt**"),
    ond = md("**One Day**"),
    jdy = md("**Jury Days**"),
    pnl = md("**Panel**"),
    c_pnl = md("**Criminal PAnel**"),
    c_swo = md("**Criminal Sworn**")
  )
 

############### AAPE since 2021 #######################
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
    list(chg = ~(.x - lead(.x)) * 100),  # Convert to percentage points
    .names = "{.col}_{.fn}"
  )) |>
  gt(rowname_col = "year") |>
  # Format MAPE columns as percentages
  fmt_percent(
    columns = c(pa, exc, dsq, tqa, srv, inc, prmpt, ond, jdy, pnl, c_pnl, c_swo),
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
    pa = "MAPE", pa_chg = "Δ pp",
    exc = "MAPE", exc_chg = "Δ pp",
    dsq = "MAPE", dsq_chg = "Δ pp",
    tqa = "MAPE", tqa_chg = "Δ pp",
    srv = "MAPE", srv_chg = "Δ pp",
    inc = "MAPE", inc_chg = "Δ pp",
    prmpt = "MAPE", prmpt_chg = "Δ pp",
    ond = "MAPE", ond_chg = "Δ pp",
    jdy = "MAPE", jdy_chg = "Δ pp",
    pnl = "MAPE", pnl_chg = "Δ pp",
    c_pnl = "MAPE", c_pnl_chg = "Δ pp",
    c_swo = "MAPE", c_swo_chg = "Δ pp"
  )

###########################################################
 


jdr_last_five |>
  filter(end_year %in% last_two_years) |>
  select(county, year = end_year, ends_with("prop")) |>
  group_by(county, year) |>
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
    c_swo = mean(abs(criminal_sworn_prop - 1), na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = year,
    values_from = c(pa, exc, dsq, tqa, srv, inc, prmpt, ond, jdy, pnl, c_pnl, c_swo),
    names_glue = "{.value}_{year}"
  ) |>
  mutate(
    pa_chg = (!!sym(paste0("pa_", current_year)) - !!sym(paste0("pa_", prior_year))) * 100,
    exc_chg = (!!sym(paste0("exc_", current_year)) - !!sym(paste0("exc_", prior_year))) * 100,
    dsq_chg = (!!sym(paste0("dsq_", current_year)) - !!sym(paste0("dsq_", prior_year))) * 100,
    tqa_chg = (!!sym(paste0("tqa_", current_year)) - !!sym(paste0("tqa_", prior_year))) * 100,
    srv_chg = (!!sym(paste0("srv_", current_year)) - !!sym(paste0("srv_", prior_year))) * 100,
    inc_chg = (!!sym(paste0("inc_", current_year)) - !!sym(paste0("inc_", prior_year))) * 100,
    prmpt_chg = (!!sym(paste0("prmpt_", current_year)) - !!sym(paste0("prmpt_", prior_year))) * 100,
    ond_chg = (!!sym(paste0("ond_", current_year)) - !!sym(paste0("ond_", prior_year))) * 100,
    jdy_chg = (!!sym(paste0("jdy_", current_year)) - !!sym(paste0("jdy_", prior_year))) * 100,
    pnl_chg = (!!sym(paste0("pnl_", current_year)) - !!sym(paste0("pnl_", prior_year))) * 100,
    c_pnl_chg = (!!sym(paste0("c_pnl_", current_year)) - !!sym(paste0("c_pnl_", prior_year))) * 100,
    c_swo_chg = (!!sym(paste0("c_swo_", current_year)) - !!sym(paste0("c_swo_", prior_year))) * 100,
    max_abs_chg = pmax(
      abs(pa_chg), abs(exc_chg), abs(dsq_chg), abs(tqa_chg),
      abs(srv_chg), abs(inc_chg), abs(prmpt_chg), abs(ond_chg),
      abs(jdy_chg), abs(pnl_chg), abs(c_pnl_chg), abs(c_swo_chg),
      na.rm = TRUE
    )
  ) |>
  # arrange(desc(max_abs_chg)) |>
  arrange(desc(!!sym(paste0("pa_", current_year)))) |>
  select(county, ends_with(paste0("_", current_year)), ends_with(paste0("_", prior_year)), ends_with("_chg"), -max_abs_chg) |>
  gt(rowname_col = "county") |>
  fmt_percent(
    columns = matches(paste0("_(", current_year, "|", prior_year, ")$")),
    decimals = 1
  ) |>
  fmt_number(
    columns = ends_with("_chg"),
    decimals = 1,
    force_sign = TRUE
  ) |>
  tab_spanner(label = md("**Potentially Available**"), columns = starts_with("pa_")) |>
  tab_spanner(label = md("**Excused**"), columns = starts_with("exc_")) |>
  tab_spanner(label = md("**Disqualified**"), columns = starts_with("dsq_")) |>
  tab_spanner(label = md("**TQA**"), columns = starts_with("tqa_")) |>
  tab_spanner(label = md("**Serving**"), columns = starts_with("srv_")) |>
  tab_spanner(label = md("**In Court**"), columns = starts_with("inc_")) |>
  tab_spanner(label = md("**Perempt**"), columns = starts_with("prmpt_")) |>
  tab_spanner(label = md("**One Day**"), columns = starts_with("ond_")) |>
  tab_spanner(label = md("**Jury Days**"), columns = starts_with("jdy_")) |>
  tab_spanner(label = md("**Panel**"), columns = starts_with("pnl_")) |>
  tab_spanner(label = md("**Criminal Panel**"), columns = starts_with("c_pnl_")) |>
  tab_spanner(label = md("**Criminal Sworn**"), columns = starts_with("c_swo_")) |>
  cols_label(
    !!sym(paste0("pa_", current_year)) := as.character(current_year),
    !!sym(paste0("pa_", prior_year)) := as.character(prior_year),
    pa_chg = "Δ pp",
    !!sym(paste0("exc_", current_year)) := as.character(current_year),
    !!sym(paste0("exc_", prior_year)) := as.character(prior_year),
    exc_chg = "Δ pp",
    !!sym(paste0("dsq_", current_year)) := as.character(current_year),
    !!sym(paste0("dsq_", prior_year)) := as.character(prior_year),
    dsq_chg = "Δ pp",
    !!sym(paste0("tqa_", current_year)) := as.character(current_year),
    !!sym(paste0("tqa_", prior_year)) := as.character(prior_year),
    tqa_chg = "Δ pp",
    !!sym(paste0("srv_", current_year)) := as.character(current_year),
    !!sym(paste0("srv_", prior_year)) := as.character(prior_year),
    srv_chg = "Δ pp",
    !!sym(paste0("inc_", current_year)) := as.character(current_year),
    !!sym(paste0("inc_", prior_year)) := as.character(prior_year),
    inc_chg = "Δ pp",
    !!sym(paste0("prmpt_", current_year)) := as.character(current_year),
    !!sym(paste0("prmpt_", prior_year)) := as.character(prior_year),
    prmpt_chg = "Δ pp",
    !!sym(paste0("ond_", current_year)) := as.character(current_year),
    !!sym(paste0("ond_", prior_year)) := as.character(prior_year),
    ond_chg = "Δ pp",
    !!sym(paste0("jdy_", current_year)) := as.character(current_year),
    !!sym(paste0("jdy_", prior_year)) := as.character(prior_year),
    jdy_chg = "Δ pp",
    !!sym(paste0("pnl_", current_year)) := as.character(current_year),
    !!sym(paste0("pnl_", prior_year)) := as.character(prior_year),
    pnl_chg = "Δ pp",
    !!sym(paste0("c_pnl_", current_year)) := as.character(current_year),
    !!sym(paste0("c_pnl_", prior_year)) := as.character(prior_year),
    c_pnl_chg = "Δ pp",
    !!sym(paste0("c_swo_", current_year)) := as.character(current_year),
    !!sym(paste0("c_swo_", prior_year)) := as.character(prior_year),
    c_swo_chg = "Δ pp"
  ) 
 

 
# Get the last 5 years dynamically
last_five_years <- jdr_last_five |> 
  pull(end_year) |> 
  unique() |> 
  sort() |> 
  tail(5)

current_year <- max(last_five_years)

jdr_last_five |>
  filter(end_year %in% last_five_years) |>
  select(county, year = end_year, ends_with("prop")) |>
  group_by(county, year) |>
  summarize(
    pa = mean(abs(potentially_available_prop - 1), na.rm = TRUE),
    tqa = mean(abs(tqa_prop - 1), na.rm = TRUE),
    inc = mean(abs(incourt_prop - 1), na.rm = TRUE),
    pnl = mean(abs(panel_prop - 1), na.rm = TRUE),
    jdy = mean(abs(jdays_prop - 1), na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = year,
    values_from = c(pa, tqa, inc, pnl, jdy),
    names_glue = "{.value}_{year}"
  ) |>
  arrange(desc(!!sym(paste0("pa_", current_year)))) |>
  slice_head(n = 10) |>
  gt(rowname_col = "county") |>
  fmt_percent(
    columns = everything(),
    decimals = 1
  ) |>
  tab_spanner(label = md("**Potentially Available**"), columns = starts_with("pa_")) |>
  tab_spanner(label = md("**TQA**"), columns = starts_with("tqa_")) |>
  tab_spanner(label = md("**In Court**"), columns = starts_with("inc_")) |>
  tab_spanner(label = md("**Panel**"), columns = starts_with("pnl_")) |>
  tab_spanner(label = md("**Jury Days**"), columns = starts_with("jdy_"))
 
# Create a priority ranking weighted by court size (summons)
priority_analysis <- jdr_last_five |>
  filter(end_year == max(end_year)) |>
  select(county, summons, ends_with("prop")) |>
  group_by(county) |>
  summarize(
    total_summons = sum(summons, na.rm = TRUE),
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
    c_swo = mean(abs(criminal_sworn_prop - 1), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    # Count how many metrics exceed 5%
    n_over_5pct = rowSums(across(pa:c_swo, ~.x > 0.05), na.rm = TRUE),
    # Count how many metrics exceed 10%
    n_over_10pct = rowSums(across(pa:c_swo, ~.x > 0.10), na.rm = TRUE),
    # Maximum MAPE across all metrics
    max_mape = pmax(pa, exc, dsq, tqa, srv, inc, prmpt, ond, jdy, pnl, c_pnl, c_swo, na.rm = TRUE),
    # Average MAPE across all metrics
    avg_mape = rowMeans(across(pa:c_swo), na.rm = TRUE),
    # Normalize summons to 0-1 scale for weighting
    summons_weight = total_summons / max(total_summons, na.rm = TRUE),
    # Priority score: base score weighted by court size
    base_score = n_over_10pct * 10 + n_over_5pct * 3 + max_mape * 100,
    priority_score = base_score * (1 + summons_weight)  # Larger courts get up to 2x multiplier
  ) |>
  arrange(desc(priority_score))

# View top priority counties
priority_analysis |>
  select(county, total_summons, n_over_5pct, n_over_10pct, max_mape, avg_mape, priority_score) |>
  head(20) |>
  gt() |>
  fmt_number(columns = total_summons, decimals = 0) |>
  fmt_percent(columns = c(max_mape, avg_mape), decimals = 1) |>
  fmt_number(columns = priority_score, decimals = 1) |>
  cols_label(
    county = "County",
    total_summons = "Summons",
    n_over_5pct = "# Metrics >5%",
    n_over_10pct = "# Metrics >10%",
    max_mape = "Max MAPE",
    avg_mape = "Avg MAPE",
    priority_score = "Priority Score"
  ) |>
  tab_header(title = "County Priority List for Data Quality Contact (Weighted by Court Size)")
 


priority_analysis |>
  head(10) |>
  arrange(desc(pa))

jdr_last_five |> filter(county == "Inyo") |>
  select(end_date, potentially_available, potentially_available_sum)

pa_absolutes <- function(county_choice) {
  jdr_last_five |> filter(county == {{county_choice}}) |>
    select(end_date, potentially_available, potentially_available_sum)
}

inc_absolutes <- function(county_choice) {
  jdr_last_five |> filter(county == {{county_choice}}) |>
    select(end_date, incourt, rel_challenge, rel_hardship, rel_perempt, not_reached, jurors_sworn)
}

pa_absolutes("Inyo")
pa_absolutes("San Bernardino")

jdr_last_five |> filter(county == "Inyo") |>
  select(end_date, ends_with("prop"))


 
mapes <- jdr_last_five |>
  filter(end_year == max(end_year)) |>
  select(county, summons, ends_with("prop")) |>
  group_by(county) |>
  summarize(
    total_summons = sum(summons, na.rm = TRUE),
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
    c_swo = mean(abs(criminal_sworn_prop - 1), na.rm = TRUE),
    .groups = "drop"
  )

mapes |>
  arrange(desc(inc)) |> print(n = 20)

mapes |> filter(county %in% c("Marin", "Napa", "San Francisco"))

pa_absolutes("Lassen")
pa_absolutes("Shasta")
inc_absolutes("Colusa")



