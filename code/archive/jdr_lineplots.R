library(tidyverse)
library(sysfonts)
library(ggrepel)

jdr <- read_csv("../../Jury/JDR/data/processed/jury_data_transformed.csv") |>
  filter(end_year != 6302) |>
  mutate(end_year = as.double(end_year))

font_add_google("Source Sans Pro")
font_add_google("Merriweather")
font_families_google()
sysfonts::font_families()

description_color <- "grey30"

x_labels <- function(x) paste0("'", substr(as.character(x), 3, 4))

jdr_last_10 <- jdr |> filter (end_year >= 2015)

# --------------------- JUROR YIELD ----------------------------------

yield_summary <- jdr_last_10 |>
  group_by(end_year) |>
  summarize(
            tqa = sum(tqa, na.rm = T),
            summons = sum(summons, na.rm = T),
            postin = sum(postin, na.rm = T),
            yield = tqa/(summons + postin),
            .groups = "drop"
    )

yield_first_last = yield_summary |>
  filter(end_year == min(end_year) | end_year == max(end_year))

yield_last_two = yield_summary |>
  filter(end_year == max(end_year) | 
           end_year == max(end_year) - 1)

yield_lineplot <- yield_summary |>
  ggplot(aes(x = end_year, y = yield)) +
  geom_line(color = "#73B3E7", linewidth = 1.5) +
  geom_point(data = 
               #yield_first_last
               yield_last_two
             , color = "#73B3E7", size = 3) +
  geom_point(data = 
               #yield_first_last
               yield_last_two
               , color = "#73B3E7", size = 5, shape = "circle open") +
  
  geom_text_repel(data = 
              yield_first_last
              #yield_last_two
              , 
            aes(label = scales::percent(yield, accuracy = 0.1)), 
            vjust = -1, size = 9, family = "serif",
            nudge_y = 0.05,    # Nudges the label higher on the y-axis
            segment.size = 0.5, # Controls the thickness of the segment line
           # segment.curvature = -0.2, # Adds some curvature to the segment
            segment.angle = 20,       # Controls the angle of the segment line
            #segment.ncp = 3           # Controls the smoothness of the curvature
           box.padding = 0.5,
           segment.color = "grey"
  ) +
  
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(2015, 2025, 1),
                     labels = x_labels) +
  labs(
    title = "Juror Yield Over Time",
    x = "Year", 
    y = "Juror Yield (%)") + # Axis labels
  theme_minimal(
    base_family = "serif"
  ) +
  coord_cartesian(xlim = c(2015, 2025),
                  ylim = c(.0, 1)) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 28),
    axis.title.y = element_blank()
    ,
    plot.title = element_text(
      size = 35,
      color = "#C2850C",
      hjust = 0, # Align title completely to the left
      vjust = 1.5,
      face = "bold",
      family = "serif",
      margin = margin(20, 0, 20, 0)
    ),
    plot.margin = margin(t = 2, l = 5, r = 5, b = 5, unit = "mm"),
    text = element_text(color = "black")
    #, aspect.ratio = 0.20
  )

ggsave("./visuals/juror_yield_last_two.png", yield_lineplot, width = 18, height = 6)


# --------------------------- JUROR UTILIZATION LINEPLOTs -------------------

utilization_summary <- jdr_last_10 |>
  group_by(end_year) |>
  summarize(challenge = sum(rel_challenge, na.rm = T),
            hardship = sum(rel_hardship, na.rm = T),
            peremptory = sum(rel_perempt, na.rm = T),
            sworn = sum(jurors_sworn, na.rm = T),
            not_reached = sum(not_reached, na.rm = T),
            
            # Percent Panel Used
            panel_used = (challenge + hardship + peremptory + sworn) /
              (challenge + hardship + peremptory + sworn + not_reached),
            
            # Percent Sent for Selection
            tqa = sum(tqa, na.rm = T),
            on_call = sum(oncall, na.rm = T),
            sent_for_sel = (challenge + hardship + peremptory + sworn + not_reached) / 
              (tqa - on_call),
            
            # Percent Told to Report
            told_to_report = (tqa - on_call) / tqa,
            
            # Jury Utilization
            juror_utilization = panel_used * sent_for_sel * told_to_report
  )
  #            |> 
  # select(end_year, panel_used, sent_for_sel, told_to_report, juror_utilization)

# First and last years of data collection
utilization_first_last <- utilization_summary |>
  filter(end_year == min(end_year) | end_year == max(end_year))

# Three most recent years of data collection
utilization_last_two <- utilization_summary |>
  filter(end_year == max(end_year) | 
           end_year == max(end_year) - 1)


utilization_lineplot <- utilization_summary |>
  ggplot(aes(end_year, juror_utilization)) +
  geom_line(color = "#73B3E7", linewidth = 1.5) +
  geom_point(data = 
               #yield_first_last
               utilization_last_two
             , color = "#73B3E7", size = 3) +
  geom_point(data = 
               #yield_first_last
               utilization_last_two
             , color = "#73B3E7", size = 5, shape = "circle open") +
  scale_x_continuous(breaks = seq(2013, 2023, 1),
                     labels = x_labels) +
  scale_y_continuous(breaks = seq(0, 1, 0.2) # could change to 0.70 as top break
                     , labels = scales::label_percent()) +
  labs(
    title = "Juror Utilization Over Time"
  ) +
  geom_text_repel(data = 
                    #utilization_first_last
                    utilization_last_two
                  , 
                  aes(label = scales::percent(juror_utilization, accuracy = 0.1)), 
                  vjust = -1, size = 9, family = "serif",
                  nudge_y = 0.05,    
                  segment.size = 0.5, 
                  # segment.curvature = -0.2, 
                  segment.angle = 20,       
                  #segment.ncp = 3           
                  box.padding = 0.5,
                  segment.color = "grey"
  ) +
  theme_minimal(
    base_family = "serif"
  ) +
  coord_cartesian(xlim = c(2013, 2023),
                  ylim = c(.0, 1)) + # Again, could change to 0.70
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 28),
    plot.title = element_text(
      size = 35,
      color = "#C2850C",
      face = "bold",
      family = "serif",
      margin = margin(20, 0, 20, 0)
    ),
    plot.margin = margin(t = 2, l = 5, unit = "mm"),
    text = element_text(color = description_color)
    # , aspect.ratio = 0.25
  )

ggsave("./visuals/juror_utilization.png", utilization_lineplot, width = 18, height = 6)

# ------------------------------------ PC PANEL USED --------------------------------

panel_used_lineplot <- utilization_summary |>
  ggplot(aes(end_year, panel_used)) +
  geom_line(color = "#73B3E7", linewidth = 1.5) +
  geom_point(data = 
               #yield_first_last
               utilization_last_two
             , color = "#73B3E7", size = 3) +
  geom_point(data = 
               #yield_first_last
               utilization_last_two
             , color = "#73B3E7", size = 5, shape = "circle open") +
  scale_x_continuous(breaks = seq(2013, 2023, 1),
                     labels = x_labels) + 
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::label_percent()) +
  labs(
    title = "Panel Used (%) Over Time",
    x = "Year", 
    y = "Panel Used (%)") + # Axis labels
  geom_text_repel(data = 
                    #utilization_first_last
                    utilization_last_two
                  , 
                  aes(label = scales::percent(panel_used, accuracy = 0.1)), 
                  vjust = -1, size = 9, family = "serif",
                  nudge_y = 0.05,    
                  segment.size = 0.5, 
                  # segment.curvature = -0.2, 
                  segment.angle = 20,       
                  #segment.ncp = 3           
                  box.padding = 0.5,
                  segment.color = "grey"
  ) +
  theme_minimal(
    base_family = "serif"
  ) +
  coord_cartesian(xlim = c(2013, 2023),
                  ylim = c(0, 1)) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 28),
    plot.title = element_text(
      size = 35,
      color = "#C2850C",
      hjust = 0, # Align title completely to the left
      vjust = 1.5,
      face = "bold",
      family = "serif",
      margin = margin(20, 0, 20, 0)
    ),
    plot.margin = margin(t = 2, l = 5, r = 5, b = 5, unit = "mm"),
    text = element_text(color = description_color)
    #, aspect.ratio = 0.20
  )

ggsave("./visuals/panel_used.png", panel_used_lineplot, width = 18, height = 6)

# ------------------------------------ PC SENT FOR SELECTION -------------------

sent_for_sel_lineplot <- utilization_summary |>
  ggplot(aes(end_year,sent_for_sel)) +
  geom_line(color = "#73B3E7", linewidth = 1.5) +
  geom_point(data = 
               #yield_first_last
               utilization_last_two
             , color = "#73B3E7", size = 3) +
  geom_point(data = 
               #yield_first_last
               utilization_last_two
             , color = "#73B3E7", size = 5, shape = "circle open") +
  scale_x_continuous(breaks = seq(2013, 2023, 1),
                     labels = x_labels) + 
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::label_percent()) +
  labs(
    title = "Sent For Selection (%) Over Time",
    x = "Year", 
    y = "Sent For Selection (%)") + # Axis labels
  geom_text_repel(data = 
                    #utilization_first_last
                    utilization_last_two
                  , 
                  aes(label = scales::percent(sent_for_sel, accuracy = 0.1)), 
                  vjust = -1, size = 9, family = "serif",
                  nudge_y = 0.05,    
                  segment.size = 0.5, 
                  # segment.curvature = -0.2, 
                  segment.angle = 20,       
                  #segment.ncp = 3           
                  box.padding = 0.5,
                  segment.color = "grey"
  ) +
  theme_minimal(
    base_family = "serif"
  ) +
  coord_cartesian(xlim = c(2013, 2023),
                  ylim = c(0, 1)) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 28),
    plot.title = element_text(
      size = 35,
      color = "#C2850C",
      hjust = 0, # Align title completely to the left
      vjust = 1.5,
      face = "bold",
      family = "serif",
      margin = margin(20, 0, 20, 0)
    ),
    plot.margin = margin(t = 2, l = 5, r = 5, b = 5, unit = "mm"),
    text = element_text(color = description_color)
    #, aspect.ratio = 0.20
  )

ggsave("./visuals/sent_for_sel.png", sent_for_sel_lineplot, width = 18, height = 6)


# ------------------------- PC TOLD TO REPORT ----------------------------------

told_to_report_lineplot <- utilization_summary |>
  ggplot(aes(end_year, told_to_report)) +
  geom_line(color = "#73B3E7", linewidth = 1.5) +
  geom_point(data = 
               #yield_first_last
               utilization_last_two
             , color = "#73B3E7", size = 3) +
  geom_point(data = 
               #yield_first_last
               utilization_last_two
             , color = "#73B3E7", size = 5, shape = "circle open") +
  scale_x_continuous(breaks = seq(2013, 2023, 1),
                     labels = x_labels) + 
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::label_percent()) +
  labs(
    title = "Told To Report (%) Over Time",
    x = "Year", 
    y = "Told To Report (%)") + # Axis labels
  geom_text_repel(data = 
                    #utilization_first_last
                    utilization_last_two
                  , 
                  aes(label = scales::percent(told_to_report, accuracy = 0.1)), 
                  vjust = -1, size = 9, family = "serif",
                  nudge_y = 0.05,    
                  segment.size = 0.5, 
                  # segment.curvature = -0.2, 
                  segment.angle = 20,       
                  #segment.ncp = 3           
                  box.padding = 0.5,
                  segment.color = "grey"
  ) +
  theme_minimal(
    base_family = "serif"
  ) +
  coord_cartesian(xlim = c(2013, 2023),
                  ylim = c(0, 1)) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 28),
    plot.title = element_text(
      size = 35,
      color = "#C2850C",
      hjust = 0, # Align title completely to the left
      vjust = 1.5,
      face = "bold",
      family = "serif",
      margin = margin(20, 0, 20, 0)
    ),
    plot.margin = margin(t = 2, l = 5, r = 5, b = 5, unit = "mm"),
    text = element_text(color = description_color)
    #, aspect.ratio = 0.20
  )

ggsave("./visuals/told_to_report.png", told_to_report_lineplot, width = 18, height = 6)

# ------------------------------- POSTPONEMENT RATIO --------------------------- 

postponement_summary <- jdr_last_10 |>
  group_by(end_year) |>
  summarize(
            postout = sum(postout, na.rm = T),
            postin = sum(postin, na.rm = T),
            postponement_ratio = postout / postin
            )

# First and last years of data collection
postponement_first_last <- postponement_summary |>
  filter(end_year == min(end_year) | end_year == max(end_year))

# Three most recent years of data collection
postponement_last_two <- postponement_summary |>
  filter(end_year == max(end_year) | 
           end_year == max(end_year) - 1)

ratio_format <- function(x) {
  paste0("1:", round(x, 1))
}

postponement_ratio_lineplot <- postponement_summary |>
  ggplot(aes(end_year, postponement_ratio)) +
  geom_line(color = "#73B3E7", linewidth = 1.5) +
  geom_point(data = 
               #yield_first_last
               postponement_last_two
             , color = "#73B3E7", size = 3) +
  geom_point(data = 
               #yield_first_last
               postponement_last_two
             , color = "#73B3E7", size = 5, shape = "circle open") +
  scale_x_continuous(breaks = seq(2013, 2023, 1),
                     labels = x_labels) + 
  scale_y_continuous(breaks = seq(0.2, 1.8, 0.4), labels = ratio_format) +
  labs(
    title = "Postponement Ratio Over Time",
    x = "Year", 
    y = "Postponement Ratio") + # Axis labels
  geom_text_repel(data = 
                    #utilization_first_last
                    postponement_last_two
                  , 
                  aes(label = paste0("1:", round(postponement_ratio, 2))), 
                  vjust = -1, size = 9, family = "serif",
                  nudge_y = 0.05,    
                  segment.size = 0.5, 
                  # segment.curvature = -0.2, 
                  segment.angle = 20,       
                  #segment.ncp = 3           
                  box.padding = 0.5,
                  segment.color = "grey"
  ) +
  theme_minimal(
    base_family = "serif"
  ) +
  coord_cartesian(xlim = c(2013, 2023),
                  ylim = c(.2, 1.8)) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 28),
    plot.title = element_text(
      size = 35,
      color = "#C2850C",
      hjust = 0, # Align title completely to the left
      vjust = 1.5,
      face = "bold",
      family = "serif",
      margin = margin(20, 0, 20, 0)
    ),
    plot.margin = margin(t = 2, l = 5, r = 5, b = 5, unit = "mm"),
    text = element_text(color = description_color)
    #, aspect.ratio = 0.20
  )

ggsave("./visuals/postponement_ratio_lineplot.png", postponement_ratio_lineplot, width = 18, height = 6)

