library(ggplot2)
library(readr)
library(lubridate)
library(here)
library(dplyr)
library(tidyr)
library(ggiraph)
library(ggrepel)
library(htmlwidgets)

# Point R to portable pandoc
# Sys.setenv(PATH = paste("C:/Users/JKent/Documents/Projects/Jury-Data-Report/tools/pandoc/bin", 
#                         Sys.getenv("PATH"), 
#                         sep = ";"))

jdr <- read_csv(here("data/processed/jury_data_transformed.csv"))

this_year <- year(today())

jdr_this_year <- jdr |>
  filter(year(end_date) == this_year-1)

pie_data <- jdr_this_year |>
  pivot_longer(cols = c(oncall, inperson),
               names_to = "type",
               values_to = "count") |>
  group_by(type) |>
  summarise(total = sum(count, na.rm = TRUE), .groups = "drop") |>
  mutate(perc = total / sum(total)) |>
  mutate(
    type_label = case_when(
      type == "inperson" ~ "In Person",
      type == "oncall" ~ "On Call"
    ),
    label = paste0(type_label, "\n", scales::percent(perc)),
    tooltip = paste0(type_label, ": ", scales::comma(total))
  ) |>
  arrange(desc(perc))

pie_chart <- pie_data |>
  ggplot(aes(x = "", y = perc)) +
  geom_col_interactive(aes(fill = reorder(type, perc),
                            tooltip = tooltip),
                       width = 1,
                       col = "white") +
  geom_label(x = 1.2,
            aes(y = cumsum(perc) - perc / 2,
                label = label),
            fill = "white",
            color = "#5C5C5C",  # Add this line
            size = 5,
            family = "open-sans") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c(inperson = "#d379b0", oncall = "#e1e1e1")) +
  scale_color_manual(values = c(inperson = "#d379b0", oncall = "#e1e1e1")) +
  theme_void() +
  guides(fill = FALSE, color = FALSE)

pie_viz <- girafe(
  ggobj = pie_chart,
  width_svg = 6.5*1.5,
  height_svg = 4*1.5,
  options = list(
    opts_tooltip(
      css = 'background:white; border:2px solid black;
             border-radius:5px; padding:7px; font-size:18px;
             color: #5C5C5C;
             font-family: open-sans;
             box-shadow: 2px 2px 5px rgba(0,0,0,0.3);'
    )
  )
)

# Save as self-contained HTML for Drupal
saveWidget(pie_viz,
           file = here("code/viz/output/inperson_pie.html"),
           selfcontained = TRUE)


