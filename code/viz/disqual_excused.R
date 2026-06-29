library(patchwork)
library(tidyverse)

# Calculate max value for both datasets to align axes
max_val <- max(
  jdr_last_five |>
    filter(end_year == 2025) |>
    select(
      disqual_citizen,
      disqual_18y,
      disqual_nores,
      disqual_nocal,
      disqual_noenglish,
      disqual_conserv,
      disqual_fel
    ) |>
    summarize(across(everything(), ~ sum(., na.rm = TRUE))) |>
    max(),
  jdr_last_five |>
    filter(end_year == 2025) |>
    select(
      excused_phys,
      excused_fin,
      excused_care,
      excused_trans,
      excused_12m,
      excused_other
    ) |>
    summarize(across(everything(), ~ sum(., na.rm = TRUE))) |>
    max()
)

############################ Disqualified #############################

p_disq <- jdr_last_five |>
  filter(
    end_year == 2025
  ) |>
  select(
    disqual_citizen,
    disqual_18y,
    disqual_nores,
    disqual_nocal,
    disqual_noenglish,
    disqual_conserv,
    disqual_fel
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "disq_type",
    values_to = "count"
  ) |>
  group_by(disq_type) |>
  summarize(total = sum(count, na.rm = TRUE)) |>
  arrange(desc(total)) |>
  mutate(
    disq_type = recode(
      disq_type,
      disqual_citizen = "Not a U.S. Citizen",
      disqual_18y = "Under 18 Years Old",
      disqual_nores = "Not a County Resident",
      disqual_nocal = "Not a California Resident",
      disqual_noenglish = "Insufficient English",
      disqual_conserv = "Under Conservatorship",
      disqual_fel = "Felony Conviction"
    )
  ) |>
  ggplot(aes(x = reorder(disq_type, total), y = total)) +
  geom_col(width = 0.6) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, max_val),
    labels = function(x) {
      dplyr::case_when(
        x >= 1e6 ~ paste0(round(x / 1e6, 1), "m"),
        x >= 1e3 ~ paste0(round(x / 1e3, 0), "k"),
        TRUE ~ scales::comma(x, accuracy = 1)
      )
    },
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Disqualifications",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(r = 4, b = 4)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 16, r = 12, b = 8, l = 12)
  )


############################ Excused #############################

p_excused <- jdr_last_five |>
  filter(
    end_year == 2025
  ) |>
  select(
    excused_phys,
    excused_fin,
    excused_care,
    excused_trans,
    excused_12m,
    excused_other
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "excuse_type",
    values_to = "count"
  ) |>
  group_by(excuse_type) |>
  summarize(total = sum(count, na.rm = TRUE)) |>
  arrange(desc(total)) |>
  mutate(
    excuse_type = recode(
      excuse_type,
      excused_phys = "Physical/Mental Condition",
      excused_fin = "Financial Hardship",
      excused_care = "Caregiving Responsibilities",
      excused_trans = "Transportation Issues",
      excused_12m = "12-Month Period",
      excused_other = "Other Reasons"
    )
  ) |>
  ggplot(aes(x = reorder(excuse_type, total), y = total)) +
  geom_col(width = 0.6) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, max_val),
    labels = function(x) {
      dplyr::case_when(
        x >= 1e6 ~ paste0(round(x / 1e6, 1), "m"),
        x >= 1e3 ~ paste0(round(x / 1e3, 0), "k"),
        TRUE ~ scales::comma(x, accuracy = 1)
      )
    },
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Excusals",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(r = 4, b = 4)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 16, r = 12, b = 8, l = 12)
  )

# Combine side by side with overarching title
p_disq +
  p_excused +
  plot_annotation(
    title = "Reasons for Disqualifications and Excusals"
  )
