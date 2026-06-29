source("code/transform.R")

sankey_data <- jury_data_transformed |>
  select(
    end_date,
    county,
   # tqa,
    told_to_report,
    sent_for_selection,
    not_selected,
    oncall,
    rel_challenge,
    rel_hardship,
    rel_perempt,
    jurors_sworn,
    not_reached
  ) |>
  mutate(end_year = year(ymd(end_date))) |>
  select(end_year, everything(), -end_date)

sankey_data_2 <- sankey_data %>%
  # First, create the TQA to Told To Report flow
  mutate(
    tqa_to_report = told_to_report,
    tqa_to_oncall = oncall
  ) %>%
  # Then create subsequent flows
  mutate(
    report_to_selection = sent_for_selection,
    report_to_not_selected = not_selected,
    selection_to_challenge = rel_challenge,
    selection_to_hardship = rel_hardship,
    selection_to_perempt = rel_perempt,
    selection_to_sworn = jurors_sworn,
    selection_to_not_reached = not_reached
  ) %>%
  # Now pivot to long format
  pivot_longer(
    cols = c(tqa_to_report, tqa_to_oncall, report_to_selection, report_to_not_selected,
             selection_to_challenge, selection_to_hardship, selection_to_perempt, 
             selection_to_sworn, selection_to_not_reached),
    names_to = "flow",
    values_to = "value"
  ) %>%
  select(end_year, county, flow, value)

View(sankey_data_2)
sankey_data_2 <- sankey_data_2 |>
  # Create source and destination columns based on flow
  mutate(
    Source = case_when(
      str_detect(flow, "^tqa_") ~ "TQA",
      str_detect(flow, "^report_") ~ "Told To Report",
      str_detect(flow, "^selection_") ~ "Sent For Selection",
      TRUE ~ NA_character_
    ),
    
    Destination = case_when(
      flow == "tqa_to_report" ~ "Told To Report",
      flow == "tqa_to_oncall" ~ "On Call",
      flow == "report_to_selection" ~ "Sent For Selection",
      flow == "report_to_not_selected" ~ "Not Selected",
      flow == "selection_to_challenge" ~ "Challenge",
      flow == "selection_to_hardship" ~ "Hardship",
      flow == "selection_to_perempt" ~ "Perempt",
      flow == "selection_to_sworn" ~ "Sworn",
      flow == "selection_to_not_reached" ~ "Not Reached",
      TRUE ~ NA_character_
    )
  ) |>
  select(-flow)

sankey_data <- jury_data_transformed |>
  filter(end_year == 2024) |>
  select(
    end_date,
    county,
    # tqa,
    told_to_report,
    sent_for_selection,
    not_selected,
    oncall,
    rel_challenge,
    rel_hardship,
    rel_perempt,
    jurors_sworn,
    not_reached
  ) |>
  mutate(end_year = year(ymd(end_date))) |>
  select(end_year, everything(), -end_date) |>
  pivot_longer(
    cols = c(told_to_report, 
             sent_for_selection,
             not_selected,
             oncall,
             rel_challenge,
             rel_hardship,
             rel_perempt,
             jurors_sworn,
             not_reached),
    names_to = "Designation",
    values_to = "value"
  ) |>
  group_by(end_year, Designation) |>
  summarize(value_sum = sum(value, na.rm = T)) |>
  mutate(
    
    Total = case_when(
      Designation == "told_to_report" ~ "Told To Report",
      Designation == "oncall" ~ "On Call",
      TRUE ~ NA_character_
    ),
    
    InCourt = case_when(
      Designation == "sent_for_selection" ~ "Sent For Selection",
      Designation == "not_selected" ~ "Not Selected",
      TRUE ~ NA_character_
    ),
    
    InSelection = case_when(
      Designation == "rel_challenge" ~ "Challenge",
      Designation == "rel_hardship" ~ "Hardship",
      Designation == "rel_perempt" ~ "Perempt",
      Designation == "jurors_sworn" ~ "Sworn",
      Designation == "not_reached" ~ "Not Reached",
      TRUE ~ NA_character_
    )
  )|>
    select(-Designation)
    
  

sankey_data_3 <- sankey_data_2 |>
  filter(end_year == 2024) |>
  filter(!is.na(value), value > 0) |>
  select(Source, Destination, value)

write_csv(sankey_data_2, "./data/processed/sankey_data.csv")
write_csv(sankey_data, "./data/processed/sankey_data_ribbon.csv")


path_data <- sankey_data_3 %>%
  mutate(obs_id = row_number())

sankey_pivot <- sankey_data_3 %>%
  group_by(Source, Destination) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") # Summing values per (Source, Destination)


# For TQA observations
tqa_flows <- path_data %>%
  filter(Source == "TQA") %>%
  select(obs_id, TQA = Destination)

# For Told To Report observations
ttr_flows <- path_data %>%
  filter(Source == "Told To Report") %>%
  select(obs_id, `Told To Report` = Destination)

# For Sent For Selection observations
sfs_flows <- path_data %>%
  filter(Source == "Sent For Selection") %>%
  select(obs_id, `Sent For Selection` = Destination)

# Join all together to create the path for each observation
paths_combined <- tqa_flows %>%
  left_join(ttr_flows, by = "obs_id") %>%
  left_join(sfs_flows, by = "obs_id") %>%
  select(-obs_id)


View(sankey_data_2)

sankey_data_4 <- sankey_data_2 |>
  mutate(
    TQA = case_when(
      str_detect(flow, "tqa_to_report") ~ "Told To Report",
      str_detect(flow, "tqa_to_oncall") ~ "On Call",
      TRUE ~ NA_character_
    ),
    
    `Told To Report` = case_when(
      flow == "report_to_selection" ~ "Sent For Selection",
      flow == "report_to_not_selected" ~ "Not Selected",
      TRUE ~ NA_character_
      
    ),
    
    `Sent For Selection` = case_when(
      flow == "report_to_not_selected" ~ "Not Selected",
      flow == "selection_to_challenge" ~ "Challenge",
      flow == "selection_to_hardship" ~ "Hardship",
      flow == "selection_to_perempt" ~ "Perempt",
      flow == "selection_to_sworn" ~ "Sworn",
      flow == "selection_to_not_reached" ~ "Not Reached",
      TRUE ~ NA_character_
      
    )
  ) |>
  select(end_year, county, TQA, `Told To Report`, `Sent For Selection`, value)



library(dplyr)

# Transform data into node structure
sankey_transformed <- sankey_data_2 %>%
  mutate(
    TQA = case_when(
      Source == "TQA" ~ Destination,
      Source == "Told To Report" ~ "Told To Report",
      Source == "Sent For Selection" ~ "Told To Report",
      TRUE ~ NA_character_
    ),
    `Told To Report` = case_when(
      Source == "Told To Report" ~ Destination,
      TRUE ~ NA_character_
    ),
    `Sent For Selection` = case_when(
      Source == "Sent For Selection" ~ Destination,
      TRUE ~ NA_character_
    )
  ) %>%
  select(TQA, `Told To Report`, `Sent For Selection`, value)  # Keep relevant columns

# Print output
print(sankey_transformed, n = 20)

