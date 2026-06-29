library(readxl)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(janitor)
library(tidyr)


read_table <- function(file_path = "./data/raw/jury-data-center.xlsx",
                       table_name = NULL,
                       excel_range = NULL,
                       n_rows = Inf,
                       n_cols = NULL,
                       na_values = c(
                         "", "NA", "N/A", "na", "Na", "n/a", "NULL", "null", "Null", "None", "none", "NONE", "Not Applicable",
                         "Not applicable", "not applicable", "Not App", "Not App.", "Not Applicable.",
                         "Not App", "nan", "NAN", "NaN", "Nan", "Missing component data", "Not tracked separately.",
                         "unknown", "not available", "Unable to determine", "Not recorded", "not answered", "No data",
                         "No postponed in data", "Missing postponed in and out data", "0 postponed in", "No trial",
                         "Missing component data", "Did not submit", "?"
                       ),
                       fields_to_lower = FALSE) {
  if (str_ends(file_path, ".xlsx") | str_ends(file_path, ".xls")) {
    output <- read_excel(
      path = file_path,
      sheet = table_name,
      range = excel_range,
      n_max = n_rows,
      na = na_values
    )
  }
  
  if (!is.null(n_cols)) {
    output <- select(output, 1:n_cols)
  }
  
  if (fields_to_lower) {
    names(output) <- tolower(names(output))
  }
  
  return(output)
}


# --------------------------- Rename Columns to Amended Data Dictonary Names -------------------


jdr_dictionary <- read_table(table_name = "Data Dictionary") |>
  # "not available" treated as NA by read_table
  # This is a field in some JDR reports and therefore mistakingly recoded as NA in the workbook_name column of the data dictionary
  # However, it's simpler to add this value back into the workbook_name column than to not treat this string as NA in read_table
  mutate(workbook_name = if_else(field_name == "n_not_available", "not available", workbook_name))

# Vector for mapping new names (field_name) to old names (workbook_name)
recode_vector <- setNames(jdr_dictionary$field_name, jdr_dictionary$workbook_name)





# -------------------------------- Bind Historical Data Together -----------------------------------


jury_data_center <- bind_rows(
  JCstats2007 <- read_table(
    table_name = "JCstats2007",
    excel_range = "A1:BS59",
    fields_to_lower = TRUE
  ) |>
    # Recoding problematic field names using mappings defined by the data dictionary
    rename_with(~ recode_vector[.x], .cols = everything()) |>
    # For some reason, one row contains a value of "1" in the fta_followup column
    # Also, reporting_period is calendar year in earlier JDRs.
    # We need to coerce this column to character so that it can be bound with rows where reporting_period is fiscal year
    mutate(
      fta_followup = if_else(fta_followup == 1, "YES", fta_followup),
      reporting_period = as.character(reporting_period)
    ),
  JCstats2008 <- read_table(
    table_name = "JCstats2008",
    excel_range = "A1:BS59",
    fields_to_lower = TRUE
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()) |>
    # One row of disqual_nocal is recorded via reference to disqual_nores
    mutate(
      disqual_nocal = as.numeric(if_else(disqual_nocal == "9C", as.character(disqual_nores), disqual_nocal)),
      reporting_period = as.character(reporting_period)
    ),
  JCstats0809 <- read_table(
    table_name = "JCstats0809",
    excel_range = "A1:BS59",
    fields_to_lower = TRUE
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()),
  JCstats0910 <- read_table(
    table_name = "JCstats0910",
    excel_range = "A1:BS59",
    fields_to_lower = TRUE
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()) |>
    # One row of disqual_grand is recorded via reference to excused_12m
    mutate(disqual_grand = as.numeric(if_else(disqual_grand == "Same as 7e", as.character(excused_12m), disqual_grand))),
  JCstats1011 <- read_table(
    table_name = "JCstats1011",
    excel_range = "A1:BS59",
    fields_to_lower = TRUE
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()),
  JCstats1112 <- read_table(
    table_name = "JCstats1112",
    excel_range = "A1:BS59",
    fields_to_lower = TRUE
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()) |>
    # One row mixes up excused_other and excused_decider
    mutate(
      excused_other = as.numeric(if_else(excused_other == "Staff", excused_decider, excused_other)),
      excused_decider = if_else(excused_decider == "1531", "Staff", excused_decider)
    ),
  JCstats1213 <- read_table(
    table_name = "JCstats1213",
    excel_range = "A1:BS59",
    fields_to_lower = TRUE
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()) |>
    # One row mixes up excused_other and excused_decider
    mutate(
      excused_other = as.numeric(if_else(excused_other == "Staff", excused_decider, excused_other)),
      excused_decider = if_else(excused_decider == "443", "Staff", excused_decider)
    ),
  JCstats1314 <- read_table(
    table_name = "JCstats1314",
    excel_range = "A1:BS59",
    fields_to_lower = TRUE
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()),
  JCstats1415 <- read_table(
    table_name = "JCstats1415",
    excel_range = "A1:BS59",
    fields_to_lower = TRUE
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()),
  JCstats1516 <- read_table(
    table_name = "JCstats1516",
    excel_range = "A1:BS59",
    fields_to_lower = TRUE
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()) |>
    # Warning message reveals that two counties enter run_time using an xx:xx:xx format
    # Excel interprets this cell as a date, changing the way that it is read into R
    mutate(
      run_time = if_else(county == "Colusa", 113324, run_time),
      run_time = if_else(county == "Madera", 121807, run_time)
    ),
  JCstats1617 <- read_table(
    table_name = "JCstats1617",
    excel_range = "A1:BS59",
    fields_to_lower = TRUE
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()),
  JCstats1718 <- read_table(
    table_name = "JCstats1718",
    excel_range = "A1:CZ59",
    fields_to_lower = TRUE
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()),
  JCstats1819 <- read_table(
    table_name = "JCstats1819",
    excel_range = "A1:CZ59",
    fields_to_lower = TRUE
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()),
  JCstats1920 <- read_table(
    table_name = "JCstats1920",
    excel_range = "A1:CZ59",
    fields_to_lower = TRUE
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()),
  JCstats2021 <- read_table(
    table_name = "JCstats2021",
    excel_range = "A1:DA59",
    fields_to_lower = TRUE
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()),
  JCstats2122 <- read_table(
    table_name = "JCstats2122",
    excel_range = "A1:DA59",
    fields_to_lower = TRUE
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()),
  JCstats2223 <- read_table(
    table_name = "JCstats2223",
    excel_range = "A1:CZ59",
    fields_to_lower = T
  ) |>
    rename_with(~ recode_vector[.x], .cols = everything()) |>
    mutate(disqual_noenglish = as.double(disqual_noenglish)),
  JCstats2324 <- read_table(
    table_name = "JCstats2324",
    excel_range = "A1:CZ59",
    fields_to_lower = T
    ) |>
    rename_with(~ recode_vector[.x], .cols = everything()) |>
    mutate(end_date = ifelse(reporting_period == "2023-24", as.double("20240630"), end_date))
  ,
    JCstats2425 <- read_table(
      table_name = "JCstats2425",
      excel_range = "A1:CZ59",
      fields_to_lower = T
    ) |>
    rename_with(~ recode_vector[.x], .cols = everything()) |>
    mutate(end_date = ifelse(reporting_period == "2024-225", as.double("20250630"), end_date),
           oneday_telweb = as.double(oneday_telweb),
           panels_other = as.double(panels_other),
           sworn_other = as.double(sworn_other))
)

# ---------------------------- Add Proportions of Subcategory Summations to Lump Sums -----------------------


jury_data_transformed <- jury_data_center |>
  
  select(reporting_period, county,
         jms_system,
         summons, postin,
         undel, fta, excused, disqual, dismiss_peace, serving, dismiss_dead, postout,
         excused, excused_phys , excused_fin , excused_care , excused_trans , excused_12m , excused_other,
         disqual, disqual_citizen , disqual_18y , disqual_nores , disqual_nocal , disqual_noenglish , disqual_conserv , disqual_fel,
         incourt, jurors_sworn, rel_challenge , rel_hardship , rel_perempt , jurors_sworn, not_reached ,
         rel_defendant_pc, rel_plaintiff_pc,
         inperson, oncall, oneday, oneday_inperson, oneday_telweb, not_reached,
         jdays, jdays_first, jdays_subs,
         n_pools, panel_cases, 
         n_panels, panels_select, panels_fel , panels_msdo , panels_civil , panels_other, panels_crim,
         juries_sworn, sworn_fel, sworn_msdo, sworn_civil, sworn_other, sworn_crim
  ) |>
  
  mutate(
    
    # Add clusters
    cluster = case_when(
      county %in% c("Alpine", "Amador", "Calaveras", "Colusa", "Del Norte", "Glenn", "Inyo", "Lassen", "Mariposa", "Modoc", "Mono", "Plumas", "San Benito", "Sierra", "Trinity") ~ 1,
      county %in% c("Butte", "El Dorado", "Humboldt", "Imperial", "Kings", "Lake", "Madera", "Marin", "Mariposa", "Mendocino", "Merced", "Napa", "Nevada", "Placer", "San Luis Obispo", "Santa Cruz", "Shasta", "Sutter", "Siskiyou", "Solano", "Sonoma", "Tehama", "Tuolumne", "Yolo", "Yuba") ~ 2,
      county %in% c("Kern", "Contra Costa", "Fresno", "Monterey", "San Joaquin", "San Mateo", "Santa Barbara", "Stanislaus", "Tulare", "Ventura") ~ 3,
      county %in% c("Alameda", "Los Angeles", "Orange", "Riverside", "Sacramento", "San Bernardino", "San Diego", "San Francisco", "Santa Clara") ~ 4
    ))
    
    jury_data_transformed <- jury_data_transformed |>
      mutate(
    # Change to date time
    reporting_period = str_remove(reporting_period, "-.*"),
    beg_date = ymd(paste0(reporting_period, "-07-01")),
    end_date = ymd(paste0(as.numeric(reporting_period) + 1, "-06-30")),
    beg_year = year(beg_date),
    end_year = year(end_date)
    ) 
    
    jury_data_transformed <- jury_data_transformed |>
      mutate(
    # Over arching categories
    potentially_available = summons + postin,
    unavailable = undel + fta + excused + disqual + dismiss_peace + dismiss_dead + postout,
    tqa = potentially_available - unavailable,
    told_to_report = tqa - oncall, # For sankey
    incourt_sum = rel_challenge + rel_hardship + rel_perempt + not_reached + jurors_sworn,
    sent_for_selection = 
      #told_to_report - 
      incourt_sum
    , # For sankey
    not_selected = told_to_report - sent_for_selection,
    
    
    
    # Under arching Categories
    potentially_available_sum = undel + fta + excused + disqual + dismiss_peace + serving + dismiss_dead + postout,
    excused_sum = excused_phys + excused_fin + excused_care + excused_trans + excused_12m + excused_other,
    disqual_sum = disqual_citizen + disqual_18y + disqual_nores + disqual_nocal + disqual_noenglish + disqual_conserv + disqual_fel,
    tqa_sum = (summons + postin) - (undel + fta + excused + disqual + dismiss_peace + dismiss_dead + postout),
    serving_sum = inperson + oncall,
    perempt_sum = rel_defendant_pc + rel_plaintiff_pc,
    one_day_sum = oneday_inperson + oneday_telweb,
    jdays_sum = jdays_first + jdays_subs,
    panels_sum = panels_fel + panels_msdo + panels_civil + panels_other,
    criminal_panels_sum = panels_fel + panels_msdo,
    juries_sworn_sum = sworn_fel + sworn_msdo + sworn_civil + sworn_other,
    
    
    # T/F Proportion Checks
    potentially_available_prop = potentially_available_sum / potentially_available,
    excused_prop = excused_sum / excused,
    disqual_prop = disqual_sum / disqual,
    tqa_prop = tqa_sum / serving,
    serving_prop = serving_sum / serving,
    incourt_prop = incourt_sum / incourt,
    perempt_prop = perempt_sum / rel_perempt,
    one_day_prop = one_day_sum / oneday,
    jdays_prop = jdays_sum / jdays,
    panel_prop = panels_sum / panels_select,
    criminal_panels_prop = criminal_panels_sum / panels_crim,
    criminal_sworn_prop = juries_sworn_sum / sworn_crim,
    
    # KPIs
    juror_yield = tqa / potentially_available,
    pc_panel_used = (rel_challenge + rel_hardship + rel_perempt + jurors_sworn) /
      (rel_challenge + rel_hardship + rel_perempt + jurors_sworn + not_reached),
    pc_sent_for_sel = (rel_challenge + rel_hardship + rel_perempt + jurors_sworn + not_reached) / (tqa - oncall),
    pc_told_to_report = (tqa - oncall) / tqa,
    juror_utilization = pc_panel_used * pc_sent_for_sel * pc_told_to_report,
    postponement_ratio = postout / postin,
    
    # JMS name cleaning
    jms_system = case_when(
      str_detect(jms_system, "Incorporated|JSI") ~ "JSI",
      str_detect(jms_system, "Web") ~ "Web Enhanced JS",
      str_detect(jms_system, "gile|learview") ~ "Agile Jury",
      str_detect(jms_system, "Custom|HOUSE|House|house|Home") ~ "Custom",
      str_detect(jms_system, "udicial") ~ "Judicial Systems Inc",
      str_detect(jms_system, "Management|Jury Management Information System (JMIS)") ~ "JMIS",
      TRUE ~ "Other"
    )
  ) |>
      mutate(
        across(
          ends_with("_prop"),
          ~ abs(.x - 1),
          .names = "tmp_{.col}"
        )
      ) |>
      rename_with(
        ~ str_c(str_remove(str_remove(.x, "^tmp_"), "_prop$"), "_aape"),
        starts_with("tmp_")
      )

    jury_data_transformed |> select(ends_with("aape"))

# --------------------------- Write to CSV ----------------------------------------

write_csv(jury_data_transformed, "./data/processed/jury_data_transformed.csv")


# -------------------------- Power BI Data ---------------------------------------- 

jury_data_bi <- jury_data_center |>
  
  select(end_date, county,
         jms_system,
         summons, postin,
         undel, fta, excused, disqual, dismiss_peace, serving, dismiss_dead, postout,
         excused, excused_phys , excused_fin , excused_care , excused_trans , excused_12m , excused_other,
         disqual, disqual_citizen , disqual_18y , disqual_nores , disqual_nocal , disqual_noenglish , disqual_conserv , disqual_fel,
         incourt, rel_challenge , rel_hardship , rel_perempt , jurors_sworn, not_reached ,
         rel_defendant_pc, rel_plaintiff_pc,
         inperson, oncall, oneday, oneday_inperson, oneday_telweb, not_reached,
         jdays, jdays_first, jdays_subs,
         n_pools, panel_cases, 
         n_panels, panels_select, panels_fel , panels_msdo , panels_civil , panels_other, panels_crim,
         juries_sworn, sworn_fel, sworn_msdo, sworn_civil, sworn_other, sworn_crim
  ) |>
  
  mutate(
    
    # Add clusters
    cluster = case_when(
      county %in% c("Alpine", "Amador", "Calaveras", "Colusa", "Del Norte", "Glenn", "Inyo", "Lassen", "Mariposa", "Modoc", "Mono", "Plumas", "San Benito", "Sierra", "Trinity") ~ 1,
      county %in% c("Butte", "El Dorado", "Humboldt", "Imperial", "Kings", "Lake", "Madera", "Marin", "Mariposa", "Mendocino", "Merced", "Napa", "Nevada", "Placer", "San Luis Obispo", "Santa Cruz", "Shasta", "Sutter", "Siskiyou", "Solano", "Sonoma", "Tehama", "Tuolumne", "Yolo", "Yuba") ~ 2,
      county %in% c("Kern", "Contra Costa", "Fresno", "Monterey", "San Joaquin", "San Mateo", "Santa Barbara", "Stanislaus", "Tulare", "Ventura") ~ 3,
      county %in% c("Alameda", "Los Angeles", "Orange", "Riverside", "Sacramento", "San Bernardino", "San Diego", "San Francisco", "Santa Clara") ~ 4
    ),
      
      # JMS name cleaning
      jms_system = case_when(
        str_detect(jms_system, "Incorporated|JSI") ~ "JSI",
        str_detect(jms_system, "Web") ~ "Web Enhanced JS",
        str_detect(jms_system, "gile|learview") ~ "Agile Jury",
        str_detect(jms_system, "Custom|HOUSE|House|house|Home") ~ "Custom",
        str_detect(jms_system, "udicial") ~ "JSI",
        str_detect(jms_system, "Management|Jury Management Information System (JMIS)") ~ "JMIS",
        TRUE ~ "Other"
      ),
    
    end_date = lubridate::ymd(end_date)
  ) |>
  
  rename(
    `End Date` = end_date,
    County = county,
    Cluster = cluster,
    `JMS System` = jms_system,
    Summons = summons,
    `Postponed In` = postin,
    Undelivered = undel,
    `Failure To Appear` = fta,
    `Excused` = excused,
    `Disqualified` = disqual,
    `Peace Officer` = dismiss_peace,
    `Serving Jurors` = serving,
    `Death` = dismiss_dead,
    `Postponed Out` = postout,
    `Physical` = excused_phys,
    `Financial` = excused_fin,
    `Caregiving` = excused_care,
    `Transportation` = excused_trans,
    `12 Months` = excused_12m,
    `Other Excused` = excused_other,
    `Citizenship` = disqual_citizen,
    `Age` = disqual_18y,
    `Residency` = disqual_nores,
    `Local` = disqual_nocal,
    `English` = disqual_noenglish,
    `Conservatorship` = disqual_conserv,
    `Felony` = disqual_fel,
    `In Court` = incourt,
    `Challenge` = rel_challenge,
    `Hardship`= rel_hardship,
    `Peremptory` = rel_perempt,
    `Jurors Sworn` = jurors_sworn,
    `Juries Sworn` = juries_sworn,
    `Not Reached` = not_reached,
    `Defendant Peremptory Charge` = rel_defendant_pc,
    `Plaintiff Peremptory Charge` = rel_plaintiff_pc,
    `In Person` = inperson,
    `On Call` = oncall,
    `One Day` = oneday,
    `One Day - In Person` = oneday_inperson,
    `One Day - Tele Web` = oneday_telweb,
    `Jury Days` = jdays,
    `First` = jdays_first,
    `Subsequent` = jdays_subs,
    `Pools` = n_pools,
    `Cases` = panel_cases,
    `Panels` = n_panels,
    `Panels Selected` = panels_select,
    `Panels - Felony` = panels_fel,
    `Panels - Misdemeanor` = panels_msdo,
    `Panels - Civil` = panels_civil,
    `Panels - Other` = panels_other,
    `Panels - Criminal` = panels_crim,
    `Juries Sworn - Felony` = sworn_fel,
    `Juries Sworn - Misdemeanor` = sworn_msdo,
    `Juries Sworn - Civil` = sworn_civil,
    `Juries Sworn - Other` = sworn_other,
    `Juries Sworn - Criminal` = sworn_crim
  )

write_csv(jury_data_bi, "./data/processed/jury_data_bi.csv")

