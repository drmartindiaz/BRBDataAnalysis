# Script: wombat_analysis_Clerks.R
# Purpose: Analyze BRB data for Clerk sessions at QEH 
# Input: CSV files in data/
# Output: Clerks/processed_data/

# Set libraries
library(readr)
library(tidyverse)  
library(lubridate) 
library(hms)

#setwd("~/Desktop/brb_analysis/brb_analysis") 

# CLERKS -------

q_wombat_type_professional <- read_delim("raw_data/wombat_type_professional.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  select(`session id`, `health_professional_desc`)

p_wombat_type_professional <- read_delim("raw_data/wombat_type_professional_2.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  select(`session id`, `health_professional_desc`)

# Load Wombat raw data for Clerks
RECORD_CLERK_FORM_BRB_v1_from_23_03_2025_to_24_04_2025 <- read_csv("raw_data/RECORD_CLERK_FORM_BRB_v1_from_23_03_2025_to_24_04_2025.csv", 
                                                                   col_types = cols(`session start` = col_time(format = "%H:%M:%S"), 
                                                                                    `session end` = col_time(format = "%H:%M:%S"), 
                                                                                    `start time` = col_time(format = "%H:%M:%S"), 
                                                                                    `end time` = col_time(format = "%H:%M:%S"), 
                                                                                    `elapsed time` = col_time(format = "%H:%M:%S"), 
                                                                                    `total time` = col_time(format = "%H:%M:%S"), 
                                                                                    `active 1` = col_time(format = "%H:%M:%S")))

# Combine with professional type data 
data_wmbt_clerks <- RECORD_CLERK_FORM_BRB_v1_from_23_03_2025_to_24_04_2025 %>% 
  left_join(q_wombat_type_professional, by = "session id") %>% 
  left_join(p_wombat_type_professional, by = "session id") %>% 
  # Merge professional descriptions from both sources
  mutate(health_professional_desc = coalesce(health_professional_desc.x, health_professional_desc.y)) %>% 
  select(-health_professional_desc.x, -health_professional_desc.y) %>% 
  mutate(
    # Convert session duration from HMS to minutes
    total_session = as.numeric(`session end` - `session start`) / 60,
    total_activity = as.numeric(`active 1`) / 60,
    # Time slot categorization
    time_slot = case_when(hour(`session start`) %in% 7:11 ~ "morning",
                          hour(`session start`) %in% 12:16 ~ "afternoon",
                          hour(`session start`) %in% 17:20 ~ "evening",
                          TRUE ~ "other"),
    # Extract task order
    task_order = as.numeric(str_extract(`observer/session/task id`, "(?<=_)\\d+$"))
  )

# Create enhanced dataset with category aggregation
data_wmbt_clerks_2 <- data_wmbt_clerks %>% 
  rename(What = `What (RECORD CLERK)`,
         Subcategories = `What (RECORD CLERK) (subcategories)`) %>% 
  mutate(
    # Combine multiple binary columns into a single categorical description
    with_whom = paste0(
      ifelse(`[With Whom] Doctor` == 1, "Doctor, ", ""),
      ifelse(`[With Whom] Nurse` == 1, "Nurse, ", ""),
      ifelse(`[With Whom] Patient` == 1, "Patient, ", ""),
      ifelse(`[With Whom] Relative` == 1, "Relative, ", ""),
      ifelse(`[With Whom] Allied Health Professional` == 1, "Allied Health Professional, ", ""),
      ifelse(`[With Whom] Other` == 1, "Other, ", "")
    ) %>%
      str_remove_all(", $") %>%
      ifelse(. == "", NA_character_, .), # NA for missing values
    # Combine technology/method columns into single description
    how = paste0(
      ifelse(`[How] Paper Based Record` == 1, "Paper, ", ""),
      ifelse(`[How] Desktop Computer` == 1, "Desktop Computer, ", ""),
      ifelse(`[How] Mobile Computer` == 1, "Mobile Computer, ", ""),
      ifelse(`[How] Phone` == 1, "Phone, ", "")
    ) %>%
      str_remove_all(", $") %>%
      ifelse(. == "", NA_character_, .), # NA for missing values
  ) %>% 
  group_by(`session start`, `session end`) %>%
  mutate(
    session_start_min = min(`start time`),
    start_normalized = as.numeric(`start time` - session_start_min),
    end_normalized = as.numeric(`end time` - session_start_min)
  ) %>%
  ungroup() %>%  
  mutate(
    start_normalized_hms = hms::as_hms(start_normalized),
    end_normalized_hms = hms::as_hms(end_normalized)
  ) %>% 
  select(- `elapsed time`,-session_start_min, -start_normalized, -end_normalized,
         -`multi with`, -`num of multitask fragments`, -`total overlapping time`,
         -`is interrupting task?`, -`interrupted by`, -`num of interrupts`)

# Write processed data 
write_csv(data_wmbt_clerks_2, "QEH/Clerks/processed_data/wmbt_clerks_processed.csv")


# Data Analysis and summaries -----

# Observer-level summary statistics
c_observer_summary <- data_wmbt_clerks_2 %>%
  group_by(`observer id`) %>%
  summarise(
    # Number of unique sessions observed by each observer
    sessions = n_distinct(`session id`),
    # Average number of activities per session
    n_activities_avg = mean(table(`session id`), na.rm = TRUE) %>% round(),
    # Average session duration in minutes (rounded to 2 decimals)
    avg_session_min = round(mean(total_session, na.rm = TRUE), 2),
    # Total activity time across all sessions (in minutes)
    total_time_min = round(sum(total_activity),2),
    # Total elapsed time in HMS format
    total_time_hms = as_hms(sum(`total time`))
  ) %>% 
  mutate(`observer id` = as.character(`observer id`))

total_data <- data_wmbt_clerks_2 %>%
  summarise(
    `observer id` = "Total",
    sessions = n_distinct(`session id`),
    n_activities_avg = mean(table(`session id`), na.rm = TRUE) %>% round(),
    avg_session_min = round(mean(total_session, na.rm = TRUE), 2),
    total_time_min = round(sum(total_activity), 2),
    total_time_hms = as_hms(sum(`total time`, na.rm = TRUE))
  ) 

c_observer_summary <- bind_rows(c_observer_summary, total_data)

# Activity type summary
c_activity_summary <- data_wmbt_clerks_2 %>%
  group_by(What) %>%
  summarise(
    # Number of sessions containing each activity type
    total_sessions = n_distinct(`session id`),
    # Total time spent on each activity type (minutes)
    total_time_min = sum(total_activity),
    # Average duration of each activity type
    avg_time_min = mean(total_activity),
    # Maximum single occurrence duration
    max_time_min = max(total_activity),
    # Minimum single occurrence duration
    min_time_min = min(total_activity)
  ) %>%
  arrange(desc(total_time_min)) %>%
  mutate(across(ends_with("_min"), ~ round(., 2))) %>% 
  mutate(
    pct_time = round((total_time_min / sum(total_time_min)) * 100, 1)
  )

# Session timing information
c_sessions_times <- data_wmbt_clerks_2 %>% 
  mutate(total_session_min = round(total_session, 2)) %>% 
  distinct(`session date`, `observer id`, time_slot, `session id`, `participant id`, `session start`, `session end`, total_session_min)


# Detailed session activity log
c_sessions_detail <- data_wmbt_clerks_2 %>%
  mutate(total_activity_min = round(total_activity, 2),
         total_time_sec = round(total_activity * 60, 2)) %>% 
  distinct(`session id`, `observer id`, task_order, What, Where, with_whom,how, Comments, total_activity_min, total_time_sec)

# Report metadata documentation
c_report_summary <- tibble::tibble(
  sheet_name = c(
    "observer_summary",
    "sessions_times", 
    "activity_summary",
    "session_detail"
  ),
  description = c(
    "Summary statistics per observer: number of sessions, average activities, and average session duration",
    "Session metadata including dates, time slots, participant IDs, start/end times, and total duration",
    "Aggregated activity analysis: total time, averages, and percentage distribution across activity types",
    "Detailed activity log per session with task order, activity type, location, companions, method, and time in both minutes and seconds"
  )
)

# Combine all analyses into named list for Excel export
reporte_final <- list(
  'report_summary' = c_report_summary,
  'observer_summary' = c_observer_summary,
  'sessions_times' = c_sessions_times,
  'activity_summary' = c_activity_summary,
  'session_detail' = c_sessions_detail
)

# Export to Excel with multiple sheets
writexl::write_xlsx(reporte_final, "QEH/Clerks/processed_data/clerks_wombat_analysis.xlsx")

