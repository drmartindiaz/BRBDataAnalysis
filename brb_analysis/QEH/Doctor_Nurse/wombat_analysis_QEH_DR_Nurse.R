# Script: wombat_analysis_QEH_DR_Nurse.R
# Purpose: Analyze BRB data for Doctor/Nurse sessions at Queen Elizabeth Hospital
# Input: CSV files in brb_analysis/raw_data/
# Output: brb_analysis/QEH/Doctor_Nurse/processed_data/

# Set libraries
library(readr)
library(tidyverse)  
library(lubridate) 
library(hms)

# QEH - Doctor/Nurses

#setwd("~/Desktop/brb_analysis/brb_analysis") 

# Load data
# Description of type of professional (Doctor-Nurse-Clerk)
q_wombat_type_professional <- read_delim("raw_data/wombat_type_professional.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  select(`session id`, `health_professional_desc`)

# Wombat raw data
DOCTOR_NURSE_FORM_BRB_v1 <- read_csv("raw_data/DOCTOR-NURSE_FORM_BRB_v1_from_23_03_2025_to_09_04_2025.csv", 
                                     col_types = cols(`session date` = col_character(), 
                                                      `session start` = col_time(format = "%H:%M:%S"), 
                                                      `session end` = col_time(format = "%H:%M:%S"), 
                                                      `start time` = col_time(format = "%H:%M:%S"), 
                                                      `end time` = col_time(format = "%H:%M:%S"), 
                                                      `elapsed time` = col_time(format = "%H:%M:%S"), 
                                                      `total time` = col_time(format = "%H:%M:%S"), 
                                                      `active 1` = col_time(format = "%H:%M:%S"))) %>% 
  mutate(`session id` = case_when(`session id`== 10202 ~ 10203, # Correct session 10202/10203: Duplicate session IDs merged into 10203
                                  TRUE ~ `session id`),
         `session start` = case_when(`session id` == "10203" ~ hms(hours = 14, minutes = 21, seconds = 00),
                                     TRUE ~ `session start`), # Update session times for merged session
         `session end` = case_when(`session id` == "10203" ~ hms(hours = 17, minutes = 47, seconds = 45),
                                   TRUE ~ `session end`)) %>% # Update session times for merged session
  # Join with professional type information
  left_join(q_wombat_type_professional, by = "session id") 

# Load Wombat BRB form version 2 data
DOCTOR_NURSE_FORM_BRB_v2 <- read_csv("raw_data/DOCTOR-NURSE_FORM_BRB_v2_from_26_03_2025_to_09_09_2025.csv", 
                                     col_types = cols(`session date` = col_character(), 
                                                      `session start` = col_time(format = "%H:%M:%S"), 
                                                      `session end` = col_time(format = "%H:%M:%S"), 
                                                      `start time` = col_time(format = "%H:%M:%S"), 
                                                      `end time` = col_time(format = "%H:%M:%S"), 
                                                      `elapsed time` = col_time(format = "%H:%M:%S"), 
                                                      `total time` = col_time(format = "%H:%M:%S"), 
                                                      `active 1` = col_time(format = "%H:%M:%S"))) %>% 
  # Remove columns not present in version 1
  select(- `[With Whom] Student`, -`int task id 1`,-`int start 1`,-`int end 1`,-`active 2`,-`inactive 1`) %>% # Saco columnas que antes no estaban y son NA
  # Rename columns to match version 1 naming convention
  rename(`What (DR-NURSE)` = `What (DR-NURSE v2)`,
         `What (DR-NURSE) (subcategories)` = `What (DR-NURSE v2) (subcategories)`,
         `[With Whom] Clerks` = `[With Whom] Clerk`)  %>% 
  left_join(q_wombat_type_professional, by = "session id") %>% 
  # Removing sessions without professional type 
  # These sessions are from Polyclinics and will be analyzed in the Polyclinics-specific analysis
  filter(!is.na(health_professional_desc))

# Combine v1 and v2 datasets
q_data_wmbt_time <- DOCTOR_NURSE_FORM_BRB_v1 %>%
  rbind(DOCTOR_NURSE_FORM_BRB_v2) %>% 
  mutate(
    # Convert session duration from HMS to minutes
    total_session = as.numeric(`session end` - `session start`) / 60,
    total_session_hms = as_hms(as.numeric(`session end` - `session start`) / 60),
    total_activity = as.numeric(`active 1`) / 60,
    # Fix participant ID inconsistencies
    `participant id` = case_when(`session id` == "10226" ~ "GL 16",
                                 `session id` == "10223" ~ "GL14",
                                 TRUE ~ `participant id`),
    # Time slot categorization
    time_slot = case_when(hour(`session start`) %in% 7:11 ~ "morning",
                          hour(`session start`) %in% 12:16 ~ "afternoon",
                          hour(`session start`) %in% 17:20 ~ "evening",
                          TRUE ~ "other"),
    # Fix categories/subcategories inconsistencies
    `What (DR-NURSE)` = case_when(`What (DR-NURSE)` == "Inter-Professional Communication" ~ "Professional Communication",
                                  TRUE ~ `What (DR-NURSE)`),
    `What (DR-NURSE) (subcategories)` = case_when(`What (DR-NURSE) (subcategories)` == "Request Tests(Lab, Img, etc)" ~ "Request Tests (Lab, Img, etc)",
                                                  `What (DR-NURSE) (subcategories)` == "Other Indiret Care" ~ "Other Indirect Care",
                                                  TRUE ~ `What (DR-NURSE) (subcategories)`),
    # Extract task order
    task_order = as.numeric(str_extract(`observer/session/task id`, "(?<=_)\\d+$"))) %>% 
  # Remove specific problematic sessions
  filter(!`session id` == 10195,
         !`observer/session/task id` == "10175_10260_60") %>% # seems like a test activity record 
  # session id = 10260 ended at 12:01:16
  mutate(`session end` = case_when(`session id` == "10260" ~ hms(hours = 12, minutes = 01, seconds = 16),
                                   TRUE ~ `session end`)) %>% 
  # Sessions excluded per stakeholder request
  filter(!`session id` == 10180,
         !`session id` == 10184, 
         !`session id` == 10189) 

q_data_wmbt_time_2 <-  q_data_wmbt_time %>% 
  rename(What = `What (DR-NURSE)`,
         Subcategories = `What (DR-NURSE) (subcategories)`) %>% 
  # Combine multiple binary columns into a single categorical description
  mutate(with_whom = paste0(
    ifelse(`[With Whom] Doctor` == 1, "Doctor, ", ""),
    ifelse(`[With Whom] Nurse` == 1, "Nurse, ", ""),
    ifelse(`[With Whom] Clerks` == 1, "Clerks, ", ""),
    ifelse(`[With Whom] Patient Relation Reps` == 1, "Patient Relation Reps, ", ""),
    ifelse(`[With Whom] Patient` == 1, "Patient, ", ""),
    ifelse(`[With Whom] Relative` == 1, "Relative, ", ""),
    ifelse(`[With Whom] Allied Health Professional` == 1, "Allied Health Professional, ", ""),
    ifelse(`[With Whom] Other` == 1, "Other, ", "")
  ) %>%
    str_remove_all(", $") %>%
    ifelse(. == "", NA_character_, .), # NA for missing values
  how = paste0(
    ifelse(`[How] Paper Based Record` == 1, "Paper, ", ""),
    ifelse(`[How] Desktop Computer` == 1, "Desktop Computer, ", ""),
    ifelse(`[How] Mobile Computer` == 1, "Mobile Computer, ", ""),
    ifelse(`[How] Phone` == 1, "Phone, ", "")
  ) %>%
    str_remove_all(", $") %>%
    ifelse(. == "", NA_character_, .)) %>% 
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
write_csv(q_data_wmbt_time_2, "QEH/Doctor_Nurse/processed_data/wmbt_qeh_dr_nurse_processed.csv")


# Data Analysis and summaries -----
q_observer_summary <- q_data_wmbt_time_2 %>%
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

total_data <- q_data_wmbt_time_2 %>%
  summarise(
    `observer id` = "Total",
    sessions = n_distinct(`session id`),
    n_activities_avg = mean(table(`session id`), na.rm = TRUE) %>% round(),
    avg_session_min = round(mean(total_session, na.rm = TRUE), 2),
    total_time_min = round(sum(total_activity), 2),
    total_time_hms = as_hms(sum(`total time`, na.rm = TRUE))
  ) 

# Combine observer and total summaries
q_observer_summary <- bind_rows(q_observer_summary, total_data)

# Activity type summary
q_activity_summary <- q_data_wmbt_time_2 %>%
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
    pct_time = round((total_time_min / sum(total_time_min)) * 100,1)
  )

# Session timing information
q_sessions_times <- q_data_wmbt_time_2 %>% 
  group_by(`session id`) %>% 
  mutate(total_session_hms = as_hms(sum(`total time`))) %>% 
  distinct(`session date`, `observer id`,time_slot, `session id`,`participant id`, `session start`, `session end`, total_session_hms)

q_sessions_detail <- q_data_wmbt_time_2 %>%
  mutate(total_activity_min = round(total_activity, 2),
         total_time_sec = round(total_activity * 60, 2)) %>% 
  distinct(`session id`, `observer id`, task_order, What, Where, with_whom,how, Comments, total_activity_min, total_time_sec)

q_report_summary <- tibble::tibble(
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

reporte_final <- list('report_summary' = q_report_summary,
                      'observer_summary' = q_observer_summary,
                      'sessions_times' = q_sessions_times,
                      'activity_summary' = q_activity_summary,
                      'session_detail' = q_sessions_detail)

writexl::write_xlsx(reporte_final, "QEH/Doctor_Nurse/processed_data/qeh_wombat_analysis.xlsx")
