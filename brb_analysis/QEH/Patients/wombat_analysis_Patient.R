# Script: wombat_analysis_Patient.R
# Purpose: Analyze BRB data for Patient sessions
# Input: CSV files in data/
# Output: Patient/processed_data/

# Set libraries
library(readr)
library(tidyverse)  
library(lubridate) 
library(hms)
library(janitor)

# PATIENT ANALYSIS -------

#setwd("~/brb_analysis")

# Load Wombat raw data for Patients
PATIENT_FORM_BRB_v1_1_all <- read_csv("raw_data/PATIENT_FORM_BRB_v1_(1)_all.csv", 
                                      col_types = cols(`session start` = col_time(format = "%H:%M:%S"), 
                                                       `session end` = col_time(format = "%H:%M:%S"), 
                                                       `start time` = col_time(format = "%H:%M:%S"), 
                                                       `end time` = col_time(format = "%H:%M:%S"), 
                                                       `elapsed time` = col_time(format = "%H:%M:%S"), 
                                                       `total time` = col_time(format = "%H:%M:%S"), 
                                                       `active 1` = col_time(format = "%H:%M:%S")))

# Process patient data
patient <- PATIENT_FORM_BRB_v1_1_all %>% 
  # Convert active time to min, extract task order, Time slot categorization
  mutate(total_activity = as.numeric(`active 1`) / 60,
         task_order = as.numeric(str_extract(`observer/session/task id`, "(?<=_)\\d+$")),
         time_slot = case_when(hour(`session start`) %in% 7:11 ~ "morning",
                               hour(`session start`) %in% 12:16 ~ "afternoon",
                               hour(`session start`) %in% 17:20 ~ "evening",
                               TRUE ~ "other"),
         # Fix participant ID inconsistencies
         `participant id` = case_when(`session id` == 10320 ~ "TL8",
                                      TRUE ~ `participant id`),
         # Correct session end time for session 10300 (midnight)
         `session end` = case_when(`session id` == "10300" ~ hms(hours = 24, minutes = 36, seconds = 20),
                                   TRUE ~ `session end`),
         total_session = as.numeric(`session end` - `session start`) / 60) %>% 
  # Combine binary columns into a single categorical description
  mutate(with_whom = paste0(
    ifelse(`[With Whom] Doctor` == 1, "Doctor, ", ""),
    ifelse(`[With Whom] Nurse` == 1, "Nurse, ", ""),
    ifelse(`[With Whom] Clerk` == 1, "Clerk, ", ""),
    ifelse(`[With Whom] Patient Relation Rpes` == 1, "Patient Relation Reps, ", ""),
    ifelse(`[With Whom] Relative (Family Member or companion)` == 1, "Relative, ", ""),
    ifelse(`[With Whom] Allied Health Professional` == 1, "Allied Health Professional, ", ""),
    ifelse(`[With Whom] Other` == 1, "Other, ", ""),
    ifelse(`[With Whom] No one` == 1, "No one", "")
  ) %>%
    str_remove_all(", $")) %>%
  # Simplify column names
  rename(What = `What (PATIENT)`,
         Subcategories = `What (PATIENT) (subcategories)`) %>% 
  group_by(`session start`, `session end`) %>%
  mutate(
    session_start_min = min(`start time`),
    start_normalized = as.numeric(`start time` - session_start_min),
    end_normalized = as.numeric(`end time` - session_start_min)
  ) %>%
  ungroup() %>%  
  # Convert normalized times to HMS 
  mutate(
    start_normalized_hms = hms::as_hms(start_normalized),
    end_normalized_hms = hms::as_hms(end_normalized)
  ) %>%
  select(- `elapsed time`,-session_start_min, -start_normalized, -end_normalized,
         -`multi with`, -`num of multitask fragments`, -`total overlapping time`,
         -`is interrupting task?`, -`interrupted by`, -`num of interrupts`) %>% 
  # Correct normalized end time for session 10300
  mutate(end_normalized_hms = case_when(`session id` == "10300" ~ hms(hours = 03, minutes = 50, seconds = 28),
                                        TRUE ~ end_normalized_hms))

# wombat_patients <- read_delim("Desktop/brb_analysis/data/wombat_patients.csv", 
#                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Write processed data 
write_csv(patient, "QEH/Patients/processed_data/wmbt_qeh_patient_processed.csv")

# Data Analysis and summaries -----

# Observer-level summary statistics
observer_summary <- patient %>%
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

total_data <- patient %>%
  summarise(
    `observer id` = "Total",
    sessions = n_distinct(`session id`),
    n_activities_avg = mean(table(`session id`), na.rm = TRUE) %>% round(),
    avg_session_min = round(mean(total_session, na.rm = TRUE), 2),
    total_time_min = round(sum(total_activity), 2),
    total_time_hms = as_hms(sum(`total time`, na.rm = TRUE))
  ) 

# Combine observer and total summaries
observer_summary <- bind_rows(observer_summary, total_data)

activity_summary <- patient %>%
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

# Waiting time analysis per session
waiting_time_session <- patient %>%
  group_by(`session id`, What) %>%
  summarise(
    total_time_min = sum(total_activity)
  ) %>%
  arrange(`session id`, desc(total_time_min)) %>%
  # Add total session time without losing other columns
  group_by(`session id`) %>%
  mutate(
    total_session_min = sum(total_time_min)
  ) %>%
  ungroup() %>%
  mutate(across(ends_with("_min"), ~ round(., 2))) %>%
  select(`session id`, What, waiting_time_min = total_time_min, total_session_min) %>% 
  # Filter for Waiting Time activities only
  filter(What == "Waiting Time") %>% 
  # Calculate waiting time as percentage of total session
  mutate(waiting_pct = round((waiting_time_min * 100) / total_session_min, 2))

# Unique sessions list for reference
sessions <- patient %>% 
  distinct(`session id`)

# Time spent with doctors/nurses analysis
with_Dr_Nurse_time <- patient %>%
  group_by(`session id`, with_whom) %>%
  summarise(
    total_time_min = sum(total_activity)
  ) %>%
  arrange(`session id`, desc(total_time_min)) %>%
  group_by(`session id`) %>%
  mutate(
    total_session_min = sum(total_time_min)
  ) %>%
  ungroup() %>%
  mutate(across(ends_with("_min"), ~ round(., 2))) %>%
  select(`session id`, with_whom, total_time_min, total_session_min) %>% 
  # Filter for interactions with doctors or nurses
  filter(str_detect(with_whom, regex("Nurse|Doctor", ignore_case = TRUE))) %>% 
  # Calculate percentage of total session time
  mutate(DR_Nurse_pct = round((total_time_min * 100) / total_session_min, 2)) %>% 
  # Summarize across all doctor/nurse interactions per session
  group_by(`session id`, total_session_min) %>% 
  summarise(
    total_time_min = sum(total_time_min),
    DR_Nurse_pct = sum(DR_Nurse_pct)
  ) %>% 
  select(`session id`, DR_Nurse_min = total_time_min, total_session_min, DR_Nurse_pct) %>% 
  # Join with all sessions (include sessions with no doctor/nurse time)
  right_join(sessions) %>% 
  # Replace NAs with 0 for sessions without doctor/nurse interactions
  mutate(across(everything(), ~ replace_na(., 0)))

# Session timing information
sessions_times <- patient %>% 
  group_by(`session id`) %>% 
  mutate(total_session_hms = as_hms(sum(`total time`))) %>% 
  distinct(`session date`, `observer id`,time_slot, `session id`,`participant id`, `session start`, `session end`, total_session_hms)

# Session 10326 4 min
# 10309, 10313, 10293

# short_sess <- patient %>% 
#   filter(`session id` %in%  c(10326, 10309, 10313, 10293))

sessions_detail <- patient %>%
  mutate(total_activity_min = round(total_activity, 2),
         total_time_sec = round(total_activity * 60, 2)) %>% 
  distinct(`session id`, `observer id`, task_order, What, Where, with_whom, Comments, total_activity_min, total_time_sec)

report_summary <- tibble::tibble(
  sheet_name = c(
    "observer_summary",
    "sessions_times", 
    "activity_summary",
    "sessions_detail",
    "waiting_time_session",
    "with_Dr_Nurse_time"
  ),
  description = c(
    "Summary statistics per observer: number of sessions, average activities, and average session duration",
    "Session metadata including dates, time slots, participant IDs, start/end times, and total duration",
    "Aggregated activity analysis: total time, averages, and percentage distribution across activity types",
    "Detailed activity timeline with task order, locations, moods, companions, and comments",
    "Waiting time analysis per session: waiting duration as percentage of total session time", 
    "Time spent with doctors/nurses per session and percentage of total session duration"
  )
)

reporte_final <- list('report_summary' = report_summary,
                      'observer_summary' = observer_summary,
                      'sessions_times' = sessions_times,
                      'activity_summary' = activity_summary,
                      'sessions_detail' = sessions_detail,
                      "waiting_time_session" = waiting_time_session,
                      "with_Dr_Nurse_time" = with_Dr_Nurse_time)

writexl::write_xlsx(reporte_final, "QEH/Patients/processed_data/patient_wombat_analysis.xlsx")

