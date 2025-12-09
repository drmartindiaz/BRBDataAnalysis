# BRB Healthcare Data Analysis

Analysis of Behavioral Research in Healthcare (BRB) data from Queen Elizabeth Hospital (QEH) and Polyclinics.

## 📁 Repository Structure
- brb_analysis/
    - QEH/ 
        - Doctor_Nurse/  
            - wombat_analysis_QEH_DR_Nurse.R
            - processed_data/
        - Clerks/ 
            - wombat_analysis_Clerks.R
            - processed_data/
        - Patients/
            - wombat_analysis_Patient.R
            - processed_data/
    - Polyclinics/
        - wombat_analysis_polyclinics_DR_Nurse.R
        - processed_data/
    - raw_data/   (CSV files)
    - visualization_analysis/  (R Markdown reports)

## 📝 Data Notes

- **Source Data:** Original CSV files in `raw_data/` 
- **Processed Data:** Generated in each analysis folder's `processed_data/` subfolder
- **Analysis Scripts:** Self-contained R scripts with commented code for clarity
- **Reports:** HTML visualizations available in `visualization_analysis/`

## 📊 Analysis Overview

### Healthcare Workers Analysis (QEH & Polyclinics)
Analysis of Doctor, Nurse, and Clerk activities including:
- **Time Calculations:** Session duration and activity time in minutes
- **Professional Classification:** Doctor, Nurse, or Clerk categorization
- **Activity Categorization:** Work activity types and subcategories
- **Time Slot Analysis:** Morning, afternoon, or evening sessions
- **Companion Analysis:** Aggregation of "With Whom" binary columns
- **Method Analysis:** Aggregation of "How" binary columns (Paper, Computer, Phone)
- **Data Quality:** Session filtering and corrections for specific cases

**Output:** Each analysis generates a cleaned CSV dataset and a multi-sheet Excel report with observer summaries, activity summaries, and session details.

### Patient Analysis (QEH Only)
Patient journey analysis with additional metrics:
- **Standard Healthcare Worker Analyses** (listed above)
- **Waiting Time Analysis:** Time spent waiting as percentage of total session
- **Doctor/Nurse Interaction Time:** Time spent with healthcare providers as percentage of session
- **Patient-Specific Variables:** Mood tracking and patient-specific companion categories

**Output:** Includes all standard outputs plus additional waiting time and interaction time analyses.

## 🛠️ Quick Usage

1. **Set Paths:** Open any R script and adjust file paths if needed
2. **Run Script:** Execute the script to process data
3. **Check Outputs:** Processed files save to each folder's `processed_data/` subfolder
