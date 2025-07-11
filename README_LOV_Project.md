
# Life Outside of Violence (LOV) Program – Data Processing Pipeline

This repository contains the data processing script and sample dashboard output developed during my time as a Data Management Intern with the Life Outside of Violence (LOV) program at Washington University's Institute for Public Health.

## 🔁 Project Overview

This project focuses on preparing participant-level data for monitoring and evaluation of the LOV program, which is a hospital-based intervention aimed at reducing repeat violence and retaliation in St. Louis.

### Key Steps:
- Source data exported from REDCap (encounter-level)
- Data cleaned and transformed in R
- Dashboard visualized using Tableau

## 🧰 Tools & Technologies
- R (tidyverse)
- REDCap
- Tableau
- Excel (for QA)

## 📂 Repository Contents

- `Data_processing_CK.R`: Main R script to collapse and clean the raw data
- `Enrolled_LOV_IDs.csv`: Sample raw data file exported from REDCap
- `DashboardSample_CK.pptx`: Example output dashboard used for stakeholder reporting

## 💻 Sample Code

```r
# Collapse encounter-level data to participant-level
Grouped_LOV_results <- Raw_LOV_data %>%
  group_by(study_id) %>%
  summarise(across(everything(), ~ .[which(!is.na(.))[1]]))
```

## 📈 Dashboard

The cleaned dataset was exported to Tableau to visualize:
- Referral sources
- Participant service engagement
- Outcome tracking over time

## 🧑‍💼 Author

**Chloe Kelly, MPH**  
Data Management Intern  
Public Health Data & Training Center  
📫 [LinkedIn](https://www.linkedin.com/in/chloemkelly) 

*Note: This is a sample project; all data has been anonymized or is synthetic.*
