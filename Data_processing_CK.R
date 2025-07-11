# = Detail comment
## = Refer to corresponding heading in readme file 

## Collapsing Data from Encounter-Level to Patient-Level
# Grouping raw data by study_id and  (collapsing to patient level over encounter level) 
Grouped_LOV_results <- Raw_LOV_data %>%
  group_by(study_id) %>%
  summarise(across(everything(), ~ .[which(!is.na(.))[1]]))

## Filtering out test cases and irrelevant records
# Filter out test cases - those cases with study id < 10
Renamed_LOV_records <- Grouped_LOV_results %>%
  filter(study_id >= 10) %>%
  filter(study_id != "4")

## Renaming multiple choice checkbox variables
# Renaming multiple choice checkbox variables so they can be coalesced into single variables 
Renamed_LOV_records <- Renamed_LOV_records %>%
  rename(
    White = initial_race___1, 
    Black = initial_race___2, 
    Asian = initial_race___3, 
    Native_Hawaiian_Pacific_Islander = initial_race___4, 
    American_Indian_Alaska_Native = initial_race___5, 
    Prefer_not_to_answer_race = initial_race___6, 
    Dont_know_race = initial_race___7, 
    Other_race = initial_race___8, 
    Could_not_obtain_race = initial_race___9, 
    
    Female = initial_sex___1, 
    Male = initial_sex___2, 
    Other_sex = initial_sex___3, 
    Prefer_not_to_answer_sex = initial_sex___4, 
    Could_not_obtain_sex = initial_sex___5, 
    
    Gun_Shot_Wound = initial_inj_typ___1, 
    Blunt_Trauma = initial_inj_typ___2, 
    Penetrating_Assault = initial_inj_typ___3, 
    Unknown_injury = initial_inj_typ___4, 
    Other_injury = initial_inj_typ___5, 
    Could_not_obtain_injury = initial_inj_typ___6, 
    
    Engaged_in_other_services_1 = initial_enroll_declined1___1, 
    Do_not_need_services_1 = initial_enroll_declined1___2, 
    Not_interested_1 = initial_enroll_declined1___3, 
    Uncomfortable_with_research_1 = initial_enroll_declined1___4, 
    Other_priorities_1 = initial_enroll_declined1___5, 
    
    Engaged_in_other_services_2 = initial_enroll_declined2___1, 
    Do_not_need_services_2 = initial_enroll_declined2___2, 
    Not_interested_2 = initial_enroll_declined2___3, 
    Uncomfortable_with_research_2 = initial_enroll_declined2___4, 
    Other_priorities_2 = initial_enroll_declined2___5
  )

## Recoding variables and categories
# Recoding single choice variable factors
Collapsedvars_LOV_records <- Renamed_LOV_records %>%
  mutate(
    initial_hosp = fct_recode(as.factor(initial_hosp), 
                              "Yes" = "1", 
                              "No, ER only" = "2", 
                              "Could not obtain" = "3"),
    initial_hospital = fct_recode(as.factor(initial_hospital), 
                                  "Barnes Jewish Hospital" = "1", 
                                  "St. Louis Children's Hospital" = "2", 
                                  "Cardinal Glennon Children's Hospital" = "3", 
                                  "St. Louis University Hospital" = "4"),
    initial_enroll = fct_recode(as.factor(initial_enroll), 
                                "Yes" = "1", 
                                "No, declined" = "2", 
                                "No, never responded" = "3", 
                                "Outreach" = "4"),
    enroll_status = fct_recode(as.factor(enroll_status), 
                               "Yes" = "1", 
                               "No" = "0"),
    initial_enroll2 = fct_recode(as.factor(initial_enroll2), 
                                 "Active Recruitment" = "1", 
                                 "Not enrolled: Ineligible" = "2", 
                                 "Not enrolled: Actively Declined" = "3", 
                                 "Not enrolled: Contacted, no followup" = "4", 
                                 "Not enrolled: Never contacted" = "5", 
                                 "Patient enrolled" = "6"),
    initial_enroll3 = fct_recode(as.factor(initial_enroll3), 
                                 "Not enrolled: Ineligible" = "2", 
                                 "Not enrolled: Actively Declined" = "3", 
                                 "Not enrolled: Contacted, no followup" = "4", 
                                 "Not enrolled: Never contacted" = "5", 
                                 "Patient enrolled" = "6"),
    initial_enroll_declined = fct_recode(as.factor(initial_enroll_declined), 
                                         "Patient declined" = "1", 
                                         "Parent/Guardian declined" = "2"),
    initial_sex_2 = fct_recode(as.factor(initial_sex_2), 
                               "Female" = "1", 
                               "Male" = "2", 
                               "Other" = "3", 
                               "Prefer not to answer" = "4", 
                               "Could not obtain sex" = "5"),
    case_plan_complete = fct_recode(as.factor(case_plan_complete), 
                                    "Incomplete" = "0", 
                                    "Incomplete" = "1", 
                                    "Complete" = "2"),
    case_goal1_cat = fct_recode(as.factor(case_goal1_cat), 
                                "Health & Healthcare: Physical Health" = "10", 
                                "Health & Healthcare: Mental Health" = "20", 
                                "Health & Healthcare: Other" = "30", 
                                "Economic Stability: Employment" = "40", 
                                "Economic Stability: Social Service Benefits" = "50", 
                                "Economic Stability: Financial Literacy" = "60", 
                                "Economic Stability: Other" = "70", 
                                "Education : Academic Engagement & Achievement" = "80", 
                                "Education : High School Completion or Equivalent" = "90", 
                                "Education : High Education" = "100", 
                                "Education : Vocational Training" = "110", 
                                "Education : Other" = "120", 
                                "Social & Community Context: Relational" = "130", 
                                "Social & Community Context: School Sport" = "140", 
                                "Social & Community Context: Offspring Wellbeing" = "150", 
                                "Social & Community Context: Other" = "160", 
                                "Neighborhood & Built Environment: Housing" = "170", 
                                "Neighborhood & Built Environment: Transportation" = "180", 
                                "Neighborhood & Built Environment: Other" = "190", 
                                "Other Basic Needs: Government-Issued Licensure" = "200", 
                                "Other Basic Needs: Personal Development" = "210", 
                                "Other Basic Needs: Food" = "220", 
                                "Other Basic Needs: Self-Care" = "230", 
                                "Other Basic Needs: Other" = "240"),
    initial_engagement_form_complete = fct_recode(as.factor(initial_engagement_form_complete), 
                                                  "Incomplete" = "0", 
                                                  "Unverified" = "1", 
                                                  "Complete" = "2"),
    interview_package_complete = fct_recode(as.factor(interview_package_complete), 
                                            "Incomplete" = "0", 
                                            "Unverified" = "1", 
                                            "Complete" = "2"),
    prog_type = fct_recode(as.factor(prog_type), 
                           "Phone" = "1", 
                           "In-person" = "2", 
                           "Electronic" = "3", 
                           "Snail mail" = "4"),
    prog_visit = fct_recode(as.factor(prog_visit), 
                            "No show" = "1", 
                            "Cancellation" = "2", 
                            "Visit Completed" = "3", 
                            "Don't know" = "4"),
    prog_incen_yn = fct_recode(as.factor(prog_incen_yn), 
                               "Yes" = "1", 
                               "No" = "2"),
    prog_incen_yn = fct_explicit_na(prog_incen_yn, na_level = "No"),
    progress_notes_complete = fct_recode(as.factor(progress_notes_complete), 
                                         "Incomplete" = "0", 
                                         "Unverified" = "1", 
                                         "Complete" = "2"),
    ref_need1 = fct_recode(as.factor(ref_need1), 
                           "Additional counseling" = "1", 
                           "Behavioral health" = "2", 
                           "Benefits coordinator" = "3", 
                           "Better Family Life" = "4", 
                           "Educational" = "5", 
                           "Employment" = "6", 
                           "Housing" = "7", 
                           "IHN CRC" = "8", 
                           "Medical provider" = "9", 
                           "Psychiatrist" = "10", 
                           "State ID" = "11", 
                           "Transportation" = "12", 
                           "Vocational services" = "13", 
                           "VOV funding" = "14", 
                           "Utilities" = "15", 
                           "Other" = "16", 
                           "Better Family Life - De-escalation" = "17"),
    prog_incen_typ___1 = fct_recode(as.factor(prog_incen_typ___1), 
                                    "No Incentive" = "0", 
                                    "Incentive" = "1"),
    prog_incen_typ___2 = fct_recode(as.factor(prog_incen_typ___2), 
                                    "No Family Assistance" = "0", 
                                    "Family Assistance" = "1"),
   
## Creating New variables by coalescing multiple columns    
# Coalescing other variables using case_when
    Race = case_when(
      !is.na(initial_race_oth) ~ "Other race",
      White + Black + Asian + Native_Hawaiian_Pacific_Islander + American_Indian_Alaska_Native > 1 ~ "Other race",
      Asian == 1 ~ "Other race",
      Black == 1 ~ "Black",
      White == 1 ~ "White",
      Native_Hawaiian_Pacific_Islander == 1 ~ "Other race",
      American_Indian_Alaska_Native == 1 ~ "Other race",
      Other_race == 1 ~ "Other race",
      Dont_know_race == 1 ~ "Other race",
      Could_not_obtain_race == 1 ~ "Other race",
      TRUE ~ "Other race"
    ),
    Sex = case_when(
      Male == 1 ~ "Male",
      Female == 1 ~ "Female",
      Other_sex == 1 ~ "Other",
      Prefer_not_to_answer_sex == 1 ~ "Could not obtain sex",
      Could_not_obtain_sex == 1 ~ "Could not obtain sex",
      TRUE ~ "Could not obtain sex"
    ),
    Sex = case_when(
      !is.na(initial_sex_2) ~ as.character(initial_sex_2),  # Convert to character
      TRUE ~ Sex
    ),
    Client_declined_reason = case_when(
      Engaged_in_other_services_1 == 1 ~ "Engaged in other services",
      Do_not_need_services_1 == 1 ~ "Do not need services",
      Not_interested_1 == 1 ~ "Other priorities",
      Uncomfortable_with_research_1 == 1 ~ "Uncomfortable with research",
      Other_priorities_1 == 1 ~ "Other priorities",
      TRUE ~ "Other priorities"
    ),
    Client_or_Guardian_declined_reason = case_when(
      Engaged_in_other_services_2 == 1 ~ "Engaged in other services",
      Do_not_need_services_2 == 1 ~ "Do not need services",
      Not_interested_2 == 1 ~ "Other priorities",
      Uncomfortable_with_research_2 == 1 ~ "Uncomfortable with research",
      Other_priorities_2 == 1 ~ "Other priorities",
      TRUE ~ "Other priorities"
    ),
    Injury_Type = case_when(
      Gun_Shot_Wound == 1 ~ "Gun Shot Wound",
      Blunt_Trauma == 1 ~ "Other injury",
      Penetrating_Assault == 1 ~ "Stabbing",
      Unknown_injury == 1 ~ "Other injury",
      Other_injury == 1 ~ "Other injury",
      Could_not_obtain_injury == 1 ~ "Other injury",
      TRUE ~ "Other injury"
    ),
    Declined_reason_final = case_when(
      Engaged_in_other_services_1 == 1 ~ "Engaged in other services",
      Engaged_in_other_services_2 == 1 ~ "Engaged in other services",
      Do_not_need_services_1 == 1 ~ "Do not need services",
      Do_not_need_services_2 == 1 ~ "Do not need services",
      Not_interested_1 == 1 ~ "Other priorities",
      Not_interested_2 == 1 ~ "Other priorities",
      Uncomfortable_with_research_1 == 1 ~ "Uncomfortable with research",
      Uncomfortable_with_research_2 == 1 ~ "Uncomfortable with research",
      Other_priorities_1 == 1 ~ "Other priorities",
      Other_priorities_2 == 1 ~ "Other priorities",
      TRUE ~ "Other priorities"
    ),
    Client_status_final_2 = case_when(
      initial_enroll3 == "Patient enrolled" | initial_enroll2 == "Patient enrolled" | initial_enroll == "Yes" ~ "Patient enrolled",
      initial_enroll3 == "Not enrolled: Actively Declined" | initial_enroll2 == "Not enrolled: Actively Declined" | initial_enroll == "No, declined" ~ "Declined",
      initial_enroll3 == "Not enrolled: Contacted, no followup" | initial_enroll2 == "Not enrolled: Contacted, no followup" | initial_enroll == "No, never responded" ~ "Contacted, no followup",
      initial_enroll3 == "Not enrolled: Never contacted" | initial_enroll2 == "Not enrolled: Never contacted" ~ "Never contacted",
      initial_enroll3 == "Not enrolled: Ineligible" | initial_enroll2 == "Not enrolled: Ineligible" ~ "Not eligible",
      initial_enroll2 == "Active Recruitment" ~ "Active Recruitment",
      initial_enroll == "Outreach" ~ "Outreach",
      TRUE ~ "N/A"
    ),
    initial_hosp = case_when(
      initial_hosp == "Yes" ~ "Yes",
      initial_hosp == "No, ER only" ~ "No, ER only",
      initial_hosp == "Could not obtain" ~ "Could not obtain",
      TRUE ~ "Could not obtain"
    ),
    initial_enroll = case_when(
      initial_enroll == "Yes" ~ "Yes",
      initial_enroll == "No, declined" ~ "No",
      initial_enroll == "No, never responded" ~ "No",
      TRUE ~ "No"
    ),
    incentive_type = case_when(
      prog_incen_typ___1 == 1 ~ "Incentive",
      prog_incen_typ___2 == 1 ~ "Family assistance",
      TRUE ~ "None"
    ),
    ineligibility_reason = case_when(
      initial_enroll_inelig___1 == 1 ~ "Current address ineligible",
      initial_enroll_inelig___7 == 1 ~ "Relocating, address ineligible",
      initial_enroll_inelig___2 == 1 ~ "Accidental Injury",
      initial_enroll_inelig___3 == 1 ~ "Intimate Partner Violence",
      initial_enroll_inelig___4 == 1 ~ "Self-Injury",
      initial_enroll_inelig___5 == 1 ~ "Incarcerated",
      initial_enroll_inelig___6 == 1 ~ "Non-English speaking",
      initial_enroll_inelig___8 == 1 ~ "Severe cognitive impairment",
      TRUE ~ NA_character_
    ),
    No_Followup_Reason = case_when(
      initial_enroll_nofolw___1 == 1 ~ "Scheduled but no-showed or cancelled",
      initial_enroll_nofolw___2 == 1 ~ "No response after initial contact",
      initial_enroll_nofolw___3 == 1 ~ "Letter mailed to address",
      initial_enroll_nofolw___4 == 1 ~ "Patient Contacted",
      initial_enroll_nofolw___5 == 1 ~ "Guardian/Parent Contacted",
      TRUE ~ NA_character_
    ),
    No_Contact_Reason = case_when(
      initial_enroll_nocontct___1 == 1 ~ "No phone available",
      initial_enroll_nocontct___2 == 1 ~ "Called phone number(s), no response",
      initial_enroll_nocontct___3 == 1 ~ "Called phone number(s), wrong number",
      initial_enroll_nocontct___4 == 1 ~ "Called phone number(s), disconnected",
      initial_enroll_nocontct___5 == 1 ~ "Left voicemail, no response",
      initial_enroll_nocontct___6 == 1 ~ "Letter mailed to address",
      TRUE ~ NA_character_
    ),
    No_Response_Reason = case_when(
      initial_noresp_reason___1 == 1 ~ "Connected, but no enrollment decision made",
      initial_noresp_reason___2 == 1 ~ "Phone number disconnected",
      initial_noresp_reason___3 == 1 ~ "No answer at phone number",
      initial_noresp_reason___4 == 1 ~ "Wrong phone number",
      initial_noresp_reason___5 == 1 ~ "Voicemail not returned",
      initial_noresp_reason___6 == 1 ~ "No response to letter (snail mail)",
      initial_noresp_reason___7 == 1 ~ "Other (describe in text box)",
      TRUE ~ NA_character_
    )
  )

## Removing unnecessary variables after coalescing
# Remove variables that have been coalesced and that we no longer need
Compressed_LOV_records <- Collapsedvars_LOV_records %>%
  select(
    -White, -Black, -Asian, -Native_Hawaiian_Pacific_Islander, -American_Indian_Alaska_Native, -Other_race, -Dont_know_race, -Could_not_obtain_race, -initial_race_oth, -Prefer_not_to_answer_race,
    -Male, -Female, -Other_sex, -Prefer_not_to_answer_sex, -Could_not_obtain_sex, -initial_sex_2, -initial_sex_oth,
    -initial_enroll3, -initial_enroll2, -initial_enroll, -enroll_status,
    -Engaged_in_other_services_1, -Do_not_need_services_1, -Not_interested_1, -Uncomfortable_with_research_1, -Other_priorities_1,
    -Engaged_in_other_services_2, -Do_not_need_services_2, -Not_interested_2, -Uncomfortable_with_research_2, -Other_priorities_2,
    -Gun_Shot_Wound, -Blunt_Trauma, -Penetrating_Assault, -Unknown_injury, -Other_injury, -Could_not_obtain_injury,
    -prog_incen_typ___1, -prog_incen_typ___2,
    -initial_enroll_inelig___1, -initial_enroll_inelig___2, -initial_enroll_inelig___3, -initial_enroll_inelig___4, -initial_enroll_inelig___5, -initial_enroll_inelig___6, -initial_enroll_inelig___7, -initial_enroll_inelig___8,
    -initial_enroll_nofolw___1, -initial_enroll_nofolw___2, -initial_enroll_nofolw___3, -initial_enroll_nofolw___4, -initial_enroll_nofolw___5,
    -initial_enroll_nocontct___1, -initial_enroll_nocontct___2, -initial_enroll_nocontct___3, -initial_enroll_nocontct___4, -initial_enroll_nocontct___5, -initial_enroll_nocontct___6,
    -initial_noresp_reason___1, -initial_noresp_reason___2, -initial_noresp_reason___3, -initial_noresp_reason___4, -initial_noresp_reason___5, -initial_noresp_reason___6, -initial_noresp_reason___7
  )

## Converting dates to proper format
# Ensure dates are in Date format (y-m-d)
Compressed_LOV_records <- Compressed_LOV_records %>%
  mutate(
    initial_record_date = ymd_hms(initial_record_date),
    initial_enroll_date = ymd(initial_enroll_date),
    inter_date = ymd(inter_date),
    case_initial_dt = ymd(case_initial_dt),
    prog_created_dt = ymd(prog_created_dt),
    prog_visit_dt = ymd(prog_visit_dt),
    ref_need1_coun_dt = ymd(ref_need1_coun_dt),
    initial_inj_dt = ymd(initial_inj_dt)
  )

## Creating LOV_IDs_only dataframe
## Cleaning Contact/outreach variables and recoding
# Change initial_con_by_who variables to characters
LOV_IDs_only <- Compressed_LOV_records %>%
  mutate(across(starts_with("initial_con") & ends_with("bywho"), as.character))

# Define the mapping of contact types to the new factor levels
contact_types <- c("Phone" = "1", "In-person: Other" = "2", "Electronic" = "3", "Snail Mail" = "4", "In Person: At Bedside" = "5")

# Define the mapping of contact by who values to the new factor levels
contacted_bywho <- c("Outreach Representative" = "10", "Case Manager" = "20")

# Recode contact type and contact by who variables as factors
LOV_IDs_only <- LOV_IDs_only %>%
  mutate(across(starts_with("initial_con") & ends_with("typ"), ~ fct_recode(as.factor(.), !!!contact_types))) %>%
  mutate(across(starts_with("initial_con") & ends_with("bywho"), ~ fct_recode(as.factor(.), !!!contacted_bywho)))

# Create columns for the counts of Outreach Representatives and Case Managers
LOV_IDs_only <- LOV_IDs_only %>%
  mutate(
    Outreach_Rep_Count = rowSums(across(starts_with("initial_con") & ends_with("bywho"), ~. == "Outreach Representative")),
    Case_Manager_Count = rowSums(across(starts_with("initial_con") & ends_with("bywho"), ~. == "Case Manager"))
  )

## Creating Enrolled_LOV_IDs dataframe
# Create the new dataframe with only enrolled participants based on client enroll status
Enrolled_LOV_IDs <- LOV_IDs_only %>%
  filter(Client_status_final_2 == "Patient enrolled")

## Calculating time-based metrics
# Calculate time to initial contact, time from initial record to enrollment, and time to enrollment 
# Start date & end date for each 
Enrolled_LOV_IDs <- Enrolled_LOV_IDs %>%
  mutate(
    time_to_initial_contact = as.integer(difftime(initial_con1, initial_record_date, units = "days")),
    time_from_initial_record = as.integer(difftime(initial_enroll_date, initial_record_date, units = "days")),
    time_to_enrollment = as.integer(difftime(initial_enroll_date, initial_con1, units = "days"))
  )


# Creating ranges of days for time between initial injury date and initial contact date
# Calculate time to initial contact and create categories for ranges of days between initial injury and initial contact
LOV_IDs_only <- LOV_IDs_only %>%
  mutate(
    time_bw_injuryandcontact = as.integer(difftime(initial_con1, initial_inj_dt, units = "days")),
    contact_time_bucket = case_when(
      time_bw_injuryandcontact <= 3 ~ "0-3",
      time_bw_injuryandcontact <= 7 ~ "4-7",
      time_bw_injuryandcontact <= 15 ~ "8-15",
      time_bw_injuryandcontact <= 30 ~ "16-30",
      time_bw_injuryandcontact > 30 ~ "Over 30",
      TRUE ~ NA_character_
    )
  )

## Calculating date ranges and error checks
# Looking for errors in dates and date range
# Calculate date ranges for Enrolled_LOV_IDs
date_ranges_enrolled <- Enrolled_LOV_IDs %>%
  summarize(
    min_initial_con1 = min(initial_con1, na.rm = TRUE),
    max_initial_con1 = max(initial_con1, na.rm = TRUE),
    min_initial_record_date = min(initial_record_date, na.rm = TRUE),
    max_initial_record_date = max(initial_record_date, na.rm = TRUE),
    min_initial_enroll_date = min(initial_enroll_date, na.rm = TRUE),
    max_initial_enroll_date = max(initial_enroll_date, na.rm = TRUE)
  )

# Calculate date ranges for LOV_IDs_only
date_ranges_lov <- LOV_IDs_only %>%
  summarize(
    min_initial_con1 = min(initial_con1, na.rm = TRUE),
    max_initial_con1 = max(initial_con1, na.rm = TRUE),
    min_initial_inj_dt = min(initial_inj_dt, na.rm = TRUE),
    max_initial_inj_dt = max(initial_inj_dt, na.rm = TRUE)
  )

# 
start_date <- as.Date("2018-01-01")
end_date <- Sys.Date()

LOV_IDs_only <- LOV_IDs_only %>%
  mutate(initial_inj_dt = case_when(
    initial_inj_dt < start_date ~ as.Date(NA),
    initial_inj_dt > end_date ~ as.Date(NA),
    TRUE ~ initial_inj_dt
  ))

## Progress notes, incentives, and referrals analysis
# Looking at progress notes, incentives, and referrals for each participant
# Filter for rows where the repeat instrument is "progress_notes" as these can be multiple for one participant, "contact points"
progress_notes_data <- Raw_LOV_data %>%
  filter(redcap_repeat_instrument == "progress_notes")

# Count the number of progress notes per participant
progress_notes_count <- progress_notes_data %>%
  group_by(study_id) %>%
  summarise(num_progress_notes = n())

# Merge this back with the original data if needed
Enrolled_LOV_IDs <- Enrolled_LOV_IDs %>%
  left_join(progress_notes_count, by = "study_id")

# Creating progress notes bucket categories 
Enrolled_LOV_IDs <- Enrolled_LOV_IDs %>%
  mutate(
    progress_notes_bucket = case_when(
      num_progress_notes == 0 ~ "0",
      num_progress_notes >= 1 & num_progress_notes <= 10 ~ "1-10",
      num_progress_notes >= 11 & num_progress_notes <= 20 ~ "11-20",
      num_progress_notes >= 21 & num_progress_notes <= 30 ~ "21-30",
      num_progress_notes >= 31 & num_progress_notes <= 50 ~ "31-50",
      num_progress_notes >= 51 & num_progress_notes <= 100 ~ "51-100",
      num_progress_notes > 100 ~ "101+",
      TRUE ~ "0"
    )
  )

# Create a yes/no indicator for incentives before collapsing by study_id
incentive_data <- Raw_LOV_data %>%
  group_by(study_id) %>%
  summarise(incentive_status = ifelse(any(prog_incen_yn == 1), "Yes", "No"))

# Merge this back with the original data if needed
Enrolled_LOV_IDs <- Enrolled_LOV_IDs %>%
  left_join(incentive_data, by = "study_id")

# Create referral_status for each participant
referral_data <- Raw_LOV_data %>%
  mutate(referral_status = ifelse(
    rowSums(select(., starts_with("prog_refneed1___")) == 1, na.rm = TRUE) > 0 | 
      !is.na(ref_need1), 
    "Yes", 
    "No"
  )) %>%
  group_by(study_id) %>%
  mutate(referral_status = ifelse(any(referral_status == "Yes"), "Yes", "No")) %>%
  summarise(across(everything(), ~ .[which(!is.na(.))[1]])) %>%
  select(study_id, referral_status)

# Merge this back with the main data
Enrolled_LOV_IDs <- Enrolled_LOV_IDs %>%
  left_join(referral_data, by = "study_id")





