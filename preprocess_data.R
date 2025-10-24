# Data preprocessing script for platform-study-rr-sleep
# This script processes raw data files and creates derived variables
# needed for analysis

# Install questionnaires package if not already installed
if (!requireNamespace("questionnaires", quietly = TRUE)) {
  # Install from LCBC-UiO r-universe
  options(repos = c(
    lcbcuio = 'https://lcbc-uio.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
  install.packages('questionnaires')
}

library(tidyverse)
library(lubridate)
library(mctq)  # For proper MCTQ chronotype calculations
library(questionnaires)  # For PSQI computation

# Function to rename PSQI columns to match Questionnaires package format
# Converts platform-study format to standard PSQI naming convention
prepare_psqi_columns <- function(data) {
  data %>%
    mutate(
      # Q1: Bedtime (HH:MM format) - combine hour, minute, AM/PM
      # Note: Some entries may already be in 24-hour format (hour > 12)
      bedtime_hour = as.numeric(`psqi_1#1_1_1`),
      bedtime_min = as.numeric(`psqi_1#1_1_2`),
      bedtime_ampm = `psqi_1#2_1`,
      bedtime_hour_24 = case_when(
        is.na(bedtime_hour) ~ NA_real_,
        bedtime_hour > 12 ~ bedtime_hour,  # Already in 24-hour format
        bedtime_ampm == "PM" & bedtime_hour != 12 ~ bedtime_hour + 12,
        bedtime_ampm == "AM" & bedtime_hour == 12 ~ 0,
        TRUE ~ bedtime_hour
      ),
      psqi_01 = if_else(is.na(bedtime_hour_24), NA_character_,
                        sprintf("%02d:%02d:00", bedtime_hour_24, bedtime_min)),

      # Q2: Minutes before sleep
      psqi_02 = as.numeric(psqi_2),

      # Q3: Rising time (HH:MM format) - combine hour, minute, AM/PM
      # Note: Some entries may already be in 24-hour format (hour > 12)
      risetime_hour = as.numeric(`psqi_3#1_1_1`),
      risetime_min = as.numeric(`psqi_3#1_1_2`),
      risetime_ampm = `psqi_3#2_1`,
      risetime_hour_24 = case_when(
        is.na(risetime_hour) ~ NA_real_,
        risetime_hour > 12 ~ risetime_hour,  # Already in 24-hour format
        risetime_ampm == "PM" & risetime_hour != 12 ~ risetime_hour + 12,
        risetime_ampm == "AM" & risetime_hour == 12 ~ 0,
        TRUE ~ risetime_hour
      ),
      psqi_03 = if_else(is.na(risetime_hour_24), NA_character_,
                        sprintf("%02d:%02d:00", risetime_hour_24, risetime_min)),

      # Q4: Hours of sleep (decimal hours)
      sleep_hours = as.numeric(`psqi_4#1_1_1`),
      sleep_minutes = as.numeric(`psqi_4#1_1_2`),
      psqi_04 = sleep_hours + sleep_minutes / 60,

      # Q5a: Trouble sleeping within 30min (frequency scale 0-3)
      psqi_05a = case_when(
        psqi_5_1 == "Not during the past month" ~ 0,
        psqi_5_1 == "Less than once a week" ~ 1,
        psqi_5_1 == "Once or twice a week" ~ 2,
        psqi_5_1 == "Three or more times a week" ~ 3,
        TRUE ~ NA_real_
      ),

      # Q5b-j: Sleep troubles (frequency scale 0-3)
      psqi_05b = case_when(
        psqi_5_2 == "Not during the past month" ~ 0,
        psqi_5_2 == "Less than once a week" ~ 1,
        psqi_5_2 == "Once or twice a week" ~ 2,
        psqi_5_2 == "Three or more times a week" ~ 3,
        TRUE ~ NA_real_
      ),
      psqi_05c = case_when(
        psqi_5_3 == "Not during the past month" ~ 0,
        psqi_5_3 == "Less than once a week" ~ 1,
        psqi_5_3 == "Once or twice a week" ~ 2,
        psqi_5_3 == "Three or more times a week" ~ 3,
        TRUE ~ NA_real_
      ),
      psqi_05d = case_when(
        psqi_5_4 == "Not during the past month" ~ 0,
        psqi_5_4 == "Less than once a week" ~ 1,
        psqi_5_4 == "Once or twice a week" ~ 2,
        psqi_5_4 == "Three or more times a week" ~ 3,
        TRUE ~ NA_real_
      ),
      psqi_05e = case_when(
        psqi_5_5 == "Not during the past month" ~ 0,
        psqi_5_5 == "Less than once a week" ~ 1,
        psqi_5_5 == "Once or twice a week" ~ 2,
        psqi_5_5 == "Three or more times a week" ~ 3,
        TRUE ~ NA_real_
      ),
      psqi_05f = case_when(
        psqi_5_6 == "Not during the past month" ~ 0,
        psqi_5_6 == "Less than once a week" ~ 1,
        psqi_5_6 == "Once or twice a week" ~ 2,
        psqi_5_6 == "Three or more times a week" ~ 3,
        TRUE ~ NA_real_
      ),
      psqi_05g = case_when(
        psqi_5_7 == "Not during the past month" ~ 0,
        psqi_5_7 == "Less than once a week" ~ 1,
        psqi_5_7 == "Once or twice a week" ~ 2,
        psqi_5_7 == "Three or more times a week" ~ 3,
        TRUE ~ NA_real_
      ),
      psqi_05h = case_when(
        psqi_5_8 == "Not during the past month" ~ 0,
        psqi_5_8 == "Less than once a week" ~ 1,
        psqi_5_8 == "Once or twice a week" ~ 2,
        psqi_5_8 == "Three or more times a week" ~ 3,
        TRUE ~ NA_real_
      ),
      psqi_05i = case_when(
        psqi_5_9 == "Not during the past month" ~ 0,
        psqi_5_9 == "Less than once a week" ~ 1,
        psqi_5_9 == "Once or twice a week" ~ 2,
        psqi_5_9 == "Three or more times a week" ~ 3,
        TRUE ~ NA_real_
      ),
      # Q5j: Other reasons (frequency scale 0-3)
      # Note: The frequency can be in either psqi_5_c or psqi_5_cr_1 depending on
      # whether a text description was provided. When text is provided, it goes in
      # psqi_5_c and frequency goes in psqi_5_cr_1. Otherwise frequency is in psqi_5_c.
      psqi_05j = case_when(
        # First check psqi_5_cr_1 (used when there's a text description)
        psqi_5_cr_1 == "Not during the past month" ~ 0,
        psqi_5_cr_1 == "Less than once a week" ~ 1,
        psqi_5_cr_1 == "Once or twice a week" ~ 2,
        psqi_5_cr_1 == "Three or more times a week" ~ 3,
        # Fall back to psqi_5_c (used when no text description provided)
        psqi_5_c == "Not during the past month" ~ 0,
        psqi_5_c == "Less than once a week" ~ 1,
        psqi_5_c == "Once or twice a week" ~ 2,
        psqi_5_c == "Three or more times a week" ~ 3,
        TRUE ~ NA_real_
      ),

      # Q6: Sleep quality (0-3 scale: 0=Very good, 1=Fairly good, 2=Fairly bad, 3=Very bad)
      psqi_06 = case_when(
        psqi_6 == "Very good" ~ 0,
        psqi_6 == "Fairly good" ~ 1,
        psqi_6 == "Fairly bad" ~ 2,
        psqi_6 == "Very bad" ~ 3,
        TRUE ~ NA_real_
      ),

      # Q7: Sleep medication (frequency scale 0-3)
      psqi_07 = case_when(
        psqi_7 == "Not during the past month" ~ 0,
        psqi_7 == "Less than once a week" ~ 1,
        psqi_7 == "Once or twice a week" ~ 2,
        psqi_7 == "Three or more times a week" ~ 3,
        TRUE ~ NA_real_
      ),

      # Q8: Trouble staying awake (frequency scale 0-3)
      psqi_08 = case_when(
        psqi_8 == "Not during the past month" ~ 0,
        psqi_8 == "Less than once a week" ~ 1,
        psqi_8 == "Once or twice a week" ~ 2,
        psqi_8 == "Three or more times a week" ~ 3,
        TRUE ~ NA_real_
      ),

      # Q9: Enthusiasm/maintaining problem (0-3 scale)
      psqi_09 = case_when(
        psqi_9 == "No problem at all" ~ 0,
        psqi_9 == "Only a very slight problem" ~ 1,
        psqi_9 == "Somewhat of a problem" ~ 2,
        psqi_9 == "A very big problem" ~ 3,
        TRUE ~ NA_real_
      )
    ) %>%
    # Remove temporary columns
    select(-bedtime_hour, -bedtime_min, -bedtime_ampm, -bedtime_hour_24,
           -risetime_hour, -risetime_min, -risetime_ampm, -risetime_hour_24,
           -sleep_hours, -sleep_minutes)
}

# Function to compute all PSQI components and global score
# Uses the questionnaires package for standardized PSQI computation
compute_psqi <- function(data) {
  # The psqi_compute function from questionnaires package calculates:
  # - All 7 PSQI components (comp1-comp7)
  # Then psqi_compute_global() sums them to create the global score
  # Note: Data must have columns psqi_01 through psqi_09 properly formatted
  # - Time columns (psqi_01, psqi_03) in "HH:MM:SS" format
  # - All 4-option questions coded as 0-3
  # - Sleep hours (psqi_04) as decimal number

  data %>%
    prepare_psqi_columns() %>%
    psqi_compute(
      bedtime = psqi_01,
      min_before_sleep = psqi_02,
      risingtime = psqi_03,
      hours_sleep = psqi_04,
      no_sleep_30min = psqi_05a,
      sleepquality = psqi_06,
      medication = psqi_07,
      keep_awake = psqi_08,
      keep_enthused = psqi_09,
      sleep_troubles = matches("^psqi_05[b-j]$"),
      components = 1:7,
      max_missing = 1,  # All components available (comp4 calculated manually due to bug in questionnaires v0.0.3)
      prefix = "psqi_",
      keep_all = TRUE
    ) %>%
    mutate(
      # Manual calculation of Component 4 (Sleep Efficiency)
      # Bug in questionnaires package v0.0.3 returns NA for comp4
      # Following PSQI Scoring Manual with correction for overnight sleep:
      bedtime_hms = suppressWarnings(hms::parse_hms(psqi_01)),
      waketime_hms = suppressWarnings(hms::parse_hms(psqi_03)),
      # Calculate difference in hours (wake_time - bed_time)
      diffhour = as.numeric(difftime(waketime_hms, bedtime_hms, units = "hours")),
      # If negative (overnight sleep), add 24 hours
      # If > 24 (unusual case), wrap around by subtracting 24
      newtib = case_when(
        diffhour < 0 ~ diffhour + 24,
        diffhour > 24 ~ diffhour - 24,
        TRUE ~ diffhour
      ),
      # Calculate sleep efficiency percentage (hours asleep / hours in bed * 100)
      tmphse = (psqi_04 / newtib) * 100,
      # Score according to PSQI manual: >85=0, ≤85 and >75=1, ≤75 and >65=2, ≤65=3
      psqi_comp4_efficiency = case_when(
        is.na(tmphse) ~ NA_real_,
        tmphse > 85 ~ 0,
        tmphse > 75 ~ 1,
        tmphse > 65 ~ 2,
        TRUE ~ 3
      ),
      # Recalculate global score with corrected component 4
      psqi_global = rowSums(pick(psqi_comp1_quality, psqi_comp2_latency, psqi_comp3_duration,
                                  psqi_comp4_efficiency, psqi_comp5_problems, psqi_comp6_medication,
                                  psqi_comp7_tired), na.rm = FALSE),
      # Extract sleep duration in hours for compatibility
      total_hours_sleep = psqi_04
    ) %>%
    select(-bedtime_hms, -waketime_hms, -diffhour, -newtib, -tmphse)
}

# Function to compute Epworth Sleepiness Scale total
compute_eps_total <- function(data) {
  data %>%
    mutate(
      # Sum EPS items (eps_1_1 through eps_1_8)
      # Convert text responses to numeric if needed
      across(starts_with("eps_1_"), ~case_when(
        . == "No chance of dozing" ~ 0,
        . == "Slight chance of dozing" ~ 1,
        . == "Moderate chance of dozing" ~ 2,
        . == "High chance of dozing" ~ 3,
        TRUE ~ as.numeric(.)
      ), .names = "{.col}_num")
    ) %>%
    mutate(
      epsTotal = rowSums(pick(ends_with("_num")), na.rm = FALSE)
    ) %>%
    select(-ends_with("_num"))
}

# Function to compute WEMWBS total
compute_wemwbs <- function(data) {
  data %>%
    mutate(
      # WEMWBS can be 7 items (SWEMWBS, range 7-35) or 14 items (full WEMWBS, range 14-70)
      # Convert text responses to numeric if needed
      across(starts_with("wemwbs_"), ~case_when(
        . == "None of the time" ~ 1,
        . == "Rarely" ~ 2,
        . == "Some of the time" ~ 3,
        . == "Often" ~ 4,
        . == "All of the time" ~ 5,
        TRUE ~ as.numeric(.)
      ), .names = "{.col}_num")
    ) %>%
    mutate(
      # Sum all available wemwbs items (1-14 if present, otherwise 1-7)
      wemwbs = rowSums(pick(matches("^wemwbs_[0-9]+_num$")), na.rm = FALSE)
    ) %>%
    select(-ends_with("_num"))
}

# Function to compute MSFsc (mid-sleep on free days corrected for sleep debt)
# Using the mctq package: https://github.com/ropensci/mctq
# Reshapes our MCTQ data to match the expected format, then uses mctq functions
compute_mctq_msf_sc <- function(data) {
  data %>%
    mutate(
      # Create a temporary dataset in mctq expected format
      # Our mapping (based on MCTQ codebook):
      # mctq_1: work (Yes/No -> TRUE/FALSE)
      # mctq_3_1: bt_w (bedtime work - Image 1: going to bed)
      # mctq_3_2: time awake in bed before sleep prep (minutes) - Image 2
      # mctq_3_3: sprep_w (sleep prep time work - Image 3: ready to fall asleep)
      # mctq_3_4: slat_w (sleep latency work, minutes - Image 4: time to fall asleep)
      # mctq_3_5: se_w (wake time work - Image 5: wake up at)
      # mctq_3_6: si_w (sleep inertia work - Image 6: time to get up after waking)
      # mctq_4_1: alarm_w (alarm on work days)
      # mctq_6_1-6: same pattern for free days
      # mctq_7_1: alarm_f (alarm on free days)
      #
      # Note: For MSFsc calculation, we use sleep prep time (mctq_3_3/mctq_6_3)
      # as the bedtime equivalent and add sleep latency (mctq_3_4/mctq_6_4).

      # Map to mctq format
      work = case_when(
        mctq_1 == "Yes" ~ TRUE,
        mctq_1 == "No" ~ FALSE,
        TRUE ~ NA
      ),

      wd = case_when(
        mctq_4_1 == "Yes" ~ 5,  # Assume 5 work days if using alarm on work days
        mctq_4_1 == "No" ~ 0,
        TRUE ~ 5
      ),

      # Data quality check: validate bedtime -> sleep prep interval
      # Calculate interval between bedtime (3_1/6_1) and sleep prep (3_3/6_3)
      bt_raw_w = hms::as_hms(mctq_3_1),
      sp_raw_w = hms::as_hms(mctq_3_3),
      bt_raw_f = hms::as_hms(mctq_6_1),
      sp_raw_f = hms::as_hms(mctq_6_3),

      # Calculate intervals (handling overnight transitions)
      # Interval 1: Bedtime -> Sleep prep
      int_bt_sp_w = as.numeric(difftime(sp_raw_w, bt_raw_w, units = "hours")),
      int_bt_sp_w = ifelse(int_bt_sp_w < 0, int_bt_sp_w + 24, int_bt_sp_w),

      int_bt_sp_f = as.numeric(difftime(sp_raw_f, bt_raw_f, units = "hours")),
      int_bt_sp_f = ifelse(int_bt_sp_f < 0, int_bt_sp_f + 24, int_bt_sp_f),

      # Interval 2: Sleep prep -> Wake time (for sleep duration validation)
      wake_raw_w = hms::as_hms(mctq_3_5),
      wake_raw_f = hms::as_hms(mctq_6_5),

      int_sp_wake_w = as.numeric(difftime(wake_raw_w, sp_raw_w, units = "hours")),
      int_sp_wake_w = ifelse(int_sp_wake_w < 0, int_sp_wake_w + 24, int_sp_wake_w),

      int_sp_wake_f = as.numeric(difftime(wake_raw_f, sp_raw_f, units = "hours")),
      int_sp_wake_f = ifelse(int_sp_wake_f < 0, int_sp_wake_f + 24, int_sp_wake_f),

      # Work days - convert to hms and apply validation (survey requested 24-hour format)
      # Validation rule: Set wake time to NA if sleep prep->wake interval < 2 hours (very short sleep only)
      bt_w = hms::as_hms(mctq_3_1),
      sprep_w = hms::as_hms(mctq_3_3),
      slat_w = lubridate::dminutes(as.numeric(mctq_3_4)),  # Sleep latency
      se_w = case_when(
        int_sp_wake_w < 2 ~ NA,  # Very short sleep duration
        TRUE ~ hms::as_hms(mctq_3_5)
      ),
      si_w = lubridate::dminutes(as.numeric(mctq_3_6)),  # Sleep inertia
      alarm_w = mctq_4_1 == "Yes",

      # Free days - convert to hms and apply validation (survey requested 24-hour format)
      # Validation rule: Set wake time to NA if sleep prep->wake interval < 2 hours (very short sleep only)
      bt_f = hms::as_hms(mctq_6_1),
      sprep_f = hms::as_hms(mctq_6_3),
      slat_f = lubridate::dminutes(as.numeric(mctq_6_4)),  # Sleep latency
      se_f = case_when(
        int_sp_wake_f < 2 ~ NA,  # Very short sleep duration
        TRUE ~ hms::as_hms(mctq_6_5)
      ),
      si_f = lubridate::dminutes(as.numeric(mctq_6_6)),  # Sleep inertia
      alarm_f = mctq_7_1 == "Yes"
    ) %>%
    mutate(
      # Now use mctq functions with properly formatted data
      # Sleep onset = sleep prep + sleep latency
      so_w = mctq::so(sprep_w, slat_w),
      so_f = mctq::so(sprep_f, slat_f),

      # Sleep duration
      sd_w = mctq::sdu(so_w, se_w),
      sd_f = mctq::sdu(so_f, se_f),

      # Mid-sleep on free days
      msf = mctq::msl(so_f, sd_f),

      # Weekly average sleep duration
      sd_week = mctq::sd_week(sd_w, sd_f, wd),

      # Sleep corrected mid-sleep on free days
      msf_sc = mctq::msf_sc(msf, sd_w, sd_f, sd_week, alarm_f),

      # Convert to numeric hours for modeling
      msf_sc_numeric = as.numeric(msf_sc) / 3600
    ) %>%
    select(-work, -wd, -bt_w, -sprep_w, -slat_w, -se_w, -si_w, -alarm_w,
           -bt_f, -sprep_f, -slat_f, -se_f, -si_f, -alarm_f,
           -so_w, -so_f, -sd_w, -sd_f, -msf, -sd_week, -msf_sc,
           -bt_raw_w, -sp_raw_w, -bt_raw_f, -sp_raw_f,
           -wake_raw_w, -wake_raw_f,
           -int_bt_sp_w, -int_bt_sp_f, -int_sp_wake_w, -int_sp_wake_f)
}

# Process intake data
process_intake <- function(intake_path) {
  intake <- read_csv(intake_path, show_col_types = FALSE) %>%
    mutate(
      # Calculate age (age is already in the data)
      age_scaled = scale(age)[,1],

      # Calculate BMI from height (inches) and weight (kg)
      # Note: height is in inches, weight is already in kg
      height_m = height * 0.0254,  # inches to meters
      bmi = weight / (height_m^2),  # weight is already in kg
      bmi_scaled = scale(bmi)[,1],

      # SES index - composite of education and employment
      # This is a simplified version - adjust based on actual needs
      edu_numeric = case_when(
        edu_level == "No formal qualifications" ~ 1,
        edu_level == "Other qualifications" ~ 2,
        str_detect(edu_level, "Higher National|Bachelor|graduate") ~ 3,
        TRUE ~ 2
      ),

      employment_numeric = case_when(
        employment == "Working full-time" ~ 3,
        employment == "Working part-time" ~ 2,
        TRUE ~ 1
      ),

      SES_index = (edu_numeric + employment_numeric) / 2,
      SES_index_scaled = scale(SES_index)[,1],

      # Create region variable from country (UK or US)
      region = country
    )

  return(intake)
}

# Process panel data
process_panel <- function(panel_path, intake_processed) {
  panel <- read_csv(panel_path, show_col_types = FALSE) %>%
    # Compute derived variables
    compute_psqi() %>%
    compute_eps_total() %>%
    compute_wemwbs() %>%
    compute_mctq_msf_sc() %>%
    # Join with intake data
    left_join(
      intake_processed %>%
        select(pid, age_scaled, bmi_scaled, SES_index_scaled, region, gender),
      by = "pid"
    ) %>%
    mutate(
      psqi_6_ord = factor(psqi_06,
                         levels = c("Very good", "Fairly good", "Fairly bad", "Very bad"),
                         ordered = TRUE),
      pid = as.character(pid)
    )

  return(panel)
}

# Main preprocessing function
preprocess_all_data <- function() {
  # Process intake
  intake_processed <- process_intake("data/intake.csv.gz")

  # Process panel
  panel_processed <- process_panel("data/panel.csv.gz", intake_processed)

  # Save processed data
  dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
  write_csv(panel_processed, "data/processed/panel_clean.csv.gz")
  write_csv(intake_processed, "data/processed/intake_clean.csv.gz")

  message("Data preprocessing complete!")
  message("Processed files saved to data/processed/")
}

# Run preprocessing if script is sourced
if (!interactive()) {
  preprocess_all_data()
}
