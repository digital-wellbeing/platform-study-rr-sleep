# Data preprocessing script for manuscript
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
      region = country,

      # Recode gender: 3-level variable with Man, Woman, and "Non-binary or other gender identity"
      gender = case_when(
        gender == "Man" ~ "Man",
        gender == "Woman" ~ "Woman",
        !is.na(gender) ~ "Non-binary or other gender identity",
        TRUE ~ NA_character_
      ),

      # Process timezone data with UK imputation
      # Convert GMT offset format (e.g., "GMT-0500") to hours offset
      tz_offset_hours = case_when(
        is.na(local_timezone) ~ NA_real_,
        str_detect(local_timezone, "GMT[+-]") ~ {
          sign <- ifelse(str_detect(local_timezone, "GMT-"), -1, 1)
          offset_str <- str_extract(local_timezone, "\\d{4}")
          hours <- as.numeric(substr(offset_str, 1, 2))
          minutes <- as.numeric(substr(offset_str, 3, 4))
          sign * (hours + minutes/60)
        },
        TRUE ~ 0  # GMT+0000
      ),
      # IMPORTANT: Impute timezone for UK participants
      # UK has only one timezone: GMT (UTC+0)
      tz_offset_hours = case_when(
        !is.na(tz_offset_hours) ~ tz_offset_hours,  # Keep existing timezone
        country == "UK" ~ 0,  # Impute GMT+0000 for UK participants
        TRUE ~ NA_real_  # Keep NA for others (US without timezone)
      ),
      timezone_source = case_when(
        !is.na(local_timezone) ~ "Reported",
        country == "UK" & is.na(local_timezone) ~ "Imputed (UK=GMT)",
        TRUE ~ "Missing"
      )
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

# Process gaming data
process_gaming_data <- function(panel_path, intake_processed) {
  library(data.table)  # For efficient concurrent session calculation

  # Load panel data to calculate day 0 (study start date) for each participant
  data.panel.raw <- read_csv(panel_path, show_col_types = FALSE) %>%
    mutate(date = as_datetime(date))

  # Calculate day 0 (first panel date) for each participant
  day_zero <- data.panel.raw %>%
    group_by(pid) %>%
    summarise(day_0 = min(date, na.rm = TRUE), .groups = "drop") %>%
    mutate(pid = as.character(pid))

  message(sprintf("Calculated day 0 for %d participants", nrow(day_zero)))

  # Identify participants with at least one valid outcome measure
  # Outcome measures: WEMWBS (wellbeing), PSQI duration/quality, ESS (sleepiness)
  outcome_check <- data.panel.raw %>%
    mutate(
      # Check if any WEMWBS items are non-missing
      has_wemwbs = !is.na(wemwbs_1) | !is.na(wemwbs_2) | !is.na(wemwbs_3) |
                   !is.na(wemwbs_4) | !is.na(wemwbs_5) | !is.na(wemwbs_6) |
                   !is.na(wemwbs_7),
      # Check if any PSQI items are non-missing (duration and quality)
      has_psqi = !is.na(`psqi_1#1_1_1`) | !is.na(psqi_2) | !is.na(psqi_6) |
                 !is.na(psqi_9),
      # Check if any ESS items are non-missing
      has_ess = !is.na(eps_1_1) | !is.na(eps_1_2) | !is.na(eps_1_3) |
                !is.na(eps_1_4) | !is.na(eps_1_5) | !is.na(eps_1_6) |
                !is.na(eps_1_7) | !is.na(eps_1_8)
    )

  valid_participants_outcomes <- outcome_check %>%
    filter(has_wemwbs | has_psqi | has_ess) %>%
    pull(pid) %>%
    unique()

  n_with_outcomes <- length(valid_participants_outcomes)
  n_without_outcomes <- nrow(day_zero) - n_with_outcomes

  message(sprintf("\n=== STEP 1: Outcome Measure Filter ==="))
  message(sprintf("Participants with valid outcome data: %d", n_with_outcomes))
  message(sprintf("Participants WITHOUT outcome data: %d (excluded)", n_without_outcomes))

  # Load timezone data from intake
  timezone_data <- intake_processed %>%
    select(pid, local_timezone, country) %>%
    mutate(
      # Convert GMT offset format (e.g., "GMT-0500") to hours offset
      tz_offset_hours = case_when(
        is.na(local_timezone) ~ NA_real_,
        str_detect(local_timezone, "GMT[+-]") ~ {
          sign <- ifelse(str_detect(local_timezone, "GMT-"), -1, 1)
          offset_str <- str_extract(local_timezone, "\\d{4}")
          hours <- as.numeric(substr(offset_str, 1, 2))
          minutes <- as.numeric(substr(offset_str, 3, 4))
          sign * (hours + minutes/60)
        },
        TRUE ~ 0  # GMT+0000
      ),
      # IMPORTANT: Impute timezone for UK participants
      # UK has only one timezone: GMT (UTC+0)
      tz_offset_hours = case_when(
        !is.na(tz_offset_hours) ~ tz_offset_hours,  # Keep existing timezone
        country == "UK" ~ 0,  # Impute GMT+0000 for UK participants
        TRUE ~ NA_real_  # Keep NA for others (US without timezone)
      ),
      timezone_source = case_when(
        !is.na(local_timezone) ~ "Reported",
        country == "UK" & is.na(local_timezone) ~ "Imputed (UK=GMT)",
        TRUE ~ "Missing"
      )
    )

  # Participants with valid timezone (all participants have timezone if they're UK)
  # For US participants, we require explicit timezone data
  valid_participants_timezone <- timezone_data %>%
    filter(!is.na(tz_offset_hours)) %>%
    pull(pid) %>%
    unique()

  n_with_timezone <- length(valid_participants_timezone)
  n_total_intake <- nrow(intake_processed)
  n_without_timezone <- n_total_intake - n_with_timezone

  # Count timezone sources
  tz_source_summary <- timezone_data %>%
    group_by(timezone_source) %>%
    summarise(n = n(), .groups = "drop")

  n_reported <- tz_source_summary %>% filter(timezone_source == "Reported") %>% pull(n)
  n_imputed <- tz_source_summary %>% filter(timezone_source == "Imputed (UK=GMT)") %>% pull(n)
  n_missing <- tz_source_summary %>% filter(timezone_source == "Missing") %>% pull(n)

  # Handle cases where categories might be empty
  if (length(n_reported) == 0) n_reported <- 0
  if (length(n_imputed) == 0) n_imputed <- 0
  if (length(n_missing) == 0) n_missing <- 0

  message(sprintf("\n=== STEP 2: Timezone Data Filter ==="))
  message(sprintf("Total participants in intake: %d", n_total_intake))
  message(sprintf("Participants with valid timezone data: %d", n_with_timezone))
  message(sprintf("  - Reported timezone: %d", n_reported))
  message(sprintf("  - Imputed UK timezone (GMT): %d", n_imputed))
  message(sprintf("Participants WITHOUT timezone data: %d (excluded - US only)", n_without_timezone))

  # Check overlap with outcome filter
  n_outcomes_and_timezone <- length(intersect(valid_participants_outcomes, valid_participants_timezone))
  n_outcomes_but_no_timezone <- length(setdiff(valid_participants_outcomes, valid_participants_timezone))

  message(sprintf("\nCombined Filter (Outcomes AND Timezone):"))
  message(sprintf("  With outcomes AND timezone: %d", n_outcomes_and_timezone))
  message(sprintf("  With outcomes but NO timezone: %d (excluded - US participants or no intake)", n_outcomes_but_no_timezone))

  # Reference date for identifying future sessions
  ref_date <- now()

  # Helper function to efficiently calculate concurrent sessions using data.table
  calculate_concurrent_sessions <- function(data) {
    # Convert to data.table for efficient overlap calculation
    dt <- as.data.table(data)
    dt[, session_id := .I]

    # Use foverlaps for efficient interval overlap detection
    dt_intervals <- dt[, .(pid, session_id, start = sessionStart, end = sessionEnd)]
    setkey(dt_intervals, pid, start, end)

    # Self-join to find overlapping sessions within each participant
    overlaps <- foverlaps(dt_intervals, dt_intervals, type = "any", nomatch = 0L)

    # Count overlaps for each session
    n_concurrent <- overlaps[, .(n_concurrent = .N), by = .(pid, session_id)]

    # Merge back to original data
    dt <- merge(dt, n_concurrent, by = c("pid", "session_id"), all.x = TRUE)
    dt[is.na(n_concurrent), n_concurrent := 1]
    dt[, session_id := NULL]
    return(as_tibble(dt))
  }

  # Load and standardize gaming data from all platforms
  message("Loading gaming data...")

  # Nintendo
  data.nin.std <- read_csv("data/nintendo.csv.gz", show_col_types = FALSE) %>%
    rename(sessionStart = session_start, sessionEnd = session_end) %>%
    mutate(
      date = as_date(sessionStart),
      duration_minutes = as.numeric(duration),
      flag_future = sessionStart > ref_date | sessionEnd > ref_date,
      flag_long_session = duration_minutes > 600,
      platform = "Nintendo"
    ) %>%
    calculate_concurrent_sessions() %>%
    mutate(
      flag_concurrent = n_concurrent >= 3,
      exclude_quality = flag_future | flag_long_session | flag_concurrent
    )

  # Xbox
  data.xbox.std <- read_csv("data/xbox.csv.gz", show_col_types = FALSE) %>%
    rename(sessionStart = session_start, sessionEnd = session_end) %>%
    mutate(
      date = as_date(sessionStart),
      duration_minutes = as.numeric(duration),
      flag_future = sessionStart > ref_date | sessionEnd > ref_date,
      flag_long_session = duration_minutes > 600,
      platform = "Xbox"
    ) %>%
    calculate_concurrent_sessions() %>%
    mutate(
      flag_concurrent = n_concurrent >= 3,
      exclude_quality = flag_future | flag_long_session | flag_concurrent
    )

  # Steam
  data.steam.std <- read_csv("data/steam.csv.gz", show_col_types = FALSE) %>%
    rename(sessionStart = session_start, sessionEnd = session_end) %>%
    mutate(
      date = as_date(sessionStart),
      duration_minutes = as.numeric(difftime(sessionEnd, sessionStart, units = "mins")),
      flag_future = sessionStart > ref_date | sessionEnd > ref_date,
      flag_long_session = duration_minutes > 600,
      platform = "Steam"
    ) %>%
    calculate_concurrent_sessions() %>%
    mutate(
      flag_concurrent = n_concurrent >= 3,
      exclude_quality = flag_future | flag_long_session | flag_concurrent
    )

  # Combine all gaming data
  message("Combining and filtering gaming data...")
  data.gaming <- bind_rows(data.xbox.std, data.steam.std, data.nin.std) %>%
    mutate(pid = as.character(pid)) %>%
    # Filter out sessions flagged for quality issues
    filter(!exclude_quality) %>%
    # Join with day_zero to calculate days from study start
    left_join(day_zero, by = "pid") %>%
    mutate(days_from_day_0 = as.numeric(difftime(sessionStart, day_0, units = "days"))) %>%
    # Filter to study window: 28 days before to 77 days after first panel
    filter(
      !is.na(day_0),  # Must have a valid day 0
      days_from_day_0 >= -28,  # At least 28 days before day 0
      days_from_day_0 <= 77    # At most 77 days after day 0
    ) %>%
    # Join with timezone data
    left_join(timezone_data %>% select(pid, tz_offset_hours, country), by = "pid") %>%
    # Filter to participants with valid timezone data
    # IMPORTANT: Without timezone data, we cannot accurately determine late-night gaming
    filter(!is.na(tz_offset_hours)) %>%
    mutate(
      # Convert UTC timestamps to local time by adding timezone offset
      sessionStart_local = sessionStart + hours(tz_offset_hours),
      sessionEnd_local = sessionEnd + hours(tz_offset_hours),

      # Use local time for all time-based calculations
      date_local = as_date(sessionStart_local),

      # Redefine day to begin and end at 6am (not midnight)
      # This properly assigns late-night sessions to the previous calendar day
      # IMPORTANT: Use local time for this calculation
      dateRecoded = if_else(hour(sessionStart_local) < 6, date_local - 1, date_local),

      # Calculate minutes_played
      minutes_played = duration_minutes,

      # Binary variable: late-night if session starts between 23:00 and 06:00 LOCAL TIME
      latenight = ifelse(hour(sessionStart_local) >= 23 | hour(sessionStart_local) < 6, 1, 0),

      # Weekend variable: Friday and Saturday nights (based on LOCAL TIME)
      isWeekend = ifelse(weekdays(sessionStart_local) %in% c("Friday", "Saturday"), 1, 0),

      # Calculate late-night minutes using interval overlap (using LOCAL TIME)
      interval_gaming = interval(sessionStart_local, sessionEnd_local),
      interval_latenight = interval(dateRecoded + hours(23), dateRecoded + hours(30)),
      latenightMinutes = as.numeric(intersect(interval_gaming, interval_latenight)) / 60
    ) %>%
    # Assign sessions to waves based on days_from_day_0
    mutate(
      wave = case_when(
        days_from_day_0 >= 0 & days_from_day_0 <= 14 ~ 1,
        days_from_day_0 >= 15 & days_from_day_0 <= 28 ~ 2,
        days_from_day_0 >= 29 & days_from_day_0 <= 42 ~ 3,
        days_from_day_0 >= 43 & days_from_day_0 <= 56 ~ 4,
        days_from_day_0 >= 57 & days_from_day_0 <= 70 ~ 5,
        days_from_day_0 >= 71 & days_from_day_0 <= 84 ~ 6,
        TRUE ~ NA_real_
      )
    ) %>%
    select(pid, sessionStart, sessionEnd, sessionStart_local, sessionEnd_local,
           platform, dateRecoded, minutes_played,
           latenight, latenightMinutes, isWeekend, wave, days_from_day_0)

  n_sessions_before_participant_filter <- nrow(data.gaming)
  n_participants_before_participant_filter <- n_distinct(data.gaming$pid)

  message(sprintf("\n=== Gaming Data After Quality & Study Window Filters ==="))
  message(sprintf("Sessions: %d", n_sessions_before_participant_filter))
  message(sprintf("Participants: %d", n_participants_before_participant_filter))

  # Identify participants with at least one valid gaming session in the study period
  participants_with_gaming <- data.gaming %>%
    pull(pid) %>%
    unique()

  n_with_gaming <- length(participants_with_gaming)

  message(sprintf("\n=== STEP 3: Gaming Data Filter ==="))
  message(sprintf("Participants with at least one valid gaming session: %d", n_with_gaming))

  # Check overlap with previous filters
  n_outcomes_timezone_and_gaming <- length(intersect(
    intersect(valid_participants_outcomes, valid_participants_timezone),
    participants_with_gaming
  ))
  n_outcomes_and_timezone_but_no_gaming <- n_outcomes_and_timezone - n_outcomes_timezone_and_gaming

  message(sprintf("\nCombined Filter (Outcomes AND Timezone AND Gaming):"))
  message(sprintf("  With outcomes, timezone AND gaming: %d", n_outcomes_timezone_and_gaming))
  message(sprintf("  With outcomes & timezone but NO gaming: %d (excluded)", n_outcomes_and_timezone_but_no_gaming))

  # Final valid participant list: must have outcomes, timezone, AND gaming data
  final_valid_participants <- intersect(
    intersect(valid_participants_outcomes, valid_participants_timezone),
    participants_with_gaming
  )

  # Filter gaming data to only include final valid participants
  data.gaming <- data.gaming %>%
    filter(pid %in% final_valid_participants)

  n_final_sessions <- nrow(data.gaming)
  n_final_participants <- n_distinct(data.gaming$pid)
  n_excluded_sessions <- n_sessions_before_participant_filter - n_final_sessions
  n_excluded_participants <- n_participants_before_participant_filter - n_final_participants

  message(sprintf("\n=== FINAL VALID SAMPLE ==="))
  message(sprintf("Final participants: %d", n_final_participants))
  message(sprintf("Final sessions: %d", n_final_sessions))
  message(sprintf("\nExcluded in final step:"))
  message(sprintf("  Participants: %d (had gaming but no outcomes or timezone)", n_excluded_participants))
  message(sprintf("  Sessions: %d", n_excluded_sessions))

  # Return both gaming data and the list of valid participants
  return(list(
    gaming_data = data.gaming,
    valid_participants = final_valid_participants
  ))
}

# Export all gaming sessions with late-night enrichment (session-level data)
create_gaming_sessions_export <- function(data.gaming) {
  message("Creating gaming sessions export with late-night enrichment...")

  # Select relevant session-level variables
  sessions_export <- data.gaming %>%
    select(
      pid,
      platform,
      sessionStart,
      sessionEnd,
      sessionStart_local,
      sessionEnd_local,
      minutes_played,
      latenight,
      latenightMinutes
    ) %>%
    # Keep only sessions within the study period (after quality filtering)
    filter(!is.na(sessionStart))

  message(sprintf("Exported %d gaming sessions from %d participants",
                  nrow(sessions_export), n_distinct(sessions_export$pid)))

  return(sessions_export)
}

# Create self-report data from panel (includes date column for gaming calculations)
create_selfreport <- function(panel_path, intake_processed, valid_participants) {
  message("Creating self-report data...")

  # Load raw panel data to get completion dates
  panel_raw <- read_csv(panel_path, show_col_types = FALSE) %>%
    select(pid, wave, date) %>%
    distinct(pid, wave, .keep_all = TRUE)

  # Process panel data with all computed variables
  panel_processed <- process_panel(panel_path, intake_processed)

  # Select relevant variables for modeling and merge with date
  # IMPORTANT: Filter to only include valid participants (passed 3-step filter)
  selfreport <- panel_processed %>%
    filter(pid %in% valid_participants) %>%  # Apply 3-step participant filter
    select(
      pid, wave,
      # Outcomes
      psqi_global, total_hours_sleep, epsTotal, wemwbs,
      # PSQI components (needed for H2a model)
      psqi_06,  # Numeric version: 0=Very good, 1=Fairly good, 2=Fairly bad, 3=Very bad
      # Moderator
      msf_sc_numeric,
      # Covariates
      age_scaled, bmi_scaled, SES_index_scaled, region, gender
    ) %>%
    filter(!is.na(wave)) %>%  # Only include waves with data
    # Now join the date from raw panel
    left_join(panel_raw, by = c("pid", "wave")) %>%
    filter(!is.na(date))  # Only keep records with valid dates

  message(sprintf("Created %d self-report records from %d participants",
                  nrow(selfreport), n_distinct(selfreport$pid)))
  message(sprintf("  (Filtered to valid participants who passed 3-step filter)"))

  return(selfreport)
}

# Main preprocessing function
preprocess_all_data <- function() {
  # Process intake
  message("Processing intake data...")
  intake_processed <- process_intake("data/intake.csv.gz")

  # Process panel
  message("Processing panel data...")
  panel_processed <- process_panel("data/panel.csv.gz", intake_processed)

  # Process gaming data (returns both gaming data and valid participants list)
  message("Processing gaming data...")
  gaming_result <- process_gaming_data("data/panel.csv.gz", intake_processed)
  gaming_data <- gaming_result$gaming_data
  valid_participants <- gaming_result$valid_participants

  # Export session-level gaming data with late-night enrichment
  message("\nExporting gaming sessions...")
  gaming_sessions <- create_gaming_sessions_export(gaming_data)

  # Create self-report data (filtered to valid participants only)
  message("\nCreating self-report data...")
  selfreport <- create_selfreport("data/panel.csv.gz", intake_processed, valid_participants)

  # Save processed data
  message("\nSaving processed data...")
  dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
  write_csv(panel_processed, "data/processed/panel_clean.csv.gz")
  write_csv(intake_processed, "data/processed/intake_clean.csv.gz")
  write_csv(gaming_sessions, "data/processed/gaming_sessions.csv.gz")
  write_csv(selfreport, "data/processed/selfreport.csv.gz")

  message("\nData preprocessing complete!")
  message("Processed files saved to data/processed/:")
  message("  - panel_clean.csv.gz")
  message("  - intake_clean.csv.gz")
  message("  - gaming_sessions.csv.gz (session-level data with late-night enrichment)")
  message("  - selfreport.csv.gz")
  message("\nNote: Gaming exposure (14-day and 28-day windows) should be")
  message("      calculated in the qmd relative to each self-report date")
}

# Run preprocessing if script is sourced
if (!interactive()) {
  preprocess_all_data()
}
