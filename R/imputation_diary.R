# imputation_diary.R
# Hierarchical two-level multiple imputation for diary-level missing data
# Uses miceadds::mice.impute.2l.pmm with participants as clusters (level 2)
# and diary days nested within (level 1), under a MAR assumption.
#
# Variables imputed (2l.pmm):
#   - life_sat: Life satisfaction (continuous 1-100)
#   - affective_valence: Affective valence (continuous)
#   - sleep_diary_quality_num: Sleep quality (numeric 1-5, ordinal treated as numeric via PMM)
#


library(tidyverse)
library(mice)
library(future)
library(future.apply)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

N_IMPUTATIONS <- 60
N_ITERATIONS  <- 20
SEED          <- 42
DIARY_DAYS    <- 1:30

N_LAG_LEADS <- 5    # lag/lead depth for temporal context (±N days per outcome)

# Outcome variables to impute
DIARY_OUTCOMES <- c("life_sat", "affective_valence", "sleep_diary_quality_num")

# Predictor variables (included in imputation model but only imputed where missing
# in observed data; on expanded days these are also NA and imputed by PMM)
#
# Note: day_type_holiday, day_type_vacation, day_type_other are excluded because
# they have near-zero base rates (~1-3% per day), causing near-singular predictor
# matrices in the wide format. They are merged into day_type_nonstandard (any
# non-regular day that is not a weekend or day off). Only day_type_dayoff and
# day_type_weekend are kept separately because they feed directly into the
# is_weekend covariate used in all diary models.
DIARY_PREDICTORS <- c(
  "played24hr_num",
  "bpnsfs_1", "bpnsfs_2", "bpnsfs_3", "bpnsfs_4", "bpnsfs_5", "bpnsfs_6",
  "had_stress_num",
  "day_type_dayoff", "day_type_weekend", "day_type_nonstandard",
  # Objective late-night gaming from telemetry (complete data; no genuine item missingness
  # on observed days). Included to make the imputation model congenial with the
  # H1a analysis model, which uses late-night gaming as the key predictor of sleep quality.
  "late_night_hours_24h"
)

# Wave-invariant auxiliary variables (never imputed, used as predictors only)
DIARY_AUXILIARY <- c(
  "age_scaled", "bmi_scaled", "SES_index_scaled",
  "chronotype_centered", "gender",
  # Person-mean of daily late-night gaming hours (between-person component of the
  # H1a predictor). Including it as a person-level auxiliary ensures imputed sleep
  # quality values reflect stable individual differences in late-night gaming,
  # satisfying the congeniality requirement for valid multiple imputation.
  "late_night_pid_mean_scaled"
)

# Source data
DIARY_DATA_PATH        <- "data/raw/survey_daily.csv.gz"
INTAKE_DATA_PATH       <- "data/processed/intake_clean.csv.gz"
PANEL_DATA_PATH        <- "data/processed/selfreport.csv.gz"
GAMING_SESSIONS_PATH   <- "data/processed/gaming_sessions.csv.gz"

# Output paths
DIARY_OUTPUT_DIR  <- "output/imputation_diary"
DIARY_MIDS_PATH   <- "data/processed/diary_imputed.rds"
DIARY_LONG_PATH   <- "data/processed/diary_imputed_long.rds"

# ==============================================================================
# FUNCTIONS
# ==============================================================================

#' Categorize gender into Male/Female/Other
#' (Mirrored from imputation_panel.R for standalone use)
categorize_gender <- function(gender_vec) {
  clean <- gender_vec |>
    as.character() |>
    str_to_lower() |>
    str_trim()
  clean[clean == ""] <- NA_character_
  categorized <- case_when(
    clean %in% c("male", "man", "m", "cis male", "cis man") ~ "Male",
    clean %in% c("female", "woman", "f", "cis female", "cis woman") ~ "Female",
    is.na(clean) ~ NA_character_,
    TRUE ~ "Other"
  )
  factor(categorized, levels = c("Male", "Female", "Other"))
}

#' Recode sleep quality to numeric 1-5
recode_sleep_quality <- function(x) {
  lvls <- c("Very poor", "Poor", "Fair", "Good", "Very good")
  out <- factor(x, levels = lvls, ordered = TRUE)
  as.integer(out)
}

#' Compute daily late-night gaming hours from platform telemetry
#'
#' For each diary entry, sums late-night gaming minutes from all gaming sessions
#' that overlap the 24-hour window ending at the diary submission timestamp,
#' using the same proportional-overlap method as the manuscript analysis model.
#' This ensures the imputation model is congenial with H1a.
#'
#' @param diary_with_ts Data frame with columns: pid, day_num, diary_ts
#' @return Data frame with columns: pid, day_num, late_night_hours_24h,
#'   late_night_pid_mean (person-mean across observed days),
#'   late_night_pid_mean_scaled (z-scored across participants)
compute_diary_late_night_gaming <- function(diary_with_ts) {
  message("  Computing late-night gaming exposure from telemetry...")

  gaming_sessions_raw <- read_csv(GAMING_SESSIONS_PATH, show_col_types = FALSE)

  gaming_sessions <- gaming_sessions_raw |>
    mutate(
      session_start    = ymd_hms(sessionStart, tz = "UTC", quiet = TRUE),
      session_end      = ymd_hms(sessionEnd,   tz = "UTC", quiet = TRUE),
      latenightMinutes = replace_na(latenightMinutes, 0)
    ) |>
    filter(!is.na(session_start), !is.na(session_end), session_end > session_start)

  # Join diary entries to all sessions for that participant, then filter
  # to sessions overlapping the 24h window ending at diary_ts
  # The cross-join by pid is intentional: each diary entry is matched against
  # all gaming sessions for that participant, then filtered to the 24h window.
  # Suppress the many-to-many warning that dplyr emits for this pattern.
  diary_gaming <- diary_with_ts |>
    select(pid, day_num, diary_ts) |>
    left_join(gaming_sessions, by = "pid", relationship = "many-to-many") |>
    filter(
      session_end   > diary_ts - hours(24),
      session_start <= diary_ts
    ) |>
    mutate(
      overlap_minutes = pmax(0, as.numeric(difftime(
        pmin(session_end, diary_ts),
        pmax(session_start, diary_ts - hours(24)),
        units = "mins"
      ))),
      session_minutes           = as.numeric(difftime(session_end, session_start, units = "mins")),
      overlap_frac              = if_else(session_minutes > 0, overlap_minutes / session_minutes, 0),
      late_night_minutes_overlap = latenightMinutes * overlap_frac
    ) |>
    group_by(pid, day_num) |>
    summarise(
      late_night_hours_24h = sum(late_night_minutes_overlap, na.rm = TRUE) / 60,
      .groups = "drop"
    )

  # Left-join back so non-gaming days get 0 (not NA)
  result <- diary_with_ts |>
    select(pid, day_num) |>
    left_join(diary_gaming, by = c("pid", "day_num")) |>
    mutate(late_night_hours_24h = replace_na(late_night_hours_24h, 0))

  # Compute person-mean (between-person component) and z-score it
  person_means <- result |>
    group_by(pid) |>
    summarise(late_night_pid_mean = mean(late_night_hours_24h, na.rm = TRUE), .groups = "drop") |>
    mutate(late_night_pid_mean_scaled = as.numeric(scale(late_night_pid_mean)))

  result <- result |>
    left_join(person_means, by = "pid")

  message(sprintf(
    "  Gaming exposure computed: %d person-day rows; person-mean range [%.2f, %.2f] hours",
    nrow(result),
    min(result$late_night_pid_mean, na.rm = TRUE),
    max(result$late_night_pid_mean, na.rm = TRUE)
  ))

  result
}


#' Prepare diary data for imputation
#'
#' Loads raw diary data, applies 10-hour gap filter, recodes categoricals,
#' joins wave-invariant covariates, and assigns sequential day numbers.
#'
#' @return Data frame with one row per participant-day (observed days only)
prepare_diary_for_imputation <- function() {
  message("=== Preparing diary data for imputation ===")

  # --- Load raw diary data ---
  diary_raw <- read_csv(DIARY_DATA_PATH, show_col_types = FALSE)
  message(sprintf("  Loaded %d diary entries from %d participants",
                  nrow(diary_raw), n_distinct(diary_raw$pid)))

  # --- Apply 10-hour gap filter ---
  diary <- diary_raw |>
    mutate(
      diary_ts = ymd_hms(date, tz = "UTC"),
      diary_day = as_date(diary_ts)
    ) |>
    arrange(pid, diary_ts) |>
    group_by(pid) |>
    mutate(
      gap_hours = as.numeric(difftime(diary_ts, lag(diary_ts), units = "hours")),
      keep_entry = is.na(gap_hours) | gap_hours >= 10
    ) |>
    filter(keep_entry) |>
    ungroup()

  message(sprintf("  After 10h gap filter: %d entries", nrow(diary)))

  # --- Recode categoricals to numeric ---
  diary <- diary |>
    mutate(
      played24hr_num = case_when(
        played24hr == "Yes" ~ 1L,
        played24hr == "No"  ~ 0L,
        TRUE ~ NA_integer_
      ),
      had_stress_num = case_when(
        had_stress == "Yes" ~ 1L,
        had_stress == "No"  ~ 0L,
        TRUE ~ NA_integer_
      ),
      sleep_diary_quality_num = recode_sleep_quality(sleep_diary_quality),
      # Dummy-code day type (reference = "Regular work day")
      day_type_dayoff      = as.integer(sleep_diary_day_type == "Regular day off"),
      day_type_weekend     = as.integer(sleep_diary_day_type == "Weekend"),
      # Merge holiday/vacation/other into a single binary to avoid near-zero
      # variance columns (base rates: holiday ~1%, vacation ~3%, other ~3%)
      day_type_nonstandard = as.integer(
        sleep_diary_day_type %in% c("Holiday", "Vacation day") |
          str_detect(coalesce(sleep_diary_day_type, ""), "Other")
      ),
      # Set all dummies to NA when day type is missing
      across(starts_with("day_type_"), ~if_else(is.na(sleep_diary_day_type), NA_integer_, .))
    )

  # --- Assign day number per participant ---
  # Use as.integer(wave) which encodes the calendar day within the 30-day diary
  # window (e.g. wave 3 = day 3 regardless of how many entries preceded it).
  # This correctly aligns with diary_late_night in the analysis model, which is
  # joined on wave_id = as.integer(wave). Using row_number() was incorrect for
  # participants with gaps (skipped days produced a shifted mapping).
  diary <- diary |>
    arrange(pid, diary_ts) |>
    mutate(day_num = as.integer(wave))

  # --- Load wave-invariant covariates ---
  message("  Loading wave-invariant covariates...")

  # Intake data (age, BMI, SES, gender)
  intake <- read_csv(INTAKE_DATA_PATH, show_col_types = FALSE)
  covariate_lookup <- intake |>
    group_by(pid) |>
    slice_head(n = 1) |>
    ungroup() |>
    transmute(
      pid,
      gender = categorize_gender(gender),
      age = suppressWarnings(as.numeric(age)),
      height_raw = suppressWarnings(as.numeric(height)),
      weight_raw = suppressWarnings(as.numeric(weight)),
      edu_level = na_if(edu_level, "NA"),
      employment = na_if(employment, "NA")
    ) |>
    mutate(
      height_m = case_when(
        is.na(height_raw) ~ NA_real_,
        height_raw < 100 ~ height_raw * 0.0254,
        TRUE ~ height_raw / 100
      ),
      weight_kg = case_when(
        is.na(weight_raw) ~ NA_real_,
        weight_raw > 150 ~ weight_raw * 0.45359237,
        TRUE ~ weight_raw
      ),
      bmi = weight_kg / (height_m^2),
      bmi = if_else(is.finite(bmi) & bmi >= 10 & bmi <= 70, bmi, NA_real_),
      edu_score = case_when(
        is.na(edu_level) ~ NA_real_,
        str_detect(str_to_lower(edu_level), "no formal") ~ 1,
        str_detect(str_to_lower(edu_level), "secondary|high school|gcse") ~ 2,
        str_detect(str_to_lower(edu_level), "some college|associate") ~ 3,
        str_detect(str_to_lower(edu_level), "bachelor") ~ 4,
        str_detect(str_to_lower(edu_level), "post-graduate|postgraduate|master|phd|doctor") ~ 5,
        TRUE ~ NA_real_
      ),
      emp_score = case_when(
        is.na(employment) ~ NA_real_,
        str_detect(str_to_lower(employment), "full-time|full time|self-employed|self employ|freelan|streamer|gig|military") ~ 4,
        str_detect(str_to_lower(employment), "part-time|part time") ~ 3,
        str_detect(str_to_lower(employment), "student") ~ 2,
        str_detect(str_to_lower(employment), "retired") ~ 2,
        str_detect(str_to_lower(employment), "homemaker|stay-at-home|disabled|unable to work|caregiver|not currently employed|unemployed|prolific") ~ 1,
        TRUE ~ NA_real_
      ),
      SES_index = edu_score + emp_score
    ) |>
    select(pid, age, bmi, SES_index, gender)

  # Restrict to diary participants and scale within subsample
  diary_pids <- unique(diary$pid)
  covariate_lookup <- covariate_lookup |>
    filter(pid %in% diary_pids) |>
    mutate(
      age_scaled = as.numeric(scale(age)),
      bmi_scaled = as.numeric(scale(bmi))
    ) |>
    select(pid, age_scaled, bmi_scaled, SES_index, gender)

  # Impute missing SES via MICE (PMM, m=5) — same as manuscript
  if (sum(is.na(covariate_lookup$SES_index)) > 0) {
    message("  Imputing missing SES_index...")
    set.seed(42)
    ses_imp <- mice::mice(
      covariate_lookup |> select(SES_index, age_scaled, bmi_scaled, gender),
      m = 5, method = "pmm", maxit = 10, seed = 42, printFlag = FALSE
    )
    ses_complete <- mice::complete(ses_imp, 1) |>
      mutate(pid = covariate_lookup$pid)
    covariate_lookup <- covariate_lookup |>
      select(-SES_index) |>
      left_join(ses_complete |> select(pid, SES_index), by = "pid")
  }

  covariate_lookup <- covariate_lookup |>
    mutate(SES_index_scaled = as.numeric(scale(SES_index))) |>
    select(pid, age_scaled, bmi_scaled, SES_index_scaled, gender)

  # Chronotype from panel wave 1
  message("  Loading chronotype from panel data...")
  panel <- read_csv(PANEL_DATA_PATH, show_col_types = FALSE)
  chrono_lookup <- panel |>
    filter(wave == 1) |>
    distinct(pid, .keep_all = TRUE) |>
    transmute(
      pid,
      chronotype_centered = {
        msf <- suppressWarnings(as.numeric(msf_sc_numeric))
        as.numeric(scale(msf, scale = FALSE))
      }
    )

  # --- Compute late-night gaming exposure from telemetry ---
  # diary_ts is still present in `diary` at this point (dropped only in the
  # select below). Gaming data is objective telemetry; no genuine missingness
  # on observed days (0 if participant did not play that night).
  diary_gaming <- compute_diary_late_night_gaming(diary)

  # Add late_night_hours_24h (time-varying) to the diary data frame
  diary <- diary |>
    left_join(
      diary_gaming |> select(pid, day_num, late_night_hours_24h),
      by = c("pid", "day_num")
    )

  # Add person-mean (late_night_pid_mean_scaled) to the covariate lookup so
  # it becomes a person-level auxiliary predictor in the imputation model
  person_mean_lookup <- diary_gaming |>
    select(pid, late_night_pid_mean_scaled) |>
    distinct()

  covariate_lookup <- covariate_lookup |>
    left_join(person_mean_lookup, by = "pid")

  # --- Join covariates to diary ---
  diary_prepped <- diary |>
    select(pid, day_num, all_of(DIARY_OUTCOMES), all_of(DIARY_PREDICTORS)) |>
    left_join(covariate_lookup, by = "pid") |>
    left_join(chrono_lookup, by = "pid")

  message(sprintf("  Prepared data: %d rows, %d columns",
                  nrow(diary_prepped), ncol(diary_prepped)))

  diary_prepped
}


#' Add imputed flags to long-format data
#'
#' @param long_data Long-format imputed data
#' @param original_data Original observed diary data (pre-expansion)
#' @return Long data with {varname}_imputed_flag columns
add_imputed_flags <- function(long_data, original_data) {
  message("  Adding imputed flags...")

  # Create lookup of observed pid-day_num combinations
  observed_key <- original_data |>
    select(pid, day_num, all_of(DIARY_OUTCOMES)) |>
    distinct()

  # For each outcome, flag whether the value was imputed

  for (var in c(DIARY_OUTCOMES, DIARY_PREDICTORS)) {
    flag_name <- paste0(var, "_imputed_flag")
    if (var %in% names(observed_key)) {
      # Variable exists in original data — flag where original was NA or day was unobserved
      orig_var <- observed_key |>
        select(pid, day_num, !!sym(var)) |>
        rename(.orig = !!sym(var))

      long_data <- long_data |>
        left_join(orig_var, by = c("pid", "day_num")) |>
        mutate(!!flag_name := is.na(.orig)) |>
        select(-.orig)
    } else {
      # Variable not in observed_key — flag based on unobserved days
      observed_days <- original_data |>
        select(pid, day_num) |>
        distinct() |>
        mutate(.observed = TRUE)

      long_data <- long_data |>
        left_join(observed_days, by = c("pid", "day_num")) |>
        mutate(!!flag_name := is.na(.observed) | is.na(!!sym(var))) |>
        select(-.observed)
    }
  }

  long_data
}


#' Create diagnostic plots for diary imputation
#'
#' @param imp mids object from mice
#' @param output_dir Directory to save plots
create_diary_diagnostics <- function(imp, output_dir = DIARY_OUTPUT_DIR) {
  message("\n=== Creating diagnostic plots ===")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Columns imputed via 2l.pmm
  imputed_cols <- names(imp$method)[imp$method == "2l.pmm"]
  cols_with_missing <- imputed_cols[sapply(imputed_cols, function(col) {
    any(is.na(imp$data[[col]]))
  })]

  # For multilevel mids, outcome columns are just the variable names directly
  outcome_diagnostic_cols <- intersect(DIARY_OUTCOMES, cols_with_missing)

  # 1. Convergence plots
  message("  Creating convergence plots...")
  pdf(file.path(output_dir, "convergence_plots.pdf"), width = 12, height = 8)
  tryCatch({
    # mice plot() with character vector of column names
    if (length(outcome_diagnostic_cols) > 0) {
      print(plot(imp, y = outcome_diagnostic_cols))
    }
  }, error = function(e) {
    message("    Warning: convergence plot failed: ", e$message)
  })
  dev.off()

  # 2. Density plots (observed vs imputed) for representative days
  message("  Creating density comparison plots...")
  pdf(file.path(output_dir, "density_plots.pdf"), width = 12, height = 10)
  for (col in outcome_diagnostic_cols) {
    if (sum(!is.na(imp$data[[col]])) >= 10) {
      tryCatch({
        print(densityplot(imp, as.formula(paste("~", col))))
      }, error = function(e) NULL)
    }
  }
  dev.off()

  # 3. Strip plots
  message("  Creating strip plots...")
  pdf(file.path(output_dir, "strip_plots.pdf"), width = 12, height = 10)
  for (col in outcome_diagnostic_cols) {
    tryCatch({
      print(stripplot(imp, as.formula(paste("~", col))))
    }, error = function(e) NULL)
  }
  dev.off()

  # 4. Summary text file
  message("  Creating imputation summary...")
  summary_file <- file.path(output_dir, "imputation_summary.txt")
  sink(summary_file)
  cat("=== Diary Multilevel Multiple Imputation Summary (2l.pmm) ===\n\n")
  cat(sprintf("Number of imputations (m): %d\n", imp$m))
  cat(sprintf("Number of iterations: %d\n", imp$iteration))
  cat(sprintf("Number of person-day rows: %d\n", nrow(imp$data)))
  cat(sprintf("Number of participants: %d\n\n",
              length(unique(imp$data$pid_int))))

  cat("Outcome variables imputed (2l.pmm):\n")
  for (var in DIARY_OUTCOMES) {
    if (var %in% cols_with_missing) {
      n_miss <- sum(is.na(imp$data[[var]]))
      cat(sprintf("  %s: %d (%.1f%%) missing\n",
                  var, n_miss, 100 * n_miss / nrow(imp$data)))
    }
  }

  pmm_cols <- names(imp$method)[imp$method == "pmm"]
  if (length(pmm_cols) > 0) {
    cat("\nPredictor variables imputed (pmm):\n")
    for (var in pmm_cols) {
      n_miss <- sum(is.na(imp$data[[var]]))
      cat(sprintf("  %s: %d (%.1f%%) missing\n",
                  var, n_miss, 100 * n_miss / nrow(imp$data)))
    }
  }
  sink()

  message(sprintf("  Diagnostics saved to %s/", output_dir))
}


# ==============================================================================
# MULTILEVEL IMPUTATION FUNCTIONS (long format, 2l.pmm via miceadds)
# ==============================================================================

#' Add temporal lag and lead columns for specified variables
#'
#' Adds {var}_lag1 ... {var}_lag{k} and {var}_lead1 ... {var}_lead{k} columns
#' for each variable in vars, computed within pid groups. These serve as
#' temporal-neighbourhood predictors (replacing the wide-format band concept).
#' Lag/lead columns are NOT imputed (method = ""); they are missing for
#' days near the boundary of each participant's diary window.
#'
#' @param data Long-format diary data with pid and day_num columns
#' @param vars Character vector of variable names to lag/lead
#' @param k    Number of lag/lead steps in each direction
#' @return data with 2 * k * length(vars) new columns appended
add_temporal_lags <- function(data, vars, k) {
  message(sprintf("  Adding ±%d day lag/lead predictors for: %s",
                  k, paste(vars, collapse = ", ")))

  data <- data |> arrange(pid, day_num)

  for (var in vars) {
    for (i in seq_len(k)) {
      lag_col  <- paste0(var, "_lag",  i)
      lead_col <- paste0(var, "_lead", i)
      data <- data |>
        group_by(pid) |>
        mutate(
          !!lag_col  := dplyr::lag( !!sym(var), n = i, default = NA_real_),
          !!lead_col := dplyr::lead(!!sym(var), n = i, default = NA_real_)
        ) |>
        ungroup()
    }
  }

  n_new <- 2L * k * length(vars)
  message(sprintf("  Added %d lag/lead columns", n_new))
  data
}


#' Prepare diary long data for multilevel mice (type conversions)
#'
#' - Converts pid to integer (required for 2l.pmm cluster coding: column coded -2)
#' - Converts gender factor to numeric integer
#' - Returns both the mice-ready data frame and a pid lookup table for
#'   restoring character pid values after imputation.
#'
#' @param data Long-format diary data (observed days + auxiliaries + lag/lead cols)
#' @return List with $data (mice-ready) and $pid_lookup (pid ↔ pid_int mapping)
prepare_for_mice_multilevel <- function(data) {
  message("  Converting pid to integer and gender to numeric for mice...")

  pid_levels  <- sort(unique(data$pid))
  pid_lookup  <- tibble(pid = pid_levels, pid_int = seq_along(pid_levels))

  out <- data |>
    left_join(pid_lookup, by = "pid") |>
    mutate(gender_num = as.integer(factor(gender))) |>
    select(-pid, -gender)

  message(sprintf("  %d participants → pid_int 1..%d", length(pid_levels), length(pid_levels)))
  list(data = out, pid_lookup = pid_lookup)
}


#' Set up predictor matrix for multilevel mice (2l.pmm + pmm)
#'
#' Coding (van Buuren, 2018, Ch. 7 Tables 7.5-7.6):
#'   -2 = cluster (pid_int) — used in rows imputed with 2l.pmm
#'    3 = fixed effect + cluster mean (for level-1 predictors of 2l.pmm targets)
#'    1 = fixed effect only (for level-2 predictors and auxiliary level-1 vars)
#'    0 = not a predictor / not imputed
#'
#' Level-2 variables are coded 1 (not 2) because they are person-level
#' constants — random slopes for constants are meaningless and cause
#' numerical instability.  Cross-outcome and time-varying level-1
#' predictors are coded 3 so that both the raw value and its
#' disaggregated cluster mean enter the 2l.pmm imputation model.
#'
#' @param data          mice-ready data frame (with pid_int, gender_num, lag cols)
#' @param lag_lead_cols character vector of lag/lead column names
#' @return Integer predictor matrix
setup_multilevel_predictor_matrix <- function(data, lag_lead_cols) {
  message("Setting up multilevel predictor matrix (-2/1/3 coding)...")

  vars   <- names(data)
  n_vars <- length(vars)
  pred   <- matrix(0L, nrow = n_vars, ncol = n_vars,
                   dimnames = list(vars, vars))

  l2_vars <- c(
    intersect(DIARY_AUXILIARY, vars),
    "late_night_pid_mean_scaled",
    "gender_num"
  )
  l1_vars <- c(
    "day_num",
    DIARY_PREDICTORS,
    lag_lead_cols
  )

  for (i in seq_len(n_vars)) {
    target <- vars[i]

    is_2l_target  <- target %in% DIARY_OUTCOMES
    is_pmm_target <- target %in% DIARY_PREDICTORS
    if (!is_2l_target && !is_pmm_target) next

    for (j in seq_len(n_vars)) {
      if (i == j) next
      pred_var <- vars[j]

      if (pred_var == "pid_int") {
        if (is_2l_target) pred[i, j] <- -2L

      } else if (pred_var %in% l2_vars) {
        pred[i, j] <- 1L

      } else if (is_2l_target && (pred_var %in% DIARY_OUTCOMES ||
                                   pred_var %in% DIARY_PREDICTORS)) {
        pred[i, j] <- 3L

      } else if (pred_var %in% l1_vars ||
                 pred_var %in% DIARY_OUTCOMES) {
        pred[i, j] <- 1L
      }
    }
  }

  if ("pid_int" %in% vars) pred["pid_int", ] <- 0L

  n_preds   <- rowSums(pred != 0)
  n_code3   <- rowSums(pred == 3)
  outcome_preds <- n_preds[DIARY_OUTCOMES[DIARY_OUTCOMES %in% vars]]
  outcome_c3    <- n_code3[DIARY_OUTCOMES[DIARY_OUTCOMES %in% vars]]
  message(sprintf("  Avg predictors per outcome: %.1f (%.1f with cluster means)",
                  mean(outcome_preds), mean(outcome_c3)))
  pred
}


#' Set up mice imputation methods for multilevel diary data
#'
#' - DIARY_OUTCOMES with any missingness: "2l.pmm" (miceadds; requires cluster = -2)
#' - DIARY_PREDICTORS with any missingness: "pmm"
#' - Everything else (auxiliaries, lag/lead, pid_int): "" (not imputed)
#'
#' @param data mice-ready data frame
#' @return Named character vector of mice methods
setup_multilevel_methods <- function(data) {
  message("Setting up multilevel imputation methods (2l.pmm / pmm)...")

  methods <- rep("", ncol(data))
  names(methods) <- names(data)

  for (var in names(data)) {
    if (var %in% DIARY_OUTCOMES && any(is.na(data[[var]]))) {
      methods[var] <- "2l.pmm"
    } else if (var %in% DIARY_PREDICTORS && any(is.na(data[[var]]))) {
      methods[var] <- "pmm"
    }
  }

  message(sprintf("  2l.pmm (outcomes): %d  |  pmm (L1 predictors): %d",
                  sum(methods == "2l.pmm"), sum(methods == "pmm")))
  methods
}


#' Run multilevel diary imputation (long format, 2l.pmm + miceadds)
#'
#' Uses the miceadds::mice.impute.2l.pmm method for outcome variables so that
#' the two-level (participant/day) structure is properly modelled during
#' imputation, reducing attenuation of between-person effects. Level-1
#' predictor variables with genuine missingness are imputed via regular pmm.
#'
#' @param data          mice-ready long-format data (pid_int as cluster integer)
#' @param lag_lead_cols Names of lag/lead columns (not imputed; predictors only)
#' @return mids object (long-format; same row count as input data)
run_multilevel_imputation <- function(data, lag_lead_cols) {
  message("\n=== Running Multilevel Diary Imputation (2l.pmm, long format) ===")
  message(sprintf("m = %d  |  maxit = %d  |  N rows (observed days) = %d",
                  N_IMPUTATIONS, N_ITERATIONS, nrow(data)))

  library(miceadds)

  # Grand-mean center continuous outcomes (van Buuren 2018, Sec 7.10.6)
  center_means <- list()
  for (v in intersect(DIARY_OUTCOMES, names(data))) {
    if (is.numeric(data[[v]])) {
      gm <- mean(data[[v]], na.rm = TRUE)
      center_means[[v]] <- gm
      data[[v]] <- data[[v]] - gm
      message(sprintf("  Centered %s (grand mean = %.3f)", v, gm))
    }
  }

  pred    <- setup_multilevel_predictor_matrix(data, lag_lead_cols)
  methods <- setup_multilevel_methods(data)

  message("\nGenuine missingness in outcome variables:")
  for (var in DIARY_OUTCOMES) {
    n_miss <- sum(is.na(data[[var]]))
    message(sprintf("  %s: %d missing (%.2f%%)", var, n_miss, 100 * n_miss / nrow(data)))
  }

  n_workers      <- min(4L, availableCores(), N_IMPUTATIONS)
  chunk_sizes    <- rep(ceiling(N_IMPUTATIONS / n_workers), n_workers)
  total_assigned <- sum(chunk_sizes)
  if (total_assigned > N_IMPUTATIONS)
    chunk_sizes[n_workers] <- chunk_sizes[n_workers] - (total_assigned - N_IMPUTATIONS)

  set.seed(SEED)
  chunk_seeds <- sample.int(1e7, n_workers)

  plan(multisession, workers = n_workers)
  message(sprintf("Launching %d parallel chunks (~%d imps each)...", n_workers, chunk_sizes[1]))
  start_time <- Sys.time()

  imp_list <- future_lapply(seq_len(n_workers), function(i) {
    library(miceadds)
    mice(
      data,
      m               = chunk_sizes[i],
      method          = methods,
      predictorMatrix = pred,
      maxit           = N_ITERATIONS,
      seed            = chunk_seeds[i],
      printFlag       = FALSE
    )
  }, future.seed = TRUE)

  plan(sequential)

  message("Combining imputed datasets...")
  imp_combined <- Reduce(ibind, imp_list)

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  message(sprintf("Multilevel imputation complete in %.1f minutes | total m = %d",
                  elapsed, imp_combined$m))
  list(imp = imp_combined, center_means = center_means)
}


#' Reshape multilevel mids object (long format) to stacked long output
#'
#' Unlike the wide-format pipeline, the mids object from multilevel imputation
#' already contains long-format completed datasets. This function stacks them,
#' restores the character pid from the integer lookup, and drops working
#' columns (pid_int, gender_num, lag/lead columns).
#'
#' @param imp          mids object (long-format input data)
#' @param pid_lookup   tibble with pid / pid_int mapping
#' @param lag_lead_cols character vector of lag/lead column names to drop
#' @return Stacked long-format data frame with .imp column
extract_multilevel_long <- function(imp, pid_lookup, lag_lead_cols,
                                    center_means = list()) {
  message("=== Extracting stacked long-format data from multilevel mids ===")

  long_list <- lapply(seq_len(imp$m), function(i) {
    completed <- mice::complete(imp, i)

    # Back-transform grand-mean centering (van Buuren 2018, Sec 7.10.6)
    for (v in names(center_means)) {
      if (v %in% names(completed))
        completed[[v]] <- completed[[v]] + center_means[[v]]
    }

    completed |>
      left_join(pid_lookup, by = "pid_int") |>
      select(-pid_int, -gender_num,
             -any_of(lag_lead_cols)) |>
      mutate(.imp = i)
  })

  # Restore gender from the original data (gender_num was a working encoding)
  # We can't recover factor labels from the integer alone without the original
  # levels, so pass through by rejoining from the original long data embedded
  # in imp$data
  orig_gender <- imp$data |>
    left_join(pid_lookup, by = "pid_int") |>
    select(pid, day_num, gender_num)

  result <- bind_rows(long_list) |>
    left_join(
      orig_gender |>
        mutate(gender = factor(
          case_when(gender_num == 1 ~ "Male",
                    gender_num == 2 ~ "Female",
                    TRUE            ~ "Other"),
          levels = c("Male", "Female", "Other")
        )) |>
        select(pid, day_num, gender),
      by = c("pid", "day_num")
    )

  message(sprintf("  %d rows × %d cols  (%d imputations × %d observed person-days)",
                  nrow(result), ncol(result), imp$m, nrow(result) / imp$m))
  result
}


# ==============================================================================
# MAIN
# ==============================================================================

main <- function() {
  message("\n", paste(rep("=", 60), collapse = ""))
  message("  DIARY MULTILEVEL MULTIPLE IMPUTATION (2l.pmm)")
  message(paste(rep("=", 60), collapse = ""), "\n")

  # Step 1: Prepare observed diary data
  diary_prepped <- prepare_diary_for_imputation()

  # Step 2: Add temporal lag/lead predictors
  message(sprintf("\n=== Adding temporal lag/lead predictors (+/-%d days) ===", N_LAG_LEADS))
  diary_with_lags <- add_temporal_lags(diary_prepped, DIARY_OUTCOMES, N_LAG_LEADS)

  lag_lead_cols <- grep(
    paste0("_(lag|lead)[0-9]+$"),
    names(diary_with_lags), value = TRUE
  )

  # Step 3: Prepare for mice (pid -> integer, gender -> numeric)
  message("\n=== Preparing data for multilevel mice ===")
  mice_prep     <- prepare_for_mice_multilevel(diary_with_lags)
  diary_mice    <- mice_prep$data
  pid_lookup    <- mice_prep$pid_lookup

  # Step 4: Run mice (2l.pmm for outcomes, pmm for L1 predictors)
  imp_result <- run_multilevel_imputation(diary_mice, lag_lead_cols)
  imp          <- imp_result$imp
  center_means <- imp_result$center_means

  # Step 5: Extract stacked long format; restore pid and gender
  diary_long <- extract_multilevel_long(imp, pid_lookup, lag_lead_cols,
                                        center_means = center_means)

  # Step 6: Add imputed flags
  diary_long <- add_imputed_flags(diary_long, diary_prepped)

  # ── Step N: Save outputs ─────────────────────────────────────────────────
  message("\n=== Saving outputs ===")
  dir.create(dirname(DIARY_MIDS_PATH), showWarnings = FALSE, recursive = TRUE)
  saveRDS(imp_result, DIARY_MIDS_PATH)
  message(sprintf("  mids + center_means saved to %s", DIARY_MIDS_PATH))

  saveRDS(diary_long, DIARY_LONG_PATH)
  message(sprintf("  Long-format data saved to %s", DIARY_LONG_PATH))
  message(sprintf("  Dimensions: %d rows × %d columns", nrow(diary_long), ncol(diary_long)))

  # Diagnostics
  create_diary_diagnostics(imp)

  message("\n", paste(rep("=", 60), collapse = ""))
  message("  DIARY IMPUTATION COMPLETE")
  message(paste(rep("=", 60), collapse = ""))

  invisible(list(imp = imp, long = diary_long))
}

# Run if sourced directly
if (sys.nframe() == 0 || identical(environment(), globalenv())) {
  result <- main()
}
