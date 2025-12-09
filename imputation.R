# imputation.R
# Multiple imputation for missing self-reported variables
# Using mice package with predictive mean matching (PMM)
# Assumes Missing at Random (MAR) mechanism
#
# Variables imputed:
#   - total_hours_sleep: Sleep duration (PSQI-derived) - waves 2, 4, 6
#   - psqi_comp1_quality through psqi_comp7_tired: PSQI components (0-3 ordinal) - waves 2, 4, 6
#     Note: psqi_comp1_quality is subjective sleep quality (formerly psqi_06)
#   - epsTotal: Epworth Sleepiness Scale total score - waves 2, 4, 6
#   - wemwbs: SWEMWBS wellbeing score - all waves (1-6)
#
# Derived variables (passive imputation):
#   - psqi_global: PSQI global score (sum of 7 components, 0-21) - waves 2, 4, 6
#
# Note: Sleep measures (PSQI, ESS) were only collected at waves 2, 4, 6 by design.
#       Wave 7 is excluded (only 11 observations, data collection artifact).
#
# Reference: van Buuren & Groothuis-Oudshoorn (2011)

library(tidyverse)
library(mice)
library(future)
library(future.apply)
library(zoo)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Number of imputations (m)
N_IMPUTATIONS <- 20

# Number of iterations for the MICE algorithm
N_ITERATIONS <- 20

# Set seed for reproducibility
SEED <- 42

# Variables to impute (outcome variables)
# PSQI components (each 0-3 ordinal scale, waves 2, 4, 6 only)
PSQI_COMPONENT_VARS <- c(
  "psqi_comp1_quality",
  "psqi_comp2_latency",
  "psqi_comp3_duration",
  "psqi_comp4_efficiency",
  "psqi_comp5_problems",
  "psqi_comp6_medication",
  "psqi_comp7_tired"
)

# Other monthly outcome measures (waves 2, 4, 6 only)
# Note: psqi_06 is identical to psqi_comp1_quality, so we only use the latter
MONTHLY_OUTCOMES <- c("total_hours_sleep", "epsTotal")

# Combined monthly variables (all to be imputed at waves 2, 4, 6)
MONTHLY_VARS <- c(MONTHLY_OUTCOMES, PSQI_COMPONENT_VARS)

# Biweekly measures (all waves 1-6)
BIWEEKLY_VARS <- c("wemwbs")

# Wave-specific gaming exposure predictors (used as auxiliary variables only)
MONTHLY_GAMING_VARS <- c(
  "total_monthly_avg_minutes_played",
  "ln_monthly_avg_minutes_played"
)
BIWEEKLY_GAMING_VARS <- c(
  "total_biweekly_avg_minutes_played",
  "ln_biweekly_avg_minutes_played"
)

# Variables to impute (excluding derived variables)
VARS_TO_IMPUTE <- c(MONTHLY_OUTCOMES, PSQI_COMPONENT_VARS, BIWEEKLY_VARS)

# Derived variables (calculated via passive imputation from imputed components)
DERIVED_VARS <- c("psqi_global")

# Gaming telemetry path (needed for auxiliary predictors)
GAMING_SESSIONS_PATH <- "data/processed/gaming_sessions.csv.gz"

# Waves for each variable type
MONTHLY_WAVES <- c(2, 4, 6)
BIWEEKLY_WAVES <- c(1, 2, 3, 4, 5, 6)

# Auxiliary variables to include in imputation model (predictors)
# These help improve imputation quality but are not themselves imputed
AUXILIARY_VARS <- c(
  "age_scaled",
  "bmi_scaled",
  "SES_index_scaled",
  "msf_sc_numeric",        # Chronotype
  "region",
  "gender"
)

# Wave-invariant covariates (measured once, repeated across waves)
WAVE_INVARIANT <- c("age_scaled", "bmi_scaled", "SES_index_scaled",
                    "msf_sc_numeric", "msf_sc_centered", "region", "gender")

# ==============================================================================
# FUNCTIONS
# ==============================================================================

#' Categorize gender into Male/Female/Other
#'
#' Ensures gender is reduced to three levels prior to imputation.
#' @param gender_vec Character vector of gender responses
#' @return Factor with levels Male, Female, Other (NA preserved)
categorize_gender <- function(gender_vec) {
  clean <- gender_vec %>%
    as.character() %>%
    str_to_lower() %>%
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

#' Extract wave number suffix from a column name
#'
#' @param name Column name containing `_wX` suffix
#' @return Integer wave number or NA if not present
extract_wave_number <- function(name) {
  match <- str_match(name, "_w([0-9]+)$")
  ifelse(is.na(match[, 2]), NA_integer_, as.integer(match[, 2]))
}

#' Build reference dates per wave using observed data
#'
#' @param data Long-format self-report data with `wave` and `date`
#' @return Tibble with wave and median date
build_wave_reference_dates <- function(data) {
  data %>%
    filter(!is.na(date)) %>%
    group_by(wave) %>%
    summarise(median_date = median(date), .groups = "drop")
}

#' Fill missing survey dates via interpolation/extrapolation
#'
#' Ensures consecutive waves are at least 14 days (2 weeks) apart.
#'
#' @param data Long-format self-report data
#' @param wave_reference_dates Tibble from build_wave_reference_dates()
#' @return Data with date_imputed and date_inferred_flag columns
fill_missing_dates <- function(data, wave_reference_dates) {
  wave_lookup <- setNames(wave_reference_dates$median_date,
                          wave_reference_dates$wave)

  MIN_DAYS_BETWEEN_WAVES <- 14  # 2 weeks

  data %>%
    arrange(pid, wave) %>%
    group_by(pid) %>%
    group_modify(function(df, ...) {
      df <- arrange(df, wave)
      wave_idx <- df$wave
      date_numeric <- as.numeric(df$date)

      # Initial fill using interpolation/extrapolation
      if (all(is.na(date_numeric))) {
        # No observed dates - use population median dates
        filled_numeric <- as.numeric(wave_lookup[as.character(wave_idx)])
      } else {
        # Use linear interpolation between observed dates
        filled_numeric <- na.approx(
          date_numeric,
          x = wave_idx,
          na.rm = FALSE,
          rule = 2
        )
        missing_idx <- is.na(filled_numeric)
        if (any(missing_idx)) {
          fallback <- as.numeric(wave_lookup[as.character(wave_idx[missing_idx])])
          filled_numeric[missing_idx] <- fallback
        }
      }

      # Enforce minimum 14-day spacing between consecutive waves
      # Go through waves sequentially and ensure each is at least 14 days after previous
      for (i in seq_len(length(filled_numeric))) {
        if (i > 1) {
          min_date <- filled_numeric[i-1] + (MIN_DAYS_BETWEEN_WAVES * 86400)  # 86400 seconds per day
          if (!is.na(filled_numeric[i]) && !is.na(filled_numeric[i-1]) &&
              filled_numeric[i] < min_date) {
            filled_numeric[i] <- min_date
          }
        }
      }

      filled_posix <- as.POSIXct(
        filled_numeric,
        origin = "1970-01-01",
        tz = attr(df$date, "tzone")
      )

      df$date_imputed <- filled_posix
      missing_original <- is.na(df$date)
      df$date_inferred_flag <- missing_original & !is.na(df$date_imputed)
      df
    }) %>%
    ungroup()
}

#' Ensure each participant has rows for all waves
#'
#' @param data Long-format self-report data
#' @param waves Vector of wave numbers to enforce
#' @return Data with complete pid × wave grid
expand_to_all_waves <- function(data, waves = c(1, 2, 3, 4, 5, 6)) {
  scaffold <- expand_grid(
    pid = unique(data$pid),
    wave = waves
  )

  data_augmented <- data %>%
    mutate(.row_present = TRUE)

  expanded <- scaffold %>%
    left_join(data_augmented, by = c("pid", "wave"))

  fill_static <- function(x) {
    idx <- which(!is.na(x))
    if (length(idx) == 0) {
      return(x)
    }
    replace(x, is.na(x), x[min(idx)])
  }

  expanded <- expanded %>%
    group_by(pid) %>%
    mutate(across(all_of(WAVE_INVARIANT), fill_static)) %>%
    ungroup()

  expanded %>%
    mutate(wave_created_flag = is.na(.row_present)) %>%
    select(-.row_present)
}

#' Write summary of date imputations and exposure comparisons
#'
#' @param date_data Data frame containing date info and exposures
#' @param output_dir Directory to save the summary file
#' @param date_inference_stats Optional list with aggregated counts
write_date_imputation_summary <- function(date_data,
                                          output_dir = "output/imputation",
                                          date_inference_stats = NULL) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  summary_file <- file.path(output_dir, "date_imputation_summary.txt")

  exposures <- c(
    "total_biweekly_avg_minutes_played",
    "ln_biweekly_avg_minutes_played",
    "total_monthly_avg_minutes_played",
    "ln_monthly_avg_minutes_played"
  )

  date_augmented <- date_data %>%
    mutate(
      date_source = if_else(date_inferred_flag, "Inferred", "Observed"),
      date_used = as.Date(date_used),
      date_original = as.Date(date_original)
    )

  overall_counts <- date_augmented %>%
    group_by(date_source) %>%
    summarise(
      records = n(),
      participants = n_distinct(pid),
      .groups = "drop"
    )

  wave_counts <- date_augmented %>%
    group_by(wave, date_source) %>%
    summarise(records = n(), .groups = "drop")

  exposure_summary <- date_augmented %>%
    pivot_longer(
      cols = any_of(exposures),
      names_to = "metric",
      values_to = "value"
    ) %>%
    group_by(date_source, metric) %>%
    summarise(
      n = sum(!is.na(value)),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      p25 = quantile(value, 0.25, na.rm = TRUE),
      p75 = quantile(value, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  sink(summary_file)
  cat("=== Date Imputation Summary ===\n\n")

  if (!is.null(date_inference_stats)) {
    cat(sprintf(
      "Dates inferred: %d records across %d participants\n\n",
      date_inference_stats$n_records,
      date_inference_stats$n_participants
    ))
  }

  cat("Overall records by date source:\n")
  overall_counts %>%
    arrange(date_source) %>%
    mutate(line = sprintf("  %s: %d records (%d participants)",
                          date_source, records, participants)) %>%
    pull(line) %>%
    cat(sep = "\n")
  cat("\n\n")

  cat("Records by wave and date source:\n")
  wave_counts %>%
    arrange(wave, date_source) %>%
    mutate(line = sprintf("  Wave %d (%s): %d records",
                          wave, date_source, records)) %>%
    pull(line) %>%
    cat(sep = "\n")
  cat("\n\n")

  cat("Exposure summary (overall vs late-night, biweekly vs monthly):\n")
  exposure_summary %>%
    arrange(metric, date_source) %>%
    mutate(line = sprintf(
      "  %s | %s: n=%d, mean=%.2f, sd=%.2f, median=%.2f, IQR=[%.2f, %.2f]",
      metric, date_source, n, mean, sd, median, p25, p75
    )) %>%
    pull(line) %>%
    cat(sep = "\n")
  cat("\n")
  sink()
}

#' Add rolling gaming exposure predictors (overall + late-night)
#'
#' Computes 14-day (biweekly) and 28-day (monthly) average minutes played for
#' total gaming as well as late-night gaming (23:00-06:00). These predictors
#' are included in the imputation model but are never imputed themselves.
#'
#' @param data Self-report data frame (long format) containing pid, wave, date
#' @param sessions_path Path to telemetry data with session-level playtime
#' @return Data frame with new auxiliary predictor columns appended
add_gaming_exposure <- function(data,
                                sessions_path = GAMING_SESSIONS_PATH) {
  if (!file.exists(sessions_path)) {
    stop(sprintf(
      "Gaming sessions file not found: %s\nRun preprocess_data.R first.",
      sessions_path
    ))
  }

  message("Calculating gaming exposure predictors (overall + late-night)...")

  sessions_daily <- read_csv(sessions_path, show_col_types = FALSE) %>%
    mutate(
      session_date = as_date(sessionStart_local),
      total_minutes = coalesce(minutes_played, 0),
      ln_minutes = coalesce(latenightMinutes, 0)
    ) %>%
    group_by(pid, session_date) %>%
    summarise(
      total_minutes = sum(total_minutes, na.rm = TRUE),
      ln_minutes = sum(ln_minutes, na.rm = TRUE),
      .groups = "drop"
    )

  minutes_lookup <- split(sessions_daily, sessions_daily$pid)

  window_sum <- function(pid_value, date_value, window_days, column) {
    if (is.na(date_value)) {
      return(NA_real_)
    }
    pid_minutes <- minutes_lookup[[pid_value]]
    if (is.null(pid_minutes)) {
      return(0)
    }
    window_start <- date_value - as.integer(window_days)
    mask <- pid_minutes$session_date >= window_start &
      pid_minutes$session_date <= date_value
    sum(pid_minutes[[column]][mask], na.rm = TRUE)
  }

  data %>%
    mutate(
      .exposure_date = as_date(
        if ("date_imputed" %in% names(data)) date_imputed else date
      )
    ) %>%
    rowwise() %>%
    mutate(
      total_biweekly_avg_minutes_played =
        window_sum(pid, .exposure_date, 14, "total_minutes") / 14,
      ln_biweekly_avg_minutes_played =
        window_sum(pid, .exposure_date, 14, "ln_minutes") / 14,
      total_monthly_avg_minutes_played = if_else(
        wave %in% MONTHLY_WAVES,
        window_sum(pid, .exposure_date, 28, "total_minutes") / 28,
        NA_real_
      ),
      ln_monthly_avg_minutes_played = if_else(
        wave %in% MONTHLY_WAVES,
        window_sum(pid, .exposure_date, 28, "ln_minutes") / 28,
        NA_real_
      )
    ) %>%
    ungroup() %>%
    select(-.exposure_date)
}

#' Reshape self-report data from long to wide format for imputation
#'
#' Multiple imputation works best with data in wide format where each
#' participant has one row and repeated measures are spread across columns.
#'
#' Note: Monthly variables (sleep, ESS) are only reshaped for waves 2, 4, 6.
#' Biweekly variables (wellbeing) are reshaped for waves 1-6.
#' Wave 7 is excluded (data collection artifact with only ~11 observations).
#'
#' @param data Data frame in long format with pid, wave, and outcome columns
#' @return Data frame in wide format
reshape_to_wide <- function(data) {
  message("Reshaping data to wide format...")

  # Exclude wave 7 (only ~11 observations, artifact)
  data <- data %>%
    filter(wave %in% 1:6)

  message(sprintf("  Filtered to waves 1-6: %d observations", nrow(data)))

  # Variables to reshape for monthly waves (2, 4, 6)
  monthly_reshape_vars <- c(
    MONTHLY_OUTCOMES,      # total_hours_sleep, epsTotal
    PSQI_COMPONENT_VARS,   # 7 PSQI components (includes psqi_comp1_quality)
    DERIVED_VARS,          # psqi_global (will be recalculated via passive imputation)
    MONTHLY_GAMING_VARS    # gaming predictors
  )

  # 1. Reshape monthly variables (waves 2, 4, 6 only)
  monthly_wide <- data %>%
    filter(wave %in% MONTHLY_WAVES) %>%
    select(pid, wave, all_of(monthly_reshape_vars)) %>%
    pivot_wider(
      id_cols = pid,
      names_from = wave,
      values_from = all_of(monthly_reshape_vars),
      names_glue = "{.value}_w{wave}"  # Ensure consistent naming
    )

  # 2. Reshape biweekly variables (all waves 1-6)
  biweekly_wide <- data %>%
    filter(wave %in% BIWEEKLY_WAVES) %>%
    select(pid, wave, all_of(c(BIWEEKLY_VARS, BIWEEKLY_GAMING_VARS))) %>%
    pivot_wider(
      id_cols = pid,
      names_from = wave,
      values_from = all_of(c(BIWEEKLY_VARS, BIWEEKLY_GAMING_VARS)),
      names_glue = "{.value}_w{wave}"  # Ensure consistent naming
    )

  # 3. Get wave-invariant variables (take first non-NA value per participant)
  wave_invariant <- data %>%
    select(pid, all_of(WAVE_INVARIANT)) %>%
    group_by(pid) %>%
    summarise(across(everything(), ~first(na.omit(.))), .groups = "drop")

  # 4. Combine all
  data_wide <- monthly_wide %>%
    left_join(biweekly_wide, by = "pid") %>%
    left_join(wave_invariant, by = "pid")

  message(sprintf("  Wide format: %d participants, %d variables",
                  nrow(data_wide), ncol(data_wide) - 1))  # -1 for pid

  # Report variable counts
  n_monthly_outcomes <- length(MONTHLY_OUTCOMES) * length(MONTHLY_WAVES)
  n_psqi_components <- length(PSQI_COMPONENT_VARS) * length(MONTHLY_WAVES)
  n_derived <- length(DERIVED_VARS) * length(MONTHLY_WAVES)
  n_biweekly <- length(BIWEEKLY_VARS) * length(BIWEEKLY_WAVES)
  n_monthly_gaming <- length(MONTHLY_GAMING_VARS) * length(MONTHLY_WAVES)
  n_biweekly_gaming <- length(BIWEEKLY_GAMING_VARS) * length(BIWEEKLY_WAVES)

  message(sprintf("  Monthly outcomes (waves 2,4,6): %d", n_monthly_outcomes))
  message(sprintf("  PSQI components (waves 2,4,6): %d", n_psqi_components))
  message(sprintf("  Derived variables (waves 2,4,6): %d", n_derived))
  message(sprintf("  Biweekly variables (waves 1-6): %d", n_biweekly))
  message(sprintf("  Monthly gaming predictors: %d", n_monthly_gaming))
  message(sprintf("  Biweekly gaming predictors: %d", n_biweekly_gaming))
  message(sprintf("  Auxiliary predictors: %d", length(WAVE_INVARIANT)))

  return(data_wide)
}


#' Reshape imputed data from wide back to long format
#'
#' @param data_wide Imputed data in wide format
#' @param original_data Original data frame in long format (for structure reference)
#' @return Data frame in long format
reshape_to_long <- function(data_wide, original_data) {
  # Get wave-varying columns
  wave_cols <- names(data_wide)[str_detect(names(data_wide), "_w[0-9]+$")]

  # Pivot longer
  data_long <- data_wide %>%
    select(pid, all_of(wave_cols)) %>%
    pivot_longer(
      cols = all_of(wave_cols),
      names_to = c(".value", "wave"),
      names_pattern = "(.+)_w([0-9]+)"
    ) %>%
    mutate(wave = as.integer(wave))

  # Filter original data to waves 1-6 (matching imputed data)
  original_filtered <- original_data %>%
    filter(wave %in% 1:6)

  # Identify which imputed and derived variables exist in the long data
  imputed_and_derived_vars <- c(VARS_TO_IMPUTE, DERIVED_VARS)
  vars_to_replace <- intersect(imputed_and_derived_vars, names(data_long))

  # Join back with original data to restore other variables not in imputation
  # Keep original structure, replacing imputed and derived variables
  result <- original_filtered %>%
    select(-any_of(vars_to_replace)) %>%
    left_join(
      data_long %>% select(pid, wave, all_of(vars_to_replace)),
      by = c("pid", "wave")
    )

  return(result)
}


#' Set up predictor matrix for mice
#'
#' Configure which variables predict which in the imputation model.
#' Uses the quickpred() helper with adjustments.
#'
#' Gaming exposure variables are included as predictors (they improve imputation
#' by ~9% despite causing minor collinearity warnings). Derived variables are
#' excluded to prevent circular dependencies.
#'
#' @param data Data frame in wide format
#' @param include_gaming_vars Whether to include gaming exposure as predictors (default TRUE)
#' @return Predictor matrix for mice
setup_predictor_matrix <- function(data, include_gaming_vars = TRUE) {
  message("Setting up predictor matrix...")

  # Identify variables to exclude as predictors
  # Gaming exposure variables - KEPT by default (improve imputation +9%)
  gaming_vars <- grep("(total_|ln_)(biweekly|monthly)_avg_minutes_played",
                      names(data), value = TRUE)

  # Derived variables (calculated from other variables, cause circular dependencies)
  # These MUST be excluded
  derived_vars <- grep(paste0("^(", paste(DERIVED_VARS, collapse = "|"), ")_w"),
                       names(data), value = TRUE)

  # Combine exclusions
  if (include_gaming_vars) {
    exclude_vars <- c("pid", derived_vars)
    message("  Including gaming exposure variables as predictors")
  } else {
    exclude_vars <- c("pid", gaming_vars, derived_vars)
    message("  Excluding gaming exposure variables from predictors")
  }

  # Start with quickpred to get initial predictor selection
  # mincor = 0.1 includes variables with at least 0.1 correlation
  pred <- quickpred(data, mincor = 0.1, exclude = exclude_vars)

  # Ensure excluded variables are never used as predictors
  for (var in exclude_vars) {
    if (var %in% colnames(pred)) {
      pred[, var] <- 0  # Don't use as predictor
    }
  }

  # Ensure pid is never imputed (it's always observed)
  pred["pid", ] <- 0

  # Summary
  n_predictors <- rowSums(pred > 0)
  message(sprintf("  Average predictors per variable: %.1f", mean(n_predictors)))
  if (include_gaming_vars) {
    message(sprintf("  Gaming predictors: %d variables (total + late-night exposure)",
                    length(gaming_vars)))
    message(sprintf("  Excluded: %d derived variables only",
                    length(derived_vars)))
  } else {
    message(sprintf("  Excluded: %d gaming + %d derived variables",
                    length(gaming_vars), length(derived_vars)))
  }

  return(pred)
}


#' Set up imputation methods
#'
#' Use predictive mean matching (PMM) for continuous and ordinal variables.
#' Leave non-imputed variables empty.
#'
#' @param data Data frame in wide format
#' @return Named vector of imputation methods
setup_methods <- function(data) {
  message("Setting up imputation methods...")

  # Get variable types
  vars_to_check <- names(data)[names(data) != "pid"]

  # Initialize all methods to empty (no imputation)
  methods <- rep("", ncol(data))
  names(methods) <- names(data)

  # Build regex patterns for different variable types
  monthly_outcomes_pattern <- paste0(
    "^(", paste(MONTHLY_OUTCOMES, collapse = "|"), ")_w[246]$"
  )

  psqi_components_pattern <- paste0(
    "^(", paste(PSQI_COMPONENT_VARS, collapse = "|"), ")_w[246]$"
  )

  biweekly_pattern <- paste0(
    "^(", paste(BIWEEKLY_VARS, collapse = "|"), ")_w[1-6]$"
  )

  derived_pattern <- paste0(
    "^(", paste(DERIVED_VARS, collapse = "|"), ")_w[246]$"
  )

  # Identify columns
  monthly_outcome_cols <- names(data)[str_detect(names(data), monthly_outcomes_pattern)]
  psqi_component_cols <- names(data)[str_detect(names(data), psqi_components_pattern)]
  biweekly_cols <- names(data)[str_detect(names(data), biweekly_pattern)]
  derived_cols <- names(data)[str_detect(names(data), derived_pattern)]

  # Set PMM for outcomes and components
  # PMM is robust for continuous and ordinal data
  methods[monthly_outcome_cols] <- "pmm"
  methods[psqi_component_cols] <- "pmm"
  methods[biweekly_cols] <- "pmm"

  # Set passive imputation for derived variables
  # psqi_global = sum of 7 PSQI components
  for (col in derived_cols) {
    # Extract wave number (e.g., "psqi_global_w2" -> "2")
    wave_num <- str_extract(col, "(?<=_w)[0-9]+$")

    # Create formula: ~I(comp1_w2 + comp2_w2 + ... + comp7_w2)
    component_cols <- paste0(PSQI_COMPONENT_VARS, "_w", wave_num)
    formula_str <- paste0(
      "~I(", paste(component_cols, collapse = " + "), ")"
    )

    methods[col] <- formula_str
  }

  # Report summary
  n_imputed <- sum(methods == "pmm")
  n_derived <- sum(str_detect(methods, "^~I\\("))

  message(sprintf("  Variables to impute (PMM): %d", n_imputed))
  message(sprintf("    Monthly outcomes: %d", length(monthly_outcome_cols)))
  message(sprintf("    PSQI components: %d", length(psqi_component_cols)))
  message(sprintf("    Biweekly variables: %d", length(biweekly_cols)))
  message(sprintf("  Derived variables (passive): %d", n_derived))
  message(sprintf("  Variables as predictors only: %d",
                  length(vars_to_check) - n_imputed - n_derived))

  return(methods)
}


#' Get list of columns to impute
#'
#' Helper function to identify which columns will be imputed.
#'
#' @param data Data frame in wide format
#' @return Character vector of column names to impute
get_imputation_columns <- function(data) {
  # Monthly outcomes + PSQI components (waves 2, 4, 6)
  monthly_pattern <- paste0(
    "^(", paste(c(MONTHLY_OUTCOMES, PSQI_COMPONENT_VARS), collapse = "|"), ")_w[246]$"
  )

  # Biweekly variables (waves 1-6)
  biweekly_pattern <- paste0(
    "^(", paste(BIWEEKLY_VARS, collapse = "|"), ")_w[1-6]$"
  )

  monthly_cols <- names(data)[str_detect(names(data), monthly_pattern)]
  biweekly_cols <- names(data)[str_detect(names(data), biweekly_pattern)]

  c(monthly_cols, biweekly_cols)
}


#' Run multiple imputation with parallel processing
#'
#' Uses the future framework to parallelize across CPU cores.
#'
#' @param data Data frame in wide format
#' @param n_imputations Number of imputed datasets to create
#' @param n_iterations Number of iterations for convergence
#' @param seed Random seed for reproducibility
#' @return mids object containing imputed datasets
run_mice_parallel <- function(data, n_imputations = N_IMPUTATIONS,
                              n_iterations = N_ITERATIONS, seed = SEED,
                              include_gaming_vars = TRUE) {

  # Detect number of available cores
  n_cores <- availableCores()
  message(sprintf("\n=== Running Multiple Imputation ==="))
  message(sprintf("Available CPU cores: %d", n_cores))
  message(sprintf("Number of imputations (m): %d", n_imputations))
  message(sprintf("Number of iterations: %d", n_iterations))

  # Set up parallel plan
  # Use multisession for compatibility across platforms
  plan(multisession, workers = min(n_cores, n_imputations))

  # Set up predictor matrix and methods
  pred <- setup_predictor_matrix(data, include_gaming_vars = include_gaming_vars)
  methods <- setup_methods(data)

  # Report missingness
  message("\nMissingness summary for variables to impute:")
  outcome_cols <- get_imputation_columns(data)
  for (var in outcome_cols) {
    n_miss <- sum(is.na(data[[var]]))
    pct_miss <- 100 * n_miss / nrow(data)
    if (n_miss > 0) {
      message(sprintf("  %s: %d (%.1f%%) missing", var, n_miss, pct_miss))
    }
  }

  # Run mice with parallel processing using the built-in future support
  # mice >= 3.16.0 supports parallelize = "future"
  message("\nStarting imputation (this may take several minutes)...")
  start_time <- Sys.time()

  # Set seed for reproducibility
  set.seed(seed)

  # Run mice
  # The future plan handles parallelization automatically
  imp <- mice(
    data = data,
    m = n_imputations,
    method = methods,
    predictorMatrix = pred,
    maxit = n_iterations,
    seed = seed,
    printFlag = TRUE
  )

  end_time <- Sys.time()
  elapsed <- difftime(end_time, start_time, units = "mins")

  message(sprintf("\nImputation complete in %.1f minutes", as.numeric(elapsed)))

  # Reset to sequential processing
  plan(sequential)

  return(imp)
}


#' Run mice in parallel using a chunked approach
#'
#' This approach runs multiple mice() calls in parallel, each generating
#' a subset of imputations, then combines them.
#'
#' @param data Data frame in wide format
#' @param n_imputations Total number of imputed datasets
#' @param n_iterations Number of iterations per imputation
#' @param seed Random seed
#' @return mids object containing all imputed datasets
run_mice_parallel_chunked <- function(data, n_imputations = N_IMPUTATIONS,
                                      n_iterations = N_ITERATIONS, seed = SEED,
                                      include_gaming_vars = TRUE) {

  n_cores <- availableCores()
  message(sprintf("\n=== Running Parallel Multiple Imputation (Chunked) ==="))
  message(sprintf("Available CPU cores: %d", n_cores))
  message(sprintf("Number of imputations (m): %d", n_imputations))
  message(sprintf("Number of iterations: %d", n_iterations))

  # Determine chunk size - aim for roughly equal chunks across cores
  n_workers <- min(n_cores, n_imputations)
  imps_per_chunk <- ceiling(n_imputations / n_workers)

  message(sprintf("Running %d parallel jobs with ~%d imputations each",
                  n_workers, imps_per_chunk))

  # Set up predictor matrix and methods
  pred <- setup_predictor_matrix(data, include_gaming_vars = include_gaming_vars)
  methods <- setup_methods(data)

  # Report missingness
  message("\nMissingness summary for variables to impute:")
  outcome_cols <- get_imputation_columns(data)
  for (var in outcome_cols) {
    n_miss <- sum(is.na(data[[var]]))
    pct_miss <- 100 * n_miss / nrow(data)
    if (n_miss > 0) {
      message(sprintf("  %s: %d (%.1f%%) missing", var, n_miss, pct_miss))
    }
  }

  # Set up parallel plan
  plan(multisession, workers = n_workers)

  message("\nStarting parallel imputation...")
  start_time <- Sys.time()

  # Create seeds for each chunk (for reproducibility)
  set.seed(seed)
  chunk_seeds <- sample.int(1e7, n_workers)

  # Create chunk assignments
  chunk_sizes <- rep(imps_per_chunk, n_workers)
  # Adjust last chunk if needed
  total_assigned <- sum(chunk_sizes)
  if (total_assigned > n_imputations) {
    chunk_sizes[n_workers] <- chunk_sizes[n_workers] - (total_assigned - n_imputations)
  }

  # Run mice in parallel using future_lapply
  imp_list <- future_lapply(seq_len(n_workers), function(i) {
    mice(
      data = data,
      m = chunk_sizes[i],
      method = methods,
      predictorMatrix = pred,
      maxit = n_iterations,
      seed = chunk_seeds[i],
      printFlag = FALSE
    )
  }, future.seed = TRUE)

  # Combine results using ibind
  message("Combining imputed datasets...")
  imp_combined <- imp_list[[1]]
  if (length(imp_list) > 1) {
    for (i in 2:length(imp_list)) {
      imp_combined <- ibind(imp_combined, imp_list[[i]])
    }
  }

  end_time <- Sys.time()
  elapsed <- difftime(end_time, start_time, units = "mins")

  message(sprintf("\nImputation complete in %.1f minutes", as.numeric(elapsed)))
  message(sprintf("Total imputed datasets: %d", imp_combined$m))

  # Reset to sequential processing
  plan(sequential)

  return(imp_combined)
}


#' Diagnostic plots for imputation
#'
#' @param imp mids object from mice
#' @param output_dir Directory to save plots
create_diagnostics <- function(imp, output_dir = "output/imputation",
                               dropped_pids = character(),
                               date_inference_stats = NULL,
                               date_diagnostics = NULL) {
  message("\nCreating diagnostic plots...")

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Get outcome columns that were actually imputed
  outcome_cols <- get_imputation_columns(imp$data)

  # Filter to columns that have at least some missing data
  cols_with_missing <- outcome_cols[sapply(outcome_cols, function(col) {
    any(is.na(imp$data[[col]]))
  })]

  # Prepare completed datasets once for downstream summaries
  completed_list <- lapply(seq_len(imp$m), function(i) complete(imp, i))

  # 1. Convergence plots (trace plots)
  message("  Creating convergence plots...")
  pdf(file.path(output_dir, "convergence_plots.pdf"), width = 12, height = 8)
  tryCatch({
    print(plot(imp))
  }, error = function(e) {
    message("    Warning: Could not create convergence plot: ", e$message)
  })
  dev.off()

  # 2. Density plots comparing observed vs imputed
  # Only for columns with sufficient observed data
  message("  Creating density comparison plots...")
  pdf(file.path(output_dir, "density_plots.pdf"), width = 12, height = 10)
  tryCatch({
    # Filter to variables with enough observed data for density estimation
    cols_for_density <- cols_with_missing[sapply(cols_with_missing, function(col) {
      sum(!is.na(imp$data[[col]])) >= 10
    })]
    if (length(cols_for_density) > 0) {
      # Create density plots for each variable
      for (col in cols_for_density) {
        tryCatch({
          print(densityplot(imp, as.formula(paste("~", col))))
        }, error = function(e) NULL)
      }
    } else {
      message("    No columns with sufficient observed data for density plots")
    }
  }, error = function(e) {
    message("    Warning: Could not create density plot: ", e$message)
  })
  dev.off()

  # 3. Strip plots
  message("  Creating strip plots...")
  pdf(file.path(output_dir, "strip_plots.pdf"), width = 12, height = 10)
  tryCatch({
    # Create strip plots for each imputed variable
    for (col in cols_with_missing) {
      tryCatch({
        print(stripplot(imp, as.formula(paste("~", col))))
      }, error = function(e) NULL)
    }
  }, error = function(e) {
    message("    Warning: Could not create strip plot: ", e$message)
  })
  dev.off()

  # 4. Summary of imputation
  message("  Creating imputation summary...")
  summary_file <- file.path(output_dir, "imputation_summary.txt")
  sink(summary_file)
  cat("=== Multiple Imputation Summary ===\n\n")
  cat(sprintf("Number of imputations (m): %d\n", imp$m))
  cat(sprintf("Number of iterations: %d\n", imp$iteration))
  cat(sprintf("Number of participants: %d\n\n", nrow(imp$data)))

  cat("Variables imputed:\n")
  for (col in cols_with_missing) {
    n_miss <- sum(is.na(imp$data[[col]]))
    pct_miss <- 100 * n_miss / nrow(imp$data)
    cat(sprintf("  %s: %d (%.1f%%) missing\n", col, n_miss, pct_miss))
  }

  if (length(dropped_pids) > 0) {
    cat("\nParticipants excluded due to missing WEMWBS at wave 1:\n")
    cat(sprintf("  %s\n", paste(dropped_pids, collapse = ", ")))
  }

  if (!is.null(date_inference_stats) &&
      date_inference_stats$n_records > 0) {
    cat(sprintf(
      "\nSurvey completion dates inferred for %d records (%d participants) to enable gaming exposure windows.\n",
      date_inference_stats$n_records,
      date_inference_stats$n_participants
    ))
  }

  # Optional: diagnostic plots for inferred survey dates
  if (!is.null(date_diagnostics) &&
      any(date_diagnostics$date_inferred_flag, na.rm = TRUE)) {
    date_plot_path <- file.path(output_dir, "date_imputation_plots.pdf")
    pdf(date_plot_path, width = 12, height = 8)
    tryCatch({
      plot_counts <- date_diagnostics %>%
        mutate(source = if_else(date_inferred_flag, "Inferred", "Observed")) %>%
        count(wave, source) %>%
        ggplot(aes(x = factor(wave), y = n, fill = source)) +
        geom_col(position = "stack") +
        labs(
          title = "Survey Dates by Wave",
          x = "Wave",
          y = "Number of Records",
          fill = "Date Source"
        ) +
        theme_minimal()

      plot_hist <- date_diagnostics %>%
        filter(date_inferred_flag) %>%
        mutate(date_used = as.Date(date_used)) %>%
        ggplot(aes(x = date_used)) +
        geom_histogram(binwidth = 3, color = "white") +
        facet_wrap(~ wave, scales = "free_y") +
        labs(
          title = "Distribution of Inferred Survey Dates",
          x = "Date (UTC)",
          y = "Count"
        ) +
        theme_minimal()

      print(plot_counts)
      print(plot_hist)
    }, error = function(e) {
      message("    Warning: Could not create date imputation plot: ", e$message)
    })
    dev.off()
  }

  # Add summary statistics comparing observed vs imputed values
  cat("\nObserved vs imputed summary statistics:\n")
  for (col in cols_with_missing) {
    missing_idx <- is.na(imp$data[[col]])
    observed_vals <- imp$data[[col]][!missing_idx]
    imputed_vals <- unlist(lapply(completed_list, function(df) df[[col]][missing_idx]))

    obs_mean <- mean(observed_vals, na.rm = TRUE)
    obs_sd <- sd(observed_vals, na.rm = TRUE)

    if (all(is.na(imputed_vals))) {
      cat(sprintf(
        "  %s: observed = %.2f ± %.2f (n = %d), imputed values unavailable (all NA)\n",
        col,
        obs_mean, obs_sd, length(observed_vals)
      ))
    } else {
      imp_mean <- mean(imputed_vals, na.rm = TRUE)
      imp_sd <- sd(imputed_vals, na.rm = TRUE)
      diff_mean <- imp_mean - obs_mean

      cat(sprintf(
        "  %s: observed = %.2f ± %.2f (n = %d), imputed = %.2f ± %.2f (n = %d), diff = %.2f\n",
        col,
        obs_mean, obs_sd, length(observed_vals),
        imp_mean, imp_sd, length(imputed_vals),
        diff_mean
      ))
    }
  }
  sink()

  message(sprintf("  Diagnostic output saved to %s/", output_dir))
}


#' Save imputed data
#'
#' @param imp mids object from mice
#' @param original_data Original data in long format
#' @param output_path Path to save imputed data
save_imputed_data <- function(imp, original_data, output_path = "data/processed/selfreport_imputed.rds") {
  message(sprintf("\nSaving imputed data to %s", output_path))

  # Create output directory if needed
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

  # Save the mids object
  saveRDS(imp, output_path)

  # Also save as individual datasets in long format for easier use
  message("Converting imputed datasets to long format...")
  long_path <- str_replace(output_path, "\\.rds$", "_long.rds")

  # Convert each imputed dataset to long format
  imputed_long_list <- lapply(1:imp$m, function(i) {
    data_wide_imp <- complete(imp, i)
    data_long_imp <- reshape_to_long(data_wide_imp, original_data)
    data_long_imp$.imp <- i
    data_long_imp
  })

  # Combine into single data frame with .imp column
  imputed_long <- bind_rows(imputed_long_list)
  saveRDS(imputed_long, long_path)

  message(sprintf("  Saved: %s (mids object)", output_path))
  message(sprintf("  Saved: %s (long format, stacked)", long_path))

  return(invisible(NULL))
}


# ==============================================================================
# MAIN IMPUTATION FUNCTION
# ==============================================================================

#' Run full imputation pipeline
#'
#' @param input_path Path to self-report data
#' @param output_path Path to save imputed data
#' @param n_imputations Number of imputations
#' @param n_iterations Number of iterations
#' @param create_diagnostics Whether to create diagnostic plots
#' @param parallel_method Either "standard" or "chunked"
#' @return mids object containing imputed datasets
run_imputation <- function(input_path = "data/processed/selfreport.csv.gz",
                           output_path = "data/processed/selfreport_imputed.rds",
                           n_imputations = N_IMPUTATIONS,
                           n_iterations = N_ITERATIONS,
                           create_diagnostics_flag = TRUE,
                           parallel_method = "chunked",
                           drop_wemwbs_w1_missing_flag = TRUE,
                           skip_preparation = FALSE,
                           include_gaming_vars = TRUE) {

  message("=== Multiple Imputation for Self-Report Variables ===\n")

  # 1. Load data
  message("Loading data...")
  if (!file.exists(input_path)) {
    stop(sprintf("Input file not found: %s\nRun preprocess_data.R first.", input_path))
  }
  data_long <- read_csv(input_path, show_col_types = FALSE)
  message(sprintf("  Loaded %d observations from %d participants",
                  nrow(data_long), n_distinct(data_long$pid)))

  # Restrict to analytic waves (1-6)
  data_long <- data_long %>%
    filter(wave %in% 1:6)

  dropped_pids <- character()
  date_inference_stats <- list(n_records = 0, n_participants = 0)

  if (!skip_preparation) {
    # Optionally drop participants missing WEMWBS at wave 1 (insufficient predictors)
    if (drop_wemwbs_w1_missing_flag) {
      dropped_pids <- data_long %>%
        filter(wave == 1, is.na(wemwbs)) %>%
        distinct(pid) %>%
        pull(pid)

      if (length(dropped_pids) > 0) {
        data_long <- data_long %>% filter(!pid %in% dropped_pids)
        message(sprintf("  Dropped %d participants lacking WEMWBS at wave 1",
                        length(dropped_pids)))
      }
    }

    # 1b. Ensure each participant has rows for all waves
    data_long <- expand_to_all_waves(data_long)

    # 1c. Estimate missing survey dates to support gaming exposure calculation
    wave_reference_dates <- build_wave_reference_dates(data_long)
    data_long <- fill_missing_dates(data_long, wave_reference_dates)
    n_inferred_dates <- sum(data_long$date_inferred_flag, na.rm = TRUE)
    n_participants_inferred <- data_long %>%
      filter(date_inferred_flag) %>%
      summarise(n = n_distinct(pid)) %>%
      pull(n)
    if (length(n_participants_inferred) == 0 || is.na(n_participants_inferred)) {
      n_participants_inferred <- 0
    }
    date_inference_stats <- list(
      n_records = ifelse(is.na(n_inferred_dates), 0, n_inferred_dates),
      n_participants = n_participants_inferred
    )

    # 1d. Categorize gender after ensuring alignment
    data_long <- data_long %>%
      mutate(gender = categorize_gender(gender))

    # 1e. Attach gaming exposure predictors (auxiliary, not imputed)
    data_long <- add_gaming_exposure(data_long, GAMING_SESSIONS_PATH)
  } else {
    message("  Skipping preparation steps (data already prepared)")
    # Extract date inference stats from prepared data
    n_inferred_dates <- sum(data_long$date_inferred_flag, na.rm = TRUE)
    n_participants_inferred <- data_long %>%
      filter(date_inferred_flag) %>%
      summarise(n = n_distinct(pid)) %>%
      pull(n)
    if (length(n_participants_inferred) == 0 || is.na(n_participants_inferred)) {
      n_participants_inferred <- 0
    }
    date_inference_stats <- list(
      n_records = ifelse(is.na(n_inferred_dates), 0, n_inferred_dates),
      n_participants = n_participants_inferred
    )
  }

  # Create date diagnostics for reporting
  date_diagnostics <- data_long %>%
    mutate(
      date_original = date,
      date_used = date_imputed
    ) %>%
    select(
      pid, wave,
      date_original, date_used,
      date_inferred_flag, wave_created_flag,
      total_biweekly_avg_minutes_played,
      ln_biweekly_avg_minutes_played,
      total_monthly_avg_minutes_played,
      ln_monthly_avg_minutes_played
    )

  # 2. Reshape to wide format
  data_wide <- reshape_to_wide(data_long)

  # 3. Run multiple imputation
  if (parallel_method == "chunked") {
    imp <- run_mice_parallel_chunked(data_wide, n_imputations, n_iterations,
                                     include_gaming_vars = include_gaming_vars)
  } else {
    imp <- run_mice_parallel(data_wide, n_imputations, n_iterations,
                            include_gaming_vars = include_gaming_vars)
  }

  # 4. Create diagnostics
  if (create_diagnostics_flag) {
    create_diagnostics(
      imp,
      dropped_pids = dropped_pids,
      date_inference_stats = date_inference_stats,
      date_diagnostics = date_diagnostics
    )
    write_date_imputation_summary(
      date_diagnostics,
      output_dir = "output/imputation",
      date_inference_stats = date_inference_stats
    )
  }

  # 5. Save results
  save_imputed_data(imp, data_long, output_path)

  message("\n=== Imputation Complete ===")
  message(sprintf("Imputed datasets: %d", imp$m))
  message(sprintf("Iterations: %d", imp$iteration))

  return(invisible(imp))
}


# ==============================================================================
# UTILITY FUNCTIONS FOR ANALYSIS
# ==============================================================================

#' Load imputed data and return as mids object
#'
#' @param path Path to saved mids object
#' @return mids object
load_imputed_mids <- function(path = "data/processed/selfreport_imputed.rds") {
  if (!file.exists(path)) {
    stop(sprintf("Imputed data not found: %s\nRun run_imputation() first.", path))
  }
  readRDS(path)
}


#' Load imputed data in long format
#'
#' @param path Path to saved long-format data
#' @return Data frame with all imputations stacked (column .imp indicates imputation number)
load_imputed_long <- function(path = "data/processed/selfreport_imputed_long.rds") {
  if (!file.exists(path)) {
    stop(sprintf("Imputed data not found: %s\nRun run_imputation() first.", path))
  }
  readRDS(path)
}


#' Pool results from models fit to imputed data
#'
#' Wrapper around mice::pool() for convenient use.
#'
#' @param fits List of model fits (one per imputation)
#' @return Pooled results
pool_results <- function(fits) {
  pool(fits)
}


# ==============================================================================
# RUN IMPUTATION (when script is sourced/executed)
# ==============================================================================

should_run_imputation <- Sys.getenv("RUN_IMPUTATION_ON_SOURCE", "1") == "1"

if (!interactive() && should_run_imputation) {
  # Parse command line arguments if any
  args <- commandArgs(trailingOnly = TRUE)

  n_imp <- ifelse(length(args) >= 1, as.integer(args[1]), N_IMPUTATIONS)
  n_iter <- ifelse(length(args) >= 2, as.integer(args[2]), N_ITERATIONS)

  run_imputation(
    n_imputations = n_imp,
    n_iterations = n_iter,
    create_diagnostics_flag = TRUE,
    parallel_method = "chunked"
  )
}

