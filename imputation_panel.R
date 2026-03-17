# imputation_panel.R
# Hierarchical two-level multiple imputation for missing self-reported variables
# Uses miceadds::mice.impute.2l.pmm with participants as clusters (level 2)
# and repeated waves nested within (level 1), under a MAR assumption.
#
# Variables imputed (2l.pmm):
#   - total_hours_sleep: Sleep duration (PSQI-derived) - waves 2, 4, 6
#   - psqi_comp1_quality through psqi_comp7_tired: PSQI components (0-3 ordinal) - waves 2, 4, 6
#   - epsTotal: Epworth Sleepiness Scale total score - waves 2, 4, 6
#   - wemwbs: SWEMWBS wellbeing score - all waves (1-6)
#
# Derived variables (passive imputation):
#   - psqi_global: PSQI global score (sum of 7 components, 0-21) - waves 2, 4, 6
#
# Structural NAs: Sleep measures (PSQI, ESS) were only collected at waves 2, 4, 6
# by design. A where matrix protects these structural NAs from imputation.
# Wave 7 is excluded (only 11 observations, data collection artifact).
#
# References:
#   van Buuren & Groothuis-Oudshoorn (2011)
#   Grund, Lüdtke & Robitzsch (2018) - multilevel multiple imputation

library(tidyverse)
library(mice)
library(miceadds)
library(future)
library(future.apply)
library(zoo)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

N_IMPUTATIONS <- 20
N_ITERATIONS <- 20
SEED <- 42

# Temporal lag/lead depth for within-person context (number of adjacent waves)
N_LAG_LEADS <- 1

PSQI_COMPONENT_VARS <- c(
  "psqi_comp1_quality",
  "psqi_comp2_latency",
  "psqi_comp3_duration",
  "psqi_comp4_efficiency",
  "psqi_comp5_problems",
  "psqi_comp6_medication",
  "psqi_comp7_tired"
)

MONTHLY_OUTCOMES <- c("total_hours_sleep", "epsTotal")
MONTHLY_VARS <- c(MONTHLY_OUTCOMES, PSQI_COMPONENT_VARS)
BIWEEKLY_VARS <- c("wemwbs")
VARS_TO_IMPUTE <- c(MONTHLY_OUTCOMES, PSQI_COMPONENT_VARS, BIWEEKLY_VARS)
DERIVED_VARS <- c("psqi_global")

BIWEEKLY_GAMING_VARS <- c(
  "total_biweekly_avg_minutes_played",
  "ln_biweekly_avg_minutes_played"
)

GAMING_SESSIONS_PATH <- "data/processed/gaming_sessions.csv.gz"

MONTHLY_WAVES <- c(2, 4, 6)
BIWEEKLY_WAVES <- c(1, 2, 3, 4, 5, 6)

# Level-2 (between-person) numeric predictors for mice
# msf_sc_numeric excluded to avoid near-perfect collinearity with msf_sc_centered
MICE_L2_NUMERIC <- c("age_scaled", "bmi_scaled", "SES_index_scaled", "msf_sc_centered")

# Wave-invariant covariates (measured once, repeated across waves in original data)
WAVE_INVARIANT <- c("age_scaled", "bmi_scaled", "SES_index_scaled",
                    "msf_sc_numeric", "msf_sc_centered", "region", "gender")

# ==============================================================================
# DATA PREPARATION FUNCTIONS
# ==============================================================================

#' Categorize gender into Male/Female/Other
#'
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

  MIN_DAYS_BETWEEN_WAVES <- 14

  data %>%
    arrange(pid, wave) %>%
    group_by(pid) %>%
    group_modify(function(df, ...) {
      df <- arrange(df, wave)
      wave_idx <- df$wave
      date_numeric <- as.numeric(df$date)

      if (all(is.na(date_numeric))) {
        filled_numeric <- as.numeric(wave_lookup[as.character(wave_idx)])
      } else {
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

      for (i in seq_len(length(filled_numeric))) {
        if (i > 1) {
          min_date <- filled_numeric[i-1] + (MIN_DAYS_BETWEEN_WAVES * 86400)
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
#' @return Data with complete pid x wave grid
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
    if (length(idx) == 0) return(x)
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
write_date_imputation_summary <- function(date_data,
                                          output_dir = "output/imputation",
                                          date_inference_stats = NULL) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  summary_file <- file.path(output_dir, "date_imputation_summary.txt")

  exposures <- c(
    "total_biweekly_avg_minutes_played",
    "ln_biweekly_avg_minutes_played"
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

  cat("Exposure summary (biweekly gaming):\n")
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

#' Add rolling gaming exposure predictors (biweekly averages)
#'
#' Computes 14-day average total and late-night minutes played. These predictors
#' are included in the imputation model as level-1 auxiliaries.
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

  message("Calculating gaming exposure predictors (biweekly averages)...")

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
    if (is.na(date_value)) return(NA_real_)
    pid_minutes <- minutes_lookup[[pid_value]]
    if (is.null(pid_minutes)) return(0)
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
        window_sum(pid, .exposure_date, 14, "ln_minutes") / 14
    ) %>%
    ungroup() %>%
    select(-.exposure_date)
}


# ==============================================================================
# MULTILEVEL IMPUTATION FUNCTIONS (long format, 2l.pmm via miceadds)
# ==============================================================================

#' Add temporal lag and lead columns for outcome variables
#'
#' Adds {var}_lag1 ... {var}_lag{k} and {var}_lead1 ... {var}_lead{k} columns
#' computed within pid groups, ordered by wave. These serve as temporal context
#' predictors in the multilevel imputation model (analogous to the banded
#' predictor matrix in a wide-format approach).
#'
#' @param data Long-format panel data with pid and wave columns
#' @param vars Character vector of variable names to lag/lead
#' @param k    Number of lag/lead steps in each direction
#' @return data with 2 * k * length(vars) new columns appended
add_temporal_lags <- function(data, vars, k) {
  message(sprintf("  Adding +/-%d wave lag/lead predictors for: %s",
                  k, paste(vars, collapse = ", ")))

  data <- data |> arrange(pid, wave)

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


#' Prepare panel long data for multilevel mice (type conversions)
#'
#' Converts pid to integer (required for 2l.pmm cluster coding: column coded -2),
#' converts gender factor to numeric, and encodes region as integer.
#' Auxiliary (non-target) predictors with NAs are mean-imputed so that the
#' lmer model inside 2l.pmm has complete predictor data (listwise deletion
#' with sparse auxiliaries would otherwise discard too many rows).
#'
#' @param data Long-format panel data with all variables
#' @param lag_lead_cols Character vector of lag/lead column names
#' @return List with $data (mice-ready), $pid_lookup, $region_lookup
prepare_for_mice_multilevel <- function(data, lag_lead_cols) {
  message("  Preparing data for multilevel mice...")

  # pid -> integer
  pid_levels  <- sort(unique(data$pid))
  pid_lookup  <- tibble(pid = pid_levels, pid_int = seq_along(pid_levels))

  # region -> integer
  region_levels <- levels(factor(data$region))
  region_lookup <- tibble(region = region_levels,
                          region_num = seq_along(region_levels))

  # Select only the columns mice needs
  mice_cols <- c(
    "pid", "wave",
    VARS_TO_IMPUTE, DERIVED_VARS,
    MICE_L2_NUMERIC,
    "gender", "region",
    BIWEEKLY_GAMING_VARS,
    lag_lead_cols
  )
  mice_cols <- intersect(mice_cols, names(data))

  out <- data |>
    select(all_of(mice_cols)) |>
    left_join(pid_lookup, by = "pid") |>
    left_join(region_lookup, by = "region") |>
    mutate(gender_num = as.integer(factor(gender,
                                          levels = c("Male", "Female", "Other")))) |>
    select(-pid, -gender, -region)

  # Mean-impute auxiliary (non-target) numeric columns that have NAs.
  # 2l.pmm uses lmer internally, which does listwise deletion on predictors.
  # If auxiliaries like msf_sc_centered (41% missing) or lag/lead columns
  # retain NAs, the model loses too many rows. Filling with the column mean
  # is standard practice for auxiliary predictors in MICE workflows.
  target_cols <- c(VARS_TO_IMPUTE, DERIVED_VARS)
  aux_numeric <- setdiff(names(out), c("pid_int", target_cols))

  n_filled <- 0L
  for (col in aux_numeric) {
    if (is.numeric(out[[col]]) && any(is.na(out[[col]]))) {
      col_mean <- mean(out[[col]], na.rm = TRUE)
      n_na <- sum(is.na(out[[col]]))
      out[[col]][is.na(out[[col]])] <- col_mean
      n_filled <- n_filled + n_na
    }
  }
  if (n_filled > 0) {
    message(sprintf("  Mean-imputed %d cells in auxiliary predictor columns", n_filled))
  }

  message(sprintf("  %d participants -> pid_int 1..%d",
                  length(pid_levels), length(pid_levels)))
  message(sprintf("  %d region levels -> region_num 1..%d",
                  length(region_levels), length(region_levels)))
  message(sprintf("  mice data: %d rows x %d cols", nrow(out), ncol(out)))

  list(data = out, pid_lookup = pid_lookup, region_lookup = region_lookup)
}


#' Set up predictor matrix for multilevel mice (2l.pmm)
#'
#' Coding for miceadds::mice.impute.2l.pmm:
#'   -2 = cluster (pid_int)
#'    2 = fixed + random effect (use only when clusters have enough observations)
#'    1 = fixed effect only
#'    0 = not a predictor / not imputed
#'
#' Panel data has at most 6 waves (3 for monthly variables), so level-2
#' (between-person) variables are coded as 1 (fixed effect only) rather than 2
#' (which would add random slopes and over-parameterise the model).
#' Only pid_int is coded -2 to specify the random-intercept cluster.
#'
#' @param data          mice-ready data frame (with pid_int, gender_num, etc.)
#' @param lag_lead_cols Character vector of lag/lead column names
#' @return Integer predictor matrix
setup_multilevel_predictor_matrix <- function(data, lag_lead_cols) {
  message("Setting up multilevel predictor matrix (-2/1 coding)...")

  vars   <- names(data)
  n_vars <- length(vars)
  pred   <- matrix(0L, nrow = n_vars, ncol = n_vars,
                   dimnames = list(vars, vars))

  l2_vars <- c(MICE_L2_NUMERIC, "gender_num", "region_num")
  l2_vars <- intersect(l2_vars, vars)

  l1_vars <- c(
    "wave",
    BIWEEKLY_GAMING_VARS,
    lag_lead_cols
  )
  l1_vars <- intersect(l1_vars, vars)

  all_predictor_vars <- c(l2_vars, l1_vars, VARS_TO_IMPUTE, DERIVED_VARS)

  for (i in seq_len(n_vars)) {
    target <- vars[i]

    is_2l_target     <- target %in% VARS_TO_IMPUTE
    is_passive_target <- target %in% DERIVED_VARS
    if (!is_2l_target && !is_passive_target) next

    for (j in seq_len(n_vars)) {
      if (i == j) next
      pred_var <- vars[j]

      if (pred_var == "pid_int") {
        if (is_2l_target) pred[i, j] <- -2L
      } else if (pred_var %in% all_predictor_vars) {
        pred[i, j] <- 1L
      }
    }
  }

  if ("pid_int" %in% vars) pred["pid_int", ] <- 0L

  n_preds <- rowSums(pred != 0)
  outcome_preds <- n_preds[intersect(VARS_TO_IMPUTE, vars)]
  message(sprintf("  Avg predictors per outcome variable: %.1f", mean(outcome_preds)))
  message(sprintf("  Level-2 vars (fixed effect): %s", paste(l2_vars, collapse = ", ")))
  message(sprintf("  Level-1 vars: %d (wave + gaming + %d lag/lead)",
                  length(l1_vars), length(lag_lead_cols)))

  pred
}


#' Run a single mice pass in parallel chunks
#'
#' @param data  mice-ready data frame
#' @param methods Named character vector of imputation methods
#' @param pred Integer predictor matrix
#' @param label Label for progress messages
#' @return mids object
run_mice_parallel <- function(data, methods, pred, label = "") {
  n_workers      <- min(4L, availableCores(), N_IMPUTATIONS)
  chunk_sizes    <- rep(ceiling(N_IMPUTATIONS / n_workers), n_workers)
  total_assigned <- sum(chunk_sizes)
  if (total_assigned > N_IMPUTATIONS)
    chunk_sizes[n_workers] <- chunk_sizes[n_workers] - (total_assigned - N_IMPUTATIONS)

  set.seed(SEED)
  chunk_seeds <- sample.int(1e7, n_workers)

  plan(multisession, workers = n_workers)
  message(sprintf("  [%s] Launching %d parallel chunks (~%d imps each)...",
                  label, n_workers, chunk_sizes[1]))
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

  message("  Combining imputed datasets...")
  imp_combined <- Reduce(ibind, imp_list)

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  message(sprintf("  [%s] Complete in %.1f minutes | m = %d",
                  label, elapsed, imp_combined$m))

  imp_combined
}


#' Run multilevel panel imputation in two passes
#'
#' Panel data has two groups of variables with different wave availability:
#'   Pass 1: Biweekly (wemwbs) - all 6 waves
#'   Pass 2: Monthly (PSQI, ESS, sleep duration) - waves 2, 4, 6 only
#'
#' Each pass runs 2l.pmm on the appropriate subset of rows so that structural
#' NAs (monthly vars at waves 1,3,5) never enter the imputation model.
#' Results are merged back into the full long-format dataset.
#'
#' @param data  mice-ready long-format data (pid_int as cluster integer)
#' @param lag_lead_cols Names of lag/lead columns
#' @return List with $biweekly_imp and $monthly_imp mids objects
run_multilevel_imputation <- function(data, lag_lead_cols) {
  message("\n=== Running Multilevel Panel Imputation (2l.pmm, long format) ===")
  message(sprintf("m = %d  |  maxit = %d  |  N rows (all waves) = %d",
                  N_IMPUTATIONS, N_ITERATIONS, nrow(data)))

  # ── Pass 1: Biweekly variables (all 6 waves) ─────────────────────────────
  message("\n--- Pass 1: Biweekly variables (wemwbs, waves 1-6) ---")

  biweekly_cols <- intersect(BIWEEKLY_VARS, names(data))
  biweekly_lag_lead <- grep(
    paste0("^(", paste(BIWEEKLY_VARS, collapse = "|"), ")_(lag|lead)[0-9]+$"),
    lag_lead_cols, value = TRUE
  )

  # Select columns relevant to this pass
  keep_bw <- unique(c("pid_int", "wave", biweekly_cols,
                       MICE_L2_NUMERIC, "gender_num", "region_num",
                       BIWEEKLY_GAMING_VARS, biweekly_lag_lead))
  keep_bw <- intersect(keep_bw, names(data))
  data_bw <- data[, keep_bw, drop = FALSE]

  pred_bw <- setup_multilevel_predictor_matrix(data_bw, biweekly_lag_lead)
  meth_bw <- rep("", ncol(data_bw)); names(meth_bw) <- names(data_bw)
  for (v in biweekly_cols) {
    if (any(is.na(data_bw[[v]]))) meth_bw[v] <- "2l.pmm"
  }
  message(sprintf("  2l.pmm targets: %s", paste(names(meth_bw[meth_bw == "2l.pmm"]),
                                                  collapse = ", ")))
  for (v in biweekly_cols) {
    n_miss <- sum(is.na(data_bw[[v]]))
    message(sprintf("  %s: %d cells to impute (%.1f%%)",
                    v, n_miss, 100 * n_miss / nrow(data_bw)))
  }

  imp_bw <- run_mice_parallel(data_bw, meth_bw, pred_bw, label = "biweekly")

  # ── Pass 2: Monthly variables (waves 2, 4, 6 only) ────────────────────────
  message("\n--- Pass 2: Monthly variables (PSQI, ESS, sleep duration, waves 2,4,6) ---")

  monthly_cols <- intersect(MONTHLY_VARS, names(data))
  derived_cols <- intersect(DERIVED_VARS, names(data))
  monthly_lag_lead <- grep(
    paste0("^(", paste(MONTHLY_VARS, collapse = "|"), ")_(lag|lead)[0-9]+$"),
    lag_lead_cols, value = TRUE
  )

  # Subset to waves 2,4,6 only (removes structural NAs)
  rows_monthly <- data$wave %in% MONTHLY_WAVES
  keep_mo <- unique(c("pid_int", "wave", monthly_cols, derived_cols,
                       MICE_L2_NUMERIC, "gender_num", "region_num",
                       BIWEEKLY_GAMING_VARS, monthly_lag_lead))
  keep_mo <- intersect(keep_mo, names(data))
  data_mo <- data[rows_monthly, keep_mo, drop = FALSE]

  pred_mo <- setup_multilevel_predictor_matrix(data_mo, monthly_lag_lead)
  meth_mo <- rep("", ncol(data_mo)); names(meth_mo) <- names(data_mo)
  for (v in monthly_cols) {
    if (any(is.na(data_mo[[v]]))) meth_mo[v] <- "2l.pmm"
  }

  # Passive imputation for psqi_global
  if ("psqi_global" %in% names(data_mo)) {
    component_cols <- intersect(PSQI_COMPONENT_VARS, names(data_mo))
    if (length(component_cols) == 7) {
      meth_mo["psqi_global"] <- paste0(
        "~I(", paste(component_cols, collapse = " + "), ")"
      )
    }
  }

  n_2l <- sum(meth_mo == "2l.pmm")
  n_passive <- sum(grepl("^~I\\(", meth_mo))
  message(sprintf("  2l.pmm targets: %d  |  passive: %d", n_2l, n_passive))
  for (v in monthly_cols) {
    n_miss <- sum(is.na(data_mo[[v]]))
    message(sprintf("  %s: %d cells to impute (%.1f%%)",
                    v, n_miss, 100 * n_miss / nrow(data_mo)))
  }

  imp_mo <- run_mice_parallel(data_mo, meth_mo, pred_mo, label = "monthly")

  list(biweekly = imp_bw, monthly = imp_mo)
}


#' Extract stacked long-format data from the two-pass multilevel results
#'
#' Combines imputed values from the biweekly pass (all waves) and the monthly
#' pass (waves 2,4,6 only), restores character pid / gender / region, and
#' joins back to the original data to restore non-mice columns.
#'
#' @param imp_result   List with $biweekly and $monthly mids objects
#' @param pid_lookup   tibble with pid / pid_int mapping
#' @param region_lookup tibble with region / region_num mapping
#' @param original_data original long-format data (pre-mice)
#' @return Stacked long-format data frame with .imp column
extract_multilevel_long <- function(imp_result, pid_lookup, region_lookup,
                                    original_data) {
  message("=== Extracting stacked long-format data from two-pass mids ===")

  imp_bw <- imp_result$biweekly
  imp_mo <- imp_result$monthly

  biweekly_vars <- intersect(BIWEEKLY_VARS, names(imp_bw$data))
  monthly_vars  <- intersect(c(MONTHLY_VARS, DERIVED_VARS), names(imp_mo$data))

  result_list <- lapply(seq_len(N_IMPUTATIONS), function(i) {
    # Extract biweekly imputed values
    bw_completed <- mice::complete(imp_bw, i) |>
      left_join(pid_lookup, by = "pid_int") |>
      select(pid, wave, all_of(biweekly_vars))

    # Extract monthly imputed values (only waves 2,4,6)
    mo_completed <- mice::complete(imp_mo, i) |>
      left_join(pid_lookup, by = "pid_int") |>
      select(pid, wave, all_of(monthly_vars))

    # Start from original data, replace imputed columns
    result <- original_data |>
      select(-any_of(c(biweekly_vars, monthly_vars))) |>
      left_join(bw_completed, by = c("pid", "wave")) |>
      left_join(mo_completed, by = c("pid", "wave")) |>
      mutate(.imp = i)

    result
  })

  result <- bind_rows(result_list)

  message(sprintf("  %d rows x %d cols  (%d imputations x %d person-waves)",
                  nrow(result), ncol(result),
                  N_IMPUTATIONS, nrow(result) / N_IMPUTATIONS))

  result
}


# ==============================================================================
# DIAGNOSTICS
# ==============================================================================

#' Diagnostic plots for the two-pass imputation
#'
#' @param imp_result List with $biweekly and $monthly mids objects
#' @param output_dir Directory to save plots
#' @param dropped_pids Character vector of excluded pids
#' @param date_inference_stats List with n_records and n_participants
#' @param date_diagnostics Data frame for date imputation reporting
create_diagnostics <- function(imp_result, output_dir = "output/imputation",
                               dropped_pids = character(),
                               date_inference_stats = NULL,
                               date_diagnostics = NULL) {
  message("\nCreating diagnostic plots...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Process each pass
  passes <- list(
    biweekly = imp_result$biweekly,
    monthly  = imp_result$monthly
  )

  all_cols_with_missing <- character()

  for (pass_name in names(passes)) {
    imp <- passes[[pass_name]]
    imputed_cols <- names(imp$method)[imp$method == "2l.pmm"]
    cols_missing <- imputed_cols[sapply(imputed_cols, function(col) {
      any(is.na(imp$data[[col]]))
    })]
    all_cols_with_missing <- c(all_cols_with_missing, cols_missing)

    # Convergence plots
    message(sprintf("  Creating convergence plots (%s)...", pass_name))
    pdf(file.path(output_dir, sprintf("convergence_%s.pdf", pass_name)),
        width = 12, height = 8)
    tryCatch({
      if (length(cols_missing) > 0) print(plot(imp, y = cols_missing))
    }, error = function(e) {
      message("    Warning: ", e$message)
    })
    dev.off()

    # Density plots
    message(sprintf("  Creating density plots (%s)...", pass_name))
    pdf(file.path(output_dir, sprintf("density_%s.pdf", pass_name)),
        width = 12, height = 10)
    for (col in cols_missing) {
      if (sum(!is.na(imp$data[[col]])) >= 10) {
        tryCatch(print(densityplot(imp, as.formula(paste("~", col)))),
                 error = function(e) NULL)
      }
    }
    dev.off()

    # Strip plots
    message(sprintf("  Creating strip plots (%s)...", pass_name))
    pdf(file.path(output_dir, sprintf("strip_%s.pdf", pass_name)),
        width = 12, height = 10)
    for (col in cols_missing) {
      tryCatch(print(stripplot(imp, as.formula(paste("~", col)))),
               error = function(e) NULL)
    }
    dev.off()
  }

  # Summary text (combined)
  message("  Creating imputation summary...")
  summary_file <- file.path(output_dir, "imputation_summary.txt")
  sink(summary_file)
  cat("=== Multilevel Multiple Imputation Summary (2l.pmm, two-pass) ===\n\n")
  cat(sprintf("Number of imputations (m): %d\n", N_IMPUTATIONS))
  cat(sprintf("Number of iterations per pass: %d\n\n", N_ITERATIONS))

  for (pass_name in names(passes)) {
    imp <- passes[[pass_name]]
    cat(sprintf("--- %s pass ---\n", toupper(pass_name)))
    cat(sprintf("Rows: %d | Participants: %d\n",
                nrow(imp$data), length(unique(imp$data$pid_int))))

    imputed_cols <- names(imp$method)[imp$method == "2l.pmm"]
    completed_list <- lapply(seq_len(imp$m), function(k) complete(imp, k))

    for (col in imputed_cols) {
      n_miss <- sum(is.na(imp$data[[col]]))
      pct_miss <- 100 * n_miss / nrow(imp$data)
      cat(sprintf("  %s: %d (%.1f%%) missing\n", col, n_miss, pct_miss))
    }

    derived <- names(imp$method)[grepl("^~I\\(", imp$method)]
    if (length(derived) > 0) {
      cat("  Passive: ")
      cat(paste(derived, collapse = ", "), "\n")
    }

    cat("\nObserved vs imputed:\n")
    for (col in imputed_cols) {
      missing_idx <- is.na(imp$data[[col]])
      observed_vals <- imp$data[[col]][!missing_idx]
      imputed_vals <- unlist(lapply(completed_list, function(df) df[[col]][missing_idx]))

      obs_mean <- mean(observed_vals, na.rm = TRUE)
      obs_sd <- sd(observed_vals, na.rm = TRUE)

      if (all(is.na(imputed_vals))) {
        cat(sprintf("  %s: observed = %.2f +/- %.2f, imputed = unavailable\n",
                    col, obs_mean, obs_sd))
      } else {
        imp_mean <- mean(imputed_vals, na.rm = TRUE)
        imp_sd <- sd(imputed_vals, na.rm = TRUE)
        cat(sprintf("  %s: obs = %.2f +/- %.2f, imp = %.2f +/- %.2f, diff = %.2f\n",
                    col, obs_mean, obs_sd, imp_mean, imp_sd, imp_mean - obs_mean))
      }
    }
    cat("\n")
  }

  if (length(dropped_pids) > 0) {
    cat("Participants excluded (missing WEMWBS at wave 1):\n")
    cat(sprintf("  %s\n", paste(dropped_pids, collapse = ", ")))
  }

  if (!is.null(date_inference_stats) &&
      date_inference_stats$n_records > 0) {
    cat(sprintf(
      "\nSurvey dates inferred: %d records (%d participants)\n",
      date_inference_stats$n_records,
      date_inference_stats$n_participants
    ))
  }
  sink()

  # Date imputation plots
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
        labs(title = "Survey Dates by Wave", x = "Wave",
             y = "Number of Records", fill = "Date Source") +
        theme_minimal()

      plot_hist <- date_diagnostics %>%
        filter(date_inferred_flag) %>%
        mutate(date_used = as.Date(date_used)) %>%
        ggplot(aes(x = date_used)) +
        geom_histogram(binwidth = 3, color = "white") +
        facet_wrap(~ wave, scales = "free_y") +
        labs(title = "Distribution of Inferred Survey Dates",
             x = "Date (UTC)", y = "Count") +
        theme_minimal()

      print(plot_counts)
      print(plot_hist)
    }, error = function(e) {
      message("    Warning: Could not create date imputation plot: ", e$message)
    })
    dev.off()
  }

  message(sprintf("  Diagnostic output saved to %s/", output_dir))
}


# ==============================================================================
# MAIN IMPUTATION FUNCTION
# ==============================================================================

#' Run full multilevel imputation pipeline
#'
#' @param input_path Path to self-report data
#' @param output_path Path to save mids object
#' @param n_imputations Number of imputations
#' @param n_iterations Number of iterations
#' @param create_diagnostics_flag Whether to create diagnostic plots
#' @param drop_wemwbs_w1_missing_flag Whether to drop participants missing wemwbs at wave 1
#' @param skip_preparation Whether to skip data preparation steps
#' @return mids object (invisibly)
run_imputation <- function(input_path = "data/processed/selfreport.csv.gz",
                           output_path = "data/processed/selfreport_imputed.rds",
                           n_imputations = N_IMPUTATIONS,
                           n_iterations = N_ITERATIONS,
                           create_diagnostics_flag = TRUE,
                           drop_wemwbs_w1_missing_flag = TRUE,
                           skip_preparation = FALSE) {

  message("=== Multilevel Multiple Imputation for Self-Report Variables (2l.pmm) ===\n")

  # 1. Load data
  message("Loading data...")
  if (!file.exists(input_path)) {
    stop(sprintf("Input file not found: %s\nRun preprocess_data.R first.", input_path))
  }
  data_long <- read_csv(input_path, show_col_types = FALSE)
  message(sprintf("  Loaded %d observations from %d participants",
                  nrow(data_long), n_distinct(data_long$pid)))

  data_long <- data_long %>%
    filter(wave %in% 1:6)

  dropped_pids <- character()
  date_inference_stats <- list(n_records = 0, n_participants = 0)

  if (!skip_preparation) {
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

    data_long <- expand_to_all_waves(data_long)

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

    data_long <- data_long %>%
      mutate(gender = categorize_gender(gender))

    data_long <- add_gaming_exposure(data_long, GAMING_SESSIONS_PATH)
  } else {
    message("  Skipping preparation steps (data already prepared)")
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

  # Date diagnostics for reporting
  date_diagnostics <- data_long %>%
    mutate(
      date_original = date,
      date_used = date_imputed
    ) %>%
    select(
      pid, wave,
      date_original, date_used,
      date_inferred_flag, wave_created_flag,
      all_of(intersect(BIWEEKLY_GAMING_VARS, names(data_long)))
    )

  # 2. Add temporal lag/lead predictors
  message(sprintf("\n=== Adding temporal lag/lead predictors (+/-%d waves) ===",
                  N_LAG_LEADS))
  data_with_lags <- add_temporal_lags(data_long, VARS_TO_IMPUTE, N_LAG_LEADS)

  lag_lead_cols <- grep("_(lag|lead)[0-9]+$", names(data_with_lags), value = TRUE)

  # 3. Prepare for mice (pid -> int, gender -> num, region -> num)
  message("\n=== Preparing data for multilevel mice ===")
  mice_prep     <- prepare_for_mice_multilevel(data_with_lags, lag_lead_cols)
  panel_mice    <- mice_prep$data
  pid_lookup    <- mice_prep$pid_lookup
  region_lookup <- mice_prep$region_lookup

  # 4. Run mice in two passes (biweekly: all waves; monthly: waves 2,4,6)
  imp_result <- run_multilevel_imputation(panel_mice, lag_lead_cols)

  # 5. Extract stacked long format and restore original columns
  imputed_long <- extract_multilevel_long(
    imp_result, pid_lookup, region_lookup, data_long
  )

  # 6. Save outputs
  message(sprintf("\nSaving imputed data..."))
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(imp_result, output_path)
  message(sprintf("  Saved: %s (list of mids objects)", output_path))

  long_path <- str_replace(output_path, "\\.rds$", "_long.rds")
  saveRDS(imputed_long, long_path)
  message(sprintf("  Saved: %s (long format, stacked)", long_path))

  # 7. Diagnostics
  if (create_diagnostics_flag) {
    create_diagnostics(
      imp_result,
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

  message("\n=== Imputation Complete ===")
  message(sprintf("Imputed datasets: %d (biweekly + monthly passes)",
                  N_IMPUTATIONS))
  message(sprintf("Iterations per pass: %d", N_ITERATIONS))

  return(invisible(imp_result))
}


# ==============================================================================
# UTILITY FUNCTIONS FOR ANALYSIS
# ==============================================================================

#' Load imputed data (list with $biweekly and $monthly mids objects)
load_imputed_mids <- function(path = "data/processed/selfreport_imputed.rds") {
  if (!file.exists(path)) {
    stop(sprintf("Imputed data not found: %s\nRun run_imputation() first.", path))
  }
  readRDS(path)
}

#' Load imputed data in long format
load_imputed_long <- function(path = "data/processed/selfreport_imputed_long.rds") {
  if (!file.exists(path)) {
    stop(sprintf("Imputed data not found: %s\nRun run_imputation() first.", path))
  }
  readRDS(path)
}

#' Pool results from models fit to imputed data
pool_results <- function(fits) {
  pool(fits)
}


# ==============================================================================
# RUN IMPUTATION (when script is sourced/executed)
# ==============================================================================

should_run_imputation <- Sys.getenv("RUN_IMPUTATION_ON_SOURCE", "1") == "1"

if (!interactive() && should_run_imputation) {
  args <- commandArgs(trailingOnly = TRUE)

  n_imp <- ifelse(length(args) >= 1, as.integer(args[1]), N_IMPUTATIONS)
  n_iter <- ifelse(length(args) >= 2, as.integer(args[2]), N_ITERATIONS)

  run_imputation(
    n_imputations = n_imp,
    n_iterations = n_iter,
    create_diagnostics_flag = TRUE
  )
}
