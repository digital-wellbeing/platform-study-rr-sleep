# imputation_diary.R
# Multiple imputation for diary-level missing data
# Using mice package with predictive mean matching (PMM)
#
# Expands each participant to a full 30-day grid, then imputes three
# outcome variables (life_sat, affective_valence, sleep_diary_quality_num)
# using daily predictors and wave-invariant covariates.
#
# Variables imputed:
#   - life_sat: Life satisfaction (continuous 1-100)
#   - affective_valence: Affective valence (continuous)
#   - sleep_diary_quality_num: Sleep quality (numeric 1-5, ordinal treated as numeric via PMM)
#
# Reference: van Buuren & Groothuis-Oudshoorn (2011)

library(tidyverse)
library(mice)
library(future)
library(future.apply)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

N_IMPUTATIONS <- 20
N_ITERATIONS  <- 20
SEED          <- 42
DIARY_DAYS    <- 1:30

# Outcome variables to impute
DIARY_OUTCOMES <- c("life_sat", "affective_valence", "sleep_diary_quality_num")

# Predictor variables (included in imputation model but only imputed where missing
# in observed data; on expanded days these are also NA and imputed by PMM)
DIARY_PREDICTORS <- c(
  "played24hr_num",
  "bpnsfs_1", "bpnsfs_2", "bpnsfs_3", "bpnsfs_4", "bpnsfs_5", "bpnsfs_6",
  "had_stress_num",
  "day_type_dayoff", "day_type_weekend", "day_type_holiday",
  "day_type_vacation", "day_type_other"
)

# Wave-invariant auxiliary variables (never imputed, used as predictors only)
DIARY_AUXILIARY <- c(
"age_scaled", "bmi_scaled", "SES_index_scaled",
  "chronotype_centered", "gender"
)

# All time-varying variables to reshape to wide
DIARY_TIMEVARYING <- c(DIARY_OUTCOMES, DIARY_PREDICTORS)

# Source data
DIARY_DATA_PATH   <- "data/raw/survey_daily.csv.gz"
INTAKE_DATA_PATH  <- "data/processed/intake_clean.csv.gz"
PANEL_DATA_PATH   <- "data/processed/selfreport.csv.gz"

# Output paths
DIARY_OUTPUT_DIR  <- "output/imputation_diary"
DIARY_MIDS_PATH   <- "data/processed/diary_imputed.rds"
DIARY_LONG_PATH   <- "data/processed/diary_imputed_long.rds"

# ==============================================================================
# FUNCTIONS
# ==============================================================================

#' Categorize gender into Male/Female/Other
#' (Mirrored from imputation.R for standalone use)
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
      day_type_dayoff   = as.integer(sleep_diary_day_type == "Regular day off"),
      day_type_weekend  = as.integer(sleep_diary_day_type == "Weekend"),
      day_type_holiday  = as.integer(sleep_diary_day_type == "Holiday"),
      day_type_vacation = as.integer(sleep_diary_day_type == "Vacation day"),
      day_type_other    = as.integer(str_detect(sleep_diary_day_type, "Other")),
      # Set all dummies to NA when day type is missing
      across(starts_with("day_type_"), ~if_else(is.na(sleep_diary_day_type), NA_integer_, .))
    )

  # --- Assign sequential day number per participant ---
  diary <- diary |>
    arrange(pid, diary_ts) |>
    group_by(pid) |>
    mutate(day_num = row_number()) |>
    ungroup()

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

  # --- Join covariates to diary ---
  diary_prepped <- diary |>
    select(pid, day_num, all_of(DIARY_OUTCOMES), all_of(DIARY_PREDICTORS)) |>
    left_join(covariate_lookup, by = "pid") |>
    left_join(chrono_lookup, by = "pid")

  message(sprintf("  Prepared data: %d rows, %d columns",
                  nrow(diary_prepped), ncol(diary_prepped)))

  diary_prepped
}


#' Expand diary data to a full 30-day grid per participant
#'
#' @param diary_data Prepared diary data with day_num column
#' @return Data frame with 30 rows per participant (unobserved days have NA)
expand_diary_grid <- function(diary_data) {
  message("=== Expanding to 30-day grid ===")

  all_pids <- unique(diary_data$pid)
  n_pids <- length(all_pids)

  # Create full grid
  full_grid <- expand_grid(
    pid = all_pids,
    day_num = DIARY_DAYS
  )

  # Left-join observed data
  expanded <- full_grid |>
    left_join(diary_data, by = c("pid", "day_num"))

  # Forward-fill wave-invariant covariates from observed data
  fill_static <- function(x) {
    idx <- which(!is.na(x))
    if (length(idx) == 0) return(x)
    replace(x, is.na(x), x[min(idx)])
  }

  expanded <- expanded |>
    group_by(pid) |>
    mutate(across(all_of(DIARY_AUXILIARY), fill_static)) |>
    ungroup()

  n_observed <- sum(!is.na(expanded$life_sat))
  n_total <- nrow(expanded)
  message(sprintf("  Grid: %d participants × 30 days = %d rows", n_pids, n_total))
  message(sprintf("  Observed: %d (%.1f%%), Structural NA: %d (%.1f%%)",
                  n_observed, 100 * n_observed / n_total,
                  n_total - n_observed, 100 * (1 - n_observed / n_total)))

  expanded
}


#' Reshape diary data to wide format for imputation
#'
#' Each time-varying variable becomes {varname}_d1, ..., {varname}_d30.
#' Auxiliary covariates remain as single columns.
#'
#' @param diary_expanded Expanded 30-day grid data
#' @return Data frame in wide format (1 row per participant)
reshape_diary_wide <- function(diary_expanded) {
  message("=== Reshaping to wide format ===")

  # Pivot time-varying variables wide
  wide <- diary_expanded |>
    select(pid, day_num, all_of(DIARY_TIMEVARYING)) |>
    pivot_wider(
      id_cols = pid,
      names_from = day_num,
      values_from = all_of(DIARY_TIMEVARYING),
      names_glue = "{.value}_d{day_num}"
    )

  # Get wave-invariant covariates (one value per pid)
  auxiliary <- diary_expanded |>
    select(pid, all_of(DIARY_AUXILIARY)) |>
    group_by(pid) |>
    summarise(across(everything(), ~first(na.omit(.))), .groups = "drop")

  # Combine
  data_wide <- wide |>
    left_join(auxiliary, by = "pid")

  message(sprintf("  Wide format: %d participants, %d variables",
                  nrow(data_wide), ncol(data_wide) - 1))

  data_wide
}


#' Set up predictor matrix for diary imputation
#'
#' @param data Wide-format diary data
#' @return Predictor matrix for mice
setup_diary_predictor_matrix <- function(data) {
  message("Setting up predictor matrix...")

  # Use quickpred with mincor threshold
  pred <- quickpred(data, mincor = 0.1, exclude = "pid")

  # Ensure pid is never used as predictor or imputed
  pred["pid", ] <- 0
  if ("pid" %in% colnames(pred)) pred[, "pid"] <- 0

  # Summary
  n_predictors <- rowSums(pred > 0)
  message(sprintf("  Average predictors per variable: %.1f", mean(n_predictors)))

  pred
}


#' Set up imputation methods for diary data
#'
#' PMM for all outcome columns (guarantees valid values for ordinal sleep quality).
#' PMM for predictor columns that have missingness on expanded days.
#' Empty string for auxiliary (wave-invariant) columns.
#'
#' @param data Wide-format diary data
#' @return Named vector of imputation methods
setup_diary_methods <- function(data) {
  message("Setting up imputation methods...")

  methods <- rep("", ncol(data))
  names(methods) <- names(data)

  # All time-varying columns get PMM
  timevarying_cols <- grep(
    paste0("^(", paste(DIARY_TIMEVARYING, collapse = "|"), ")_d[0-9]+$"),
    names(data), value = TRUE
  )

  # Only set PMM where there is actual missingness
  for (col in timevarying_cols) {
    if (any(is.na(data[[col]]))) {
      methods[col] <- "pmm"
    }
  }

  n_pmm <- sum(methods == "pmm")
  n_auxiliary <- length(intersect(DIARY_AUXILIARY, names(data)))
  message(sprintf("  Variables to impute (PMM): %d", n_pmm))
  message(sprintf("  Auxiliary predictors (not imputed): %d", n_auxiliary))

  methods
}


#' Run diary imputation using chunked parallel approach
#'
#' @param data Wide-format diary data
#' @return mids object
run_diary_imputation <- function(data) {
  n_cores <- availableCores()
  message(sprintf("\n=== Running Diary Multiple Imputation (Chunked) ==="))
  message(sprintf("Available CPU cores: %d", n_cores))
  message(sprintf("Number of imputations (m): %d", N_IMPUTATIONS))
  message(sprintf("Number of iterations: %d", N_ITERATIONS))

  # Limit workers to avoid memory exhaustion (each worker copies the full data)
  n_workers <- min(4, n_cores, N_IMPUTATIONS)
  imps_per_chunk <- ceiling(N_IMPUTATIONS / n_workers)

  message(sprintf("Running %d parallel jobs with ~%d imputations each",
                  n_workers, imps_per_chunk))

  # Set up predictor matrix and methods
  pred <- setup_diary_predictor_matrix(data)
  methods <- setup_diary_methods(data)

  # Report missingness for outcome columns
  message("\nMissingness summary for outcome variables:")
  for (var in DIARY_OUTCOMES) {
    outcome_cols <- grep(paste0("^", var, "_d[0-9]+$"), names(data), value = TRUE)
    for (col in outcome_cols) {
      n_miss <- sum(is.na(data[[col]]))
      pct_miss <- 100 * n_miss / nrow(data)
      if (n_miss > 0 && pct_miss > 50) next  # Skip reporting structural NA days
    }
    # Report aggregate
    all_cols <- grep(paste0("^", var, "_d[0-9]+$"), names(data), value = TRUE)
    total_cells <- length(all_cols) * nrow(data)
    total_miss <- sum(sapply(all_cols, function(col) sum(is.na(data[[col]]))))
    message(sprintf("  %s: %d/%d cells missing (%.1f%%)",
                    var, total_miss, total_cells, 100 * total_miss / total_cells))
  }

  # Set up parallel plan
  plan(multisession, workers = n_workers)

  message("\nStarting parallel imputation...")
  start_time <- Sys.time()

  set.seed(SEED)
  chunk_seeds <- sample.int(1e7, n_workers)

  chunk_sizes <- rep(imps_per_chunk, n_workers)
  total_assigned <- sum(chunk_sizes)
  if (total_assigned > N_IMPUTATIONS) {
    chunk_sizes[n_workers] <- chunk_sizes[n_workers] - (total_assigned - N_IMPUTATIONS)
  }

  imp_list <- future_lapply(seq_len(n_workers), function(i) {
    mice(
      data = data,
      m = chunk_sizes[i],
      method = methods,
      predictorMatrix = pred,
      maxit = N_ITERATIONS,
      seed = chunk_seeds[i],
      printFlag = FALSE
    )
  }, future.seed = TRUE)

  # Combine results
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

  plan(sequential)

  imp_combined
}


#' Reshape imputed wide data back to long format
#'
#' @param imp mids object from mice
#' @return Data frame in long format with .imp column (stacked imputations)
reshape_diary_to_long <- function(imp) {
  message("=== Reshaping imputed data to long format ===")

  long_list <- lapply(seq_len(imp$m), function(i) {
    completed <- complete(imp, i)

    # Identify time-varying columns
    tv_cols <- grep(
      paste0("^(", paste(DIARY_TIMEVARYING, collapse = "|"), ")_d[0-9]+$"),
      names(completed), value = TRUE
    )

    # Pivot longer
    long <- completed |>
      select(pid, all_of(tv_cols)) |>
      pivot_longer(
        cols = all_of(tv_cols),
        names_to = c(".value", "day_num"),
        names_pattern = "(.+)_d([0-9]+)"
      ) |>
      mutate(day_num = as.integer(day_num))

    # Add auxiliary covariates back
    aux <- completed |>
      select(pid, all_of(intersect(DIARY_AUXILIARY, names(completed))))

    long <- long |>
      left_join(aux, by = "pid") |>
      mutate(.imp = i)

    long
  })

  result <- bind_rows(long_list)

  message(sprintf("  Long format: %d rows (%d participants × %d days × %d imputations)",
                  nrow(result),
                  n_distinct(result$pid),
                  max(result$day_num),
                  imp$m))

  result
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

  # Get columns with missingness
  all_cols <- names(imp$data)[imp$method != ""]
  cols_with_missing <- all_cols[sapply(all_cols, function(col) {
    any(is.na(imp$data[[col]]))
  })]

  # Limit diagnostic plots to a representative subset (first 5 days per outcome)
  outcome_diagnostic_cols <- unlist(lapply(DIARY_OUTCOMES, function(var) {
    cols <- grep(paste0("^", var, "_d[1-5]$"), cols_with_missing, value = TRUE)
    head(cols, 5)
  }))

  # 1. Convergence plots
  message("  Creating convergence plots...")
  pdf(file.path(output_dir, "convergence_plots.pdf"), width = 12, height = 8)
  tryCatch({
    print(plot(imp, outcome_diagnostic_cols))
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
  cat("=== Diary Multiple Imputation Summary ===\n\n")
  cat(sprintf("Number of imputations (m): %d\n", imp$m))
  cat(sprintf("Number of iterations: %d\n", imp$iteration))
  cat(sprintf("Number of participants: %d\n\n", nrow(imp$data)))

  cat("Outcome variables imputed:\n")
  for (var in DIARY_OUTCOMES) {
    outcome_cols <- grep(paste0("^", var, "_d[0-9]+$"),
                         cols_with_missing, value = TRUE)
    if (length(outcome_cols) > 0) {
      total_cells <- length(outcome_cols) * nrow(imp$data)
      total_miss <- sum(sapply(outcome_cols, function(col) sum(is.na(imp$data[[col]]))))
      cat(sprintf("  %s: %d/%d cells (%.1f%%) missing across %d day-columns\n",
                  var, total_miss, total_cells, 100 * total_miss / total_cells,
                  length(outcome_cols)))
    }
  }

  cat("\nPredictor variables imputed:\n")
  for (var in DIARY_PREDICTORS) {
    pred_cols <- grep(paste0("^", var, "_d[0-9]+$"),
                      cols_with_missing, value = TRUE)
    if (length(pred_cols) > 0) {
      total_cells <- length(pred_cols) * nrow(imp$data)
      total_miss <- sum(sapply(pred_cols, function(col) sum(is.na(imp$data[[col]]))))
      cat(sprintf("  %s: %d/%d cells (%.1f%%) missing\n",
                  var, total_miss, total_cells, 100 * total_miss / total_cells))
    }
  }
  sink()

  message(sprintf("  Diagnostics saved to %s/", output_dir))
}


# ==============================================================================
# MAIN
# ==============================================================================

main <- function() {
  message("\n", paste(rep("=", 60), collapse = ""))
  message("  DIARY MULTIPLE IMPUTATION")
  message(paste(rep("=", 60), collapse = ""), "\n")

  # Step 1-2: Prepare diary data
  diary_prepped <- prepare_diary_for_imputation()

  # Step 3: Expand to 30-day grid
  diary_expanded <- expand_diary_grid(diary_prepped)

  # Step 4: Reshape to wide format
  diary_wide <- reshape_diary_wide(diary_expanded)

  # Step 5-6: Run MICE imputation
  imp <- run_diary_imputation(diary_wide)

  # Step 7: Reshape back to long + add flags
  diary_long <- reshape_diary_to_long(imp)
  diary_long <- add_imputed_flags(diary_long, diary_prepped)

  # Step 8: Save outputs
  message("\n=== Saving outputs ===")
  dir.create(dirname(DIARY_MIDS_PATH), showWarnings = FALSE, recursive = TRUE)
  saveRDS(imp, DIARY_MIDS_PATH)
  message(sprintf("  mids object saved to %s", DIARY_MIDS_PATH))

  saveRDS(diary_long, DIARY_LONG_PATH)
  message(sprintf("  Long-format data saved to %s", DIARY_LONG_PATH))
  message(sprintf("  Dimensions: %d rows × %d columns", nrow(diary_long), ncol(diary_long)))

  # Step 9: Diagnostics
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
