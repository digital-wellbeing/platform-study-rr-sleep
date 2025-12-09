# Helper Functions for Late-Night Gaming, Sleep and Wellbeing Analysis
# This file contains all helper functions used in the manuscript.qmd analysis

# ==============================================================================
# Data Filtering and Preparation
# ==============================================================================

#' Filter data by outcome variable version (imputed vs complete case)
#'
#' @param df Data frame to filter
#' @param outcome_vars Character vector of outcome variable names
#' @param version Either "imputed" or "completecase"
#' @return Filtered data frame with .version column added
filter_by_outcome <- function(df, outcome_vars, version = c("imputed", "completecase")) {
  version <- match.arg(version)

  if (version == "imputed") {
    # Use all data (imputed values included)
    df$.version <- "imputed"
    return(df)
  } else if (version == "completecase") {
    # Complete case: filter out rows where any of the outcome variables were imputed
    flag_vars <- paste0(outcome_vars, "_imputed_flag")

    # Check which flag columns exist
    existing_flags <- flag_vars[flag_vars %in% names(df)]

    if (length(existing_flags) > 0) {
      # Keep only rows where ALL specified outcome flags are FALSE (not imputed)
      for (flag in existing_flags) {
        df <- df[!df[[flag]] | is.na(df[[flag]]), ]
      }
    }

    df$.version <- "completecase"
    return(df)
  }
}

#' Create version-specific datasets for each outcome
#'
#' @param df Data frame to split
#' @param outcome_var Outcome variable name
#' @return List with imputed and complete-case versions filtered by that outcome
create_outcome_versions <- function(df, outcome_var) {
  list(
    imputed = filter_by_outcome(df, outcome_var, "imputed"),
    completecase = filter_by_outcome(df, outcome_var, "completecase")
  )
}

#' Ensure monthly regressions only draw from waves 2, 4, and 6
#'
#' @param df Data frame to filter
#' @param waves Numeric vector of wave numbers to keep (default: c(2, 4, 6))
#' @return Filtered data frame
enforce_monthly_wave_subset <- function(df, waves = c(2, 4, 6)) {
  if ("wave" %in% names(df)) {
    df <- df |>
      filter(wave %in% waves)
  } else if ("month" %in% names(df)) {
    df <- df |>
      filter(month %in% waves)
  }

  df
}

#' Build gaming input datasets for analysis
#'
#' @param selfreport_data Self-report data frame
#' @return List with gamingBiweekly and gamingMonthly datasets
build_gaming_inputs <- function(selfreport_data) {
  # Calculate isWeekend based on survey completion date if not already present
  # Friday and Saturday are coded as weekend (following original logic)
  if (!"isWeekend" %in% names(selfreport_data)) {
    selfreport_data <- selfreport_data |>
      mutate(
        # wday: 1=Sunday, 2=Monday, ..., 6=Friday, 7=Saturday
        isWeekend = ifelse(wday(date) %in% c(6, 7), 1, 0)
      )
  }

  # Gaming exposures are already calculated in selfreport_data
  # Just filter and prepare for analysis
  gamingBiweekly <- selfreport_data |>
    arrange(as.integer(pid), wave) |>
    dplyr::select(pid, wave, msf_sc_centered, everything())

  gamingMonthly <- selfreport_data |>
    filter(wave %in% c(2, 4, 6)) |>
    rename(month = wave) |>
    arrange(as.integer(pid), month)

  list(
    gamingBiweekly = gamingBiweekly,
    gamingMonthly = gamingMonthly
  )
}

# ==============================================================================
# Table Building Functions
# ==============================================================================

#' Add continuous variable row to summary table (mean, SD)
#'
#' @param var_name Variable name in the data
#' @param label Display label for the table
#' @param digits Number of decimal places (default: 1)
#' @return Tibble with one row for the summary table
add_continuous <- function(var_name, label, digits = 1) {
  stats_total <- sprintf(paste0("%.", digits, "f (%.1f)"),
                        mean(master_data[[var_name]], na.rm = TRUE),
                        sd(master_data[[var_name]], na.rm = TRUE))

  tibble(
    Characteristic = label,
    Total = stats_total
  )
}

#' Add continuous variable row to summary table (median, IQR)
#'
#' @param var_name Variable name in the data
#' @param label Display label for the table
#' @param digits Number of decimal places (default: 1)
#' @return Tibble with one row for the summary table
add_median_iqr <- function(var_name, label, digits = 1) {
  med_total <- median(master_data[[var_name]], na.rm = TRUE)
  iqr_total <- IQR(master_data[[var_name]], na.rm = TRUE)
  stats_total <- sprintf(paste0("%.", digits, "f (%.1f)"), med_total, iqr_total)

  tibble(
    Characteristic = label,
    Total = stats_total
  )
}

#' Add categorical variable rows to summary table
#'
#' @param var_name Variable name in the data
#' @param levels_order Optional vector specifying the order of levels
#' @return Tibble with rows for each level of the categorical variable
add_categorical <- function(var_name, levels_order = NULL) {
  total_n <- nrow(master_data)

  # Get counts by category
  counts_total <- master_data |>
    count(.data[[var_name]], name = "n_total")

  # Order levels if specified
  if (!is.null(levels_order)) {
    counts_total <- counts_total |>
      filter(.data[[var_name]] %in% levels_order) |>
      arrange(match(.data[[var_name]], levels_order))
  }

  # Build rows for each level
  rows <- list()
  for (level in counts_total[[var_name]]) {
    n_tot <- counts_total |> filter(.data[[var_name]] == level) |> pull(n_total)
    pct_tot <- sprintf("%d (%.1f%%)", n_tot, 100 * n_tot / total_n)

    rows[[length(rows) + 1]] <- tibble(
      Characteristic = paste0("    ", level),
      Total = pct_tot
    )
  }

  bind_rows(rows)
}

# ==============================================================================
# Formatting Functions
# ==============================================================================

#' Convert decimal hours to HH:MM format
#'
#' @param h Numeric hours value
#' @return Character string in HH:MM format
hours_to_hhmm <- function(h) {
  hrs <- floor(h)
  mins <- round((h - hrs) * 60)
  sprintf("%02d:%02d", hrs, mins)
}

# ==============================================================================
# H1 Model Functions
# ==============================================================================

#' Extract and format effect from H1 model
#'
#' @param path Path to saved model RDS file
#' @param term Parameter name to extract
#' @param exponentiate Logical, whether to exponentiate (default: FALSE)
#' @param label_if_exp Label to use if exponentiated (default: "OR")
#' @param label_if_b Label to use if not exponentiated (default: "b")
#' @return Formatted string with estimate, CI, and p-value
get_h1_effect <- function(path, term, exponentiate = FALSE, label_if_exp = "OR", label_if_b = "b") {
  if (!file.exists(path)) {
    return("estimate unavailable")
  }
  m <- readRDS(path)
  mp <- parameters::model_parameters(m, exponentiate = exponentiate)
  row <- mp[mp$Parameter == term, , drop = FALSE]
  if (nrow(row) == 0) {
    return("estimate unavailable")
  }

  # Try to identify common column names used by parameters::model_parameters()
  est_col <- intersect(c("Coefficient", "Estimate", "Est."), colnames(row))[1]
  ci_low_col <- intersect(c("CI_low", "CI_low_95", "CI_low_0.95"), colnames(row))[1]
  ci_high_col <- intersect(c("CI_high", "CI_high_95", "CI_high_0.95"), colnames(row))[1]
  p_col <- intersect(c("p", "p_value", "p.value"), colnames(row))[1]

  if (is.na(est_col) || is.na(ci_low_col) || is.na(ci_high_col)) {
    return("estimate unavailable")
  }

  est <- as.numeric(row[[est_col]])
  ci_low <- as.numeric(row[[ci_low_col]])
  ci_high <- as.numeric(row[[ci_high_col]])
  p_val <- if (!is.na(p_col)) as.numeric(row[[p_col]]) else NA_real_

  p_txt <- if (is.na(p_val)) {
    ""
  } else if (p_val < 0.001) {
    "p < .001"
  } else {
    sprintf("p = %.3f", p_val)
  }

  label <- if (isTRUE(exponentiate)) label_if_exp else label_if_b

  if (nzchar(p_txt)) {
    sprintf("%s = %.2f, 95%% CI [%.2f, %.2f], %s", label, est, ci_low, ci_high, p_txt)
  } else {
    sprintf("%s = %.2f, 95%% CI [%.2f, %.2f]", label, est, ci_low, ci_high)
  }
}

#' Get file paths for H1 models
#'
#' @param version_label Version label (e.g., "imputed", "completecase")
#' @return Named list of model file paths
get_h1_model_paths <- function(version_label) {
  list(
    `H1a: Sleep Quality` = glue("output/models/{version_label}_model_h1a.rds"),
    `H1b: Sleep Duration` = glue("output/models/{version_label}_model_h1b.rds"),
    `H1c: Wellbeing` = glue("output/models/{version_label}_model_h1c.rds"),
    `H1d: Daytime Sleepiness` = glue("output/models/{version_label}_model_h1d.rds")
  )
}

#' Load H1 models from disk
#'
#' @param version_label Version label (e.g., "imputed", "completecase")
#' @return List of loaded models or NULL if any are missing
load_h1_models_from_disk <- function(version_label) {
  paths <- get_h1_model_paths(version_label)
  paths_vec <- unlist(paths, use.names = TRUE)
  missing <- names(paths_vec)[!file.exists(paths_vec)]

  if (length(missing) > 0) {
    message(
      sprintf(
        "Missing cached H1 models for %s (%s). Run with `-P refit_h1:true` to regenerate.",
        version_label,
        paste(basename(paths_vec[missing]), collapse = ", ")
      )
    )
    return(NULL)
  }

  lapply(paths, readRDS)
}

# ==============================================================================
# H2 Model Functions
# ==============================================================================

#' Extract and format interaction effect from H2 model
#'
#' @param path Path to saved model RDS file
#' @param term Parameter name to extract
#' @param exponentiate Logical, whether to exponentiate (default: FALSE)
#' @param label_if_exp Label to use if exponentiated (default: "OR")
#' @param label_if_b Label to use if not exponentiated (default: "b")
#' @return Formatted string with estimate, CI, and p-value
get_h2_interaction <- function(path, term, exponentiate = FALSE, label_if_exp = "OR", label_if_b = "b") {
  if (!file.exists(path)) {
    return("estimate unavailable")
  }
  m <- readRDS(path)
  mp <- parameters::model_parameters(m, exponentiate = exponentiate)
  row <- mp[mp$Parameter == term, , drop = FALSE]
  if (nrow(row) == 0) {
    return("estimate unavailable")
  }

  est_col <- intersect(c("Coefficient", "Estimate", "Est."), colnames(row))[1]
  ci_low_col <- intersect(c("CI_low", "CI_low_95", "CI_low_0.95"), colnames(row))[1]
  ci_high_col <- intersect(c("CI_high", "CI_high_95", "CI_high_0.95"), colnames(row))[1]
  p_col <- intersect(c("p", "p_value", "p.value"), colnames(row))[1]

  if (is.na(est_col) || is.na(ci_low_col) || is.na(ci_high_col)) {
    return("estimate unavailable")
  }

  est <- as.numeric(row[[est_col]])
  ci_low <- as.numeric(row[[ci_low_col]])
  ci_high <- as.numeric(row[[ci_high_col]])
  p_val <- if (!is.na(p_col)) as.numeric(row[[p_col]]) else NA_real_

  p_txt <- if (is.na(p_val)) {
    ""
  } else if (p_val < 0.001) {
    "p < .001"
  } else {
    sprintf("p = %.3f", p_val)
  }

  label <- if (isTRUE(exponentiate)) label_if_exp else label_if_b

  if (nzchar(p_txt)) {
    sprintf("%s = %.2f, 95%% CI [%.2f, %.2f], %s", label, est, ci_low, ci_high, p_txt)
  } else {
    sprintf("%s = %.2f, 95%% CI [%.2f, %.2f]", label, est, ci_low, ci_high)
  }
}

#' Get file paths for H2 models
#'
#' @param version_label Version label (e.g., "imputed", "completecase")
#' @return Named list of model file paths
get_h2_model_paths <- function(version_label) {
  list(
    `H2a: Sleep Quality` = glue("output/models/{version_label}_model_h2a.rds"),
    `H2b: Sleep Duration` = glue("output/models/{version_label}_model_h2b.rds"),
    `H2c: Wellbeing` = glue("output/models/{version_label}_model_h2c.rds"),
    `H2d: Daytime Sleepiness` = glue("output/models/{version_label}_model_h2d.rds")
  )
}

#' Load H2 models from disk
#'
#' @param version_label Version label (e.g., "imputed", "completecase")
#' @return List of loaded models or NULL if any are missing
load_h2_models_from_disk <- function(version_label) {
  paths <- get_h2_model_paths(version_label)
  paths_vec <- unlist(paths, use.names = TRUE)
  missing <- names(paths_vec)[!file.exists(paths_vec)]

  if (length(missing) > 0) {
    message(
      sprintf(
        "Missing cached H2 models for %s (%s). Run with `-P refit_h2:true` to regenerate.",
        version_label,
        paste(basename(paths_vec[missing]), collapse = ", ")
      )
    )
    return(NULL)
  }

  lapply(paths, readRDS)
}

# ==============================================================================
# Goodness-of-Fit Statistics Functions
# ==============================================================================

#' Calculate ICC values for a list of models using performance::icc()
#'
#' @param models Named list of fitted models
#' @return Data frame formatted for use with modelsummary's add_rows parameter
get_icc_rows <- function(models) {
  icc_values <- lapply(models, function(model) {
    tryCatch({
      icc_result <- performance::icc(model)
      # Extract adjusted ICC if available, otherwise use ICC_conditional
      if (!is.null(icc_result$ICC_adjusted)) {
        sprintf("%.2f", icc_result$ICC_adjusted)
      } else if (!is.null(icc_result$ICC_conditional)) {
        sprintf("%.2f", icc_result$ICC_conditional)
      } else {
        NA_character_
      }
    }, error = function(e) {
      NA_character_
    })
  })

  # Create a data frame in the format modelsummary expects
  icc_df <- data.frame(
    term = "ICC",
    stringsAsFactors = FALSE
  )

  for (i in seq_along(models)) {
    icc_df[[names(models)[i]]] <- icc_values[[i]]
  }

  icc_df
}
