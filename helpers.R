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

  gamingBiweekly <- gamingBiweekly |>
    mutate(ln_biweekly_avg_minutes_played_10 = ln_biweekly_avg_minutes_played / 10)

  gamingMonthly <- gamingMonthly |>
    mutate(ln_monthly_avg_minutes_played_10 = ln_monthly_avg_minutes_played / 10)

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
  fmt <- paste0("%.", digits, "f (%.", digits, "f)")
  stats_total <- sprintf(fmt,
                        mean(master_data_total[[var_name]], na.rm = TRUE),
                        sd(master_data_total[[var_name]], na.rm = TRUE))
  stats_analytical <- sprintf(fmt,
                        mean(master_data[[var_name]], na.rm = TRUE),
                        sd(master_data[[var_name]], na.rm = TRUE))

  tibble(
    Characteristic = label,
    Total = stats_total,
    Analytical = stats_analytical
  )
}

#' Add continuous variable row to summary table (median, IQR)
#'
#' @param var_name Variable name in the data
#' @param label Display label for the table
#' @param digits Number of decimal places (default: 1)
#' @return Tibble with one row for the summary table
add_median_iqr <- function(var_name, label, digits = 1) {
  fmt <- paste0("%.", digits, "f (%.", digits, "f)")
  med_tot <- median(master_data_total[[var_name]], na.rm = TRUE)
  iqr_tot <- IQR(master_data_total[[var_name]], na.rm = TRUE)
  stats_total <- sprintf(fmt, med_tot, iqr_tot)

  med_ana <- median(master_data[[var_name]], na.rm = TRUE)
  iqr_ana <- IQR(master_data[[var_name]], na.rm = TRUE)
  stats_analytical <- sprintf(fmt, med_ana, iqr_ana)

  tibble(
    Characteristic = label,
    Total = stats_total,
    Analytical = stats_analytical
  )
}

#' Add categorical variable rows to summary table
#'
#' @param var_name Variable name in the data
#' @param levels_order Optional vector specifying the order of levels
#' @return Tibble with rows for each level of the categorical variable
add_categorical <- function(var_name, levels_order = NULL) {
  n_total <- nrow(master_data_total)
  n_analytical <- nrow(master_data)

  counts_tot <- master_data_total |>
    count(.data[[var_name]], name = "n_tot")
  counts_ana <- master_data |>
    count(.data[[var_name]], name = "n_ana")

  counts <- counts_tot |>
    full_join(counts_ana, by = var_name) |>
    mutate(n_tot = replace_na(n_tot, 0), n_ana = replace_na(n_ana, 0))

  if (!is.null(levels_order)) {
    counts <- counts |>
      filter(.data[[var_name]] %in% levels_order) |>
      arrange(match(.data[[var_name]], levels_order))
  }

  rows <- list()
  for (level in counts[[var_name]]) {
    row <- counts |> filter(.data[[var_name]] == level)
    fmt_tot <- sprintf("%d (%.1f%%)", row$n_tot, 100 * row$n_tot / n_total)
    fmt_ana <- sprintf("%d (%.1f%%)", row$n_ana, 100 * row$n_ana / n_analytical)

    rows[[length(rows) + 1]] <- tibble(
      Characteristic = paste0("    ", level),
      Total = fmt_tot,
      Analytical = fmt_ana
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

#' Recode sleep quality from text labels to ordered integer
#'
#' @param x Character vector of sleep quality labels
#' @return Integer vector (1 = Very poor, ..., 5 = Very good)
recode_sleep_quality <- function(x) {
  lvls <- c("Very poor", "Poor", "Fair", "Good", "Very good")
  out <- factor(x, levels = lvls, ordered = TRUE)
  as.integer(out)
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
get_h1_effect <- function(path = NULL, term, model = NULL, exponentiate = FALSE, label_if_exp = "OR", label_if_b = "b") {
  m <- model
  if (is.null(m)) {
    if (is.null(path) || !file.exists(path)) return("estimate unavailable")
    m <- readRDS(path)
  }
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
    sprintf("%s = %.3f, 95%% CI [%.3f, %.3f], %s", label, est, ci_low, ci_high, p_txt)
  } else {
    sprintf("%s = %.3f, 95%% CI [%.3f, %.3f]", label, est, ci_low, ci_high)
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
get_h2_interaction <- function(path = NULL, term, model = NULL, exponentiate = FALSE, label_if_exp = "OR", label_if_b = "b") {
  m <- model
  if (is.null(m)) {
    if (is.null(path) || !file.exists(path)) return("estimate unavailable")
    m <- readRDS(path)
  }

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
    sprintf("%s = %.3f, 95%% CI [%.3f, %.3f], %s", label, est, ci_low, ci_high, p_txt)
  } else {
    sprintf("%s = %.3f, 95%% CI [%.3f, %.3f]", label, est, ci_low, ci_high)
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

#' Get custom rows for regression tables (participant counts + ICC)
#'
#' @param models Named list of fitted models (lmer or clmm)
#' @return Data frame with both participant counts and ICC rows, positioned at start of GOF section
get_custom_rows <- function(models) {
  # Initialize data frames for N obs, participant counts, and ICC values
  nobs_df <- data.frame(term = "N Obs", stringsAsFactors = FALSE)
  participant_df <- data.frame(
    term = "N Participants",
    stringsAsFactors = FALSE
  )

  icc_df <- data.frame(
    term = "ICC",
    stringsAsFactors = FALSE
  )

  # Extract values for each model
  for (i in seq_along(models)) {
    model <- models[[i]]
    model_name <- names(models)[i]

    # Extract N observations based on model type
    n_obs <- tryCatch({
      if (inherits(model, "clmm")) {
        model$dims$nobs
      } else if (inherits(model, "clm")) {
        model$dims$nobs %||% nobs(model)
      } else if (inherits(model, "lmerMod")) {
        nobs(model)
      } else {
        NA_integer_
      }
    }, error = function(e) NA_integer_)
    nobs_df[[model_name]] <- as.character(n_obs[1])

    # Extract N participants based on model type
    n_part <- tryCatch({
      if (inherits(model, "clmm")) {
        nrow(ranef(model)$pid)
      } else if (inherits(model, "clm")) {
        stored <- attr(model, "n_participants_pid", exact = TRUE)
        if (!is.null(stored)) {
          stored
        } else if (!is.null(model$model) && "pid" %in% names(model$model)) {
          length(unique(model$model$pid))
        } else {
          NA_integer_
        }
      } else if (inherits(model, "lmerMod")) {
        lme4::ngrps(model, "pid")
      } else {
        NA_integer_
      }
    }, error = function(e) NA_integer_)

    # Ensure n_part is a scalar before converting to character
    participant_df[[model_name]] <- as.character(n_part[1])

    # Extract ICC (reuse existing logic from get_icc_rows)
    icc_value <- tryCatch({
      icc_result <- performance::icc(model)
      if (!is.null(icc_result$ICC_adjusted)) {
        sprintf("%.2f", icc_result$ICC_adjusted)
      } else if (!is.null(icc_result$ICC_conditional)) {
        sprintf("%.2f", icc_result$ICC_conditional)
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)

    # Ensure icc_value is a scalar
    icc_df[[model_name]] <- as.character(icc_value[1])
  }

  # Combine rows: N obs, participant count, then ICC
  combined <- rbind(nobs_df, participant_df, icc_df)

  return(combined)
}

# ==============================================================================
# Custom tidy/glance methods for clmm (ordinal package)
# Required for modelsummary to extract estimates with confidence intervals
# ==============================================================================

#' @export
tidy.clmm <- function(x, conf.int = TRUE, conf.level = 0.95, ...) {
  requireNamespace("ordinal", quietly = TRUE)

  # Fixed effects (thresholds + betas)
  coefs <- x$coefficients

  # Try vcov(); fall back to manual Wald SEs from Hessian if not positive definite
  vc <- tryCatch(vcov(x), error = function(e) NULL)
  if (!is.null(vc)) {
    vc_fixed <- vc[names(coefs), names(coefs), drop = FALSE]
    se <- sqrt(diag(vc_fixed))
  } else if (!is.null(x$Hessian)) {
    # Hessian is negative log-likelihood; invert -H for variance-covariance
    vc_full <- tryCatch(solve(-x$Hessian), error = function(e) {
      tryCatch(MASS::ginv(-x$Hessian), error = function(e2) NULL)
    })
    if (!is.null(vc_full)) {
      se_full <- sqrt(pmax(diag(vc_full), 0))
      names(se_full) <- colnames(x$Hessian)
      se <- se_full[names(coefs)]
    } else {
      se <- rep(NA_real_, length(coefs))
    }
  } else {
    se <- rep(NA_real_, length(coefs))
  }

  z_val <- coefs / se
  p_val <- 2 * pnorm(abs(z_val), lower.tail = FALSE)
  crit <- qnorm(1 - (1 - conf.level) / 2)

  fixed_df <- data.frame(
    term = names(coefs),
    estimate = unname(coefs),
    std.error = unname(se),
    statistic = unname(z_val),
    p.value = unname(p_val),
    conf.low = unname(coefs - crit * se),
    conf.high = unname(coefs + crit * se),
    conf.level = conf.level,
    effect = "fixed",
    group = "",
    stringsAsFactors = FALSE
  )

  # Random effects (SD of random intercepts/slopes)
  re_rows <- list()
  for (grp_name in names(x$ST)) {
    st_mat <- x$ST[[grp_name]]
    sd_vals <- diag(st_mat)
    re_names <- rownames(st_mat)
    for (j in seq_along(sd_vals)) {
      re_rows[[length(re_rows) + 1]] <- data.frame(
        term = paste0("SD (", re_names[j], " ", grp_name, ")"),
        estimate = sd_vals[j],
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        conf.level = conf.level,
        effect = "random",
        group = grp_name,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(re_rows) > 0) {
    re_df <- do.call(rbind, re_rows)
    rbind(fixed_df, re_df)
  } else {
    fixed_df
  }
}

#' @export
glance.clmm <- function(x, ...) {
  data.frame(
    AIC = AIC(x),
    BIC = BIC(x),
    logLik = as.numeric(logLik(x)),
    nobs = x$dims$nobs,
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# Rubin's Rules Infrastructure for Multiple Imputation
# ==============================================================================

#' Fit a model on each imputed dataset and pool via Rubin's rules
#'
#' @param imputed_long Data frame with .imp column (1..m)
#' @param prep_fn Function(data) -> data: prepares one imputed dataset for modelling
#' @param fit_fn Function(data) -> fitted model
#' @param parallel Logical, whether to use future_lapply (default: FALSE)
#' @return List with fits, mira, pooled
fit_on_imputations <- function(imputed_long, prep_fn, fit_fn, parallel = FALSE) {
  imp_ids <- sort(unique(imputed_long$.imp))
  m <- length(imp_ids)

  fit_one <- function(i) {
    d <- imputed_long[imputed_long$.imp == i, ]
    d$.imp <- NULL
    d <- prep_fn(d)
    tryCatch(fit_fn(d), error = function(e) {
      warning(sprintf("Imputation %d failed: %s", i, e$message))
      NULL
    })
  }

  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    fits <- future.apply::future_lapply(imp_ids, fit_one, future.seed = TRUE)
  } else {
    fits <- lapply(imp_ids, fit_one)
  }

  # Remove failed fits

  ok <- !vapply(fits, is.null, logical(1))
  if (sum(ok) == 0) stop("All imputation fits failed")
  if (sum(ok) < m) warning(sprintf("%d of %d fits failed", m - sum(ok), m))
  fits <- fits[ok]

  mira <- mice::as.mira(fits)

  # For clmm/clm models, mice::pool() cannot determine dfcom (complete-data
  # residual df) from glance(), resulting in near-zero df and absurdly wide CIs.
  # Fix: compute dfcom = nobs - n_fixed_params from the first fit.
  is_ordinal <- inherits(fits[[1]], "clmm") || inherits(fits[[1]], "clm")
  pool_args <- list(object = mira)
  if (is_ordinal) {
    nobs <- fits[[1]]$dims$nobs
    n_pars <- length(coef(fits[[1]]))
    pool_args$dfcom <- nobs - n_pars
  }

  pooled <- tryCatch(
    do.call(mice::pool, pool_args),
    error = function(e) {
      warning("mice::pool() failed: ", e$message)
      NULL
    }
  )

  list(fits = fits, mira = mira, pooled = pooled)
}

#' Compute pooled vcov matrix via Rubin's formula
#'
#' T = Ubar + (1 + 1/m) * B
#' where Ubar = mean of within-imputation vcov matrices,
#' B = between-imputation variance of coefficient estimates
#'
#' @param fits List of fitted models (lmer or clmm)
#' @return List with qbar (pooled coefficients) and vcov (pooled vcov matrix)
pool_vcov <- function(fits) {
  m <- length(fits)

  # Extract coefficients and vcov from each fit
  coef_list <- lapply(fits, function(f) {
    if (inherits(f, "clmm") || inherits(f, "clm")) {
      coef(f)
    } else {
      lme4::fixef(f)
    }
  })

  vcov_list <- lapply(fits, function(f) {
    as.matrix(vcov(f))
  })

  # Align on common parameter names (vcov may cover fewer params than coef for clmm)
  pnames <- Reduce(intersect, c(
    lapply(coef_list, names),
    lapply(vcov_list, rownames)
  ))

  coef_list <- lapply(coef_list, function(x) x[pnames])
  vcov_list <- lapply(vcov_list, function(x) x[pnames, pnames, drop = FALSE])

  # Pooled point estimates: Qbar = mean across imputations
  Q_mat <- do.call(rbind, coef_list)
  qbar <- colMeans(Q_mat)

  # Within-imputation variance: Ubar = mean of vcov matrices
  Ubar <- Reduce("+", vcov_list) / m

  # Between-imputation variance: B = var of coefficient estimates
  B <- var(Q_mat)  # m x p covariance matrix

  # Total variance: T = Ubar + (1 + 1/m) * B
  total_vcov <- Ubar + (1 + 1/m) * B

  list(qbar = qbar, vcov = total_vcov)
}

#' Extract and format a pooled effect from a Rubin's rules bundle
#'
#' @param bundle Output of fit_on_imputations() (must have $pooled)
#' @param term Parameter name to extract
#' @param exponentiate Logical, whether to exponentiate (default: FALSE)
#' @param label_if_exp Label to use if exponentiated (default: "OR")
#' @param label_if_b Label to use if not exponentiated (default: "b")
#' @return Formatted string with estimate, CI, and p-value
get_pooled_effect <- function(bundle, term, exponentiate = FALSE,
                               label_if_exp = "OR", label_if_b = "b") {
  if (is.null(bundle) || is.null(bundle$pooled)) {
    return("estimate unavailable")
  }

  s <- summary(bundle$pooled, conf.int = TRUE)
  row <- s[s$term == term, , drop = FALSE]

  if (nrow(row) == 0) {
    return("estimate unavailable")
  }

  est <- row$estimate
  ci_low <- row$`2.5 %`
  ci_high <- row$`97.5 %`
  p_val <- row$p.value

  if (isTRUE(exponentiate)) {
    est <- exp(est)
    ci_low <- exp(ci_low)
    ci_high <- exp(ci_high)
  }

  label <- if (isTRUE(exponentiate)) label_if_exp else label_if_b

  p_txt <- if (is.na(p_val)) {
    ""
  } else if (p_val < 0.001) {
    "p < .001"
  } else {
    sprintf("p = %.3f", p_val)
  }

  if (nzchar(p_txt)) {
    sprintf("%s = %.3f, 95%% CI [%.3f, %.3f], %s", label, est, ci_low, ci_high, p_txt)
  } else {
    sprintf("%s = %.3f, 95%% CI [%.3f, %.3f]", label, est, ci_low, ci_high)
  }
}

#' Get custom rows (N participants, ICC) for pooled model tables
#'
#' @param model_bundles Named list of fit_on_imputations() bundles
#' @return Data frame with participant counts and ICC rows
get_custom_rows_pooled <- function(model_bundles) {
  nobs_df <- data.frame(term = "N Obs", stringsAsFactors = FALSE)
  participant_df <- data.frame(term = "N Participants", stringsAsFactors = FALSE)
  icc_df <- data.frame(term = "ICC", stringsAsFactors = FALSE)

  for (i in seq_along(model_bundles)) {
    bundle <- model_bundles[[i]]
    model_name <- names(model_bundles)[i]
    first_fit <- bundle$fits[[1]]

    # N observations from first fit (identical across imputations)
    n_obs <- tryCatch({
      if (inherits(first_fit, "clmm")) {
        first_fit$dims$nobs
      } else if (inherits(first_fit, "lmerMod")) {
        nobs(first_fit)
      } else {
        NA_integer_
      }
    }, error = function(e) NA_integer_)
    nobs_df[[model_name]] <- as.character(n_obs[1])

    # N participants from first fit (identical across imputations)
    n_part <- tryCatch({
      if (inherits(first_fit, "clmm")) {
        nrow(ranef(first_fit)$pid)
      } else if (inherits(first_fit, "lmerMod")) {
        lme4::ngrps(first_fit, "pid")
      } else {
        NA_integer_
      }
    }, error = function(e) NA_integer_)
    participant_df[[model_name]] <- as.character(n_part[1])

    # ICC: average across fits. performance::icc() returns a scalar NA (atomic)
    # when it cannot compute variance components — e.g. on singular lmer fits
    # where (1|gender) has near-zero variance. The scalar NA must be handled
    # before the $ICC_adjusted accessor, or the $ operator raises on atomic
    # vectors and the outer tryCatch turns the entire column into NA. When
    # some but not all imputations yield a valid ICC, average over the valid
    # ones; when none do, return NA.
    icc_value <- tryCatch({
      icc_vals <- vapply(bundle$fits, function(f) {
        icc_result <- tryCatch(performance::icc(f), error = function(e) NA)
        if (!inherits(icc_result, "icc")) return(NA_real_)
        if (!is.null(icc_result$ICC_adjusted) &&
            !is.na(icc_result$ICC_adjusted)) {
          icc_result$ICC_adjusted
        } else if (!is.null(icc_result$ICC_conditional) &&
                   !is.na(icc_result$ICC_conditional)) {
          icc_result$ICC_conditional
        } else {
          NA_real_
        }
      }, numeric(1))
      if (all(is.na(icc_vals))) NA_character_
      else sprintf("%.2f", mean(icc_vals, na.rm = TRUE))
    }, error = function(e) NA_character_)
    icc_df[[model_name]] <- as.character(icc_value[1])
  }

  combined <- rbind(nobs_df, participant_df, icc_df)
  # No position attribute — rows will appear at the bottom of the table,
  # which is correct for pooled models that lack a GOF section.
  combined
}

#' Average a scalar quantity extracted from each fit
#'
#' @param fits List of fitted models
#' @param extract_fn Function that takes a model and returns a numeric scalar
#' @return Mean of extracted values across fits
average_re_quantity <- function(fits, extract_fn) {
  vals <- vapply(fits, function(f) {
    tryCatch(extract_fn(f), error = function(e) NA_real_)
  }, numeric(1))
  mean(vals, na.rm = TRUE)
}
