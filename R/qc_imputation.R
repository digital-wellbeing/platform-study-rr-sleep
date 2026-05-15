# qc_imputation.R
# Quality control diagnostics for panel and diary multilevel imputations
# Loads saved mids objects and generates publication-quality QC plots in
# output/qc_imputation/panel/ and output/qc_imputation/diary/
#
# Plots produced (van Buuren 2018, Ch. 4 & 6):
#   - Convergence trace plots (mean/SD across iterations)
#   - Density comparison (observed vs imputed)
#   - Strip plots (imputed vs observed per imputation)
#   - Proportion of missing data by variable
#   - Between-imputation variability (boxplots of imputed means across m)
#   - Observed vs imputed summary statistics (text)

library(tidyverse)
library(mice)

QC_OUTPUT_DIR <- "output/qc_imputation"
PANEL_MIDS_PATH <- "data/processed/selfreport_imputed.rds"
DIARY_MIDS_PATH <- "data/processed/diary_imputed.rds"

# ==============================================================================
# HELPERS
# ==============================================================================

#' Get imputed variable names with missingness from a mids object
get_imputed_cols_with_missing <- function(imp) {
  imputed_cols <- names(imp$method)[imp$method %in% c("2l.pmm", "pmm")]
  if (length(imputed_cols) == 0) return(character(0))
  keep <- vapply(imputed_cols, function(col) {
    col %in% names(imp$data) && any(is.na(imp$data[[col]]))
  }, logical(1))
  imputed_cols[keep]
}

#' Create QC plots for a single mids object
#'
#' @param imp mids object
#' @param output_dir Directory to save plots (e.g. output/qc_imputation/panel/)
#' @param label Short label for plot titles (e.g. "panel_biweekly")
create_qc_plots <- function(imp, output_dir, label = "imputation") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  cols_missing <- get_imputed_cols_with_missing(imp)

  if (length(cols_missing) == 0) {
    message(sprintf("  [%s] No imputed variables with missingness, skipping QC plots", label))
    return(invisible(NULL))
  }

  # 1. Convergence trace plots (van Buuren Sec 4.5.6)
  message(sprintf("  [%s] Convergence plots...", label))
  pdf(file.path(output_dir, "convergence.pdf"), width = 12, height = 8)
  tryCatch({
    if (length(cols_missing) > 0) print(plot(imp, y = cols_missing))
  }, error = function(e) message("    Warning: ", e$message))
  dev.off()

  # 2. Density comparison (observed vs imputed)
  message(sprintf("  [%s] Density plots...", label))
  pdf(file.path(output_dir, "density.pdf"), width = 12, height = 10)
  for (col in cols_missing) {
    if (sum(!is.na(imp$data[[col]])) >= 10) {
      tryCatch({
        print(densityplot(imp, as.formula(paste("~", col))))
      }, error = function(e) NULL)
    }
  }
  dev.off()

  # 3. Strip plots (imputed vs observed per imputation)
  message(sprintf("  [%s] Strip plots...", label))
  pdf(file.path(output_dir, "strip.pdf"), width = 12, height = 10)
  for (col in cols_missing) {
    tryCatch({
      print(stripplot(imp, as.formula(paste("~", col))))
    }, error = function(e) NULL)
  }
  dev.off()

  # 4. Proportion of missing data by variable (bar chart)
  message(sprintf("  [%s] Missingness bar chart...", label))
  miss_pct <- sapply(cols_missing, function(col) {
    100 * sum(is.na(imp$data[[col]])) / nrow(imp$data)
  })
  miss_df <- tibble(variable = names(miss_pct), pct_missing = miss_pct)

  p_miss <- ggplot(miss_df, aes(x = reorder(variable, pct_missing), y = pct_missing)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(
      title = sprintf("Proportion of Missing Data by Variable (%s)", label),
      x = "", y = "% Missing"
    ) +
    theme_minimal(base_size = 12)

  ggsave(
    file.path(output_dir, "missingness.pdf"),
    p_miss, width = 8, height = max(5, 0.4 * length(cols_missing))
  )

  # 5. Between-imputation variability (boxplots of imputed means across m)
  message(sprintf("  [%s] Between-imputation variability...", label))
  imp_means <- lapply(seq_len(imp$m), function(k) {
    completed <- mice::complete(imp, k)
    missing_idx <- lapply(cols_missing, function(col) is.na(imp$data[[col]]))
    names(missing_idx) <- cols_missing
    sapply(cols_missing, function(col) {
      idx <- missing_idx[[col]]
      if (sum(idx) > 0) mean(completed[[col]][idx], na.rm = TRUE) else NA_real_
    })
  })
  imp_means_df <- bind_rows(lapply(seq_along(imp_means), function(i) {
    as_tibble(as.list(imp_means[[i]])) |> mutate(.imp = i)
  })) |>
    pivot_longer(-.imp, names_to = "variable", values_to = "mean_imputed")

  p_between <- ggplot(imp_means_df, aes(x = variable, y = mean_imputed)) +
    geom_boxplot(fill = "lightblue", alpha = 0.7) +
    labs(
      title = sprintf("Between-Imputation Variability of Imputed Means (%s)", label),
      subtitle = "Monte Carlo error: spread of mean imputed value across m datasets",
      x = "", y = "Mean of imputed values"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(
    file.path(output_dir, "between_imputation_variability.pdf"),
    p_between, width = 10, height = 6
  )

  # 6. Observed vs imputed summary (text)
  message(sprintf("  [%s] Summary statistics...", label))
  completed_list <- lapply(seq_len(imp$m), function(k) mice::complete(imp, k))
  summary_lines <- character()
  for (col in cols_missing) {
    missing_idx <- is.na(imp$data[[col]])
    observed_vals <- imp$data[[col]][!missing_idx]
    imputed_vals <- unlist(lapply(completed_list, function(df) df[[col]][missing_idx]))

    obs_mean <- mean(observed_vals, na.rm = TRUE)
    obs_sd <- sd(observed_vals, na.rm = TRUE)
    obs_min <- min(observed_vals, na.rm = TRUE)
    obs_max <- max(observed_vals, na.rm = TRUE)

    if (all(is.na(imputed_vals))) {
      summary_lines <- c(summary_lines,
        sprintf("  %s: observed = %.2f (SD %.2f), range [%.2f, %.2f]; imputed = N/A",
                col, obs_mean, obs_sd, obs_min, obs_max))
    } else {
      imp_mean <- mean(imputed_vals, na.rm = TRUE)
      imp_sd <- sd(imputed_vals, na.rm = TRUE)
      imp_min <- min(imputed_vals, na.rm = TRUE)
      imp_max <- max(imputed_vals, na.rm = TRUE)
      summary_lines <- c(summary_lines,
        sprintf("  %s: obs = %.2f +/- %.2f [%.2f, %.2f] | imp = %.2f +/- %.2f [%.2f, %.2f] | diff = %.2f",
                col, obs_mean, obs_sd, obs_min, obs_max,
                imp_mean, imp_sd, imp_min, imp_max, imp_mean - obs_mean))
    }
  }
  summary_file <- file.path(output_dir, "observed_vs_imputed_summary.txt")
  sink(summary_file)
  cat(sprintf("=== Observed vs Imputed Summary (%s) ===\n\n", label))
  cat(sprintf("m = %d imputations, %d iterations\n\n", imp$m, imp$iteration))
  cat(paste(summary_lines, collapse = "\n"), "\n")
  sink()

  message(sprintf("  [%s] QC plots saved to %s/", label, output_dir))
  invisible(NULL)
}

# ==============================================================================
# PANEL QC
# ==============================================================================

run_panel_qc <- function() {
  message("\n=== Panel Imputation QC ===")
  if (!file.exists(PANEL_MIDS_PATH)) {
    message(sprintf("  Panel mids not found: %s (run imputation_panel.R first)", PANEL_MIDS_PATH))
    return(invisible(NULL))
  }

  panel_result <- readRDS(PANEL_MIDS_PATH)
  panel_dir <- file.path(QC_OUTPUT_DIR, "panel")
  dir.create(panel_dir, showWarnings = FALSE, recursive = TRUE)

  # The saved panel mids has its $data slot post-processed to completed values
  # (no NAs, empty $imp cells). Inline diagnostics produced when imputation_panel.R
  # was first run live in output/imputation/. Detect this case and skip.
  panel_diagnosable <- function(imp) {
    inherits(imp, "mids") && any(is.na(imp$data)) && sum(imp$nmis) > 0
  }

  # Biweekly pass
  if (!is.null(panel_result$biweekly)) {
    if (panel_diagnosable(panel_result$biweekly)) {
      create_qc_plots(panel_result$biweekly,
        file.path(panel_dir, "biweekly"),
        label = "panel_biweekly"
      )
    } else {
      message("  [panel_biweekly] Saved mids is non-diagnosable (data slot already completed); skipping. See output/imputation/ for inline QC produced during imputation.")
    }
  }

  # Monthly pass
  if (!is.null(panel_result$monthly)) {
    if (panel_diagnosable(panel_result$monthly)) {
      create_qc_plots(panel_result$monthly,
        file.path(panel_dir, "monthly"),
        label = "panel_monthly"
      )
    } else {
      message("  [panel_monthly] Saved mids is non-diagnosable (data slot already completed); skipping. See output/imputation/ for inline QC produced during imputation.")
    }
  }

  # Combined summary
  sink(file.path(panel_dir, "qc_summary.txt"))
  cat("=== Panel Imputation QC Summary ===\n\n")
  cat(sprintf("Source: %s\n", PANEL_MIDS_PATH))
  if (!is.null(panel_result$biweekly)) {
    cat(sprintf("Biweekly pass: m=%d, rows=%d\n",
      panel_result$biweekly$m, nrow(panel_result$biweekly$data)))
  }
  if (!is.null(panel_result$monthly)) {
    cat(sprintf("Monthly pass: m=%d, rows=%d\n",
      panel_result$monthly$m, nrow(panel_result$monthly$data)))
  }
  sink()

  message(sprintf("  Panel QC complete: %s/", panel_dir))
  invisible(NULL)
}

# ==============================================================================
# DIARY QC
# ==============================================================================

run_diary_qc <- function() {
  message("\n=== Diary Imputation QC ===")
  if (!file.exists(DIARY_MIDS_PATH)) {
    message(sprintf("  Diary mids not found: %s (run imputation_diary.R first)", DIARY_MIDS_PATH))
    return(invisible(NULL))
  }

  diary_result <- readRDS(DIARY_MIDS_PATH)
  diary_dir <- file.path(QC_OUTPUT_DIR, "diary")
  dir.create(diary_dir, showWarnings = FALSE, recursive = TRUE)

  imp <- if (!is.null(diary_result$imp)) diary_result$imp else diary_result
  if (inherits(imp, "mids")) {
    create_qc_plots(imp, diary_dir, label = "diary")
  } else {
    message("  No mids object found in diary result")
  }

  sink(file.path(diary_dir, "qc_summary.txt"))
  cat("=== Diary Imputation QC Summary ===\n\n")
  cat(sprintf("Source: %s\n", DIARY_MIDS_PATH))
  if (inherits(imp, "mids")) {
    cat(sprintf("m=%d, iterations=%d, rows=%d\n",
      imp$m, imp$iteration, nrow(imp$data)))
  }
  sink()

  message(sprintf("  Diary QC complete: %s/", diary_dir))
  invisible(NULL)
}

# ==============================================================================
# MAIN
# ==============================================================================

main <- function() {
  message("\n", paste(rep("=", 60), collapse = ""))
  message("  IMPUTATION QUALITY CONTROL (van Buuren 2018)")
  message(paste(rep("=", 60), collapse = ""))
  message(sprintf("\nOutput directory: %s/", QC_OUTPUT_DIR))

  run_panel_qc()
  run_diary_qc()

  message("\n", paste(rep("=", 60), collapse = ""))
  message("  QC COMPLETE")
  message(paste(rep("=", 60), collapse = ""))
}

# Run when sourced
if (sys.nframe() == 0 || identical(environment(), globalenv())) {
  main()
}
