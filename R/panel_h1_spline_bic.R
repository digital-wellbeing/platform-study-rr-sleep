# Refit the panel H1 natural-spline sensitivity check with the harmonised
# df grid (2:6) so the cached output/models/panel_h1_spline_*.rds files
# match the manuscript prose without requiring a full -P refit_appendix:true
# render. Mirrors the `appendix-h1-spline-fit` chunk in manuscript.qmd.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(splines)
  library(lme4)
  library(ordinal)
  library(readr)
})

# Run from the project root.
if (!file.exists("manuscript.qmd")) {
  stop("Run this script from the project root: ", getwd(),
       " does not contain manuscript.qmd.")
}

source("helpers.R")

selfreport <- read_csv("data/processed/selfreport.csv.gz", show_col_types = FALSE)

OUTCOME_VARS <- c("total_hours_sleep", "psqi_comp1_quality", "epsTotal",
                  "wemwbs", "psqi_global")
selfreport_completecase <- filter_by_outcome(selfreport, OUTCOME_VARS, "completecase")
gaming_inputs <- list(completecase = build_gaming_inputs(selfreport_completecase))

# --- Helper: factory function that captures df in the closure (mirrors chunk) ---
make_spline_fit_fn <- function(model_type, outcome, predictor, df,
                                extra_gender_re = FALSE) {
  force(model_type); force(outcome); force(predictor)
  force(df);         force(extra_gender_re)
  re_str <- if (extra_gender_re) "(1 | pid) + (1 | gender)" else "(1 | pid)"
  if (model_type == "clmm") {
    function(d) clmm(
      as.formula(sprintf(
        "%s ~ ns(%s, df = %d) + age_scaled + bmi_scaled + SES_index_scaled + region + isWeekend + (1 | pid)",
        outcome, predictor, df
      )),
      data = d, link = "probit", Hess = TRUE,
      control = clmm.control(maxIter = 5000, gradTol = 1e-3)
    )
  } else {
    function(d) lmer(
      as.formula(sprintf(
        "%s ~ ns(%s, df = %d) + age_scaled + bmi_scaled + SES_index_scaled + region + isWeekend + %s",
        outcome, predictor, df, re_str
      )),
      data = d, control = lmerControl(optimizer = "bobyqa")
    )
  }
}

cc_monthly_h1a_sp <- gaming_inputs[["completecase"]]$gamingMonthly |>
  enforce_monthly_wave_subset() |>
  mutate(
    psqi_comp1_quality_rounded = round(psqi_comp1_quality),
    psqi_6_ord = factor(
      psqi_comp1_quality_rounded,
      levels = c(0, 1, 2, 3),
      labels = c("Very good", "Fairly good", "Fairly bad", "Very bad"),
      ordered = TRUE
    )
  ) |>
  filter(!is.na(psqi_6_ord))

cc_monthly_sp  <- gaming_inputs[["completecase"]]$gamingMonthly |> enforce_monthly_wave_subset()
cc_biweekly_sp <- gaming_inputs[["completecase"]]$gamingBiweekly

run_ic_sel <- function(data, fit_fn_factory, dfs) {
  fits <- lapply(dfs, function(df) {
    message("  fit df=", df, " ...")
    tryCatch(fit_fn_factory(df)(data), error = function(e) {
      message("    failed: ", conditionMessage(e)); NULL
    })
  })
  aics <- vapply(fits, function(f) if (is.null(f)) Inf else AIC(f), numeric(1))
  bics <- vapply(fits, function(f) if (is.null(f)) Inf else BIC(f), numeric(1))
  best_idx <- which.min(bics)
  list(
    aics     = setNames(aics, dfs),
    bics     = setNames(bics, dfs),
    best_df  = dfs[best_idx],
    best_aic = unname(aics[best_idx]),
    best_bic = unname(bics[best_idx])
  )
}

dfs_try <- 2:6
saveRDS(dfs_try, "output/models/panel_h1_spline_dfs.rds")

message("Fitting H1a (sleep quality) splines ...")
h1a_sel <- run_ic_sel(cc_monthly_h1a_sp,
  function(df) make_spline_fit_fn("clmm", "psqi_6_ord",
    "ln_monthly_avg_minutes_played_10", df), dfs_try)
message("Fitting H1b (sleep duration) splines ...")
h1b_sel <- run_ic_sel(cc_monthly_sp,
  function(df) make_spline_fit_fn("lmer", "total_hours_sleep",
    "ln_monthly_avg_minutes_played_10", df, FALSE), dfs_try)
message("Fitting H1c (daytime sleepiness) splines ...")
h1c_sel <- run_ic_sel(cc_monthly_sp,
  function(df) make_spline_fit_fn("lmer", "epsTotal",
    "ln_monthly_avg_minutes_played_10", df, TRUE), dfs_try)
message("Fitting H1d (wellbeing) splines ...")
h1d_sel <- run_ic_sel(cc_biweekly_sp,
  function(df) make_spline_fit_fn("lmer", "wemwbs",
    "ln_biweekly_avg_minutes_played_10", df, TRUE), dfs_try)

message("Fitting linear baselines ...")
cc_h1a_linear <- tryCatch(clmm(
  psqi_6_ord ~ ln_monthly_avg_minutes_played_10 +
    age_scaled + bmi_scaled + SES_index_scaled + region + isWeekend + (1 | pid),
  data = cc_monthly_h1a_sp, link = "probit", Hess = TRUE,
  control = clmm.control(maxIter = 5000, gradTol = 1e-3)), error = function(e) NULL)
cc_h1b_linear <- tryCatch(lmer(
  total_hours_sleep ~ ln_monthly_avg_minutes_played_10 +
    age_scaled + bmi_scaled + SES_index_scaled + region + isWeekend + (1 | pid),
  data = cc_monthly_sp, control = lmerControl(optimizer = "bobyqa")), error = function(e) NULL)
cc_h1c_linear <- tryCatch(lmer(
  epsTotal ~ ln_monthly_avg_minutes_played_10 +
    age_scaled + bmi_scaled + SES_index_scaled + region + isWeekend +
    (1 | pid) + (1 | gender),
  data = cc_monthly_sp, control = lmerControl(optimizer = "bobyqa")), error = function(e) NULL)
cc_h1d_linear <- tryCatch(lmer(
  wemwbs ~ ln_biweekly_avg_minutes_played_10 +
    age_scaled + bmi_scaled + SES_index_scaled + region + isWeekend +
    (1 | pid) + (1 | gender),
  data = cc_biweekly_sp, control = lmerControl(optimizer = "bobyqa")), error = function(e) NULL)

.safe_ic <- function(fit, fn) {
  if (is.null(fit)) return(NA_real_)
  tryCatch(fn(fit), error = function(e) NA_real_)
}

panel_h1_spline_aic <- tibble(
  Outcome    = c("H1a: Sleep Quality", "H1b: Sleep Duration",
                 "H1c: Daytime Sleepiness", "H1d: Wellbeing"),
  Model      = c("CLMM probit", "lmer", "lmer", "lmer"),
  `Best df`  = c(h1a_sel$best_df, h1b_sel$best_df, h1c_sel$best_df, h1d_sel$best_df),
  `Linear AIC` = round(c(.safe_ic(cc_h1a_linear, AIC), .safe_ic(cc_h1b_linear, AIC),
                          .safe_ic(cc_h1c_linear, AIC), .safe_ic(cc_h1d_linear, AIC)), 1),
  `Spline AIC` = round(c(h1a_sel$best_aic, h1b_sel$best_aic,
                          h1c_sel$best_aic, h1d_sel$best_aic), 1),
  `Linear BIC` = round(c(.safe_ic(cc_h1a_linear, BIC), .safe_ic(cc_h1b_linear, BIC),
                          .safe_ic(cc_h1c_linear, BIC), .safe_ic(cc_h1d_linear, BIC)), 1),
  `Spline BIC` = round(c(h1a_sel$best_bic, h1b_sel$best_bic,
                          h1c_sel$best_bic, h1d_sel$best_bic), 1)
) |> mutate(
  `ΔAIC` = round(`Spline AIC` - `Linear AIC`, 1),
  `ΔBIC` = round(`Spline BIC` - `Linear BIC`, 1),
  `2|ΔBIC|` = round(2 * abs(`ΔBIC`), 1),
  Interpretation = case_when(
    `2|ΔBIC|` < 2  ~ "Not worth mentioning",
    `2|ΔBIC|` < 6  ~ ifelse(`ΔBIC` < 0, "Positive (spline)",  "Positive (linear)"),
    `2|ΔBIC|` < 10 ~ ifelse(`ΔBIC` < 0, "Strong (spline)",    "Strong (linear)"),
    TRUE           ~ ifelse(`ΔBIC` < 0, "Very strong (spline)", "Very strong (linear)")
  )
)

saveRDS(panel_h1_spline_aic, "output/models/panel_h1_spline_aic.rds")

saveRDS(
  list(
    H1a = list(aics = h1a_sel$aics, bics = h1a_sel$bics),
    H1b = list(aics = h1b_sel$aics, bics = h1b_sel$bics),
    H1c = list(aics = h1c_sel$aics, bics = h1c_sel$bics),
    H1d = list(aics = h1d_sel$aics, bics = h1d_sel$bics)
  ),
  "output/models/panel_h1_spline_ic_grid.rds"
)

message("Fitting complete-case spline models for prediction curves ...")
cc_h1a_spline <- make_spline_fit_fn("clmm", "psqi_6_ord",
  "ln_monthly_avg_minutes_played_10", h1a_sel$best_df)(cc_monthly_h1a_sp)
cc_h1b_spline <- make_spline_fit_fn("lmer", "total_hours_sleep",
  "ln_monthly_avg_minutes_played_10", h1b_sel$best_df, FALSE)(cc_monthly_sp)
cc_h1c_spline <- make_spline_fit_fn("lmer", "epsTotal",
  "ln_monthly_avg_minutes_played_10", h1c_sel$best_df, TRUE)(cc_monthly_sp)
cc_h1d_spline <- make_spline_fit_fn("lmer", "wemwbs",
  "ln_biweekly_avg_minutes_played_10", h1d_sel$best_df, TRUE)(cc_biweekly_sp)

saveRDS(list(
  h1a = cc_h1a_spline, h1b = cc_h1b_spline,
  h1c = cc_h1c_spline, h1d = cc_h1d_spline,
  h1a_linear = cc_h1a_linear, h1b_linear = cc_h1b_linear,
  h1c_linear = cc_h1c_linear, h1d_linear = cc_h1d_linear,
  h1a_df = h1a_sel$best_df, h1b_df = h1b_sel$best_df,
  h1c_df = h1c_sel$best_df, h1d_df = h1d_sel$best_df,
  x_h1a  = cc_monthly_h1a_sp$ln_monthly_avg_minutes_played_10,
  x_h1b  = cc_monthly_sp$ln_monthly_avg_minutes_played_10,
  x_h1c  = cc_monthly_sp$ln_monthly_avg_minutes_played_10,
  x_h1d  = cc_biweekly_sp$ln_biweekly_avg_minutes_played_10
), "output/models/panel_h1_spline_cc_fits.rds")

message("Wrote: panel_h1_spline_aic.rds, panel_h1_spline_dfs.rds, ",
        "panel_h1_spline_ic_grid.rds, panel_h1_spline_cc_fits.rds")
print(panel_h1_spline_aic)
