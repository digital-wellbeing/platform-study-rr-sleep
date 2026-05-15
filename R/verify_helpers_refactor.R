# Verify the refactored build_spline_curve / build_linear_curve helpers
# produce the same numbers as the standalone test script
# (R/test_panel_h1_fixed.R) and confirm linear CIs are not artificially
# inflated relative to spline CIs in the lmer panels.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(splines)
  library(lme4)
  library(ordinal)
  library(marginaleffects)
})

stopifnot(file.exists("helpers.R"))
source("helpers.R")
fits <- readRDS("output/models/panel_h1_spline_cc_fits.rds")

curve_a_sp  <- build_spline_curve(fits$h1a,        fits$x_h1a, fits$h1a_df,
                                  "ln_monthly_avg_minutes_played_10")
curve_a_lin <- build_linear_curve(fits$h1a_linear, fits$x_h1a,
                                  "ln_monthly_avg_minutes_played_10")

curve_b_sp  <- build_spline_curve(fits$h1b,        fits$x_h1b, fits$h1b_df,
                                  "ln_monthly_avg_minutes_played_10")
curve_b_lin <- build_linear_curve(fits$h1b_linear, fits$x_h1b,
                                  "ln_monthly_avg_minutes_played_10")

curve_c_sp  <- build_spline_curve(fits$h1c,        fits$x_h1c, fits$h1c_df,
                                  "ln_monthly_avg_minutes_played_10")
curve_c_lin <- build_linear_curve(fits$h1c_linear, fits$x_h1c,
                                  "ln_monthly_avg_minutes_played_10")

curve_d_sp  <- build_spline_curve(fits$h1d,        fits$x_h1d, fits$h1d_df,
                                  "ln_biweekly_avg_minutes_played_10")
curve_d_lin <- build_linear_curve(fits$h1d_linear, fits$x_h1d,
                                  "ln_biweekly_avg_minutes_played_10")

summarise_widths <- function(name, sp, lin) {
  width_sp  <- mean(sp$hi  - sp$lo,  na.rm = TRUE)
  width_lin <- if (is.null(lin)) NA else mean(lin$hi - lin$lo, na.rm = TRUE)
  flag <- if (!is.na(width_lin) && width_lin > width_sp) " <-- LINEAR WIDER" else ""
  cat(sprintf("%-25s  spline CI width = %8.4f  | linear CI width = %8.4f%s\n",
              name, width_sp, width_lin, flag))
}

cat("\nMean CI widths after helpers.R refactor:\n")
summarise_widths("H1a (clmm probit)",       curve_a_sp, curve_a_lin)
summarise_widths("H1b (lmer hours)",        curve_b_sp, curve_b_lin)
summarise_widths("H1c (lmer ESS)",          curve_c_sp, curve_c_lin)
summarise_widths("H1d (lmer SWEMWBS)",      curve_d_sp, curve_d_lin)
