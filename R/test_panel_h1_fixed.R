# Test fix for figure A1 (appendix-panel-h1-spline-bic).
#
# Bug: build_linear_curve() (lmer branch) included Var(Intercept) in the SE,
# whereas build_spline_curve() (lmer branch) only used Var(spline coefs).
# This inflated the linear CI ribbon relative to the spline ribbon in H1b,
# H1c, H1d (the lmer panels).
#
# Fix:
#   - For lmer panels (H1b, H1c, H1d): use marginaleffects::predictions()
#     with re.form = NA, which builds the full design matrix row and uses
#     the full vcov() consistently for both linear and spline models.
#   - For clmm panel (H1a, not supported by marginaleffects): manual delta
#     method on the marginal probit, but using the FULL covariance block
#     between the threshold and the gaming coefficients (current code
#     dropped the threshold variance entirely).
#
# Output: output/figures/panel_h1_spline_bic_TEST.png

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(patchwork)
  library(splines)
  library(lme4)
  library(ordinal)
  library(marginaleffects)
})

stopifnot(file.exists("manuscript.qmd"))
fits <- readRDS("output/models/panel_h1_spline_cc_fits.rds")

# -----------------------------------------------------------------------------
# Reference-grid helpers
# -----------------------------------------------------------------------------
ref_newdata <- function(fit, predictor, x_vals) {
  # All non-gaming covariates pinned at reference values:
  #   scaled continuous predictors -> 0
  #   isWeekend                    -> 0
  #   region                       -> first level
  frame <- if (inherits(fit, "lmerMod")) fit@frame else fit$model
  region_lvls <- levels(frame$region)
  data.frame(
    setNames(list(x_vals), predictor),
    age_scaled       = 0,
    bmi_scaled       = 0,
    SES_index_scaled = 0,
    isWeekend        = 0L,
    region           = region_lvls[1],
    stringsAsFactors = FALSE
  )
}

x_grid_for <- function(x_train, n = 150) {
  x_lo <- max(0, min(x_train, na.rm = TRUE))
  x_hi <- min(9, max(x_train, na.rm = TRUE))  # per-10-min scale → 0..90 min
  seq(x_lo, x_hi, length.out = n)
}

# -----------------------------------------------------------------------------
# lmer prediction curves via marginaleffects (re.form = NA = population-level)
# -----------------------------------------------------------------------------
curve_lmer <- function(fit, x_train, predictor) {
  if (is.null(fit)) return(NULL)
  x_vals <- x_grid_for(x_train)
  nd <- ref_newdata(fit, predictor, x_vals)
  p <- predictions(fit, newdata = nd, re.form = NA)
  tibble(
    x    = x_vals,
    pred = p$estimate,
    lo   = p$conf.low,
    hi   = p$conf.high
  )
}

# -----------------------------------------------------------------------------
# clmm marginal probit curve, full delta method including threshold variance
# -----------------------------------------------------------------------------
curve_clmm <- function(fit, x_train, predictor,
                       basis_fn,            # function(x) returning design block
                       threshold = "Fairly good|Fairly bad") {
  if (is.null(fit)) return(NULL)
  x_vals <- x_grid_for(x_train)
  X_basis <- basis_fn(x_vals)               # n_grid x p_basis matrix
  if (!is.matrix(X_basis)) X_basis <- matrix(X_basis, ncol = 1)

  beta_all  <- coef(fit)
  is_thr    <- grepl("\\|", names(beta_all))
  alpha_thr <- beta_all[threshold]

  basis_names <- if (ncol(X_basis) == 1) {
    # linear → single coefficient named predictor
    predictor
  } else {
    # spline → ns(predictor, df = k) terms
    grep(sprintf("ns\\(%s", predictor), names(beta_all)[!is_thr], value = TRUE)
  }
  beta_basis <- beta_all[basis_names]
  stopifnot(length(beta_basis) == ncol(X_basis))

  # marginal SD (random-intercept variance + 1 for latent residual)
  sigma_u   <- tryCatch(fit$ST$pid[1, 1], error = function(e) 0)
  marg_sd   <- sqrt(1 + sigma_u^2)

  eta  <- as.numeric(X_basis %*% beta_basis)
  z    <- (alpha_thr - eta) / marg_sd
  pred <- 1 - pnorm(z)

  # Full vcov block: rows/cols = c(threshold, basis terms)
  V  <- vcov(fit)
  keep <- c(threshold, basis_names)
  V_keep <- V[keep, keep, drop = FALSE]

  # Gradient of pred w.r.t. (alpha_thr, beta_basis):
  #   d pred / d alpha_thr = -phi(z) / marg_sd
  #   d pred / d beta_j    =  phi(z) / marg_sd * X_basis[, j]
  phi_z   <- dnorm(z)
  g_thr   <- -phi_z / marg_sd                          # n_grid
  g_basis <-  (phi_z / marg_sd) * X_basis              # n_grid x p_basis
  G       <- cbind(g_thr, g_basis)                     # n_grid x (1 + p_basis)

  se_pred <- sqrt(rowSums((G %*% V_keep) * G))

  tibble(
    x    = x_vals,
    pred = pred,
    lo   = pred - 1.96 * se_pred,
    hi   = pred + 1.96 * se_pred
  )
}

# Build the spline basis using the ORIGINAL training x (so knots match the fit)
spline_basis_fn <- function(x_train, best_df) {
  ns_obj <- splines::ns(x_train, df = best_df)
  function(x_new) predict(ns_obj, newx = x_new)
}
linear_basis_fn <- function() {
  function(x_new) matrix(x_new, ncol = 1)
}

# -----------------------------------------------------------------------------
# Compute all 8 curves
# -----------------------------------------------------------------------------
message("H1a (clmm) ...")
curve_a_sp  <- curve_clmm(fits$h1a,        fits$x_h1a,
                          "ln_monthly_avg_minutes_played_10",
                          spline_basis_fn(fits$x_h1a, fits$h1a_df))
curve_a_lin <- curve_clmm(fits$h1a_linear, fits$x_h1a,
                          "ln_monthly_avg_minutes_played_10",
                          linear_basis_fn())

message("H1b (lmer) ...")
curve_b_sp  <- curve_lmer(fits$h1b,        fits$x_h1b,
                          "ln_monthly_avg_minutes_played_10")
curve_b_lin <- curve_lmer(fits$h1b_linear, fits$x_h1b,
                          "ln_monthly_avg_minutes_played_10")

message("H1c (lmer) ...")
curve_c_sp  <- curve_lmer(fits$h1c,        fits$x_h1c,
                          "ln_monthly_avg_minutes_played_10")
curve_c_lin <- curve_lmer(fits$h1c_linear, fits$x_h1c,
                          "ln_monthly_avg_minutes_played_10")

message("H1d (lmer) ...")
curve_d_sp  <- curve_lmer(fits$h1d,        fits$x_h1d,
                          "ln_biweekly_avg_minutes_played_10")
curve_d_lin <- curve_lmer(fits$h1d_linear, fits$x_h1d,
                          "ln_biweekly_avg_minutes_played_10")

# Per-10-min → min/day
to_min <- function(d) if (is.null(d)) NULL else mutate(d, minutes = x * 10)
curve_a_sp <- to_min(curve_a_sp); curve_a_lin <- to_min(curve_a_lin)
curve_b_sp <- to_min(curve_b_sp); curve_b_lin <- to_min(curve_b_lin)
curve_c_sp <- to_min(curve_c_sp); curve_c_lin <- to_min(curve_c_lin)
curve_d_sp <- to_min(curve_d_sp); curve_d_lin <- to_min(curve_d_lin)

# -----------------------------------------------------------------------------
# Plot
# -----------------------------------------------------------------------------
spline_theme <- theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")
x_scale <- scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, 15),
                              expand = c(0, 0))

mk_panel <- function(curve_sp, curve_lin, title, ylab) {
  ggplot(curve_sp, aes(minutes, pred)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#0072B2", alpha = 0.20) +
    {
      if (!is.null(curve_lin))
        geom_ribbon(data = curve_lin, aes(minutes, ymin = lo, ymax = hi),
                    fill = "#D55E00", alpha = 0.15, inherit.aes = FALSE)
    } +
    geom_line(aes(linetype = "Spline"), color = "#0072B2", linewidth = 0.9) +
    {
      if (!is.null(curve_lin))
        geom_line(data = curve_lin,
                  aes(minutes, pred, linetype = "Linear"),
                  color = "#D55E00", linewidth = 0.9)
    } +
    scale_linetype_manual(name = "Model",
                          values = c("Spline" = "solid", "Linear" = "dashed")) +
    x_scale +
    labs(title = title, x = NULL, y = ylab) +
    spline_theme
}

p_a <- mk_panel(curve_a_sp, curve_a_lin,
                sprintf("H1a: Sleep Quality  (df = %d)", fits$h1a_df),
                "P(Fairly bad or Very bad)") +
       labs(x = "Late-night gaming (min/day, monthly avg)")
p_b <- mk_panel(curve_b_sp, curve_b_lin,
                sprintf("H1b: Sleep Duration  (df = %d)", fits$h1b_df),
                "Sleep duration (hours)") +
       labs(x = "Late-night gaming (min/day, monthly avg)")
p_c <- mk_panel(curve_c_sp, curve_c_lin,
                sprintf("H1c: Daytime Sleepiness  (df = %d)", fits$h1c_df),
                "ESS score") +
       labs(x = "Late-night gaming (min/day, monthly avg)")
p_d <- mk_panel(curve_d_sp, curve_d_lin,
                sprintf("H1d: Wellbeing  (df = %d)", fits$h1d_df),
                "SWEMWBS score") +
       labs(x = "Late-night gaming (min/day, biweekly avg)")

fig <- (p_a | p_b) / (p_d | p_c) +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
out_path <- "output/figures/panel_h1_spline_bic_TEST.png"
ggsave(out_path, fig, width = 9, height = 8, dpi = 150)
message("Wrote: ", out_path)

# Print numeric summary for a sanity diff
summarise_widths <- function(name, sp, lin) {
  width_sp  <- mean(sp$hi  - sp$lo,  na.rm = TRUE)
  width_lin <- if (is.null(lin)) NA else mean(lin$hi - lin$lo, na.rm = TRUE)
  cat(sprintf("%-25s  mean spline CI width = %8.4f  | mean linear CI width = %8.4f\n",
              name, width_sp, width_lin))
}
cat("\nMean CI widths (after fix):\n")
summarise_widths("H1a (clmm probit)",       curve_a_sp, curve_a_lin)
summarise_widths("H1b (lmer hours)",        curve_b_sp, curve_b_lin)
summarise_widths("H1c (lmer ESS)",          curve_c_sp, curve_c_lin)
summarise_widths("H1d (lmer SWEMWBS)",      curve_d_sp, curve_d_lin)
