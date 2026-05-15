# Replicate the manuscript chunk fig-appendix-panel-h1-spline-bic using the
# refactored helpers, write to output/figures/panel_h1_spline_bic.png so we
# can eyeball it next to the TEST version before rendering the manuscript.

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

source("helpers.R")
sp <- readRDS("output/models/panel_h1_spline_cc_fits.rds")

curve_a <- build_spline_curve(sp$h1a, sp$x_h1a, sp$h1a_df, "ln_monthly_avg_minutes_played_10")
curve_b <- build_spline_curve(sp$h1b, sp$x_h1b, sp$h1b_df, "ln_monthly_avg_minutes_played_10")
curve_c <- build_spline_curve(sp$h1d, sp$x_h1d, sp$h1d_df, "ln_biweekly_avg_minutes_played_10")
curve_d <- build_spline_curve(sp$h1c, sp$x_h1c, sp$h1c_df, "ln_monthly_avg_minutes_played_10")

lin_a <- build_linear_curve(sp$h1a_linear, sp$x_h1a, "ln_monthly_avg_minutes_played_10")
lin_b <- build_linear_curve(sp$h1b_linear, sp$x_h1b, "ln_monthly_avg_minutes_played_10")
lin_c <- build_linear_curve(sp$h1d_linear, sp$x_h1d, "ln_biweekly_avg_minutes_played_10")
lin_d <- build_linear_curve(sp$h1c_linear, sp$x_h1c, "ln_monthly_avg_minutes_played_10")

to_min <- function(d) if (is.null(d)) NULL else dplyr::mutate(d, minutes = x * 10)
curve_a <- to_min(curve_a); curve_b <- to_min(curve_b)
curve_c <- to_min(curve_c); curve_d <- to_min(curve_d)
lin_a   <- to_min(lin_a);   lin_b   <- to_min(lin_b)
lin_c   <- to_min(lin_c);   lin_d   <- to_min(lin_d)

spline_theme  <- theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")
xlab_monthly  <- "Late-night gaming (min/day, monthly avg)"
xlab_biweekly <- "Late-night gaming (min/day, biweekly avg)"
x_scale <- scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, 15),
                              expand = c(0, 0))

lin_ribbon <- function(d) {
  if (is.null(d) || !all(c("lo", "hi") %in% names(d))) return(NULL)
  geom_ribbon(data = d, aes(minutes, ymin = lo, ymax = hi),
              fill = "#D55E00", alpha = 0.15, inherit.aes = FALSE)
}
mk_panel <- function(curve, lin, title, ylab, xlab) {
  ggplot(curve, aes(minutes, pred)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#0072B2", alpha = 0.20) +
    lin_ribbon(lin) +
    geom_line(aes(linetype = "Spline"), color = "#0072B2", linewidth = 0.9) +
    { if (!is.null(lin)) geom_line(data = lin, aes(minutes, pred, linetype = "Linear"),
                                    color = "#D55E00", linewidth = 0.9) } +
    scale_linetype_manual(name = "Model",
                          values = c("Spline" = "solid", "Linear" = "dashed")) +
    x_scale +
    labs(title = title, x = xlab, y = ylab) +
    spline_theme
}

p_a <- mk_panel(curve_a, lin_a,
                sprintf("H1a: Sleep Quality  (df = %d)", sp$h1a_df),
                "P(Fairly bad or Very bad)", xlab_monthly)
p_b <- mk_panel(curve_b, lin_b,
                sprintf("H1b: Sleep Duration  (df = %d)", sp$h1b_df),
                "Sleep duration (hours)", xlab_monthly)
p_c <- mk_panel(curve_c, lin_c,
                sprintf("H1d: Wellbeing  (df = %d)", sp$h1d_df),
                "SWEMWBS score", xlab_biweekly)
p_d <- mk_panel(curve_d, lin_d,
                sprintf("H1c: Daytime Sleepiness  (df = %d)", sp$h1c_df),
                "ESS score", xlab_monthly)

fig <- (p_a | p_b) / (p_c | p_d) +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
out_path <- "output/figures/panel_h1_spline_bic.png"
ggsave(out_path, fig, width = 9, height = 8, dpi = 150)
message("Wrote: ", out_path)
