# Refit the diary spline sensitivity comparison and save it with BIC
# alongside AIC, so the manuscript can report the Jones (2001) 2|ΔBIC|
# verdict consistently with the panel H1 spline sensitivity check.
#
# diary_ocat is built by the cached `diary-data-prep` chunk in manuscript.qmd.
# Rather than replicate that ~250 lines of upstream prep here, we load the
# chunk's lazy-load cache directly. This script is intended to be run from the
# project root.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
  library(ordinal)
  library(splines)
})

# Run this script from the project root (where manuscript.qmd lives).
if (!file.exists("manuscript.qmd")) {
  stop("Run this script from the project root: ", getwd(),
       " does not contain manuscript.qmd.")
}

# --- Locate diary-data-prep cache and load diary_ocat ---
cache_dir <- "manuscript_cache"
cand <- list.files(cache_dir,
                   pattern = "^diary-data-prep_.*\\.rdb$",
                   full.names = TRUE)
if (length(cand) == 0) {
  stop("No diary-data-prep cache found under ", cache_dir,
       ". Render manuscript.qmd at least once with caching enabled, or run ",
       "with -P refit_diary:true.")
}
db_path <- sub("\\.rdb$", "", cand[[1]])
env <- new.env()
lazyLoad(db_path, envir = env)
if (!exists("diary_ocat", envir = env)) {
  stop("diary_ocat not present in cached environment at ", db_path)
}
diary_ocat <- env$diary_ocat

message("Loaded diary_ocat from ", db_path,
        " (", nrow(diary_ocat), " rows, ",
        length(unique(diary_ocat$pid)), " participants).")

# --- Refit spline df = 2:6 and the linear baseline ---
diary_covariate_terms <- c("ln_between_10", "age_scaled", "bmi_scaled",
                           "SES_index_scaled", "region", "is_weekend")

diary_spline_fits <- purrr::map(2:6, \(df) {
  f <- as.formula(paste(
    "sleep_quality_ord ~",
    paste(c(sprintf("splines::ns(ln_within_10, df = %d)", df),
            diary_covariate_terms, "(1 | pid)"), collapse = " + ")
  ))
  message("Fitting spline df=", df, " ...")
  mod <- clmm(f, data = diary_ocat, link = "probit", Hess = TRUE,
              control = clmm.control(maxIter = 5000, gradTol = 1e-3))
  list(df = df, model = mod)
})

message("Fitting linear baseline ...")
diary_clmm_linear <- clmm(
  sleep_quality_ord ~ ln_within_10 + ln_between_10 +
    age_scaled + bmi_scaled + SES_index_scaled + region + is_weekend + (1 | pid),
  data = diary_ocat, link = "probit", Hess = TRUE,
  control = clmm.control(maxIter = 5000, gradTol = 1e-3)
)

# --- Build comparison tibble with both AIC and BIC ---
diary_spline_compare <- tibble(
  df  = purrr::map_int(diary_spline_fits, "df"),
  AIC = purrr::map_dbl(diary_spline_fits, \(x) AIC(x$model)),
  BIC = purrr::map_dbl(diary_spline_fits, \(x) BIC(x$model))
) |> arrange(BIC) |>
  mutate(deltaBIC = BIC - min(BIC),
         deltaAIC = AIC - min(AIC))

diary_linear_aic <- AIC(diary_clmm_linear)
diary_linear_bic <- BIC(diary_clmm_linear)
diary_spline_compare$linear_AIC <- diary_linear_aic
diary_spline_compare$linear_BIC <- diary_linear_bic

# Jones, Nagin & Roeder (2001) interpret 2|ΔBIC| on a 0-2 / 2-6 / 6-10 / >10
# ladder; precompute the verdict here for the manuscript prose.
.best_bic <- diary_spline_compare$BIC[1]
.two_dbic <- 2 * abs(.best_bic - diary_linear_bic)
.favoured <- if (.best_bic < diary_linear_bic) "spline" else "linear"
.verdict  <- if (.two_dbic < 2) {
  "not worth mentioning"
} else if (.two_dbic < 6) {
  "positive evidence"
} else if (.two_dbic < 10) {
  "strong evidence"
} else {
  "very strong evidence"
}
attr(diary_spline_compare, "jones_2dBIC")    <- .two_dbic
attr(diary_spline_compare, "jones_favoured") <- .favoured
attr(diary_spline_compare, "jones_verdict")  <- .verdict

out_path <- "output/models/diary_spline_compare.rds"
saveRDS(diary_spline_compare, out_path)

message("Wrote ", out_path)
message("Linear  AIC = ", sprintf("%.1f", diary_linear_aic),
        " | BIC = ", sprintf("%.1f", diary_linear_bic))
print(diary_spline_compare)
message("Best spline df = ", diary_spline_compare$df[1],
        " | 2|ΔBIC| = ", sprintf("%.1f", .two_dbic),
        " (", .verdict, " for ", .favoured, ")")
