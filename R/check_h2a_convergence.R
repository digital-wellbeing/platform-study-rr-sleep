suppressPackageStartupMessages({
  library(ordinal)
})

cat("=== completecase H2a ===\n")
cc <- readRDS("output/models/completecase_model_h2a.rds")
cat("Class:", paste(class(cc), collapse = ", "), "\n")
cat("Converged (cc$convergence$code, optional):\n")
print(cc$convergence)
cat("\nSummary:\n")
print(summary(cc))
cat("\nVarcov diag (sign of identifiability):\n")
vc <- tryCatch(vcov(cc), error = function(e) e)
print(vc)
cat("\nAny NA in fixed-effect SEs?\n")
print(any(is.na(sqrt(diag(vc)))))

cat("\n\n=== imputed Rubin bundle for H2a ===\n")
b <- readRDS("output/models/imputed_h2a_rubin.rds")
cat("Names: ", paste(names(b), collapse = ", "), "\n")
if (!is.null(b$fits)) {
  cat("Number of fits:", length(b$fits), "\n")
  convs <- vapply(b$fits, function(f) {
    cc <- tryCatch(f$convergence, error = function(e) NULL)
    if (is.null(cc)) NA_character_ else paste(unlist(cc$code), collapse = "/")
  }, character(1))
  cat("Convergence codes across fits:\n")
  print(table(convs, useNA = "ifany"))
  ses_na <- vapply(b$fits, function(f) {
    v <- tryCatch(vcov(f), error = function(e) NULL)
    if (is.null(v)) TRUE else any(!is.finite(sqrt(diag(v))))
  }, logical(1))
  cat("Fits with non-finite SEs:", sum(ses_na), "/", length(b$fits), "\n")
}
if (!is.null(b$pooled)) {
  cat("\nPooled coefficient table:\n")
  print(b$pooled)
}
