#!/usr/bin/env Rscript
# Pre-render hook: mirror frozen Quarto figures into manuscript_files/.
#
# Why: Quarto's html freeze stores `supporting: []` for this project, so the
# PNGs cached under _freeze/manuscript/figure-html/ are not restored to
# manuscript_files/figure-html/ on render. pandoc then warns
# "Could not fetch resource manuscript_files/figure-html/fig-*.png" and the
# embed-resources output drops those images.

src <- "_freeze/manuscript/figure-html"
dst <- "manuscript_files/figure-html"

if (dir.exists(src)) {
  dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  pngs <- list.files(src, pattern = "\\.png$", full.names = TRUE)
  if (length(pngs)) {
    file.copy(pngs, dst, overwrite = TRUE)
    message(sprintf("[pre-render] mirrored %d figure(s) %s -> %s",
                    length(pngs), src, dst))
  }
}
