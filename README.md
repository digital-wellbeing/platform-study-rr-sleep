# Late-Night Gaming, Sleep and Wellbeing: The Pri(n)ce of Playing Past Bedtime

[![Build and Deploy](https://github.com/digital-wellbeing/platform-study-rr-sleep/actions/workflows/build-manuscript.yml/badge.svg)](https://github.com/digital-wellbeing/platform-study-rr-sleep/actions/workflows/build-manuscript.yml)

## About

This repository contains the analysis and manuscript for a **Stage 1 Registered Report** investigating the relationships between late-night gaming, sleep, and wellbeing. The study uses data from the [Open Play dataset](https://github.com/digital-wellbeing/open-play), which combines objective behavioral telemetry from gaming platforms with self-reported psychological measures.

### Hypotheses

**H1**: Late-night gaming (23:00-06:00) is associated with:
- **H1a**: Poorer sleep quality
- **H1b**: Shorter sleep duration  
- **H1c**: Higher daytime sleepiness
- **H1d**: Lower wellbeing

**H2**: Chronotype moderates these relationships, with stronger negative associations for individuals with eveningness chronotypes.

## View the Manuscript

**📄 [Read the latest manuscript online](https://digital-wellbeing.github.io/platform-study-rr-sleep/manuscript.html)**

The manuscript is automatically built and deployed whenever changes are pushed to the main branch.

## Repository Structure

```
.
├── manuscript.qmd           # Main manuscript source (Quarto)
├── helpers.R                # Analysis helper functions
├── imputation.R             # Multiple imputation procedures
├── preprocess_data.R        # Data preprocessing pipeline
├── bibliography.bib         # References
├── _quarto.yml             # Quarto configuration
├── _extensions/            # Quarto extensions (preprint template)
├── _freeze/                # Quarto freeze cache (speeds up rendering)
├── data/                   # Data files
│   ├── processed/          # Processed datasets (committed)
│   └── *.csv.gz            # Raw survey data
├── output/                 # Analysis outputs
│   └── models/             # Pre-fitted model objects (.rds)
└── R/                      # Exploratory/helper scripts
```

## Local Development

### Prerequisites

- [Quarto](https://quarto.org/docs/get-started/) >= 1.7.29 (tested with 1.8.25)
- [R](https://www.r-project.org/) >= 4.5.0 (tested with 4.5.1)
- Required R packages (see below)

### Installation

1. Clone the repository:
```bash
git clone https://github.com/digital-wellbeing/platform-study-rr-sleep.git
cd platform-study-rr-sleep
```

2. Install R packages:
```r
# Install pacman if needed
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# Configure r-universe for questionnaires package
options(repos = c(
  lcbcuio = 'https://lcbc-uio.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'
))
install.packages('questionnaires')

# Install other packages via pacman
pacman::p_load(
  tidyverse, lme4, marginaleffects, glmmTMB, mice, ordinal, modelsummary,
  tinytable, lubridate, data.table, mctq, glue, parameters, performance,
  withr, report, ggdist, patchwork, ggpattern, zoo, future, future.apply
)
```

3. Render the manuscript:
```bash
quarto render manuscript.qmd
```

The rendered manuscript will be saved as `manuscript.html` (and `manuscript.pdf` if you have LaTeX installed).

### Build Options

- **Quick render** (uses freeze cache): `quarto render manuscript.qmd`
- **Force re-execution**: `quarto render manuscript.qmd --cache-refresh`
- **HTML only**: `quarto render manuscript.qmd --to html`
- **Refit models** (slow): 
  ```bash
  quarto render manuscript.qmd -P refit_h1:true -P refit_h2:true
  ```

### Notes on Data and Models

This repository includes:
- ✅ Pre-processed data files in `data/processed/`
- ✅ Pre-fitted statistical models in `output/models/`
- ✅ Quarto freeze cache in `_freeze/`

These committed artifacts mean you can render the manuscript **without** running computationally expensive preprocessing or model fitting steps. The default behavior is to load pre-computed results.

## Project Status

**Current Stage**: Stage 1 Registered Report (Accepted)

The analysis plan, hypotheses, and statistical models are pre-registered and peer-reviewed. Changes to the registered plan are not permitted except in case of genuine errors or unforeseen technical problems.

## Citation

If you use this work, please cite:

```bibtex
@unpublished{foldes2024latenight,
  title={Late-Night Gaming, Sleep and Wellbeing: The Pri(n)ce of Playing Past Bedtime},
  author={F{\"o}ldes, Tam{\'a}s A. and [Second Author]},
  year={2024},
  note={Stage 1 Registered Report}
}
```

## License

[Specify license here - e.g., CC-BY 4.0 for manuscript, MIT for code]

## Contact

**Corresponding Author**: Tamás A. Földes (tamas.foldes@oii.ox.ac.uk)

## Acknowledgments

This research uses data from the Open Play dataset. See the [Open Play repository](https://github.com/digital-wellbeing/open-play) for details on data collection and study procedures.
