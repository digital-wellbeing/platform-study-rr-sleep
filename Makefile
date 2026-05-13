# Makefile for downloading data from open-play repository

# GitHub repository details
REPO := digital-wellbeing/open-play
TAG := v1.0.0
BASE_URL := https://raw.githubusercontent.com/$(REPO)/$(TAG)/data/clean

# Data directory
DATA_DIR := data

# List of data files
DATA_FILES := survey_daily.csv.gz \
              survey_intake.csv.gz \
              survey_biweekly.csv.gz \
              nintendo.csv.gz \
              simon.csv.gz \
              steam.csv.gz \
              xbox.csv.gz

# Full paths to data files
DATA_PATHS := $(addprefix $(DATA_DIR)/, $(DATA_FILES))

.PHONY: all clean data

# Default target: download all data
all: data

# Download all data files
data: $(DATA_PATHS)

# Rule to download each data file
$(DATA_DIR)/%.csv.gz:
	@mkdir -p $(DATA_DIR)
	@echo "Downloading $@..."
	@wget -q -O $@ $(BASE_URL)/$*.csv.gz
	@echo "Downloaded $@"

# Clean up downloaded data
clean:
	@echo "Removing data files..."
	@rm -f $(DATA_PATHS)
	@echo "Data files removed"

# Show what would be downloaded
list:
	@echo "Data files to download:"
	@for file in $(DATA_FILES); do echo "  - $$file"; done

