# Makefile for downloading data from platform-study-rr repository

# GitHub repository details
REPO := digital-wellbeing/platform-study-rr
COMMIT := 8eefedde66b3a90a605f3ec5e921450bfe287335
BASE_URL := https://raw.githubusercontent.com/$(REPO)/$(COMMIT)/data/clean

# Data directory
DATA_DIR := data

# List of data files
DATA_FILES := diary.csv.gz \
              intake.csv.gz \
              nintendo.csv.gz \
              panel.csv.gz \
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

