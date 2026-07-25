# Universal Data Wrangler

An interactive R Shiny web application designed to streamline dataset ingestion, cleaning, validation, and preprocessing workflows without requiring manual code writing. 

## Overview
Built as a final project for STAT 440, the Universal Data Wrangler bridges the gap between raw data sources and analysis-ready formats. It provides a modular interface for users to import data from URLs, inspect column structures, handle missing values, convert data types, and export clean datasets .

## Key Features
* Multi-Format URL Ingestion:** Supports direct loading of CSV, JSON, and Excel files via URL .
* Smart Date Detection:** Automatically identifies and parses character columns containing date strings into standard Date formats based on a 70% threshold .
* Automated Type Suggestion & Conversion:** Inspects columns to suggest optimal data types (numeric, character, factor, Date, logical) and allows seamless interactive conversion .
* Missing Value Handling:** Provides flexible options to drop rows with missing values or impute numeric NAs with means/medians and character NAs with placeholders .
* Data Reshaping & Filtering:** Includes regex-based text filtering, column renaming, duplicate removal, and pivoting capabilities (`pivot_longer` and `pivot_wider`) .
* Reproducible Export:** Allows users to download clean, processed datasets instantly as CSV files .

## Tech Stack
* Language: R 
* Framework: Shiny 
* Data Wrangling & Manipulation: `dplyr`, `tidyr`, `stringr`, `lubridate`
* Data Input/Output: `readr`, `readxl`, `jsonlite'
* UI & Tables: `DT` (Interactive DataTables)

## Usage
1. Clone the repository or open the script in RStudio.
2. Ensure required libraries (`shiny`, `dplyr`, `tidyr`, `readr`, `readxl`, `jsonlite`, `DT`, `lubridate`, `stringr`) are installed.
3. Run the application using `shiny::runApp()`.
4. Paste a valid dataset URL, select the file type, and click 'Load Dataset' to begin wrangling .
