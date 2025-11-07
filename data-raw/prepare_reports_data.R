# Data Preparation Script for SITUAS Reports Metadata
# This script reads the situas_api.xlsx file and prepares it as internal package data
#
# Run this script manually whenever the Excel file is updated:
# source("data-raw/prepare_reports_data.R")

# Load required packages
library(data.table)

# Check if openxlsx is available
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required. Install it with: install.packages('openxlsx')")
}

message("Reading situas_api.xlsx...")

# Path to Excel file (relative to package root)
excel_path <- "situas_api.xlsx"

if (!file.exists(excel_path)) {
  stop("Excel file not found at: ", excel_path)
}

# Read Excel file
raw_data <- openxlsx::read.xlsx(
  excel_path,
  sheet = 1,
  detectDates = FALSE  # Keep dates as character strings
)

message("Processing ", nrow(raw_data), " reports...")

# Convert to data.table
dt <- as.data.table(raw_data)

# Map column names from Italian to English
# Expected columns in Excel:
# - "Id report" -> pfun
# - "Titolo report" -> title
# - "Inizio/fine validità report" -> date_range
# - "Analisi temporale" -> analysis_type
# - "Informazioni" -> info

setnames(
  dt,
  old = c("Id.report", "Titolo.report", "Inizio/fine.validità.report",
          "Analisi.temporale", "Informazioni"),
  new = c("pfun", "title", "date_range", "analysis_type", "info"),
  skip_absent = TRUE
)

# Clean and validate data
# 1. Ensure pfun is integer
dt[, pfun := as.integer(pfun)]

# 2. Remove any rows with missing pfun (shouldn't happen but be safe)
dt <- dt[!is.na(pfun)]

# 3. Trim whitespace from character columns
char_cols <- c("title", "date_range", "analysis_type", "info")
dt[, (char_cols) := lapply(.SD, function(x) {
  if (is.character(x)) trimws(x) else x
}), .SDcols = char_cols]

# 4. Remove duplicate pfun values (keep first occurrence)
if (any(duplicated(dt$pfun))) {
  warning("Found duplicate report IDs. Keeping first occurrence.")
  dt <- unique(dt, by = "pfun")
}

# 5. Sort by pfun
setorder(dt, pfun)

# Store as situas_reports_metadata
situas_reports_metadata <- dt

message("Processed data summary:")
message("  - Total reports: ", nrow(situas_reports_metadata))
message("  - Report ID range: ", min(situas_reports_metadata$pfun),
        " to ", max(situas_reports_metadata$pfun))
message("  - Analysis types: ",
        paste(unique(situas_reports_metadata$analysis_type), collapse = ", "))

# Save as internal package data (sysdata.rda)
message("Saving to R/sysdata.rda...")

# Ensure R directory exists
if (!dir.exists("R")) {
  dir.create("R")
}

# Save as internal data
save(
  situas_reports_metadata,
  file = "R/sysdata.rda",
  compress = "bzip2",
  version = 2  # Use version 2 for better compatibility
)

message("Done! Internal data saved successfully.")
message("\nTo use this data in package functions, simply reference:")
message("  situas_reports_metadata")
