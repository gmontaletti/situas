# Excel Metadata Integration - Implementation Summary

## Overview

Successfully integrated the SITUAS Excel file (`data-raw/situas_api.xlsx`) as the primary metadata source for all SITUAS API reports. The package now provides user-facing functions to work with, search, and update this metadata.

## What Was Implemented

### 1. New Functions

#### `update_reports_metadata()` (R/update_metadata.R)
- **Purpose**: Read and update the SITUAS reports Excel file
- **Features**:
  - Reads Excel files with Italian column names
  - Validates data structure and content
  - Detects changes from current metadata
  - Saves to internal package data (`R/sysdata.rda`)
  - Can work with custom Excel files
- **Usage**:
  ```r
  # Update from default Excel file
  update_reports_metadata()

  # Update from custom file without saving
  metadata <- update_reports_metadata(
    excel_path = "my_reports.xlsx",
    save_to_package = FALSE
  )
  ```

#### `search_reports()` (R/search_reports.R)
- **Purpose**: Search and filter SITUAS reports by various criteria
- **Features**:
  - Keyword search in titles (case-insensitive)
  - Filter by analysis type (DATA, PERIODO, ATTUALIZZAZIONE)
  - Filter by report ID range
  - Exact or partial title matching
- **Usage**:
  ```r
  # Find all municipality reports
  comuni_reports <- search_reports("comuni")

  # Find only DATA type reports about municipalities
  comuni_data <- search_reports("comuni", analysis_type = "DATA")

  # Find reports in a specific ID range
  reports_60_70 <- search_reports(pfun_range = c(60, 70))
  ```

#### `get_report_details()` (R/search_reports.R)
- **Purpose**: Get detailed information about a specific report by ID
- **Usage**:
  ```r
  report_61 <- get_report_details(61)
  print(report_61$title)
  ```

#### `list_report_categories()` (R/search_reports.R)
- **Purpose**: Browse reports organized by common categories
- **Categories**:
  - municipalities, provinces, regions
  - municipality_characteristics
  - municipality_changes, province_changes, region_changes
  - labor_systems, urban_areas
  - european_nuts, translations
- **Usage**:
  ```r
  categories <- list_report_categories()
  print(categories$municipalities)
  ```

### 2. Enhanced Existing Functions

#### `list_available_reports()`
- Now returns full titles from internal metadata
- Includes `date_range` column
- Shows ATTUALIZZAZIONE type (not just DATA/PERIODO)

#### `get_situas_tables()`
- Enhanced verbose messages with report title and validity dates
- Better error messages using report titles

### 3. Testing

Created comprehensive test suites:
- **test-update_metadata.R**: 7 tests for Excel file reading and processing
- **test-search_reports.R**: 63 tests for all search and filter functionality

All tests passing: **63/63 ✓**

### 4. Package Dependencies

Added `openxlsx` to DESCRIPTION Imports for Excel file reading.

### 5. Documentation

All functions fully documented with roxygen2:
- Detailed parameter descriptions
- Return value specifications
- Multiple usage examples
- Type information (DATA, PERIODO, ATTUALIZZAZIONE explained)

## Excel File Structure

The package expects the Excel file (`data-raw/situas_api.xlsx`) to have these columns:

| Italian Column Name | English Name | Description |
|---------------------|--------------|-------------|
| Id report | pfun | Report ID (integer) |
| Titolo report | title | Full report title |
| Inizio/fine validità report | date_range | Validity period (DD/MM/YYYY - DD/MM/YYYY) |
| Analisi temporale | analysis_type | DATA, PERIODO, or ATTUALIZZAZIONE |
| Informazioni | info | Additional information (optional) |

Currently contains: **65 reports** (IDs 42-451)

## Analysis Types Explained

1. **DATA**: Single date parameter
   - Example: Get municipalities list as of 2020-01-01
   - Used for: Current territorial units, characteristics

2. **PERIODO**: Date range parameters (start and end)
   - Example: Get municipality changes from 2020 to 2025
   - Used for: Changes, suppressions, creations

3. **ATTUALIZZAZIONE**: Actualization/translation between dates
   - Example: Translate municipal codes from one period to another
   - Used for: Code translation, composition comparisons

## How to Update Metadata

When ISTAT adds new reports to the SITUAS API:

1. Update the Excel file (`data-raw/situas_api.xlsx`) with new report information
2. Run: `update_reports_metadata()`
3. Reload the package: `devtools::load_all()` or restart R session

Alternatively, run the data preparation script:
```r
source("data-raw/prepare_reports_data.R")
```

## Files Modified/Created

### New Files
- `R/update_metadata.R` - Update functions
- `R/search_reports.R` - Search and browse functions
- `R/globals.R` - Global variable declarations
- `tests/testthat/test-update_metadata.R` - Update tests
- `tests/testthat/test-search_reports.R` - Search tests

### Modified Files
- `R/list_reports.R` - Enhanced to use full titles
- `R/get_tables.R` - Improved error messages
- `DESCRIPTION` - Added openxlsx dependency
- `NAMESPACE` - Exported new functions

### Documentation Generated
- `man/update_reports_metadata.Rd`
- `man/search_reports.Rd`
- `man/get_report_details.Rd`
- `man/list_report_categories.Rd`
- `man/list_available_reports.Rd` (updated)

## Code Quality

- ✓ All non-ASCII characters replaced with Unicode escapes
- ✓ All data.table global variables properly declared
- ✓ Comprehensive input validation
- ✓ Informative error messages
- ✓ All functions documented
- ✓ All tests passing

## Example Workflows

### Workflow 1: Discover and Use a Report

```r
# Search for reports about municipalities
reports <- search_reports("comuni")

# View results
print(reports[, .(pfun, title, analysis_type)])

# Get details for a specific report
details <- get_report_details(61)

# Download the data
data <- get_situas_tables(pfun = 61, verbose = TRUE)
```

### Workflow 2: Find Translation Reports

```r
# Get all translation/actualization reports
translations <- search_reports(analysis_type = "ATTUALIZZAZIONE")

# Or use the category function
categories <- list_report_categories()
print(categories$translations)
```

### Workflow 3: Update Metadata from New Excel File

```r
# Download new Excel file from ISTAT
# Save as "situas_reports_2025.xlsx"

# Update metadata
new_metadata <- update_reports_metadata(
  excel_path = "situas_reports_2025.xlsx",
  verbose = TRUE
)

# Review changes
print(new_metadata[, .(pfun, title)])
```

## Next Steps (Optional Enhancements)

1. Add vignette with usage examples
2. Create function to download latest report list from ISTAT website
3. Add caching for report metadata queries
4. Create helper functions for common report combinations
5. Add multilingual support (translate Italian titles to English)

## Bug Fix Applied

Fixed an issue where `list_available_reports()` was not properly using metadata from the Excel file. The problem was using `!!pfun` (rlang/tidyverse non-standard evaluation) instead of proper data.table filtering syntax `situas_reports_metadata$pfun == pfun_value`.

After the fix:
- ✓ All 65 reports show correct titles from Excel
- ✓ All 3 analysis types properly recognized (DATA: 46, PERIODO: 11, ATTUALIZZAZIONE: 8)
- ✓ Both `list_available_reports()` and `search_reports()` return consistent data

## Summary

The package now fully leverages the Excel file for report metadata, making it easy to:
- ✓ Search and discover reports
- ✓ Update metadata when new reports are added
- ✓ Browse reports by category
- ✓ Get detailed information about any report
- ✓ Maintain consistency across the package

All functionality is tested, documented, and ready to use!
