# Download Classification Standards

This document describes the `download_classification_standards()` function and related functionality for downloading Italian Labor Ministry classification standard files.

## Overview

The Italian Ministry of Labor and Social Policies maintains official classification standards used in administrative data systems (URP - Unità Raccolta Permessi). These classifications are published as Excel files at:

https://urponline.lavoro.gov.it/s/standard-tecnici/classificazioni-standard

The `download_classification_standards()` function automates:
- Scraping the web page to find available files
- Parsing version numbers from filenames
- Downloading only the latest versions (or all versions if requested)
- Saving metadata about downloaded files

## Installation Requirements

The function requires two additional packages for web scraping:

```r
install.packages(c("rvest", "polite"))
```

These are listed as "Suggests" dependencies, so they're optional if you don't need this functionality.

## Basic Usage

```r
library(situas)

# Download latest versions to default directory
files <- download_classification_standards()
```

This will:
1. Scrape the Labor Ministry website
2. Find all Excel classification files
3. Identify the latest version of each type
4. Download to `data-raw/classifications/`
5. Save metadata to `data-raw/classifications/download_metadata.rds`
6. Return a data.table with file information

## File Types

The function recognizes three main classification types:

### 1. classificazioni_standard
Main classification standards file (e.g., `Rev.090-ST-Classificazioni-Standard.xlsx`).

Contains multiple sheets with various classification codes:
- Professional qualifications (CP)
- Economic activities (ATECO)
- Job titles
- Educational qualifications
- And more

### 2. allegato
Annex files with additional information or mappings (e.g., `AllegatoA_AnalisiMigrazioneCp2011-Cp2021.xlsx`).

### 3. migrazione
Migration analysis files showing changes between classification versions.

## Function Parameters

```r
download_classification_standards(
  output_dir = "data-raw/classifications",  # Where to save files
  url = "https://urponline.lavoro.gov.it/s/standard-tecnici/classificazioni-standard?language=it",
  download_all = FALSE,          # Download all versions or just latest?
  force_refresh = FALSE,         # Re-download existing files?
  use_js_scraping = FALSE,       # Use JavaScript scraping? (not yet implemented)
  classification_types = NULL,   # Filter by type(s)?
  verbose = TRUE                 # Print progress messages?
)
```

## Return Value

Returns a data.table (invisibly) with:
- `filename`: Downloaded filename
- `url`: Source URL
- `link_text`: Original link text from webpage
- `version`: Extracted version number (NA if not parseable)
- `type`: Classification type
- `file_path`: Full path to downloaded file
- `download_date`: Timestamp when downloaded
- `file_size`: File size in bytes

## Examples

### Download to Custom Directory

```r
files <- download_classification_standards(
  output_dir = "my_data/classifications"
)
```

### Download Specific Types

```r
# Only main classification standards
files <- download_classification_standards(
  classification_types = "classificazioni_standard"
)

# Multiple types
files <- download_classification_standards(
  classification_types = c("classificazioni_standard", "allegato")
)
```

### Download All Versions

By default, only the latest version of each type is downloaded. To get all versions:

```r
files <- download_classification_standards(
  download_all = TRUE
)
```

### Force Re-download

```r
# Update files even if they already exist
files <- download_classification_standards(
  force_refresh = TRUE
)
```

### Silent Mode

```r
files <- download_classification_standards(
  verbose = FALSE
)
```

## Metadata

After downloading, a metadata file is saved containing:

```r
metadata <- readRDS("data-raw/classifications/download_metadata.rds")

# Structure:
# - download_date: When files were downloaded
# - source_url: URL of the source page
# - r_version: R version used
# - package_version: situas package version
# - files: data.table with file information
```

This metadata can be used to:
- Track when files were last updated
- Check for new versions
- Reproduce the download environment

## Version Parsing

The function automatically parses version numbers from filenames:

| Filename | Version | Type |
|----------|---------|------|
| `Rev.090-ST-Classificazioni-Standard.xlsx` | 90 | classificazioni_standard |
| `Rev.005-ST-Classificazioni-Standard-1.xlsx` | 5 | classificazioni_standard |
| `AllegatoA_AnalisiMigrazioneCp2011-Cp2021.xlsx` | NA | allegato |

When `download_all = FALSE`, the function keeps only the file with the highest version number for each type.

## Error Handling

The function will error if:

1. **No files found**: Page structure changed or requires authentication
   ```r
   # Error: No Excel files found on page.
   # Page may require JavaScript, authentication, or has changed structure.
   ```

2. **Network error**: Connection failed
   ```r
   # Error: Failed to connect to URL: ...
   ```

3. **Download failed**: Individual file download error
   ```r
   # Error: Failed to download <filename>
   ```

4. **Missing packages**: Required packages not installed
   ```r
   # Error: Package 'rvest' is required for web scraping.
   # Install it with: install.packages('rvest')
   ```

### Handling Errors

```r
result <- tryCatch(
  {
    download_classification_standards()
  },
  error = function(e) {
    message("Download failed: ", e$message)
    message("Try downloading manually from:")
    message("https://urponline.lavoro.gov.it/s/standard-tecnici/classificazioni-standard")
    NULL
  }
)
```

## Reading Downloaded Files

After downloading, you can read the Excel files with standard R packages:

```r
library(openxlsx)

# Get latest classification standards file
files <- download_classification_standards(
  classification_types = "classificazioni_standard"
)

# Read specific sheet
cp_data <- read.xlsx(
  files$file_path[1],
  sheet = "CP2021"  # Professional qualifications 2021
)

# List all sheets
sheet_names <- getSheetNames(files$file_path[1])
print(sheet_names)

# Read multiple sheets
all_sheets <- lapply(sheet_names, function(sheet) {
  read.xlsx(files$file_path[1], sheet = sheet)
})
names(all_sheets) <- sheet_names
```

## Typical Workflow

```r
library(situas)
library(openxlsx)
library(data.table)

# 1. Download latest classification standards
files <- download_classification_standards(
  classification_types = "classificazioni_standard"
)

# 2. Check what was downloaded
cat("Downloaded:", files$filename[1], "\n")
cat("Version:", files$version[1], "\n")
cat("Size:", round(files$file_size[1] / 1024^2, 2), "MB\n")

# 3. Read the file
class_file <- files$file_path[1]
sheet_names <- getSheetNames(class_file)

# 4. Load professional qualifications (CP2021)
cp2021 <- as.data.table(read.xlsx(class_file, sheet = "CP2021"))

# 5. Explore the data
print(head(cp2021))
print(names(cp2021))

# 6. Use in your analysis
# ... your code here ...
```

## Maintenance Notes

### Website Changes

The Labor Ministry website is a Salesforce Community portal that may:
- Change HTML structure
- Require JavaScript rendering
- Update file naming conventions
- Move to a different URL

If scraping fails, you can:
1. Download files manually
2. Set `use_js_scraping = TRUE` (when implemented)
3. File an issue at: https://github.com/gmontaletti/situas/issues

### JavaScript Scraping

JavaScript-enabled scraping is planned but not yet implemented. It will require:
- RSelenium (Selenium WebDriver)
- Or chromote (Chrome DevTools Protocol)

To enable when available:
```r
files <- download_classification_standards(
  use_js_scraping = TRUE
)
```

## Helper Functions

The implementation uses three internal helper functions:

### parse_classification_version()
Extracts version number and type from filename.

```r
# Internal use only (not exported)
situas:::parse_classification_version("Rev.090-ST-Classificazioni-Standard.xlsx")
# Returns: list(version = 90, type = "classificazioni_standard")
```

### scrape_classification_links()
Scrapes the web page for Excel file links.

```r
# Internal use only
situas:::scrape_classification_links(
  url = "https://urponline.lavoro.gov.it/...",
  verbose = TRUE
)
# Returns: data.table with filename, url, link_text
```

### download_classification_file()
Downloads a single file with error handling.

```r
# Internal use only
situas:::download_classification_file(
  url = "...",
  filename = "file.xlsx",
  output_dir = "data-raw",
  force = FALSE,
  verbose = TRUE
)
# Returns: path to downloaded file
```

## Testing

Comprehensive tests are included in `tests/testthat/test-download_classifications.R`:

```r
# Run tests
devtools::test_file("tests/testthat/test-download_classifications.R")
```

Tests cover:
- Version parsing from various filename formats
- Link scraping with mocked web responses
- File download logic
- Input validation
- Type filtering
- Latest version selection
- Metadata saving

## See Also

- SITUAS API documentation: [CLAUDE.md](.claude/CLAUDE.md)
- Package documentation: `?download_classification_standards`
- Example script: [examples/download_classifications_example.R](examples/download_classifications_example.R)
- Official source: https://urponline.lavoro.gov.it/s/standard-tecnici/classificazioni-standard

## Author

Giampaolo Montaletti (giampaolo.montaletti@gmail.com)

ORCID: https://orcid.org/0009-0002-5327-1122

GitHub: https://github.com/gmontaletti
