# Example: Download Italian Labor Ministry Classification Standards
#
# This script demonstrates how to use the download_classification_standards()
# function to download official classification files from the Italian Ministry
# of Labor and Social Policies.
#
# Author: Giampaolo Montaletti

# Load the package
library(situas)
library(data.table)

# 1. Basic usage: Download latest versions -----
# This will download only the latest version of each classification type
# to the default directory (data-raw/classifications)

files <- download_classification_standards()

# View what was downloaded
print(files)

# Check file details
files[, .(filename, version, type, file_size_mb = round(file_size / 1024^2, 2))]


# 2. Download to custom directory -----

custom_dir <- "my_classifications"
files <- download_classification_standards(
  output_dir = custom_dir
)


# 3. Download specific classification types -----

# Download only the main classification standards
files <- download_classification_standards(
  classification_types = "classificazioni_standard"
)

# Download multiple types
files <- download_classification_standards(
  classification_types = c("classificazioni_standard", "allegato")
)


# 4. Download all versions (not just latest) -----

# This will download all available versions of each classification type
files <- download_classification_standards(
  download_all = TRUE
)


# 5. Force re-download -----

# Re-download files even if they already exist locally
files <- download_classification_standards(
  force_refresh = TRUE
)


# 6. Silent mode -----

# Download without progress messages
files <- download_classification_standards(
  verbose = FALSE
)


# 7. Check download metadata -----

# After downloading, you can load the metadata to see what was downloaded
metadata <- readRDS("data-raw/classifications/download_metadata.rds")

# View metadata
str(metadata)

# When were files downloaded?
metadata$download_date

# What was the source URL?
metadata$source_url

# What R version was used?
metadata$r_version

# File details
print(metadata$files)


# 8. Practical workflow -----

# Download latest classification standards
files <- download_classification_standards(
  output_dir = "data-raw/classifications"
)

# Check what versions were downloaded
cat("\nDownloaded files:\n")
for (i in seq_len(nrow(files))) {
  cat(sprintf(
    "  %s (version %s, type: %s, size: %.2f MB)\n",
    files$filename[i],
    ifelse(is.na(files$version[i]), "N/A", files$version[i]),
    files$type[i],
    files$file_size[i] / 1024^2
  ))
}

# Now you can read the Excel files with openxlsx or readxl
# For example:
# library(openxlsx)
# class_data <- read.xlsx(files$file_path[1], sheet = 1)


# 9. Error handling -----

# The function will error if:
# - No files are found on the page
# - Network connection fails
# - Required packages (rvest, polite) are not installed
# - Invalid parameters are provided

# You can wrap in tryCatch for production code:
result <- tryCatch(
  {
    download_classification_standards()
  },
  error = function(e) {
    message("Failed to download: ", e$message)
    NULL
  }
)

if (is.null(result)) {
  message("Download failed, using cached files...")
  # Load from cache or handle error
}


# 10. Check for updates -----

# To check if new versions are available without downloading,
# you can compare with previously downloaded metadata

# Load previous metadata
if (file.exists("data-raw/classifications/download_metadata.rds")) {
  old_metadata <- readRDS("data-raw/classifications/download_metadata.rds")

  # Download current files (with verbose = FALSE to avoid clutter)
  new_files <- download_classification_standards(verbose = FALSE)

  # Compare versions
  if (nrow(new_files) > 0 && nrow(old_metadata$files) > 0) {
    old_versions <- old_metadata$files[type == "classificazioni_standard", version]
    new_versions <- new_files[type == "classificazioni_standard", version]

    if (length(old_versions) > 0 && length(new_versions) > 0) {
      if (max(new_versions) > max(old_versions)) {
        message("New version available!")
      } else {
        message("Already have latest version")
      }
    }
  }
}
