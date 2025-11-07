# Debug script to check cache and API
library(data.table)

# Check cached file
cache_file <- "/Users/giampaolomontaletti/Library/Caches/org.R-project.R/R/situas/situas_tables.rds"

cat("=== Checking Cache ===\n")
if (file.exists(cache_file)) {
  cached_data <- readRDS(cache_file)
  cat("Cache exists\n")
  cat("Class:", class(cached_data), "\n")
  cat("Dimensions:", dim(cached_data), "\n")
  cat("Timestamp:", as.character(attr(cached_data, "cache_timestamp")), "\n")
  print(str(cached_data))
} else {
  cat("No cache file found\n")
}

cat("\n=== Testing API Call ===\n")
# Load package
devtools::load_all(".")

# Try API call with debug info
response <- tryCatch({
  situas_api_call("/api/Report/GetFunzione")
}, error = function(e) {
  cat("API Error:", e$message, "\n")
  NULL
})

if (!is.null(response)) {
  cat("API Response structure:\n")
  print(str(response, max.level = 2))

  cat("\n=== Parsing Response ===\n")
  parsed <- parse_funzione_response(response)
  cat("Parsed dimensions:", dim(parsed), "\n")
  cat("Parsed structure:\n")
  print(str(parsed))
  print(head(parsed))
}
