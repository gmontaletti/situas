# Simple test script for API without devtools
library(httr)
library(jsonlite)
library(data.table)

# Source the functions directly
source("R/api_client.R")

cat("=== Testing Direct API Call ===\n")
response <- tryCatch({
  situas_api_call("/api/Report/GetFunzione")
}, error = function(e) {
  cat("API Error:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(response)) {
  cat("\nAPI Response structure (first 2 levels):\n")
  print(str(response, max.level = 2))

  cat("\n=== Checking for 'items' field ===\n")
  cat("Has 'items':", "items" %in% names(response), "\n")

  if ("items" %in% names(response)) {
    cat("Items length:", length(response$items), "\n")
    if (length(response$items) > 0) {
      cat("\nFirst item structure:\n")
      print(str(response$items[[1]]))
    }
  }

  cat("\n=== Testing parse_funzione_response ===\n")
  parsed <- parse_funzione_response(response)
  cat("Parsed class:", class(parsed), "\n")
  cat("Parsed dimensions:", dim(parsed), "\n")

  if (nrow(parsed) > 0) {
    cat("\nColumn names:\n")
    print(names(parsed))
    cat("\nFirst few rows:\n")
    print(head(parsed))
  } else {
    cat("\nParsed data is empty!\n")
  }
}
