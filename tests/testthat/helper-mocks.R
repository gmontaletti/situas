# Helper functions and mock data for situas package tests

# Mock API Response Generators
# These functions create realistic mock responses for testing API client functions

#' Create a mock SITUAS API response
#'
#' Generates a mock response structure matching the SITUAS API GetFunzione endpoint.
#'
#' @param n_items Integer. Number of items to include in the response.
#' @param include_metadata Logical. Whether to include additional metadata fields.
#' @return A list structure matching SITUAS API response format
mock_funzione_response <- function(n_items = 3, include_metadata = FALSE) {
  items <- lapply(seq_len(n_items), function(i) {
    item <- list(
      id = i,
      name = paste0("Table", i),
      description = paste0("Description for table ", i),
      type = "report",
      category = paste0("Category", (i %% 3) + 1)
    )

    if (include_metadata) {
      item$metadata <- list(
        created = "2024-01-01",
        updated = "2024-01-15",
        version = "1.0"
      )
    }

    item
  })

  list(
    items = items,
    total = n_items,
    status = "success"
  )
}

#' Create a mock empty API response
#'
#' Generates an empty but valid API response structure.
#'
#' @return A list with empty items
mock_empty_response <- function() {
  list(
    items = list(),
    total = 0,
    status = "success"
  )
}

#' Create a mock cached data.table
#'
#' Generates a data.table with cache timestamp attribute for testing cache functions.
#'
#' @param n_rows Integer. Number of rows to include.
#' @param age_hours Numeric. Age of the cache in hours (for timestamp).
#' @return A data.table with cache_timestamp attribute
mock_cached_data <- function(n_rows = 3, age_hours = 0) {
  dt <- data.table::data.table(
    id = seq_len(n_rows),
    name = paste0("Item", seq_len(n_rows)),
    description = paste0("Description ", seq_len(n_rows))
  )

  # Set timestamp to specified age
  timestamp <- Sys.time() - as.difftime(age_hours, units = "hours")
  attr(dt, "cache_timestamp") <- timestamp

  dt
}

#' Create a mock HTTP response object
#'
#' Generates a mock httr response object for testing API calls.
#'
#' @param status_code Integer. HTTP status code.
#' @param content Character. JSON content string.
#' @return A mock response object
mock_http_response <- function(status_code = 200,
                                content = '{"items":[],"status":"success"}') {
  structure(
    list(
      status_code = status_code,
      headers = list(`content-type` = "application/json"),
      content = charToRaw(content)
    ),
    class = "response"
  )
}

# Test Data Fixtures
# Predefined test datasets for consistent testing

#' Sample SITUAS tables data
#'
#' A sample dataset representing typical SITUAS table metadata
#'
#' @format A data.table with 5 rows and 4 columns:
#' \describe{
#'   \item{id}{Integer identifier}
#'   \item{name}{Table name}
#'   \item{description}{Table description}
#'   \item{category}{Table category}
#' }
sample_tables_data <- function() {
  data.table::data.table(
    id = 1:5,
    name = c(
      "Comuni",
      "Province",
      "Regioni",
      "ASL",
      "Distretti"
    ),
    description = c(
      "Elenco dei comuni italiani",
      "Elenco delle province",
      "Elenco delle regioni",
      "Aziende Sanitarie Locali",
      "Distretti sanitari"
    ),
    category = c(
      "Territorial",
      "Territorial",
      "Territorial",
      "Health",
      "Health"
    )
  )
}

# Test Utilities
# Helper functions for common test operations

#' Create a temporary cache directory
#'
#' Sets up a temporary directory for cache testing and returns its path.
#' The directory is automatically cleaned up after the test.
#'
#' @return Character. Path to temporary cache directory
setup_temp_cache <- function() {
  temp_dir <- withr::local_tempdir()
  cache_dir <- file.path(temp_dir, "situas_cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_dir
}

#' Save mock data to cache for testing
#'
#' Saves a data.table to a cache file with proper timestamp attribute.
#'
#' @param data A data.table to cache
#' @param cache_key Character. Cache key (filename without extension)
#' @param cache_dir Character. Directory where cache should be saved
#' @param age_hours Numeric. Age in hours for the cache timestamp
save_mock_cache <- function(data, cache_key, cache_dir, age_hours = 0) {
  timestamp <- Sys.time() - as.difftime(age_hours, units = "hours")
  attr(data, "cache_timestamp") <- timestamp

  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
  saveRDS(data, file = cache_file, compress = TRUE)

  invisible(cache_file)
}

#' Check if mockery package is available
#'
#' Helper to skip tests if mockery is not installed
#'
#' @return Logical. TRUE if mockery is installed, FALSE otherwise
has_mockery <- function() {
  requireNamespace("mockery", quietly = TRUE)
}

# Expected Error Messages
# Constants for error message testing

ERROR_MESSAGES <- list(
  cache_key_invalid = "cache_key must be a non-empty character string",
  max_age_invalid = "max_age_hours must be a positive numeric value",
  force_refresh_invalid = "force_refresh must be logical",
  verbose_invalid = "verbose must be logical",
  data_missing = "data must be provided"
)
