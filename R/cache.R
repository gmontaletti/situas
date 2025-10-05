#' Get cache directory for situas package
#'
#' Returns the path to the cache directory for the situas package, creating it
#' if it doesn't exist. Uses the standard R user directory for cache storage.
#'
#' @return A character string containing the absolute path to the cache directory.
#'
#' @keywords internal
#' @noRd
get_cache_dir <- function() {
  cache_dir <- tools::R_user_dir("situas", which = "cache")

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  return(cache_dir)
}


#' Save data to cache
#'
#' Saves a data.table object to the cache directory as an RDS file with a
#' timestamp attribute for tracking cache age.
#'
#' @param data A data.table object to be cached.
#' @param cache_key A character string used as the filename (without extension)
#'   for the cached file.
#'
#' @return Logical. Returns \code{TRUE} if the save operation was successful,
#'   \code{FALSE} if it failed (with a warning message).
#'
#' @keywords internal
#' @noRd
save_to_cache <- function(data, cache_key) {
  stopifnot(
    "data must be provided" = !missing(data),
    "cache_key must be a non-empty character string" =
      is.character(cache_key) && length(cache_key) == 1L && nzchar(cache_key)
  )

  tryCatch({
    cache_dir <- get_cache_dir()
    cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))

    # Add timestamp attribute to track when data was cached
    attr(data, "cache_timestamp") <- Sys.time()

    # Save as RDS file
    saveRDS(data, file = cache_file, compress = TRUE)

    return(TRUE)
  }, error = function(e) {
    warning(
      sprintf("Failed to save data to cache '%s': %s", cache_key, e$message),
      call. = FALSE
    )
    return(FALSE)
  })
}


#' Load data from cache
#'
#' Loads and returns a cached RDS file from the cache directory.
#'
#' @param cache_key A character string identifying the cached file (without
#'   extension) to load.
#'
#' @return The cached object if the file exists, \code{NULL} if the file
#'   doesn't exist or cannot be loaded.
#'
#' @keywords internal
#' @noRd
load_from_cache <- function(cache_key) {
  stopifnot(
    "cache_key must be a non-empty character string" =
      is.character(cache_key) && length(cache_key) == 1L && nzchar(cache_key)
  )

  cache_dir <- get_cache_dir()
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))

  if (!file.exists(cache_file)) {
    return(NULL)
  }

  tryCatch({
    readRDS(cache_file)
  }, error = function(e) {
    warning(
      sprintf("Failed to load cache '%s': %s", cache_key, e$message),
      call. = FALSE
    )
    return(NULL)
  })
}


#' Check if cached data is valid
#'
#' Determines whether a cached file exists and is within the specified maximum
#' age threshold.
#'
#' @param cache_key A character string identifying the cached file (without
#'   extension) to check.
#' @param max_age_hours Numeric. Maximum age in hours for the cache to be
#'   considered valid. Default is 24 hours.
#'
#' @return Logical. Returns \code{TRUE} if the cache file exists and is within
#'   the maximum age threshold, \code{FALSE} otherwise.
#'
#' @keywords internal
#' @noRd
is_cache_valid <- function(cache_key, max_age_hours = 24) {
  stopifnot(
    "cache_key must be a non-empty character string" =
      is.character(cache_key) && length(cache_key) == 1L && nzchar(cache_key),
    "max_age_hours must be a positive numeric value" =
      is.numeric(max_age_hours) && length(max_age_hours) == 1L &&
      max_age_hours > 0
  )

  cache_dir <- get_cache_dir()
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))

  # Check if file exists
  if (!file.exists(cache_file)) {
    return(FALSE)
  }

  # Load cached data to check timestamp
  cached_data <- tryCatch({
    readRDS(cache_file)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(cached_data)) {
    return(FALSE)
  }

  # Get timestamp attribute
  cache_timestamp <- attr(cached_data, "cache_timestamp")

  if (is.null(cache_timestamp)) {
    # If no timestamp, consider cache invalid
    return(FALSE)
  }

  # Calculate age in hours
  cache_age_hours <- as.numeric(difftime(Sys.time(), cache_timestamp,
                                         units = "hours"))

  # Return TRUE if within max age
  return(cache_age_hours <= max_age_hours)
}


#' Get cache information
#'
#' Retrieves metadata information about a cached file, specifically the
#' timestamp indicating when the data was cached.
#'
#' @param cache_key A character string identifying the cached file (without
#'   extension) to retrieve information for.
#'
#' @return A list with a single element \code{timestamp} containing the cache
#'   timestamp as a POSIXct object. If the cache doesn't exist or has no
#'   timestamp attribute, returns a list with \code{timestamp = NULL}.
#'
#' @keywords internal
#' @noRd
get_cache_info <- function(cache_key) {
  stopifnot(
    "cache_key must be a non-empty character string" =
      is.character(cache_key) && length(cache_key) == 1L && nzchar(cache_key)
  )

  # Load cached data
  cached_data <- load_from_cache(cache_key)

  # Return NULL timestamp if cache doesn't exist
  if (is.null(cached_data)) {
    return(list(timestamp = NULL))
  }

  # Extract timestamp attribute
  cache_timestamp <- attr(cached_data, "cache_timestamp")

  # Return as list
  return(list(timestamp = cache_timestamp))
}
