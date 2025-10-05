#' Get Available SITUAS Tables
#'
#' Downloads the list of available tables/reports from the SITUAS API. This
#' function uses caching to enable offline use and reduce unnecessary API calls.
#'
#' @title Get Available SITUAS Tables
#'
#' @description
#' Retrieves the list of available tables and reports from the SITUAS (Sistema
#' Informativo Territoriale Unificato delle Aziende Sanitarie) API. The function
#' implements intelligent caching to minimize API calls and enable offline use.
#' Cached data is stored locally and can be refreshed as needed.
#'
#' @param force_refresh Logical. If `TRUE`, bypass cache and download fresh data
#'   from the API. If `FALSE` (default), use cached data if valid.
#' @param max_age_hours Numeric. Maximum age of cache in hours before automatic
#'   refresh. Default is 24 hours. Ignored if `force_refresh = TRUE`.
#' @param verbose Logical. If `TRUE` (default), print informative messages about
#'   cache status and download progress.
#'
#' @return A `data.table` containing information about available SITUAS tables
#'   and reports. The structure includes columns such as table identifiers,
#'   descriptions, and metadata from the API response.
#'
#' @export
#'
#' @importFrom data.table data.table as.data.table
#' @importFrom httr GET stop_for_status user_agent content status_code http_status
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom tools R_user_dir
#'
#' @examples
#' \dontrun{
#' # Get tables using cache if available
#' tables <- get_situas_tables()
#'
#' # Force refresh from API
#' tables <- get_situas_tables(force_refresh = TRUE)
#'
#' # Use cached data up to 48 hours old
#' tables <- get_situas_tables(max_age_hours = 48)
#'
#' # Suppress messages
#' tables <- get_situas_tables(verbose = FALSE)
#' }
get_situas_tables <- function(force_refresh = FALSE,
                              max_age_hours = 24,
                              verbose = TRUE) {

  # Input validation
  stopifnot(
    "force_refresh must be logical" = is.logical(force_refresh) && length(force_refresh) == 1,
    "max_age_hours must be numeric and positive" = is.numeric(max_age_hours) && length(max_age_hours) == 1 && max_age_hours > 0,
    "verbose must be logical" = is.logical(verbose) && length(verbose) == 1
  )

  # Define cache key
  cache_key <- "situas_tables"

  # Check cache if not forcing refresh
  if (!force_refresh) {
    if (is_cache_valid(cache_key, max_age_hours)) {
      # Load from cache
      cached_data <- load_from_cache(cache_key)

      if (verbose) {
        cache_info <- get_cache_info(cache_key)
        if (!is.null(cache_info$timestamp)) {
          message("Using cached data from ", format(cache_info$timestamp, "%Y-%m-%d %H:%M:%S"))
        } else {
          message("Using cached data")
        }
      }

      return(cached_data)
    }
  }

  # Download fresh data from API
  if (verbose) {
    message("Downloading available tables from SITUAS API...")
  }

  # Call API
  tryCatch({
    response <- situas_api_call("/api/Report/GetFunzione")

    # Parse response
    data <- parse_funzione_response(response)

    # Save to cache
    save_to_cache(data, cache_key)

    if (verbose) {
      message("Successfully downloaded and cached ", nrow(data), " table(s)")
    }

    return(data)

  }, error = function(e) {
    # Enhanced error handling
    stop("Failed to retrieve SITUAS tables: ", conditionMessage(e), call. = FALSE)
  })
}
