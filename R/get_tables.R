#' Get Available SITUAS Tables
#'
#' Downloads the list of available territorial units from the SITUAS API. This
#' function uses caching to enable offline use and reduce unnecessary API calls.
#' By default, it retrieves the municipalities list (report ID 61).
#'
#' @title Get Available SITUAS Tables
#'
#' @description
#' Retrieves territorial unit data from the SITUAS (Sistema Informativo Territoriale
#' delle Unità Amministrative e Statistiche) API. The function implements intelligent
#' caching to minimize API calls and enable offline use. Cached data is stored
#' locally and can be refreshed as needed.
#'
#' This is a convenience wrapper around \code{\link{situas_get_report_data}} that
#' provides caching functionality and defaults to retrieving the municipalities
#' list (report ID 61).
#'
#' @param pfun Integer. Report ID to retrieve. Defaults to 61 (municipalities list).
#'   See \code{\link{list_available_reports}} for all available reports.
#' @param date Date, POSIXct, or character string. The reference date for the
#'   report. For DATA type reports, this is the single date parameter. For
#'   PERIODO/ATTUALIZZAZIONE type reports, this is the start date. Defaults to
#'   current date.
#' @param date_end Date, POSIXct, or character string. End date for PERIODO and
#'   ATTUALIZZAZIONE type reports. Required for these report types, ignored for
#'   DATA type reports. Default is \code{NULL}.
#' @param force_refresh Logical. If \code{TRUE}, bypass cache and download fresh
#'   data from the API. If \code{FALSE} (default), use cached data if valid.
#' @param max_age_hours Numeric. Maximum age of cache in hours before automatic
#'   refresh. Default is 24 hours. Ignored if \code{force_refresh = TRUE}.
#' @param verbose Logical. If \code{TRUE} (default), print informative messages
#'   about cache status and download progress.
#'
#' @return A \code{data.table} containing the requested report data. The structure
#'   depends on the report ID requested. For the default municipalities report
#'   (ID 61), it includes columns such as:
#'   \describe{
#'     \item{COD_RIP}{Geographic partition code}
#'     \item{COD_REG}{Region code}
#'     \item{COD_UTS}{Province/UTS code}
#'     \item{PRO_COM}{Municipality code (numeric)}
#'     \item{PRO_COM_T}{Municipality code (text)}
#'     \item{COMUNE}{Municipality name}
#'     \item{DEN_REG}{Region name}
#'     \item{DEN_UTS}{Province/UTS name}
#'     \item{SIGLA_AUTOMOBILISTICA}{Car registration code}
#'     \item{COD_CATASTO}{Cadastral code}
#'   }
#'
#' @export
#'
#' @importFrom data.table data.table as.data.table
#'
#' @examples
#' \dontrun{
#' # Get current municipalities list using cache if available
#' municipalities <- get_situas_tables()
#'
#' # Force refresh from API
#' municipalities <- get_situas_tables(force_refresh = TRUE)
#'
#' # Get municipalities as of a specific date
#' municipalities_2020 <- get_situas_tables(date = as.Date("2020-01-01"))
#'
#' # Get provinces instead of municipalities
#' provinces <- get_situas_tables(pfun = 64)
#'
#' # Get municipality changes over a period (PERIODO type report)
#' changes <- get_situas_tables(
#'   pfun = 99,
#'   date = as.Date("2020-01-01"),
#'   date_end = as.Date("2025-01-01")
#' )
#'
#' # Use cached data up to 48 hours old
#' municipalities <- get_situas_tables(max_age_hours = 48)
#'
#' # Suppress messages
#' municipalities <- get_situas_tables(verbose = FALSE)
#' }
get_situas_tables <- function(pfun = 61,
                              date = Sys.Date(),
                              date_end = NULL,
                              force_refresh = FALSE,
                              max_age_hours = 24,
                              verbose = TRUE) {

  # Input validation
  stopifnot(
    "pfun must be a single integer" =
      is.numeric(pfun) && length(pfun) == 1 && pfun == as.integer(pfun),
    "force_refresh must be logical" =
      is.logical(force_refresh) && length(force_refresh) == 1 && !is.na(force_refresh),
    "max_age_hours must be numeric and positive" =
      is.numeric(max_age_hours) && length(max_age_hours) == 1 && max_age_hours > 0,
    "verbose must be logical" =
      is.logical(verbose) && length(verbose) == 1 && !is.na(verbose)
  )

  # 1. Validate pfun against known reports -----
  # Store pfun value to avoid column name collision in data.table filtering
  pfun_value <- as.integer(pfun)
  report_info <- situas_reports_metadata[pfun == pfun_value]

  if (nrow(report_info) == 0) {
    # Unknown report ID - provide helpful error message with suggestions
    available_pfuns <- situas_reports_metadata$pfun

    # Find closest report IDs (within +/- 10)
    nearby_pfuns <- available_pfuns[abs(available_pfuns - pfun) <= 10]

    error_msg <- sprintf(
      "Report ID %d is not in the list of known SITUAS reports.\n  Available report IDs range from %d to %d.\n  Total available reports: %d",
      pfun,
      min(available_pfuns),
      max(available_pfuns),
      length(available_pfuns)
    )

    if (length(nearby_pfuns) > 0) {
      error_msg <- paste0(
        error_msg,
        sprintf("\n  Nearby report IDs: %s", paste(sort(nearby_pfuns), collapse = ", "))
      )
    }

    error_msg <- paste0(
      error_msg,
      "\n  Use list_available_reports() to see all available reports."
    )

    stop(error_msg, call. = FALSE)
  }

  # 2. Intelligent type detection and date parameter validation -----
  # Extract report type (DATA, PERIODO, or ATTUALIZZAZIONE)
  report_type <- report_info$analysis_type[1]

  # Check if report requires date_end parameter
  if (report_type %in% c("PERIODO", "ATTUALIZZAZIONE")) {
    if (is.null(date_end)) {
      stop(
        sprintf(
          "Report %d is a %s type report and requires both date (start) and date_end parameters",
          pfun, report_type
        ),
        call. = FALSE
      )
    }
  } else if (report_type == "DATA") {
    # Warn if date_end is provided for DATA type report
    if (!is.null(date_end)) {
      warning(
        sprintf(
          "Report %d is a DATA type report. The date_end parameter will be ignored.",
          pfun
        ),
        call. = FALSE
      )
      # Set date_end to NULL to ensure proper cache key generation
      date_end <- NULL
    }
  }

  # 3. Provide informative message about the report -----
  if (verbose) {
    message(
      sprintf("Report %d: %s", pfun, report_info$title[1])
    )
    message(
      sprintf("  Type: %s | Valid: %s",
              report_info$analysis_type[1],
              report_info$date_range[1])
    )
  }

  # 4. Define cache key based on report type and date parameters -----
  date_str <- format(as.Date(date), "%Y%m%d")

  # For PERIODO/ATTUALIZZAZIONE reports, include date_end in cache key
  if (!is.null(date_end)) {
    date_end_str <- format(as.Date(date_end), "%Y%m%d")
    cache_key <- paste0("situas_report_", pfun, "_", date_str, "_", date_end_str)
  } else {
    # For DATA reports, use only single date
    cache_key <- paste0("situas_report_", pfun, "_", date_str)
  }

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

  # 5. Download fresh data from API -----
  if (verbose) {
    if (!is.null(date_end)) {
      # Show date range for PERIODO/ATTUALIZZAZIONE reports
      message(
        sprintf("Downloading data for period %s to %s...",
                format(as.Date(date), "%Y-%m-%d"),
                format(as.Date(date_end), "%Y-%m-%d"))
      )
    } else {
      # Show single date for DATA reports
      message("Downloading data for date ", format(as.Date(date), "%Y-%m-%d"), "...")
    }
  }

  # Call API with appropriate parameters
  tryCatch({
    data <- situas_get_report_data(pfun = pfun, date = date, date_end = date_end)

    # Save to cache
    save_to_cache(data, cache_key)

    if (verbose) {
      message("Successfully downloaded and cached ", nrow(data), " row(s)")
    }

    return(data)

  }, error = function(e) {
    # Enhanced error handling
    stop("Failed to retrieve SITUAS report ", pfun, ": ", conditionMessage(e), call. = FALSE)
  })
}
