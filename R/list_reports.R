#' List All Available SITUAS Reports
#'
#' Returns a data.table containing information about all available SITUAS reports,
#' including report IDs, names, descriptions, types, and row counts. This function
#' can work with a built-in list of known SITUAS reports or explore custom report
#' IDs. When exploring custom IDs, it can automatically detect whether each report
#' uses DATA (single date) or PERIODO (date range) format.
#'
#' @param date Date, POSIXct, or character string. The reference date for checking
#'   report availability and row counts. Defaults to current date.
#' @param verbose Logical. If TRUE, print progress messages while fetching report
#'   information. Defaults to FALSE.
#' @param report_ids Vector of integer report IDs to explore. If NULL (default),
#'   uses built-in list of known SITUAS reports. Provide custom IDs to explore
#'   specific reports or discover new ones.
#' @param auto_detect_type Logical. If TRUE (default), automatically detects
#'   whether each report ID uses DATA (single date) or PERIODO (date range) format
#'   by trying both. If FALSE, uses known types or defaults to DATA format for
#'   unknown IDs. Only relevant when report_ids is provided.
#'
#' @return A data.table with columns:
#'   \describe{
#'     \item{pfun}{Report ID (integer)}
#'     \item{title}{Full report title in Italian (character)}
#'     \item{type}{Report type: "DATA", "PERIODO", or "ATTUALIZZAZIONE" (character)}
#'     \item{date_range}{Validity period of the report (character)}
#'     \item{row_count}{Number of rows at the given date (integer)}
#'   }
#'
#' @export
#'
#' @importFrom data.table data.table rbindlist
#'
#' @examples
#' \dontrun{
#' # Get list of all available reports (using built-in list)
#' reports <- list_available_reports()
#' print(reports)
#'
#' # Show only DATA type reports
#' data_reports <- reports[type == "DATA"]
#'
#' # Find municipality-related reports
#' munic_reports <- reports[grepl("omun", title, ignore.case = TRUE)]
#'
#' # Explore specific custom report IDs with auto-detection
#' custom_reports <- list_available_reports(report_ids = c(80, 85, 90))
#'
#' # Explore custom IDs without auto-detection (faster but less flexible)
#' known_reports <- list_available_reports(report_ids = c(61, 63), auto_detect_type = FALSE)
#'
#' # Discover potentially new report IDs with verbose output
#' new_reports <- list_available_reports(report_ids = 150:160, verbose = TRUE)
#' }
list_available_reports <- function(date = Sys.Date(),
                                   verbose = FALSE,
                                   report_ids = NULL,
                                   auto_detect_type = TRUE) {
  # Determine which IDs to use
  if (is.null(report_ids)) {
    # Use all reports from metadata
    all_reports <- situas_reports_metadata$pfun
    use_auto_detect <- FALSE  # We already know the types from metadata
  } else {
    # Use custom report IDs
    all_reports <- as.integer(report_ids)
    use_auto_detect <- auto_detect_type
  }

  # Create list to store report info
  report_list <- list()

  for (pfun in all_reports) {
    if (verbose) {
      message("Fetching info for report ", pfun, "...")
    }

    tryCatch({
      # Determine report type
      if (use_auto_detect) {
        # Auto-detect type: try DATA first, then PERIODO
        count_data <- tryCatch({
          situas_get_report_count(pfun = pfun, date = date)
        }, error = function(e) {
          NULL
        })

        if (!is.null(count_data) && !is.na(count_data)) {
          # DATA type worked
          is_data_type <- TRUE
          count <- count_data
          metadata <- situas_get_report_metadata(pfun = pfun, date = date)
        } else {
          # Try PERIODO type
          date_start <- as.Date(date) - 365
          count <- situas_get_report_count(
            pfun = pfun,
            date = date_start,
            date_end = date
          )
          metadata <- situas_get_report_metadata(
            pfun = pfun,
            date = date_start,
            date_end = date
          )
          is_data_type <- FALSE
        }
      } else {
        # Use type from metadata (or default to DATA for unknown IDs)
        pfun_value <- pfun
        report_info <- situas_reports_metadata[situas_reports_metadata$pfun == pfun_value, ]

        if (nrow(report_info) > 0) {
          # Known report - use metadata type
          # ATTUALIZZAZIONE uses same API call pattern as PERIODO (date range)
          is_data_type <- report_info$analysis_type[1] == "DATA"
        } else {
          # Unknown report - default to DATA
          is_data_type <- TRUE
        }

        # Get count
        if (is_data_type) {
          count <- situas_get_report_count(pfun = pfun, date = date)
          metadata <- situas_get_report_metadata(pfun = pfun, date = date)
        } else {
          # For PERIODO/ATTUALIZZAZIONE type, use a 1-year range
          date_start <- as.Date(date) - 365
          count <- situas_get_report_count(
            pfun = pfun,
            date = date_start,
            date_end = date
          )
          metadata <- situas_get_report_metadata(
            pfun = pfun,
            date = date_start,
            date_end = date
          )
        }
      }

      # Extract metadata from API response
      api_name <- metadata$`REPORT NAME` %||% NA_character_

      # Get title and date_range from internal metadata
      pfun_value <- pfun
      report_info <- situas_reports_metadata[situas_reports_metadata$pfun == pfun_value, ]

      if (nrow(report_info) > 0) {
        title <- report_info$title[1]
        date_range <- report_info$date_range[1]
        # Use internal metadata type if available (includes ATTUALIZZAZIONE)
        report_type <- report_info$analysis_type[1]
      } else {
        # Fallback to API name if not in internal metadata
        title <- api_name
        date_range <- NA_character_
        report_type <- if (is_data_type) "DATA" else "PERIODO"
      }

      report_list[[length(report_list) + 1]] <- data.table::data.table(
        pfun = pfun,
        title = title,
        type = report_type,
        date_range = date_range,
        row_count = count
      )
    }, error = function(e) {
      if (verbose) {
        warning("Failed to fetch info for report ", pfun, ": ", e$message)
      }
      # Add entry with NA values
      # Determine type for error case
      if (use_auto_detect) {
        type_for_error <- "DATA"  # Default to DATA when auto-detecting fails
      } else {
        # Use type from metadata if available
        pfun_value <- pfun
        report_info <- situas_reports_metadata[situas_reports_metadata$pfun == pfun_value, ]
        if (nrow(report_info) > 0) {
          type_for_error <- report_info$analysis_type[1]
        } else {
          type_for_error <- "DATA"  # Default for unknown reports
        }
      }

      # Get title from internal metadata if available
      pfun_value <- pfun
      report_info <- situas_reports_metadata[situas_reports_metadata$pfun == pfun_value, ]

      if (nrow(report_info) > 0) {
        title <- report_info$title[1]
        date_range <- report_info$date_range[1]
      } else {
        title <- NA_character_
        date_range <- NA_character_
      }

      report_list[[length(report_list) + 1]] <<- data.table::data.table(
        pfun = pfun,
        title = title,
        type = type_for_error,
        date_range = date_range,
        row_count = NA_integer_
      )
    })
  }

  # Combine all reports into single data.table
  result <- data.table::rbindlist(report_list)

  return(result)
}


#' Scan for Valid SITUAS Report IDs
#'
#' Automatically discovers valid report IDs by testing a range of IDs against
#' the SITUAS API. This function can be used to find new reports that may have
#' been added to the API.
#'
#' @param max_id Integer. Maximum report ID to test. Defaults to 200.
#' @param date Date, POSIXct, or character string. The reference date for testing
#'   reports. Defaults to current date.
#' @param verbose Logical. If TRUE, print progress messages during scanning.
#'   Defaults to TRUE.
#'
#' @return A data.table with columns:
#'   \describe{
#'     \item{pfun}{Report ID (integer)}
#'     \item{type}{Report type: "DATA" or "PERIODO" (character)}
#'     \item{row_count}{Number of rows at the given date (integer)}
#'   }
#'
#' @export
#'
#' @importFrom data.table data.table rbindlist
#'
#' @examples
#' \dontrun{
#' # Scan for all valid reports up to ID 150
#' valid_reports <- scan_situas_reports(max_id = 150)
#' print(valid_reports)
#'
#' # Scan without progress messages
#' valid_reports <- scan_situas_reports(max_id = 100, verbose = FALSE)
#' }
scan_situas_reports <- function(max_id = 200,
                                 date = Sys.Date(),
                                 verbose = TRUE) {
  stopifnot(
    "max_id must be a positive integer" =
      is.numeric(max_id) && max_id > 0 && max_id == as.integer(max_id)
  )

  if (verbose) {
    message("Scanning SITUAS API for valid report IDs up to ", max_id, "...")
  }

  valid_reports <- list()

  for (pfun in 1:max_id) {
    if (verbose && pfun %% 10 == 0) {
      message("  Testing ID ", pfun, "/", max_id, "...")
    }

    # Try DATA type first (single date)
    count_data <- tryCatch({
      situas_get_report_count(pfun = pfun, date = date)
    }, error = function(e) {
      NULL
    })

    if (!is.null(count_data) && !is.na(count_data)) {
      valid_reports[[length(valid_reports) + 1]] <- data.table::data.table(
        pfun = pfun,
        type = "DATA",
        row_count = count_data
      )
      next
    }

    # Try PERIODO type (date range)
    date_start <- as.Date(date) - 365
    count_periodo <- tryCatch({
      situas_get_report_count(pfun = pfun, date = date_start, date_end = date)
    }, error = function(e) {
      NULL
    })

    if (!is.null(count_periodo) && !is.na(count_periodo)) {
      valid_reports[[length(valid_reports) + 1]] <- data.table::data.table(
        pfun = pfun,
        type = "PERIODO",
        row_count = count_periodo
      )
    }
  }

  if (length(valid_reports) == 0) {
    if (verbose) {
      message("No valid reports found.")
    }
    return(data.table::data.table(
      pfun = integer(0),
      type = character(0),
      row_count = integer(0)
    ))
  }

  result <- data.table::rbindlist(valid_reports)

  if (verbose) {
    message("Found ", nrow(result), " valid report IDs")
  }

  return(result)
}


# Helper function for null coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
