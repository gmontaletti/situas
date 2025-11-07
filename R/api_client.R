#' Get SITUAS Base URL
#'
#' Returns the base URL for the SITUAS public API services.
#'
#' @return A character string containing the base URL
#'
#' @keywords internal
#' @noRd
situas_base_url <- function() {
  "https://situas-servizi.istat.it/publish/"
}


#' Format Date for SITUAS API
#'
#' Converts a Date object or character string to the DD/MM/YYYY format required
#' by the SITUAS API.
#'
#' @param date A Date object, POSIXct, or character string. If character, it
#'   should be in a format parsable by \code{as.Date()}.
#'
#' @return A character string in DD/MM/YYYY format
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' format_situas_date(Sys.Date())
#' format_situas_date(as.Date("2025-10-05"))
#' format_situas_date("2025-10-05")
#' }
format_situas_date <- function(date) {
  if (is.character(date)) {
    date <- as.Date(date)
  }
  format(date, "%d/%m/%Y")
}


#' Make API Call to SITUAS
#'
#' Constructs a URL and makes an HTTP GET request to the SITUAS public API,
#' returning the parsed JSON response.
#'
#' @param endpoint Character string specifying the API endpoint
#'   (e.g., "reportspooljson")
#' @param query_params Named list of query parameters to include in the request
#'
#' @return A list containing the parsed JSON response
#'
#' @keywords internal
#' @noRd
situas_api_call <- function(endpoint, query_params = list()) {
  # Construct full URL
  base_url <- situas_base_url()
  full_url <- paste0(base_url, endpoint)

  # Set user agent
  user_agent <- httr::user_agent(
    "situas R package (https://github.com/gmontaletti/situas)"
  )

  # Make API call with error handling
  response <- tryCatch(
    {
      httr::GET(full_url, query = query_params, user_agent)
    },
    error = function(e) {
      stop(
        "Network error when calling SITUAS API: ", e$message,
        "\nURL: ", full_url,
        call. = FALSE
      )
    }
  )

  # Check HTTP status
  tryCatch(
    {
      httr::stop_for_status(response)
    },
    error = function(e) {
      status <- httr::status_code(response)
      msg <- httr::http_status(response)$message

      # Try to extract error message from response
      content <- tryCatch({
        httr::content(response, as = "text", encoding = "UTF-8")
      }, error = function(e) NULL)

      if (!is.null(content) && nzchar(content)) {
        parsed <- tryCatch({
          jsonlite::fromJSON(content, simplifyVector = FALSE)
        }, error = function(e) NULL)

        if (!is.null(parsed) && !is.null(parsed$message)) {
          msg <- parsed$message
        }
      }

      stop(
        "SITUAS API returned an error (HTTP ", status, "): ", msg,
        "\nURL: ", full_url,
        call. = FALSE
      )
    }
  )

  # Parse JSON response
  content <- tryCatch(
    {
      httr::content(response, as = "text", encoding = "UTF-8")
    },
    error = function(e) {
      stop(
        "Failed to extract content from API response: ", e$message,
        call. = FALSE
      )
    }
  )

  parsed <- tryCatch(
    {
      jsonlite::fromJSON(content, simplifyVector = FALSE)
    },
    error = function(e) {
      stop(
        "Failed to parse JSON response from SITUAS API: ", e$message,
        call. = FALSE
      )
    }
  )

  return(parsed)
}


#' Parse SITUAS Resultset Response
#'
#' Extracts and converts the "resultset" element from a SITUAS API response
#' to a data.table.
#'
#' @param response List containing the parsed API response with a "resultset"
#'   element
#'
#' @return A data.table containing the items from the response, or an empty
#'   data.table with 0 rows if the response is empty or NULL
#'
#' @keywords internal
#' @noRd
parse_resultset_response <- function(response) {
  # Check if response is NULL or doesn't contain resultset
  if (is.null(response) || is.null(response$resultset)) {
    return(data.table::data.table())
  }

  # Extract resultset
  resultset <- response$resultset

  # Check if resultset is empty
  if (length(resultset) == 0) {
    return(data.table::data.table())
  }

  # Convert to data.table
  dt <- tryCatch(
    {
      # If resultset is an atomic vector (named or unnamed), convert to list
      # This prevents jsonlite deprecated warning about keep_vec_names
      if (is.atomic(resultset) && !is.list(resultset)) {
        resultset <- as.list(resultset)
      }

      # Convert list to data.frame first (jsonlite standard approach)
      df <- jsonlite::fromJSON(
        jsonlite::toJSON(resultset, auto_unbox = TRUE),
        simplifyDataFrame = TRUE
      )
      data.table::as.data.table(df)
    },
    error = function(e) {
      stop(
        "Failed to convert API response resultset to data.table: ", e$message,
        call. = FALSE
      )
    }
  )

  return(dt)
}


#' Get SITUAS Report Data
#'
#' Downloads data from a specific SITUAS report. Supports both DATA type reports
#' (single date) and PERIODO type reports (date range).
#'
#' @param pfun Integer. Report ID (function number). See \code{\link{list_available_reports}}
#'   for a list of valid report IDs.
#' @param date Date, POSIXct, or character string. The reference date for the
#'   report. For DATA type reports, this is the single date parameter. For
#'   PERIODO type reports, this is the start date. Defaults to current date.
#' @param date_end Date, POSIXct, or character string. End date for PERIODO type
#'   reports. Leave NULL for DATA type reports. Defaults to NULL.
#'
#' @return A data.table containing the report data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get current municipalities list (DATA type report)
#' municipalities <- situas_get_report_data(pfun = 61)
#'
#' # Get municipalities as of a specific date
#' municipalities_2020 <- situas_get_report_data(
#'   pfun = 61,
#'   date = as.Date("2020-01-01")
#' )
#'
#' # Get municipality changes over a period (PERIODO type report)
#' changes <- situas_get_report_data(
#'   pfun = 99,
#'   date = as.Date("2020-01-01"),
#'   date_end = as.Date("2025-01-01")
#' )
#' }
situas_get_report_data <- function(pfun,
                                    date = Sys.Date(),
                                    date_end = NULL) {
  # Input validation
  stopifnot(
    "pfun must be a single integer" =
      is.numeric(pfun) && length(pfun) == 1 && pfun == as.integer(pfun),
    "date must be provided" = !is.null(date)
  )

  # Format dates
  pdata <- format_situas_date(date)

  # Build query parameters
  if (is.null(date_end)) {
    # DATA type report
    query_params <- list(pfun = pfun, pdata = pdata)
  } else {
    # PERIODO type report
    pdataa <- format_situas_date(date_end)
    query_params <- list(pfun = pfun, pdatada = pdata, pdataa = pdataa)
  }

  # Call API
  response <- situas_api_call("reportspooljson", query_params)

  # Parse and return
  parse_resultset_response(response)
}


#' Get SITUAS Report Row Count
#'
#' Returns the number of rows that would be returned by a SITUAS report query
#' without downloading the full dataset.
#'
#' @inheritParams situas_get_report_data
#'
#' @return Integer. The number of rows in the report.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Check how many municipalities exist today
#' count <- situas_get_report_count(pfun = 61)
#' print(count)  # Should be around 7896
#'
#' # Check report size before downloading
#' if (situas_get_report_count(pfun = 73) < 10000) {
#'   data <- situas_get_report_data(pfun = 73)
#' }
#' }
situas_get_report_count <- function(pfun,
                                     date = Sys.Date(),
                                     date_end = NULL) {
  # Input validation
  stopifnot(
    "pfun must be a single integer" =
      is.numeric(pfun) && length(pfun) == 1 && pfun == as.integer(pfun),
    "date must be provided" = !is.null(date)
  )

  # Format dates
  pdata <- format_situas_date(date)

  # Build query parameters
  if (is.null(date_end)) {
    # DATA type report
    query_params <- list(pfun = pfun, pdata = pdata)
  } else {
    # PERIODO type report
    pdataa <- format_situas_date(date_end)
    query_params <- list(pfun = pfun, pdatada = pdata, pdataa = pdataa)
  }

  # Call API
  response <- situas_api_call("reportspooljsoncount", query_params)

  # Extract count
  if (is.null(response$resultset) || length(response$resultset) == 0) {
    return(0L)
  }

  count <- response$resultset[[1]]$NUM_ROWS

  if (is.null(count)) {
    return(0L)
  }

  return(as.integer(count))
}


#' Get SITUAS Report Metadata
#'
#' Returns detailed metadata about a SITUAS report, including column descriptions,
#' data sources, and field information.
#'
#' @inheritParams situas_get_report_data
#'
#' @return A list containing report metadata including:
#'   \describe{
#'     \item{REPORT_NAME}{Report title}
#'     \item{REPORT_DESCRIZIONE}{Report description}
#'     \item{COL_DETAILS}{List of column metadata}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get metadata for municipalities report
#' metadata <- situas_get_report_metadata(pfun = 61)
#' cat("Report:", metadata$`REPORT NAME`, "\n")
#' cat("Description:", metadata$`REPORT DESCRIZIONE`, "\n")
#'
#' # List all columns
#' if (!is.null(metadata$COL_DETAILS)) {
#'   for (col in metadata$COL_DETAILS) {
#'     cat(col$COLNAME, ":", col$LABEL, "\n")
#'   }
#' }
#' }
situas_get_report_metadata <- function(pfun,
                                        date = Sys.Date(),
                                        date_end = NULL) {
  # Input validation
  stopifnot(
    "pfun must be a single integer" =
      is.numeric(pfun) && length(pfun) == 1 && pfun == as.integer(pfun),
    "date must be provided" = !is.null(date)
  )

  # Format dates
  pdata <- format_situas_date(date)

  # Build query parameters
  if (is.null(date_end)) {
    # DATA type report
    query_params <- list(pfun = pfun, pdata = pdata)
  } else {
    # PERIODO type report
    pdataa <- format_situas_date(date_end)
    query_params <- list(pfun = pfun, pdatada = pdata, pdataa = pdataa)
  }

  # Call API
  response <- situas_api_call("anagrafica_report_metadato_web", query_params)

  return(response)
}
