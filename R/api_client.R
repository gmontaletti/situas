#' Get SITUAS Base URL
#'
#' Returns the base URL for the SITUAS API.
#'
#' @return A character string containing the base URL
#'
#' @keywords internal
situas_base_url <- function() {
  "https://situas.istat.it/ShibO2Module"
}


#' Make API Call to SITUAS
#'
#' Constructs a full URL from the base URL and endpoint, makes an HTTP GET
#' request to the SITUAS API, and parses the JSON response.
#'
#' @param endpoint Character string specifying the API endpoint
#'   (e.g., "/api/Report/GetFunzione")
#' @param ... Additional arguments passed to \code{httr::GET()}, such as
#'   query parameters
#'
#' @return A list containing the parsed JSON response
#'
#' @keywords internal
situas_api_call <- function(endpoint, ...) {
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
      httr::GET(full_url, user_agent, ...)
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
      stop(
        "SITUAS API returned an error (HTTP ", httr::status_code(response), "): ",
        httr::http_status(response)$message,
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


#' Parse GetFunzione API Response
#'
#' Extracts and converts the "items" element from a SITUAS API response
#' to a data.table.
#'
#' @param response List containing the parsed API response
#'
#' @return A data.table containing the items from the response, or an empty
#'   data.table with 0 rows if the response is empty or NULL
#'
#' @keywords internal
parse_funzione_response <- function(response) {
  # Check if response is NULL or doesn't contain items
  if (is.null(response) || is.null(response$items)) {
    return(data.table::data.table())
  }

  # Extract items
  items <- response$items

  # Check if items is empty
  if (length(items) == 0) {
    return(data.table::data.table())
  }

  # Convert to data.table
  dt <- tryCatch(
    {
      # Convert list to data.frame first (jsonlite standard approach)
      df <- jsonlite::fromJSON(
        jsonlite::toJSON(items, auto_unbox = TRUE),
        simplifyDataFrame = TRUE
      )
      data.table::as.data.table(df)
    },
    error = function(e) {
      stop(
        "Failed to convert API response items to data.table: ", e$message,
        call. = FALSE
      )
    }
  )

  return(dt)
}
