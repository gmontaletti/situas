#' Search SITUAS Reports by Keywords
#'
#' Searches through available SITUAS reports using keywords in titles or
#' descriptions. Supports filtering by analysis type and date range. This
#' function provides a convenient way to discover relevant reports without
#' knowing their specific IDs.
#'
#' @param keywords Character vector or single string. Keywords to search for
#'   in report titles. If NULL (default), returns all reports. Case-insensitive.
#'   Multiple keywords are combined with OR logic.
#' @param analysis_type Character vector. Filter by analysis type. Valid values
#'   are "DATA", "PERIODO", and "ATTUALIZZAZIONE". If NULL (default), includes
#'   all types.
#' @param pfun_range Integer vector of length 2. Filter reports by ID range,
#'   e.g., c(60, 80) returns reports with IDs between 60 and 80 (inclusive).
#'   If NULL (default), includes all IDs.
#' @param exact_match Logical. If TRUE, only returns reports where the entire
#'   title matches the keyword (case-insensitive). If FALSE (default), returns
#'   reports containing any of the keywords.
#'
#' @return A data.table with columns:
#'   \describe{
#'     \item{pfun}{Report ID (integer)}
#'     \item{title}{Full report title in Italian (character)}
#'     \item{analysis_type}{Analysis type: DATA, PERIODO, or ATTUALIZZAZIONE (character)}
#'     \item{date_range}{Validity period of the report (character)}
#'     \item{info}{Additional information (character, may be NA)}
#'   }
#'
#' @details
#' \strong{Analysis Types:}
#' \itemize{
#'   \item \strong{DATA}: Single date parameter. Use for reports that show
#'     territorial units at a specific date.
#'   \item \strong{PERIODO}: Date range parameters (start and end). Use for
#'     reports showing changes over a period.
#'   \item \strong{ATTUALIZZAZIONE}: Actualization/translation between dates.
#'     Uses date range like PERIODO but focuses on code translations.
#' }
#'
#' @export
#'
#' @importFrom data.table data.table copy
#'
#' @examples
#' \dontrun{
#' # Find all reports about municipalities (comuni)
#' comuni_reports <- search_reports("comuni")
#'
#' # Find reports about provinces or regions
#' admin_reports <- search_reports(c("province", "regioni"))
#'
#' # Find only DATA type reports about municipalities
#' comuni_data <- search_reports("comuni", analysis_type = "DATA")
#'
#' # Find reports in a specific ID range
#' reports_60_70 <- search_reports(pfun_range = c(60, 70))
#'
#' # Find exact title match
#' exact_report <- search_reports("Elenco Regioni", exact_match = TRUE)
#'
#' # Browse all available reports
#' all_reports <- search_reports()
#' }
search_reports <- function(keywords = NULL,
                           analysis_type = NULL,
                           pfun_range = NULL,
                           exact_match = FALSE) {
  # Input validation
  if (!is.null(keywords)) {
    stopifnot(
      "keywords must be a character vector" = is.character(keywords)
    )
  }

  if (!is.null(analysis_type)) {
    stopifnot(
      "analysis_type must be a character vector" = is.character(analysis_type)
    )

    valid_types <- c("DATA", "PERIODO", "ATTUALIZZAZIONE")
    invalid_types <- setdiff(analysis_type, valid_types)

    if (length(invalid_types) > 0) {
      stop(
        "Invalid analysis_type values: ", paste(invalid_types, collapse = ", "),
        "\nValid values are: ", paste(valid_types, collapse = ", "),
        call. = FALSE
      )
    }
  }

  if (!is.null(pfun_range)) {
    stopifnot(
      "pfun_range must be an integer vector of length 2" =
        is.numeric(pfun_range) && length(pfun_range) == 2,
      "pfun_range values must be positive" =
        all(pfun_range > 0)
    )
    pfun_range <- as.integer(pfun_range)
  }

  stopifnot(
    "exact_match must be logical" =
      is.logical(exact_match) && length(exact_match) == 1
  )

  # Start with all reports
  result <- data.table::copy(situas_reports_metadata)

  # Apply keyword filter
  if (!is.null(keywords)) {
    if (exact_match) {
      # Exact match: title must equal one of the keywords (case-insensitive)
      matches <- result[, tolower(title) %in% tolower(keywords)]
      result <- result[matches]
    } else {
      # Partial match: title must contain at least one keyword
      # Create regex pattern for multiple keywords (OR logic)
      pattern <- paste(keywords, collapse = "|")

      # Case-insensitive search
      matches <- result[, grepl(pattern, title, ignore.case = TRUE)]
      result <- result[matches]
    }
  }

  # Apply analysis_type filter
  if (!is.null(analysis_type)) {
    # Use data.table filtering
    analysis_type_filter <- analysis_type
    result <- result[analysis_type %in% analysis_type_filter]
  }

  # Apply pfun_range filter
  if (!is.null(pfun_range)) {
    min_pfun <- min(pfun_range)
    max_pfun <- max(pfun_range)
    result <- result[pfun >= min_pfun & pfun <= max_pfun]
  }

  # Return sorted by pfun
  data.table::setorder(result, pfun)

  return(result)
}


#' Get Report Details by ID
#'
#' Retrieves detailed information about a specific SITUAS report by its ID.
#' This is a convenience function that returns metadata for a single report.
#'
#' @param pfun Integer. Report ID to look up.
#'
#' @return A data.table with one row containing report details, or an empty
#'   data.table if the report ID is not found. Columns are the same as
#'   \code{\link{search_reports}}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get details for the municipalities report (ID 61)
#' report_61 <- get_report_details(61)
#' print(report_61$title)
#'
#' # Check if a report exists
#' if (nrow(get_report_details(999)) == 0) {
#'   message("Report 999 does not exist")
#' }
#' }
get_report_details <- function(pfun) {
  stopifnot(
    "pfun must be a single integer" =
      is.numeric(pfun) && length(pfun) == 1 && pfun == as.integer(pfun)
  )

  pfun_value <- as.integer(pfun)

  # Filter by pfun using data.table syntax
  result <- situas_reports_metadata[pfun == pfun_value]

  return(result)
}


#' List Reports by Category
#'
#' Returns a named list of commonly used report categories with their
#' corresponding report IDs and titles. This provides a quick reference
#' to the most commonly used SITUAS reports.
#'
#' @return A named list where each element corresponds to a report category.
#'   Each category contains a data.table with report details.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get categorized reports
#' categories <- list_report_categories()
#'
#' # View municipalities reports
#' print(categories$municipalities)
#'
#' # View all category names
#' names(categories)
#' }
list_report_categories <- function() {
  categories <- list(
    # Current territorial units (DATA type)
    municipalities = search_reports(
      keywords = c("Elenco dei codici e delle denominazioni delle unit\u00e0 territoriali"),
      analysis_type = "DATA",
      exact_match = TRUE
    ),

    provinces = search_reports(
      keywords = c("Elenco Province", "Province/Uts"),
      analysis_type = "DATA"
    ),

    regions = search_reports(
      keywords = "Elenco Regioni",
      analysis_type = "DATA",
      exact_match = TRUE
    ),

    # Municipality characteristics
    municipality_characteristics = search_reports(
      keywords = c("Comuni - Caratteristiche", "Comuni - Dimensione", "Comuni - Aree"),
      analysis_type = "DATA"
    ),

    # Changes over time (PERIODO type)
    municipality_changes = search_reports(
      keywords = c("Comuni soppressi", "Comuni costituiti", "Comuni con variazioni",
                   "Comuni con cambio denominazione", "Traslazione Comuni"),
      analysis_type = c("PERIODO", "ATTUALIZZAZIONE")
    ),

    province_changes = search_reports(
      keywords = c("Province-Uts soppresse", "Province-Uts costituite",
                   "Province-Uts con cambio denominazione"),
      analysis_type = "PERIODO"
    ),

    region_changes = search_reports(
      keywords = c("Regioni soppresse", "Regioni costituite",
                   "Regioni con cambio denominazione", "Compartimenti"),
      analysis_type = "PERIODO"
    ),

    # Labor market systems
    labor_systems = search_reports(
      keywords = "Sistemi Locali del Lavoro",
      analysis_type = "DATA"
    ),

    # Functional urban areas
    urban_areas = search_reports(
      keywords = c("Zone urbane funzionali", "Citt\u00e0"),
      analysis_type = "DATA"
    ),

    # European nomenclatures
    european_nuts = search_reports(
      keywords = c("Nuts", "Nomenclature europee")
    ),

    # Translation/actualization tools
    translations = search_reports(
      analysis_type = "ATTUALIZZAZIONE"
    )
  )

  return(categories)
}
