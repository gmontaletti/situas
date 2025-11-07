#' Update SITUAS Reports Metadata from Excel File
#'
#' Reads a new or updated version of the SITUAS API reports Excel file and
#' updates the internal package metadata. This function is useful when ISTAT
#' adds new reports to the SITUAS API or when you have an updated version of
#' the reports list.
#'
#' @param excel_path Character string. Path to the Excel file containing the
#'   updated SITUAS reports list. If NULL (default), uses the file in the
#'   package's data-raw directory (\code{data-raw/situas_api.xlsx}).
#' @param save_to_package Logical. If TRUE (default), saves the updated metadata
#'   to the package's internal data file (\code{R/sysdata.rda}). If FALSE,
#'   only returns the processed metadata without saving. Set to FALSE if you
#'   don't have write permissions to the package directory.
#' @param verbose Logical. If TRUE (default), prints informative messages about
#'   the update process.
#'
#' @return A data.table containing the processed reports metadata with columns:
#'   \describe{
#'     \item{pfun}{Integer. Report ID (function number)}
#'     \item{title}{Character. Full title of the report in Italian}
#'     \item{date_range}{Character. Validity period of the report}
#'     \item{analysis_type}{Character. Type of temporal analysis (DATA, PERIODO, or ATTUALIZZAZIONE)}
#'     \item{info}{Character. Additional information about the report}
#'   }
#'
#' @details
#' The Excel file must have the following columns (in Italian):
#' \itemize{
#'   \item \strong{Id report}: Report ID number (integer)
#'   \item \strong{Titolo report}: Full title of the report
#'   \item \strong{Inizio/fine validita' report}: Date range in format DD/MM/YYYY - DD/MM/YYYY
#'   \item \strong{Analisi temporale}: Analysis type (DATA, PERIODO, or ATTUALIZZAZIONE)
#'   \item \strong{Informazioni}: Additional information (optional)
#' }
#'
#' Note: Column names may appear with dots instead of spaces (e.g., "Id.report")
#' due to R's data frame naming conventions.
#'
#' @export
#'
#' @importFrom data.table as.data.table setnames setorder
#'
#' @examples
#' \dontrun{
#' # Update from the default package Excel file
#' new_metadata <- update_reports_metadata()
#'
#' # Update from a custom Excel file (without saving to package)
#' custom_metadata <- update_reports_metadata(
#'   excel_path = "path/to/my_situas_reports.xlsx",
#'   save_to_package = FALSE
#' )
#'
#' # Update silently
#' update_reports_metadata(verbose = FALSE)
#' }
update_reports_metadata <- function(excel_path = NULL,
                                     save_to_package = TRUE,
                                     verbose = TRUE) {
  # Input validation
  stopifnot(
    "save_to_package must be logical" =
      is.logical(save_to_package) && length(save_to_package) == 1,
    "verbose must be logical" =
      is.logical(verbose) && length(verbose) == 1
  )

  # Determine Excel file path
  if (is.null(excel_path)) {
    # Use package default path
    pkg_root <- find_package_root()
    excel_path <- file.path(pkg_root, "data-raw", "situas_api.xlsx")

    if (verbose) {
      message("Using default Excel file: ", excel_path)
    }
  } else {
    stopifnot(
      "excel_path must be a character string" =
        is.character(excel_path) && length(excel_path) == 1
    )
  }

  # Check if file exists
  if (!file.exists(excel_path)) {
    stop(
      "Excel file not found at: ", excel_path,
      "\nPlease provide a valid path to the SITUAS reports Excel file.",
      call. = FALSE
    )
  }

  if (verbose) {
    message("Reading Excel file...")
  }

  # Read Excel file
  raw_data <- tryCatch(
    {
      openxlsx::read.xlsx(
        excel_path,
        sheet = 1,
        detectDates = FALSE  # Keep dates as character strings
      )
    },
    error = function(e) {
      stop(
        "Failed to read Excel file: ", e$message,
        "\nPlease ensure the file is a valid Excel file (.xlsx format).",
        call. = FALSE
      )
    }
  )

  if (verbose) {
    message("Processing ", nrow(raw_data), " reports...")
  }

  # Convert to data.table
  dt <- data.table::as.data.table(raw_data)

  # Map column names from Italian to English
  # Handle both space and dot separators in column names
  old_names <- names(dt)

  # Create mapping for potential column name variations
  name_mapping <- list(
    pfun = c("Id.report", "Id report", "Id_report"),
    title = c("Titolo.report", "Titolo report", "Titolo_report"),
    date_range = c("Inizio/fine.validit\u00e0.report", "Inizio/fine validit\u00e0 report",
                   "Inizio.fine.validit\u00e0.report", "Inizio.fine.validita.report"),
    analysis_type = c("Analisi.temporale", "Analisi temporale", "Analisi_temporale"),
    info = c("Informazioni", "Informazioni")
  )

  # Find and rename columns
  new_names <- old_names
  for (target_name in names(name_mapping)) {
    for (possible_name in name_mapping[[target_name]]) {
      if (possible_name %in% old_names) {
        new_names[old_names == possible_name] <- target_name
        break
      }
    }
  }

  # Apply new names
  data.table::setnames(dt, old_names, new_names)

  # Validate required columns
  required_cols <- c("pfun", "title", "date_range", "analysis_type")
  missing_cols <- setdiff(required_cols, names(dt))

  if (length(missing_cols) > 0) {
    stop(
      "Excel file is missing required columns: ", paste(missing_cols, collapse = ", "),
      "\nExpected columns: Id report, Titolo report, Inizio/fine validita' report, Analisi temporale",
      call. = FALSE
    )
  }

  # Clean and validate data
  # 1. Ensure pfun is integer
  dt[, pfun := as.integer(pfun)]

  # 2. Remove any rows with missing pfun
  rows_before <- nrow(dt)
  dt <- dt[!is.na(pfun)]
  if (nrow(dt) < rows_before && verbose) {
    message("Removed ", rows_before - nrow(dt), " rows with missing report IDs")
  }

  # 3. Trim whitespace from character columns
  char_cols <- intersect(c("title", "date_range", "analysis_type", "info"), names(dt))
  dt[, (char_cols) := lapply(.SD, function(x) {
    if (is.character(x)) trimws(x) else x
  }), .SDcols = char_cols]

  # 4. Validate analysis_type values
  valid_types <- c("DATA", "PERIODO", "ATTUALIZZAZIONE")
  invalid_types <- unique(dt$analysis_type[!dt$analysis_type %in% valid_types])
  if (length(invalid_types) > 0) {
    warning(
      "Found invalid analysis_type values: ", paste(invalid_types, collapse = ", "),
      "\nValid values are: ", paste(valid_types, collapse = ", "),
      call. = FALSE
    )
  }

  # 5. Remove duplicate pfun values (keep first occurrence)
  if (any(duplicated(dt$pfun))) {
    n_dupes <- sum(duplicated(dt$pfun))
    if (verbose) {
      message("Found ", n_dupes, " duplicate report ID(s). Keeping first occurrence.")
    }
    dt <- unique(dt, by = "pfun")
  }

  # 6. Sort by pfun
  data.table::setorder(dt, pfun)

  # Print summary
  if (verbose) {
    message("\nProcessed metadata summary:")
    message("  - Total reports: ", nrow(dt))
    message("  - Report ID range: ", min(dt$pfun), " to ", max(dt$pfun))
    message("  - Analysis types: ", paste(unique(dt$analysis_type), collapse = ", "))
  }

  # Compare with existing metadata if available
  if (exists("situas_reports_metadata", envir = asNamespace("situas"))) {
    old_metadata <- get("situas_reports_metadata", envir = asNamespace("situas"))

    # Find new reports
    new_pfuns <- setdiff(dt$pfun, old_metadata$pfun)
    removed_pfuns <- setdiff(old_metadata$pfun, dt$pfun)

    if (verbose && (length(new_pfuns) > 0 || length(removed_pfuns) > 0)) {
      message("\nChanges from current metadata:")
      if (length(new_pfuns) > 0) {
        message("  - New reports: ", paste(new_pfuns, collapse = ", "))
      }
      if (length(removed_pfuns) > 0) {
        message("  - Removed reports: ", paste(removed_pfuns, collapse = ", "))
      }
    }
  }

  # Save to package if requested
  if (save_to_package) {
    pkg_root <- find_package_root()
    sysdata_path <- file.path(pkg_root, "R", "sysdata.rda")

    if (verbose) {
      message("\nSaving to ", sysdata_path, "...")
    }

    tryCatch(
      {
        situas_reports_metadata <- dt
        save(
          situas_reports_metadata,
          file = sysdata_path,
          compress = "bzip2",
          version = 2
        )

        if (verbose) {
          message("Metadata saved successfully!")
          message("\nIMPORTANT: Reload the package to use the new metadata:")
          message("  devtools::load_all()  # or restart R session")
        }
      },
      error = function(e) {
        warning(
          "Failed to save metadata to package: ", e$message,
          "\nYou may not have write permissions to the package directory.",
          call. = FALSE
        )
      }
    )
  }

  return(dt)
}


#' Find Package Root Directory
#'
#' Searches for the package root directory by looking for the DESCRIPTION file.
#'
#' @return Character string with the path to the package root, or stops with
#'   an error if not found.
#'
#' @keywords internal
#' @noRd
find_package_root <- function() {
  # Start from current working directory
  current_dir <- getwd()

  # Search up to 5 levels up
  for (i in 1:5) {
    desc_file <- file.path(current_dir, "DESCRIPTION")

    if (file.exists(desc_file)) {
      # Verify it's the situas package
      tryCatch(
        {
          desc_content <- readLines(desc_file, n = 10)
          if (any(grepl("^Package:\\s*situas", desc_content))) {
            return(current_dir)
          }
        },
        error = function(e) {
          # Continue searching
        }
      )
    }

    # Move up one directory
    parent_dir <- dirname(current_dir)
    if (parent_dir == current_dir) {
      # Reached root
      break
    }
    current_dir <- parent_dir
  }

  stop(
    "Could not find package root directory. ",
    "Please run this function from within the package directory structure.",
    call. = FALSE
  )
}
