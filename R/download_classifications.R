# 1. Helper: Parse Classification Version -----

#' Parse Classification Version from Filename
#'
#' Extracts version number and classification type from Italian Labor Ministry
#' classification standard filenames.
#'
#' @param filename Character string. The filename to parse.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{version}{Integer version number extracted from "Rev.XXX" pattern,
#'       or NA if not found}
#'     \item{type}{Character classification type ("classificazioni_standard",
#'       "allegato", or "unknown")}
#'   }
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' parse_classification_version("Rev.090-ST-Classificazioni-Standard.xlsx")
#' # Returns: list(version = 90, type = "classificazioni_standard")
#'
#' parse_classification_version("Rev.005-ST-Classificazioni-Standard-1.xlsx")
#' # Returns: list(version = 5, type = "classificazioni_standard")
#'
#' parse_classification_version("AllegatoA_AnalisiMigrazioneCp2011-Cp2021.xlsx")
#' # Returns: list(version = NA, type = "allegato")
#' }
parse_classification_version <- function(filename) {
  # Initialize result
  result <- list(version = NA_integer_, type = "unknown")

  # Extract version from "Rev.XXX" pattern
  version_match <- regexpr("Rev\\.([0-9]+)", filename, perl = TRUE)
  if (version_match > 0) {
    # Extract the number part
    version_text <- regmatches(filename, version_match)
    version_num <- gsub("Rev\\.", "", version_text)
    result$version <- as.integer(version_num)
  }

  # Determine classification type
  if (grepl("ST-Classificazioni-Standard", filename, ignore.case = TRUE)) {
    result$type <- "classificazioni_standard"
  } else if (grepl("Allegato", filename, ignore.case = TRUE)) {
    result$type <- "allegato"
  } else if (grepl("Migrazione", filename, ignore.case = TRUE)) {
    result$type <- "migrazione"
  }

  return(result)
}


# 2. Helper: Scrape Classification Links -----

#' Scrape Classification Links from Web Page
#'
#' Extracts links to Excel classification files from the Italian Labor Ministry
#' classification standards page.
#'
#' @param url Character. URL of the classifications page.
#' @param use_js Logical. If TRUE, use JavaScript-enabled scraping (requires
#'   RSelenium or chromote). Default is FALSE.
#' @param verbose Logical. Print progress messages. Default is TRUE.
#'
#' @return A data.table with columns:
#'   \describe{
#'     \item{filename}{Basename of the file}
#'     \item{url}{Full or relative URL to the file}
#'     \item{link_text}{Visible text of the link}
#'   }
#'
#' @keywords internal
#' @noRd
scrape_classification_links <- function(url,
                                       use_js = FALSE,
                                       verbose = TRUE) {
  # Check for required packages
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop(
      "Package 'rvest' is required for web scraping. ",
      "Install it with: install.packages('rvest')",
      call. = FALSE
    )
  }

  if (!requireNamespace("polite", quietly = TRUE)) {
    stop(
      "Package 'polite' is required for respectful web scraping. ",
      "Install it with: install.packages('polite')",
      call. = FALSE
    )
  }

  # JavaScript scraping not yet implemented
  if (use_js) {
    if (!requireNamespace("RSelenium", quietly = TRUE) &&
        !requireNamespace("chromote", quietly = TRUE)) {
      stop(
        "JavaScript scraping requires RSelenium or chromote package. ",
        "Install one with: install.packages('RSelenium') or install.packages('chromote')",
        call. = FALSE
      )
    }
    stop("JavaScript scraping not yet implemented. Set use_js = FALSE.", call. = FALSE)
  }

  # Approach 1: Simple HTTP with rvest and polite
  if (verbose) message("Connecting to: ", url)

  # Create polite session
  session <- tryCatch(
    {
      polite::bow(
        url,
        user_agent = "situas R package (https://github.com/gmontaletti/situas; educational use)"
      )
    },
    error = function(e) {
      stop(
        "Failed to connect to URL: ", url, "\n",
        "Error: ", e$message,
        call. = FALSE
      )
    }
  )

  # Scrape the page
  page <- tryCatch(
    {
      polite::scrape(session)
    },
    error = function(e) {
      stop(
        "Failed to scrape page: ", url, "\n",
        "Error: ", e$message,
        call. = FALSE
      )
    }
  )

  # Extract Excel file links
  excel_links <- tryCatch(
    {
      # Look for links ending in .xls or .xlsx
      nodes <- rvest::html_nodes(page, "a[href$='.xls'], a[href$='.xlsx']")

      if (length(nodes) == 0) {
        # Try alternative: any link containing .xls
        nodes <- rvest::html_nodes(page, "a[href*='.xls']")
      }

      urls <- rvest::html_attr(nodes, "href")
      texts <- rvest::html_text(nodes, trim = TRUE)

      list(urls = urls, texts = texts)
    },
    error = function(e) {
      stop(
        "Failed to extract links from page. ",
        "Error: ", e$message,
        call. = FALSE
      )
    }
  )

  # Check if any links found
  if (length(excel_links$urls) == 0) {
    if (verbose) {
      message("No Excel files found. Page may require JavaScript or authentication.")
    }
    return(data.table::data.table(
      filename = character(0),
      url = character(0),
      link_text = character(0)
    ))
  }

  # Build result data.table
  result <- data.table::data.table(
    url = excel_links$urls,
    link_text = excel_links$texts
  )

  # Extract filename from URL
  result[, filename := basename(url)]

  # Clean up link text (remove extra whitespace)
  result[, link_text := gsub("\\s+", " ", trimws(link_text))]

  # Reorder columns
  result <- result[, .(filename, url, link_text)]

  if (verbose) {
    message("Found ", nrow(result), " Excel file link(s)")
  }

  return(result)
}


# 3. Helper: Download Single Classification File -----

#' Download a Single Classification File
#'
#' Downloads one classification Excel file from a URL to a specified directory.
#'
#' @param url Character. URL or relative path to the file.
#' @param filename Character. Name to save the file as.
#' @param output_dir Character. Directory to save the file in.
#' @param force Logical. If TRUE, re-download even if file exists. Default is FALSE.
#' @param verbose Logical. Print progress messages. Default is TRUE.
#'
#' @return Character. Full path to the downloaded file.
#'
#' @keywords internal
#' @noRd
download_classification_file <- function(url,
                                        filename,
                                        output_dir,
                                        force = FALSE,
                                        verbose = TRUE) {
  # Create output path
  output_path <- file.path(output_dir, filename)

  # Skip if exists and not forcing
  if (file.exists(output_path) && !force) {
    if (verbose) message("  File already exists: ", filename)
    return(output_path)
  }

  if (verbose) message("  Downloading: ", filename)

  # Handle relative URLs (add base if needed)
  if (!grepl("^https?://", url)) {
    base_url <- "https://urponline.lavoro.gov.it"
    full_url <- paste0(base_url, url)
  } else {
    full_url <- url
  }

  # Download with error handling
  tryCatch(
    {
      # Use download.file with mode = "wb" for binary files
      download.file(
        url = full_url,
        destfile = output_path,
        mode = "wb",
        quiet = !verbose,
        method = "auto"
      )

      # Verify file was downloaded
      if (!file.exists(output_path)) {
        stop("File was not created after download", call. = FALSE)
      }

      # Check file size
      file_size <- file.size(output_path)

      if (file_size < 1000) {
        warning(
          "Downloaded file is suspiciously small: ", filename,
          " (", file_size, " bytes). ",
          "May be an error page or invalid file.",
          call. = FALSE
        )
      }

      if (verbose) {
        message(
          "  Downloaded: ", basename(output_path),
          " (", round(file_size / 1024^2, 2), " MB)"
        )
      }

      return(output_path)
    },
    error = function(e) {
      # Clean up partial download
      if (file.exists(output_path)) {
        unlink(output_path)
      }
      stop(
        "Failed to download ", filename, "\n",
        "URL: ", full_url, "\n",
        "Error: ", e$message,
        call. = FALSE
      )
    }
  )
}


# 4. Main Function: Download Classification Standards -----

#' Download Italian Labor Ministry Classification Standards
#'
#' Downloads the latest versions of classification standard Excel files from
#' the Italian Ministry of Labor and Social Policies website. These files
#' contain official classifications used in administrative data systems (URP).
#'
#' The function can:
#' \itemize{
#'   \item Scrape the classifications page to find available files
#'   \item Parse version numbers from filenames
#'   \item Download only the latest version of each classification type
#'   \item Skip already-downloaded files (unless force_refresh = TRUE)
#'   \item Save metadata about downloaded files
#' }
#'
#' @param output_dir Character. Directory where files will be saved.
#'   Defaults to "data-raw/classifications". Created if it doesn't exist.
#' @param url Character. URL of the classifications page. Default is the
#'   official Italian Labor Ministry standards page.
#' @param download_all Logical. If TRUE, download all versions found. If FALSE
#'   (default), download only the latest version of each classification type.
#' @param force_refresh Logical. If TRUE, re-download files even if they already
#'   exist. Default is FALSE.
#' @param use_js_scraping Logical. If TRUE, use JavaScript-enabled scraping
#'   (requires RSelenium or chromote). Default is FALSE. Currently not implemented.
#' @param classification_types Character vector. Types to download. NULL (default)
#'   downloads all types. Available types include: "classificazioni_standard",
#'   "allegato", "migrazione".
#' @param verbose Logical. Print progress messages. Default is TRUE.
#'
#' @return Invisibly returns a data.table with information about downloaded files:
#'   \describe{
#'     \item{filename}{Downloaded filename}
#'     \item{url}{Source URL}
#'     \item{link_text}{Original link text from webpage}
#'     \item{version}{Extracted version number (NA if not parseable)}
#'     \item{type}{Classification type}
#'     \item{file_path}{Full path to downloaded file}
#'     \item{download_date}{Timestamp when file was downloaded}
#'     \item{file_size}{File size in bytes}
#'   }
#'
#'   A metadata file is also saved to \code{output_dir/download_metadata.rds}
#'   containing download information and file list.
#'
#' @section Classification Types:
#'
#' The main classification types are:
#' \describe{
#'   \item{classificazioni_standard}{Main classification standards file
#'     (Rev.XXX-ST-Classificazioni-Standard.xlsx). Contains multiple sheets
#'     with various classification codes used in Italian administrative systems.}
#'   \item{allegato}{Annexes with additional information or mappings}
#'   \item{migrazione}{Migration analysis files showing changes between
#'     classification versions}
#' }
#'
#' @section Note:
#'
#' This function requires the \code{rvest} and \code{polite} packages for
#' web scraping. Install them with:
#' \code{install.packages(c("rvest", "polite"))}
#'
#' The Labor Ministry website may change structure over time. If scraping fails,
#' you may need to download files manually from:
#' \url{https://urponline.lavoro.gov.it/s/standard-tecnici/classificazioni-standard}
#'
#' @importFrom data.table data.table
#' @importFrom utils download.file packageVersion
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Download latest versions only (recommended)
#' files <- download_classification_standards()
#'
#' # Download to specific directory
#' files <- download_classification_standards(
#'   output_dir = "my_data/classifications"
#' )
#'
#' # Force re-download of all files
#' files <- download_classification_standards(force_refresh = TRUE)
#'
#' # Download only main classification standards
#' files <- download_classification_standards(
#'   classification_types = "classificazioni_standard"
#' )
#'
#' # Download all versions (not just latest)
#' files <- download_classification_standards(download_all = TRUE)
#'
#' # View downloaded files
#' print(files)
#'
#' # Check what was downloaded
#' files[, .(filename, version, type, file_size)]
#' }
download_classification_standards <- function(
    output_dir = "data-raw/classifications",
    url = "https://urponline.lavoro.gov.it/s/standard-tecnici/classificazioni-standard?language=it",
    download_all = FALSE,
    force_refresh = FALSE,
    use_js_scraping = FALSE,
    classification_types = NULL,
    verbose = TRUE) {

  # 1. Input validation -----

  stopifnot(
    "output_dir must be a single character string" =
      is.character(output_dir) && length(output_dir) == 1,
    "url must be a single character string" =
      is.character(url) && length(url) == 1,
    "download_all must be logical" =
      is.logical(download_all) && length(download_all) == 1,
    "force_refresh must be logical" =
      is.logical(force_refresh) && length(force_refresh) == 1,
    "use_js_scraping must be logical" =
      is.logical(use_js_scraping) && length(use_js_scraping) == 1,
    "verbose must be logical" =
      is.logical(verbose) && length(verbose) == 1
  )

  if (!is.null(classification_types)) {
    stopifnot(
      "classification_types must be character vector" =
        is.character(classification_types)
    )
  }

  # 2. Create output directory -----

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (verbose) {
      message("Created output directory: ", output_dir)
    }
  }

  # 3. Scrape page for links -----

  if (verbose) {
    message("\n=== Scraping Classification Standards Page ===")
  }

  links_dt <- scrape_classification_links(
    url = url,
    use_js = use_js_scraping,
    verbose = verbose
  )

  if (nrow(links_dt) == 0) {
    stop(
      "No Excel files found on page.\n",
      "The page may require JavaScript, authentication, or has changed structure.\n",
      "Try downloading files manually from: ", url,
      call. = FALSE
    )
  }

  if (verbose) {
    message("\nFound ", nrow(links_dt), " Excel file(s)")
  }

  # 4. Parse versions and types -----

  if (verbose) {
    message("\n=== Parsing File Metadata ===")
  }

  # Add version and type columns
  links_dt[, c("version", "type") := {
    parsed <- parse_classification_version(filename)
    list(parsed$version, parsed$type)
  }, by = seq_len(nrow(links_dt))]

  if (verbose) {
    message("\nClassification types found:")
    type_summary <- links_dt[, .N, by = type]
    print(type_summary)
  }

  # 5. Filter by classification type -----

  if (!is.null(classification_types)) {
    rows_before <- nrow(links_dt)
    links_dt <- links_dt[type %in% classification_types]

    if (nrow(links_dt) == 0) {
      stop(
        "No files found matching requested types: ",
        paste(classification_types, collapse = ", "),
        "\nAvailable types: ",
        paste(unique(type_summary$type), collapse = ", "),
        call. = FALSE
      )
    }

    if (verbose) {
      message(
        "\nFiltered to types: ", paste(classification_types, collapse = ", "),
        " (", nrow(links_dt), " files)"
      )
    }
  }

  # 6. Filter to latest versions -----

  if (!download_all) {
    if (verbose) {
      message("\n=== Selecting Latest Versions ===")
    }

    # Keep only the highest version for each type
    # For files without version number, keep them all
    links_dt[, keep := {
      if (all(is.na(version))) {
        # No versions, keep all
        rep(TRUE, .N)
      } else {
        # Keep the row(s) with max version
        version == max(version, na.rm = TRUE) | is.na(version)
      }
    }, by = type]

    links_dt <- links_dt[keep == TRUE]
    links_dt[, keep := NULL]

    if (verbose) {
      message("Selected ", nrow(links_dt), " file(s) (latest versions only)")
      if (nrow(links_dt) > 0) {
        message("\nFiles to download:")
        for (i in seq_len(nrow(links_dt))) {
          message(
            "  - ", links_dt$filename[i],
            " (type: ", links_dt$type[i],
            ", version: ",
            ifelse(is.na(links_dt$version[i]), "N/A", links_dt$version[i]),
            ")"
          )
        }
      }
    }
  }

  # 7. Download files -----

  if (verbose) {
    message("\n=== Downloading Files ===")
  }

  downloaded_files <- character(nrow(links_dt))

  for (i in seq_len(nrow(links_dt))) {
    downloaded_files[i] <- download_classification_file(
      url = links_dt$url[i],
      filename = links_dt$filename[i],
      output_dir = output_dir,
      force = force_refresh,
      verbose = verbose
    )
  }

  links_dt[, file_path := downloaded_files]
  links_dt[, download_date := Sys.time()]
  links_dt[, file_size := file.size(file_path)]

  # 8. Save metadata -----

  metadata <- list(
    download_date = Sys.time(),
    source_url = url,
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    package_version = utils::packageVersion("situas"),
    files = links_dt
  )

  metadata_path <- file.path(output_dir, "download_metadata.rds")
  saveRDS(metadata, metadata_path)

  if (verbose) {
    message("\nSaved metadata to: ", metadata_path)
  }

  # 9. Print summary -----

  if (verbose) {
    message("\n=== Download Complete ===")
    message("Files saved to: ", normalizePath(output_dir, mustWork = FALSE))
    message("Number of files: ", nrow(links_dt))
    message(
      "Total size: ",
      round(sum(links_dt$file_size, na.rm = TRUE) / 1024^2, 2),
      " MB"
    )
  }

  # 10. Return result -----

  invisible(links_dt)
}
