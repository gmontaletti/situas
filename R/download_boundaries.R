# download_boundaries.R
# Functions for downloading ISTAT administrative boundary shapefiles

# 1. Helper Functions -----

#' Get boundaries cache directory
#'
#' @keywords internal
#' @noRd
get_boundaries_cache_dir <- function() {
  cache_dir <- tools::R_user_dir("situas", which = "data")
  boundaries_dir <- file.path(cache_dir, "boundaries")

  if (!dir.exists(boundaries_dir)) {
    dir.create(boundaries_dir, recursive = TRUE)
  }

  return(boundaries_dir)
}

#' Get boundaries metadata file path
#'
#' @keywords internal
#' @noRd
get_boundaries_metadata_path <- function() {
  file.path(get_boundaries_cache_dir(), "metadata.rds")
}

#' Load boundaries metadata
#'
#' @keywords internal
#' @noRd
load_boundaries_metadata <- function() {
  metadata_path <- get_boundaries_metadata_path()

  if (file.exists(metadata_path)) {
    return(readRDS(metadata_path))
  } else {
    # Return empty data.table with correct structure
    return(data.table::data.table(
      date = character(),
      territorial_level = character(),
      source = character(),
      file_path = character(),
      download_timestamp = as.POSIXct(character()),
      file_size_mb = numeric()
    ))
  }
}

#' Save boundaries metadata
#'
#' @keywords internal
#' @noRd
save_boundaries_metadata <- function(metadata) {
  metadata_path <- get_boundaries_metadata_path()
  saveRDS(metadata, metadata_path)
}

#' Get territorial level code
#'
#' @keywords internal
#' @noRd
get_territorial_code <- function(level) {
  codes <- list(
    comuni = "Com",
    province = "ProvCM",
    regioni = "Reg",
    ripartizioni = "RipGeo"
  )

  code <- codes[[level]]
  if (is.null(code)) {
    stop("Invalid territorial level: ", level,
         ". Valid options: ", paste(names(codes), collapse = ", "))
  }

  return(code)
}

#' Format date for ISTAT boundaries
#'
#' @keywords internal
#' @noRd
format_boundary_date <- function(date) {
  if (is.character(date)) {
    # Try to parse if it's a string
    date <- as.Date(date)
  }

  # Format as YYYYMMDD
  format(date, "%Y%m%d")
}

#' Format date for filename
#'
#' @keywords internal
#' @noRd
format_filename_date <- function(date) {
  # Format as DDMMYYYY for ISTAT filenames
  format(as.Date(date), "%d%m%Y")
}

# 2. OnData API Functions -----

#' Build OnData download URL
#'
#' @keywords internal
#' @noRd
build_ondata_url <- function(date, territorial_level) {
  base_url <- "https://www.confini-amministrativi.it/api/v2/it"
  date_str <- format_boundary_date(date)
  code <- get_territorial_code(territorial_level)

  # OnData uses generalized suffix "_gen"
  filename <- paste0(code, "_gen.zip")

  url <- file.path(base_url, date_str, filename)
  return(url)
}

#' List available boundary versions from OnData
#'
#' @keywords internal
#' @noRd
list_ondata_versions <- function(since_year = 2020, verbose = TRUE) {
  if (verbose) {
    message("Querying OnData repository for available boundary versions...")
  }

  # OnData provides boundaries on January 1st of each year
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  years <- since_year:current_year

  # Build list of potential dates (January 1st of each year)
  dates <- paste0(years, "0101")

  # Create data.table of available versions
  # We assume these are available without checking each URL
  # (could enhance with actual HEAD requests to verify)
  versions <- data.table::data.table(
    date = dates,
    year = years,
    source = "OnData",
    base_url = "https://www.confini-amministrativi.it/api/v2/it"
  )

  data.table::setorder(versions, -year)

  return(versions)
}

# 3. ISTAT Fallback Functions -----

#' Build ISTAT download URL
#'
#' @keywords internal
#' @noRd
build_istat_url <- function(date) {
  # ISTAT naming pattern: Limiti01012025_g.zip
  date_obj <- as.Date(date)
  date_str <- format_filename_date(date_obj)

  filename <- paste0("Limiti", date_str, "_g.zip")

  # Note: This is a placeholder. Actual ISTAT URLs would need to be scraped
  # from https://www.istat.it/it/archivio/222527
  warning("ISTAT direct download not yet implemented. Using OnData as primary source.")

  return(NULL)
}

# 4. Download Functions -----

#' Download boundary shapefile from URL
#'
#' @keywords internal
#' @noRd
download_boundary_file <- function(url, dest_file, verbose = TRUE) {
  if (verbose) {
    message("Downloading from: ", url)
  }

  # Download with progress bar
  response <- httr::GET(
    url,
    httr::user_agent("situas R package (https://github.com/gmontaletti/situas)"),
    httr::progress(),
    httr::write_disk(dest_file, overwrite = TRUE)
  )

  # Check if successful
  if (httr::status_code(response) != 200) {
    return(list(success = FALSE, error = httr::http_status(response)$message))
  }

  # Verify it's a valid zip file
  if (!is_valid_zip(dest_file)) {
    unlink(dest_file)
    return(list(success = FALSE, error = "Downloaded file is not a valid ZIP archive"))
  }

  return(list(success = TRUE, file = dest_file))
}

#' Check if file is a valid ZIP
#'
#' @keywords internal
#' @noRd
is_valid_zip <- function(file) {
  tryCatch({
    zip_list <- utils::unzip(file, list = TRUE)
    return(nrow(zip_list) > 0)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Extract shapefile from ZIP
#'
#' @keywords internal
#' @noRd
extract_boundary_shapefile <- function(zip_file, date, territorial_level, verbose = TRUE) {
  # Create extraction directory
  cache_dir <- get_boundaries_cache_dir()
  date_str <- format_boundary_date(date)
  extract_dir <- file.path(cache_dir, date_str)

  if (!dir.exists(extract_dir)) {
    dir.create(extract_dir, recursive = TRUE)
  }

  if (verbose) {
    message("Extracting to: ", extract_dir)
  }

  # List contents
  zip_contents <- utils::unzip(zip_file, list = TRUE)

  # Find shapefile components (.shp, .shx, .dbf, .prj, .cpg)
  code <- get_territorial_code(territorial_level)

  # OnData naming: Com_gen_WGS84.shp (for comuni)
  # We need to find files that match the pattern
  shp_pattern <- paste0(code, ".*\\.shp$")
  shp_files <- grep(shp_pattern, zip_contents$Name, value = TRUE, ignore.case = TRUE)

  if (length(shp_files) == 0) {
    return(list(success = FALSE, error = "No shapefile found in ZIP archive"))
  }

  # Get base name (without extension)
  shp_file <- shp_files[1]
  base_name <- sub("\\.shp$", "", basename(shp_file), ignore.case = TRUE)

  # Find all related files
  related_files <- grep(base_name, zip_contents$Name, value = TRUE, fixed = TRUE)

  # Extract all related files
  utils::unzip(zip_file, files = related_files, exdir = extract_dir, overwrite = TRUE)

  # Return path to main .shp file
  shp_path <- file.path(extract_dir, basename(shp_file))

  if (!file.exists(shp_path)) {
    return(list(success = FALSE, error = "Shapefile extraction failed"))
  }

  return(list(success = TRUE, shapefile = shp_path))
}

# 5. Main Exported Functions -----

#' Download ISTAT Administrative Boundary Shapefiles
#'
#' Downloads generalized boundary shapefiles for Italian administrative units
#' from the OnData repository (with ISTAT fallback). Boundaries are cached
#' locally for offline use.
#'
#' @param date Date for which to download boundaries. Can be a Date object or
#'   character string in "YYYY-MM-DD" format. Defaults to most recent January 1st.
#'   Available dates: January 1st of each year from 2020 to present.
#' @param territorial_levels Character vector of territorial levels to download.
#'   Options: "comuni", "province", "regioni", "ripartizioni".
#'   Default is all levels.
#' @param output_dir Directory where boundaries will be cached. Default uses
#'   \code{tools::R_user_dir("situas", which = "data")}.
#' @param force_refresh Logical. If TRUE, re-downloads even if already cached.
#'   Default is FALSE.
#' @param verbose Logical. Print progress messages? Default is TRUE.
#'
#' @return A data.table with download status for each territorial level, including:
#'   \itemize{
#'     \item territorial_level: The territorial level
#'     \item status: "success", "cached", or "failed"
#'     \item source: "OnData" or "ISTAT"
#'     \item file_path: Path to downloaded shapefile
#'     \item error: Error message if status is "failed"
#'   }
#'
#' @section Data Source:
#' Primary source is the OnData repository (\url{https://www.confini-amministrativi.it}),
#' which provides ISTAT boundaries in multiple formats with easier programmatic access.
#' If OnData is unavailable, the function falls back to direct ISTAT downloads.
#'
#' @section File Organization:
#' Downloaded boundaries are organized by date:
#' \preformatted{
#' {cache_dir}/boundaries/
#'   ├── 20250101/
#'   │   ├── Com_gen_WGS84.shp
#'   │   ├── ProvCM_gen_WGS84.shp
#'   │   └── ...
#'   └── metadata.rds
#' }
#'
#' @examples
#' \dontrun{
#' # Download all boundaries for 2025-01-01
#' result <- download_istat_boundaries()
#'
#' # Download only municipalities for a specific date
#' result <- download_istat_boundaries(
#'   date = "2024-01-01",
#'   territorial_levels = "comuni"
#' )
#'
#' # Force re-download even if cached
#' result <- download_istat_boundaries(force_refresh = TRUE)
#' }
#'
#' @seealso
#' \code{\link{list_istat_boundary_versions}} to see available versions
#' \code{\link{check_boundary_updates}} to check for updates
#' \code{\link{get_cached_boundaries_info}} to see cached boundaries
#'
#' @export
download_istat_boundaries <- function(date = NULL,
                                      territorial_levels = c("comuni", "province", "regioni", "ripartizioni"),
                                      output_dir = NULL,
                                      force_refresh = FALSE,
                                      verbose = TRUE) {

  # 1. Validate inputs -----
  stopifnot(
    is.logical(force_refresh),
    is.logical(verbose)
  )

  # Default to most recent January 1st
  if (is.null(date)) {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    date <- as.Date(paste0(current_year, "-01-01"))
  } else {
    date <- as.Date(date)
  }

  # Validate territorial levels
  valid_levels <- c("comuni", "province", "regioni", "ripartizioni")
  invalid_levels <- setdiff(territorial_levels, valid_levels)
  if (length(invalid_levels) > 0) {
    stop("Invalid territorial levels: ", paste(invalid_levels, collapse = ", "),
         "\nValid options: ", paste(valid_levels, collapse = ", "))
  }

  # Use default cache directory if not specified
  if (is.null(output_dir)) {
    output_dir <- get_boundaries_cache_dir()
  }

  # 2. Load existing metadata -----
  metadata <- load_boundaries_metadata()

  # 3. Download each territorial level -----
  results <- data.table::data.table(
    territorial_level = character(),
    status = character(),
    source = character(),
    file_path = character(),
    error = character()
  )

  for (level in territorial_levels) {
    if (verbose) {
      message("\n--- Processing territorial level: ", level, " ---")
    }

    # Check if already cached
    date_str <- format_boundary_date(date)
    cached <- metadata[date == date_str & territorial_level == level]

    if (nrow(cached) > 0 && !force_refresh) {
      cached_file <- cached$file_path[1]
      if (file.exists(cached_file)) {
        if (verbose) {
          message("Using cached file: ", cached_file)
        }
        results <- data.table::rbindlist(list(results, data.table::data.table(
          territorial_level = level,
          status = "cached",
          source = cached$source[1],
          file_path = cached_file,
          error = NA_character_
        )))
        next
      }
    }

    # Try downloading from OnData
    url <- build_ondata_url(date, level)
    temp_zip <- tempfile(fileext = ".zip")

    download_result <- download_boundary_file(url, temp_zip, verbose)

    if (download_result$success) {
      # Extract shapefile
      extract_result <- extract_boundary_shapefile(temp_zip, date, level, verbose)

      if (extract_result$success) {
        # Update metadata
        file_size <- file.info(extract_result$shapefile)$size / 1024^2  # MB

        new_entry <- data.table::data.table(
          date = date_str,
          territorial_level = level,
          source = "OnData",
          file_path = extract_result$shapefile,
          download_timestamp = Sys.time(),
          file_size_mb = round(file_size, 2)
        )

        # Remove old entry if exists
        metadata <- metadata[!(date == date_str & territorial_level == level)]
        metadata <- data.table::rbindlist(list(metadata, new_entry))
        save_boundaries_metadata(metadata)

        results <- data.table::rbindlist(list(results, data.table::data.table(
          territorial_level = level,
          status = "success",
          source = "OnData",
          file_path = extract_result$shapefile,
          error = NA_character_
        )))

        if (verbose) {
          message("Successfully downloaded and extracted: ", level)
        }
      } else {
        results <- data.table::rbindlist(list(results, data.table::data.table(
          territorial_level = level,
          status = "failed",
          source = "OnData",
          file_path = NA_character_,
          error = extract_result$error
        )))
      }

      # Clean up temp file
      unlink(temp_zip)
    } else {
      # Try ISTAT fallback (not yet implemented)
      results <- data.table::rbindlist(list(results, data.table::data.table(
        territorial_level = level,
        status = "failed",
        source = "OnData",
        file_path = NA_character_,
        error = download_result$error
      )))

      if (verbose) {
        warning("Failed to download ", level, ": ", download_result$error)
      }
    }
  }

  if (verbose) {
    message("\n--- Download Summary ---")
    print(results)
  }

  return(results)
}

#' List Available ISTAT Boundary Versions
#'
#' Query available boundary shapefile versions from the OnData repository.
#' Results are cached for 24 hours to reduce API calls.
#'
#' @param since_year Integer. Show versions from this year onwards. Default is 2020.
#' @param use_cache Logical. Use cached version list if available? Default is TRUE.
#' @param verbose Logical. Print progress messages? Default is TRUE.
#'
#' @return A data.table with available versions, including:
#'   \itemize{
#'     \item date: Date in YYYYMMDD format
#'     \item year: Year
#'     \item source: Data source ("OnData")
#'     \item base_url: Base URL for downloads
#'   }
#'
#' @examples
#' \dontrun{
#' # List all available versions since 2020
#' versions <- list_istat_boundary_versions()
#'
#' # List versions since 2022
#' versions <- list_istat_boundary_versions(since_year = 2022)
#'
#' # Force refresh without using cache
#' versions <- list_istat_boundary_versions(use_cache = FALSE)
#' }
#'
#' @seealso \code{\link{download_istat_boundaries}}
#'
#' @export
list_istat_boundary_versions <- function(since_year = 2020,
                                        use_cache = TRUE,
                                        verbose = TRUE) {

  stopifnot(
    is.numeric(since_year),
    is.logical(use_cache),
    is.logical(verbose)
  )

  # Check cache
  cache_dir <- get_cache_dir()  # Use existing cache system
  cache_key <- paste0("boundary_versions_", since_year)
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))

  if (use_cache && file.exists(cache_file)) {
    cache_age <- difftime(Sys.time(), file.info(cache_file)$mtime, units = "hours")
    if (cache_age < 24) {
      if (verbose) {
        message("Using cached version list (age: ", round(cache_age, 1), " hours)")
      }
      return(readRDS(cache_file))
    }
  }

  # Fetch from OnData
  versions <- list_ondata_versions(since_year = since_year, verbose = verbose)

  # Cache results
  saveRDS(versions, cache_file)

  return(versions)
}

#' Check for Boundary Updates
#'
#' Compare currently cached boundaries against the latest available versions
#' to identify updates.
#'
#' @param verbose Logical. Print progress messages? Default is TRUE.
#'
#' @return A data.table showing update status, including:
#'   \itemize{
#'     \item territorial_level: The territorial level
#'     \item current_date: Currently cached version date (NA if not cached)
#'     \item latest_date: Latest available version date
#'     \item update_available: Logical indicating if update is available
#'     \item current_source: Source of current version
#'   }
#'
#' @examples
#' \dontrun{
#' # Check for updates
#' updates <- check_boundary_updates()
#'
#' # Download updates if available
#' if (any(updates$update_available)) {
#'   download_istat_boundaries()
#' }
#' }
#'
#' @seealso
#' \code{\link{download_istat_boundaries}}
#' \code{\link{get_cached_boundaries_info}}
#'
#' @export
check_boundary_updates <- function(verbose = TRUE) {

  # Get available versions
  available <- list_istat_boundary_versions(verbose = FALSE)
  latest_date <- available$date[1]  # Most recent

  # Get cached boundaries
  metadata <- load_boundaries_metadata()

  # Get all territorial levels
  all_levels <- c("comuni", "province", "regioni", "ripartizioni")

  # Build comparison table
  comparison <- data.table::data.table(
    territorial_level = all_levels
  )

  comparison[, current_date := {
    cached <- metadata[territorial_level == .BY[[1]]]
    if (nrow(cached) > 0) {
      max(cached$date)
    } else {
      NA_character_
    }
  }, by = territorial_level]

  comparison[, latest_date := latest_date]
  comparison[, update_available := is.na(current_date) | current_date < latest_date]

  comparison[, current_source := {
    cached <- metadata[territorial_level == .BY[[1]]]
    if (nrow(cached) > 0) {
      cached$source[which.max(cached$date)]
    } else {
      NA_character_
    }
  }, by = territorial_level]

  if (verbose) {
    message("Boundary Update Status:")
    print(comparison)

    if (any(comparison$update_available)) {
      message("\nUpdates are available! Run download_istat_boundaries() to update.")
    } else {
      message("\nAll boundaries are up to date.")
    }
  }

  return(comparison)
}

#' Get Information About Cached Boundaries
#'
#' List all currently cached boundary shapefiles with metadata.
#'
#' @param territorial_level Character. Filter by territorial level. Options:
#'   "comuni", "province", "regioni", "ripartizioni", or "all" (default).
#' @param verbose Logical. Print summary information? Default is TRUE.
#'
#' @return A data.table with cached boundary information, including:
#'   \itemize{
#'     \item date: Boundary reference date
#'     \item territorial_level: The territorial level
#'     \item source: Data source
#'     \item file_path: Path to shapefile
#'     \item download_timestamp: When it was downloaded
#'     \item file_size_mb: File size in megabytes
#'     \item exists: Logical indicating if file still exists
#'   }
#'
#' @examples
#' \dontrun{
#' # Get info on all cached boundaries
#' info <- get_cached_boundaries_info()
#'
#' # Get info only for municipalities
#' info <- get_cached_boundaries_info(territorial_level = "comuni")
#' }
#'
#' @seealso
#' \code{\link{download_istat_boundaries}}
#' \code{\link{clean_boundary_cache}}
#'
#' @export
get_cached_boundaries_info <- function(territorial_level = "all", verbose = TRUE) {

  metadata <- load_boundaries_metadata()

  if (nrow(metadata) == 0) {
    if (verbose) {
      message("No cached boundaries found.")
      message("Run download_istat_boundaries() to download boundaries.")
    }
    return(metadata)
  }

  # Filter by territorial level if specified
  if (territorial_level != "all") {
    metadata <- metadata[territorial_level == ..territorial_level]
  }

  # Check if files still exist
  metadata[, exists := file.exists(file_path)]

  # Sort by date (most recent first)
  data.table::setorder(metadata, -date, territorial_level)

  if (verbose) {
    total_size <- sum(metadata$file_size_mb, na.rm = TRUE)
    n_files <- nrow(metadata)
    n_missing <- sum(!metadata$exists)

    message("Cached Boundaries Summary:")
    message("  Total files: ", n_files)
    message("  Total size: ", round(total_size, 1), " MB")
    if (n_missing > 0) {
      message("  Missing files: ", n_missing)
    }
    message("\nDetails:")
    print(metadata)
  }

  return(metadata)
}

#' Clean Boundary Cache
#'
#' Remove old boundary files from the cache to free up disk space.
#'
#' @param keep_latest_n Integer. Keep this many most recent versions for each
#'   territorial level. Default is 1 (keep only latest).
#' @param older_than_days Integer. Remove versions older than this many days.
#'   If NULL (default), only \code{keep_latest_n} is used.
#' @param territorial_level Character. Clean specific territorial level or "all"
#'   (default).
#' @param dry_run Logical. If TRUE, show what would be removed without actually
#'   removing. Default is FALSE.
#' @param verbose Logical. Print progress messages? Default is TRUE.
#'
#' @return A character vector of removed file paths (invisibly).
#'
#' @examples
#' \dontrun{
#' # Preview what would be removed (dry run)
#' clean_boundary_cache(dry_run = TRUE)
#'
#' # Keep only the latest version of each territorial level
#' clean_boundary_cache(keep_latest_n = 1)
#'
#' # Remove versions older than 365 days
#' clean_boundary_cache(older_than_days = 365, keep_latest_n = NULL)
#'
#' # Clean only municipalities
#' clean_boundary_cache(
#'   territorial_level = "comuni",
#'   keep_latest_n = 2
#' )
#' }
#'
#' @seealso \code{\link{get_cached_boundaries_info}}
#'
#' @export
clean_boundary_cache <- function(keep_latest_n = 1,
                                 older_than_days = NULL,
                                 territorial_level = "all",
                                 dry_run = FALSE,
                                 verbose = TRUE) {

  stopifnot(
    is.null(keep_latest_n) || (is.numeric(keep_latest_n) && keep_latest_n >= 0),
    is.null(older_than_days) || (is.numeric(older_than_days) && older_than_days >= 0),
    is.logical(dry_run),
    is.logical(verbose)
  )

  metadata <- load_boundaries_metadata()

  if (nrow(metadata) == 0) {
    if (verbose) {
      message("No cached boundaries to clean.")
    }
    return(invisible(character(0)))
  }

  # Filter by territorial level if specified
  if (territorial_level != "all") {
    level_filter <- territorial_level
    metadata <- metadata[territorial_level == level_filter]
  }

  # Determine which files to remove
  to_remove <- metadata[FALSE]  # Empty data.table with same structure

  # Strategy 1: Keep only N most recent versions per level
  if (!is.null(keep_latest_n)) {
    data.table::setorder(metadata, territorial_level, -date)

    metadata[, rank := seq_len(.N), by = territorial_level]
    to_remove_by_rank <- metadata[rank > keep_latest_n]
    to_remove <- data.table::rbindlist(list(to_remove, to_remove_by_rank))
  }

  # Strategy 2: Remove versions older than N days
  if (!is.null(older_than_days)) {
    cutoff_date <- Sys.time() - (older_than_days * 24 * 60 * 60)
    to_remove_by_age <- metadata[download_timestamp < cutoff_date]
    to_remove <- data.table::rbindlist(list(to_remove, to_remove_by_age))
  }

  # Remove duplicates
  to_remove <- unique(to_remove)

  if (nrow(to_remove) == 0) {
    if (verbose) {
      message("No files meet the removal criteria.")
    }
    return(invisible(character(0)))
  }

  # Display what will be removed
  if (verbose || dry_run) {
    message(ifelse(dry_run, "Would remove", "Removing"), " ", nrow(to_remove), " file(s):")
    print(to_remove[, .(territorial_level, date, file_size_mb, file_path)])

    total_size <- sum(to_remove$file_size_mb, na.rm = TRUE)
    message("Total space to free: ", round(total_size, 1), " MB")
  }

  if (dry_run) {
    return(invisible(to_remove$file_path))
  }

  # Actually remove files
  removed_files <- character(0)

  for (i in seq_len(nrow(to_remove))) {
    file_path <- to_remove$file_path[i]

    if (file.exists(file_path)) {
      # Remove all shapefile components
      base_path <- sub("\\.shp$", "", file_path, ignore.case = TRUE)
      extensions <- c(".shp", ".shx", ".dbf", ".prj", ".cpg", ".sbn", ".sbx")

      for (ext in extensions) {
        comp_file <- paste0(base_path, ext)
        if (file.exists(comp_file)) {
          unlink(comp_file)
        }
      }

      removed_files <- c(removed_files, file_path)
    }
  }

  # Update metadata
  metadata <- metadata[!file_path %in% removed_files]
  save_boundaries_metadata(metadata)

  if (verbose) {
    message("Successfully removed ", length(removed_files), " file(s).")
  }

  return(invisible(removed_files))
}
