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
#' Returns the ISTAT short code used in the official archive naming scheme
#' (e.g. \code{Com01012026_g/Com01012026_g_WGS84.shp}).
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
    stop(
      "Invalid territorial level: ",
      level,
      ". Valid options: ",
      paste(names(codes), collapse = ", ")
    )
  }

  return(code)
}

#' Get OnData division name
#'
#' Returns the division slug used by the OnData API v2. The API addresses
#' territorial levels by division name, not by ISTAT short code.
#'
#' @keywords internal
#' @noRd
get_ondata_division <- function(level) {
  divisions <- list(
    comuni = "comuni",
    province = "unita-territoriali-sovracomunali",
    regioni = "regioni",
    ripartizioni = "ripartizioni-geografiche"
  )

  division <- divisions[[level]]
  if (is.null(division)) {
    stop(
      "Invalid territorial level: ",
      level,
      ". Valid options: ",
      paste(names(divisions), collapse = ", ")
    )
  }

  return(division)
}

#' Canonical ISTAT boundary attribute names
#'
#' @keywords internal
#' @noRd
istat_boundary_fields <- function() {
  c(
    "OBJECTID",
    "COD_RIP",
    "COD_REG",
    "COD_PROV",
    "COD_CM",
    "COD_UTS",
    "PRO_COM",
    "PRO_COM_T",
    "COMUNE",
    "COMUNE_A",
    "CC_UTS",
    "DEN_PROV",
    "DEN_CM",
    "DEN_UTS",
    "DEN_REG",
    "DEN_RIP",
    "SIGLA",
    "TIPO_UTS",
    "Shape_Leng",
    "Shape_Le_1",
    "Shape_Area"
  )
}

#' Normalise boundary attribute names to the ISTAT convention
#'
#' The OnData distribution ships lowercase attribute names (\code{pro_com},
#' \code{cod_reg}, ...) while the ISTAT distribution uses the uppercase form
#' (\code{PRO_COM}, \code{COD_REG}, ...). Downstream functions such as
#' \code{prepare_territorial_maps()} join on the ISTAT names, so attributes are
#' normalised at read time regardless of the source.
#'
#' Names matching a canonical ISTAT field (case-insensitively) are replaced by
#' the canonical spelling; any other attribute is uppercased. The geometry
#' column is left untouched. The transformation is idempotent.
#'
#' @param x An \code{sf} object or a data.frame.
#'
#' @return The object with normalised attribute names.
#'
#' @keywords internal
#' @noRd
normalize_boundary_fields <- function(x) {
  nms <- names(x)
  if (is.null(nms) || length(nms) == 0) {
    return(x)
  }

  lookup <- istat_boundary_fields()
  names(lookup) <- tolower(lookup)

  geom_col <- attr(x, "sf_column")

  new_nms <- vapply(
    nms,
    function(nm) {
      if (!is.null(geom_col) && identical(nm, geom_col)) {
        return(nm)
      }
      hit <- unname(lookup[tolower(nm)])
      if (is.na(hit)) toupper(nm) else hit
    },
    character(1),
    USE.NAMES = FALSE
  )

  names(x) <- new_nms
  return(x)
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

#' Base URL of the OnData API v2
#'
#' @keywords internal
#' @noRd
ondata_base_url <- function() {
  "https://www.confini-amministrativi.it/api/v2/it"
}

#' User agent string used for all HTTP requests
#'
#' @keywords internal
#' @noRd
situas_user_agent <- function() {
  "situas R package (https://github.com/gmontaletti/situas)"
}

#' Build OnData download URL
#'
#' The OnData API v2 addresses bulk downloads as
#' \code{/api/v2/it/{YYYYMMDD}/{division}.{format}}, where \code{division} is
#' the division slug (see \code{get_ondata_division()}), not the ISTAT short
#' code.
#'
#' @param date Date or character. Release date.
#' @param territorial_level Character. One of "comuni", "province", "regioni",
#'   "ripartizioni".
#' @param format Character. Distribution format. "zip" returns the shapefile
#'   bundle.
#'
#' @keywords internal
#' @noRd
build_ondata_url <- function(date, territorial_level, format = "zip") {
  format <- match.arg(
    format,
    c("zip", "geo.json", "topo.json", "gpkg", "geo.parquet", "csv")
  )

  date_str <- format_boundary_date(date)
  division <- get_ondata_division(territorial_level)

  paste0(ondata_base_url(), "/", date_str, "/", division, ".", format)
}

#' Build OnData index URL
#'
#' @keywords internal
#' @noRd
build_ondata_index_url <- function(date = NULL) {
  if (is.null(date)) {
    paste0(ondata_base_url(), "/index.json")
  } else {
    paste0(ondata_base_url(), "/", format_boundary_date(date), "/index.json")
  }
}

#' Fetch the list of published releases from the OnData index
#'
#' Reads the HAL index published by OnData and returns the release identifiers
#' actually available, in descending order. Returns NULL when the index cannot
#' be reached or parsed, so callers can decide how to degrade.
#'
#' @keywords internal
#' @noRd
fetch_ondata_releases <- function(verbose = TRUE) {
  url <- build_ondata_index_url()

  response <- tryCatch(
    httr::GET(
      url,
      httr::user_agent(situas_user_agent()),
      httr::timeout(30)
    ),
    error = function(e) NULL
  )

  if (is.null(response) || httr::status_code(response) != 200) {
    if (verbose) {
      message("Could not read the OnData release index at ", url)
    }
    return(NULL)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(
      httr::content(response, as = "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    ),
    error = function(e) NULL
  )

  items <- parsed[["_links"]][["item"]]
  if (is.null(items) || length(items) == 0) {
    if (verbose) {
      message("The OnData release index returned no entries")
    }
    return(NULL)
  }

  releases <- vapply(
    items,
    function(item) as.character(item[["name"]]),
    character(1)
  )
  releases <- releases[grepl("^[0-9]{8}$", releases)]

  if (length(releases) == 0) {
    return(NULL)
  }

  sort(unique(releases), decreasing = TRUE)
}

#' Fallback release list used when the OnData index is unreachable
#'
#' @keywords internal
#' @noRd
fallback_ondata_releases <- function() {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  sort(paste0(2020:current_year, "0101"), decreasing = TRUE)
}

#' List available boundary versions from OnData
#'
#' @keywords internal
#' @noRd
list_ondata_versions <- function(since_year = 2020, verbose = TRUE) {
  if (verbose) {
    message("Querying OnData repository for available boundary versions...")
  }

  releases <- fetch_ondata_releases(verbose = verbose)

  if (is.null(releases)) {
    warning(
      "Could not read the OnData release index; ",
      "falling back to the list of expected annual releases.",
      call. = FALSE
    )
    releases <- fallback_ondata_releases()
  }

  versions <- data.table::data.table(
    date = releases,
    year = as.integer(substr(releases, 1, 4)),
    source = "OnData",
    base_url = ondata_base_url()
  )

  versions <- versions[year >= since_year]
  data.table::setorder(versions, -date)

  return(versions)
}

# 3. ISTAT Fallback Functions -----

#' Base URL of the ISTAT boundary archive
#'
#' @keywords internal
#' @noRd
istat_base_url <- function() {
  "https://www.istat.it/storage/cartografia/confini_amministrativi"
}

#' Earliest year served by the ISTAT storage archive
#'
#' @keywords internal
#' @noRd
istat_min_year <- function() {
  2022L
}

#' Build ISTAT download URL
#'
#' ISTAT publishes a single bundle per reference date containing all four
#' territorial levels, named \code{Limiti{DDMMYYYY}_g.zip} (generalized) or
#' \code{Limiti{DDMMYYYY}.zip} (non generalized).
#'
#' Only January 1st releases from \code{istat_min_year()} onwards are served by
#' the storage endpoint; the function returns NULL for any other date so that
#' callers can skip the ISTAT attempt.
#'
#' @param date Date or character. Reference date.
#' @param generalized Logical. Use the generalized geometries? Default TRUE,
#'   matching the OnData distribution.
#'
#' @return A character URL, or NULL when ISTAT cannot serve the requested date.
#'
#' @keywords internal
#' @noRd
build_istat_url <- function(date, generalized = TRUE) {
  date_obj <- as.Date(date)
  year <- as.integer(format(date_obj, "%Y"))

  # ISTAT storage only exposes January 1st releases, from 2022 onwards
  if (format(date_obj, "%m%d") != "0101" || year < istat_min_year()) {
    return(NULL)
  }

  date_str <- format_filename_date(date_obj)

  if (generalized) {
    paste0(
      istat_base_url(),
      "/generalizzati/",
      year,
      "/Limiti",
      date_str,
      "_g.zip"
    )
  } else {
    paste0(
      istat_base_url(),
      "/non_generalizzati/",
      year,
      "/Limiti",
      date_str,
      ".zip"
    )
  }
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

  # Show a progress bar only when the caller asked for progress messages
  request_args <- list(
    url,
    httr::user_agent(situas_user_agent()),
    httr::write_disk(dest_file, overwrite = TRUE)
  )
  if (verbose) {
    request_args <- c(request_args, list(httr::progress()))
  }

  response <- tryCatch(
    do.call(httr::GET, request_args),
    error = function(e) e
  )

  if (inherits(response, "error")) {
    return(list(success = FALSE, error = conditionMessage(response)))
  }

  # Check if successful
  if (httr::status_code(response) != 200) {
    return(list(success = FALSE, error = httr::http_status(response)$message))
  }

  # Verify it's a valid zip file
  if (!is_valid_zip(dest_file)) {
    unlink(dest_file)
    return(list(
      success = FALSE,
      error = "Downloaded file is not a valid ZIP archive"
    ))
  }

  return(list(success = TRUE, file = dest_file))
}

#' Check if file is a valid ZIP
#'
#' @keywords internal
#' @noRd
is_valid_zip <- function(file) {
  tryCatch(
    {
      zip_list <- utils::unzip(file, list = TRUE)
      return(nrow(zip_list) > 0)
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

#' Build the regex matching the shapefile of a territorial level inside a ZIP
#'
#' The two supported sources ship different archive layouts:
#' \itemize{
#'   \item OnData: one archive per division, shapefile at the root
#'     (\code{comuni.shp}, \code{unita-territoriali-sovracomunali.shp}, ...)
#'   \item ISTAT: one bundle for all levels, shapefile inside a per-level
#'     directory (\code{Com01012026_g/Com01012026_g_WGS84.shp})
#' }
#'
#' @keywords internal
#' @noRd
boundary_shp_pattern <- function(territorial_level, source = "OnData") {
  if (identical(source, "ISTAT")) {
    paste0(
      "(^|/)",
      get_territorial_code(territorial_level),
      "[0-9]{8}_g/[^/]*\\.shp$"
    )
  } else {
    paste0("(^|/)", get_ondata_division(territorial_level), "\\.shp$")
  }
}

#' Extract shapefile from ZIP
#'
#' @param zip_file Path to the downloaded archive.
#' @param date Date or character. Reference date, used to build the cache path.
#' @param territorial_level Character. Territorial level to extract.
#' @param source Character. "OnData" or "ISTAT"; selects the archive layout.
#' @param verbose Logical. Print progress messages?
#'
#' @keywords internal
#' @noRd
extract_boundary_shapefile <- function(
  zip_file,
  date,
  territorial_level,
  source = "OnData",
  verbose = TRUE
) {
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

  # Locate the shapefile for this territorial level
  shp_pattern <- boundary_shp_pattern(territorial_level, source)
  shp_files <- grep(
    shp_pattern,
    zip_contents$Name,
    value = TRUE,
    ignore.case = TRUE
  )

  if (length(shp_files) == 0) {
    return(list(
      success = FALSE,
      error = paste0(
        "No shapefile matching '",
        shp_pattern,
        "' found in ZIP archive"
      )
    ))
  }

  # All sidecar files share the shapefile stem (.shx, .dbf, .prj, .cpg, ...)
  shp_file <- shp_files[1]
  stem <- sub("\\.[^.]*$", "", shp_file)
  related_files <- zip_contents$Name[
    sub("\\.[^.]*$", "", zip_contents$Name) == stem
  ]

  # Extract all related files
  utils::unzip(
    zip_file,
    files = related_files,
    exdir = extract_dir,
    overwrite = TRUE
  )

  # Archive paths are preserved on extraction (ISTAT nests one level deep)
  shp_path <- file.path(extract_dir, shp_file)

  if (!file.exists(shp_path)) {
    return(list(success = FALSE, error = "Shapefile extraction failed"))
  }

  return(list(success = TRUE, shapefile = shp_path))
}

# 5. Main Exported Functions -----

#' Download ISTAT Administrative Boundary Shapefiles
#'
#' Downloads generalized boundary shapefiles for Italian administrative units
#' from the OnData repository, falling back to the ISTAT archive. Boundaries are
#' cached locally for offline use.
#'
#' @param date Date for which to download boundaries. Can be a Date object or
#'   character string in "YYYY-MM-DD" format. Defaults to most recent January 1st.
#'   Use \code{list_istat_boundary_versions()} for the list of published releases.
#' @param territorial_levels Character vector of territorial levels to download.
#'   Options: "comuni", "province", "regioni", "ripartizioni".
#'   Default is all levels.
#' @param output_dir Directory where boundaries will be cached. Default uses
#'   \code{tools::R_user_dir("situas", which = "data")}.
#' @param source Character. Which source to use: "auto" (default) tries OnData
#'   first and falls back to ISTAT, "ondata" and "istat" restrict the download
#'   to a single source.
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
#' which provides ISTAT boundaries in multiple formats with easier programmatic
#' access and a longer historical series (releases from 1991 onwards). Bulk
#' downloads are addressed as
#' \code{/api/v2/it/{YYYYMMDD}/{division}.zip}.
#'
#' If OnData is unavailable, the function falls back to the ISTAT archive
#' (\code{Limiti{DDMMYYYY}_g.zip}), which serves January 1st releases from 2022
#' onwards as a single bundle covering all territorial levels.
#'
#' Attribute names differ between the two distributions (OnData ships lowercase
#' names, ISTAT uppercase ones). They are normalised to the ISTAT convention on
#' read, so downstream functions behave identically regardless of source.
#'
#' @section File Organization:
#' Downloaded boundaries are organized by date:
#' \preformatted{
#' {cache_dir}/boundaries/
#'   ├── 20260101/
#'   │   ├── comuni.shp                                  # OnData
#'   │   ├── unita-territoriali-sovracomunali.shp
#'   │   └── Com01012026_g/Com01012026_g_WGS84.shp       # ISTAT fallback
#'   └── metadata.rds
#' }
#'
#' @examples
#' \dontrun{
#' # Download all boundaries for the current release
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
#'
#' # Bypass OnData and use the ISTAT archive directly
#' result <- download_istat_boundaries(
#'   date = "2026-01-01",
#'   source = "istat"
#' )
#' }
#'
#' @seealso
#' \code{\link{list_istat_boundary_versions}} to see available versions
#' \code{\link{check_boundary_updates}} to check for updates
#' \code{\link{get_cached_boundaries_info}} to see cached boundaries
#'
#' @export
download_istat_boundaries <- function(
  date = NULL,
  territorial_levels = c("comuni", "province", "regioni", "ripartizioni"),
  output_dir = NULL,
  source = c("auto", "ondata", "istat"),
  force_refresh = FALSE,
  verbose = TRUE
) {
  # 1. Validate inputs -----
  stopifnot(
    is.logical(force_refresh),
    is.logical(verbose)
  )

  source <- match.arg(source)

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
    stop(
      "Invalid territorial levels: ",
      paste(invalid_levels, collapse = ", "),
      "\nValid options: ",
      paste(valid_levels, collapse = ", ")
    )
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

  # Sources are tried in order; the ISTAT bundle covers all levels, so it is
  # downloaded at most once per call and reused across the loop
  attempt_sources <- switch(
    source,
    auto = c("OnData", "ISTAT"),
    ondata = "OnData",
    istat = "ISTAT"
  )

  istat_bundle <- NULL
  release_hint <- NULL

  on.exit(
    {
      if (!is.null(istat_bundle)) {
        unlink(istat_bundle)
      }
    },
    add = TRUE
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
        results <- data.table::rbindlist(list(
          results,
          data.table::data.table(
            territorial_level = level,
            status = "cached",
            source = cached$source[1],
            file_path = cached_file,
            error = NA_character_
          )
        ))
        next
      }
    }

    # Try each source in turn until one yields a usable shapefile
    outcome <- NULL
    errors <- character(0)

    for (src in attempt_sources) {
      if (identical(src, "OnData")) {
        url <- build_ondata_url(date, level)
        zip_path <- tempfile(fileext = ".zip")

        download_result <- download_boundary_file(url, zip_path, verbose)

        if (!download_result$success) {
          errors <- c(errors, paste0("OnData: ", download_result$error))
          unlink(zip_path)
          next
        }
      } else {
        url <- build_istat_url(date)

        if (is.null(url)) {
          errors <- c(
            errors,
            paste0(
              "ISTAT: no direct download for ",
              date_str,
              " (January 1st releases from ",
              istat_min_year(),
              " onwards only)"
            )
          )
          next
        }

        # The ISTAT bundle contains every level: download it only once
        if (is.null(istat_bundle)) {
          bundle_path <- tempfile(fileext = ".zip")
          download_result <- download_boundary_file(url, bundle_path, verbose)

          if (!download_result$success) {
            errors <- c(errors, paste0("ISTAT: ", download_result$error))
            unlink(bundle_path)
            next
          }

          istat_bundle <- bundle_path
        }

        zip_path <- istat_bundle
      }

      extract_result <- extract_boundary_shapefile(
        zip_path,
        date,
        level,
        source = src,
        verbose = verbose
      )

      # The OnData archive is per-level and no longer needed once extracted
      if (identical(src, "OnData")) {
        unlink(zip_path)
      }

      if (!extract_result$success) {
        errors <- c(errors, paste0(src, ": ", extract_result$error))
        next
      }

      outcome <- list(source = src, shapefile = extract_result$shapefile)
      break
    }

    if (!is.null(outcome)) {
      # Update metadata
      file_size <- file.info(outcome$shapefile)$size / 1024^2 # MB

      new_entry <- data.table::data.table(
        date = date_str,
        territorial_level = level,
        source = outcome$source,
        file_path = outcome$shapefile,
        download_timestamp = Sys.time(),
        file_size_mb = round(file_size, 2)
      )

      # Remove old entry if exists
      metadata <- metadata[!(date == date_str & territorial_level == level)]
      metadata <- data.table::rbindlist(list(metadata, new_entry))
      save_boundaries_metadata(metadata)

      results <- data.table::rbindlist(list(
        results,
        data.table::data.table(
          territorial_level = level,
          status = "success",
          source = outcome$source,
          file_path = outcome$shapefile,
          error = NA_character_
        )
      ))

      if (verbose) {
        message(
          "Successfully downloaded and extracted: ",
          level,
          " (source: ",
          outcome$source,
          ")"
        )
      }
    } else {
      error_msg <- paste(errors, collapse = "; ")

      # A 404 usually means the release itself does not exist: say which do
      if (any(grepl("404", errors, fixed = TRUE))) {
        if (is.null(release_hint)) {
          release_hint <- fetch_ondata_releases(verbose = FALSE)
        }

        if (!is.null(release_hint) && !date_str %in% release_hint) {
          shown <- release_hint[seq_len(min(8, length(release_hint)))]
          error_msg <- paste0(
            error_msg,
            ". No boundary release published for ",
            date_str,
            "; most recent available: ",
            paste(shown, collapse = ", "),
            if (length(release_hint) > length(shown)) ", ..." else "",
            ". See list_istat_boundary_versions() for the full list"
          )
        }
      }

      results <- data.table::rbindlist(list(
        results,
        data.table::data.table(
          territorial_level = level,
          status = "failed",
          source = paste(attempt_sources, collapse = "/"),
          file_path = NA_character_,
          error = error_msg
        )
      ))

      if (verbose) {
        warning("Failed to download ", level, ": ", error_msg)
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
#' Query available boundary shapefile versions from the OnData repository. The
#' list is read from the repository index, so it reflects the releases actually
#' published (the series starts in 1991 and not every release falls on
#' January 1st). Results are cached for 24 hours to reduce API calls.
#'
#' If the index cannot be reached, the function warns and returns the expected
#' annual releases from 2020 onwards as a fallback.
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
list_istat_boundary_versions <- function(
  since_year = 2020,
  use_cache = TRUE,
  verbose = TRUE
) {
  stopifnot(
    is.numeric(since_year),
    is.logical(use_cache),
    is.logical(verbose)
  )

  # Check cache
  cache_dir <- get_cache_dir() # Use existing cache system
  cache_key <- paste0("boundary_versions_", since_year)
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))

  if (use_cache && file.exists(cache_file)) {
    cache_age <- difftime(
      Sys.time(),
      file.info(cache_file)$mtime,
      units = "hours"
    )
    if (cache_age < 24) {
      if (verbose) {
        message(
          "Using cached version list (age: ",
          round(cache_age, 1),
          " hours)"
        )
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
  # Get available versions (ordered by date, most recent first)
  available <- list_istat_boundary_versions(verbose = FALSE)
  latest_date <- available$date[1]

  # Get cached boundaries
  metadata <- load_boundaries_metadata()

  # Get all territorial levels
  all_levels <- c("comuni", "province", "regioni", "ripartizioni")

  # Build comparison table
  comparison <- data.table::data.table(
    territorial_level = all_levels
  )

  comparison[,
    current_date := {
      cached <- metadata[territorial_level == .BY[[1]]]
      if (nrow(cached) > 0) {
        max(cached$date)
      } else {
        NA_character_
      }
    },
    by = territorial_level
  ]

  comparison[, latest_date := latest_date]
  comparison[,
    update_available := is.na(current_date) | current_date < latest_date
  ]

  comparison[,
    current_source := {
      cached <- metadata[territorial_level == .BY[[1]]]
      if (nrow(cached) > 0) {
        cached$source[which.max(cached$date)]
      } else {
        NA_character_
      }
    },
    by = territorial_level
  ]

  if (verbose) {
    message("Boundary Update Status:")
    print(comparison)

    if (any(comparison$update_available)) {
      message(
        "\nUpdates are available! Run download_istat_boundaries() to update."
      )
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
get_cached_boundaries_info <- function(
  territorial_level = "all",
  verbose = TRUE
) {
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
    level_filter <- territorial_level
    metadata <- metadata[territorial_level == level_filter]
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
clean_boundary_cache <- function(
  keep_latest_n = 1,
  older_than_days = NULL,
  territorial_level = "all",
  dry_run = FALSE,
  verbose = TRUE
) {
  stopifnot(
    "keep_latest_n must be NULL or a non-negative number" = is.null(
      keep_latest_n
    ) ||
      (is.numeric(keep_latest_n) && keep_latest_n >= 0),
    "older_than_days must be NULL or a non-negative number" = is.null(
      older_than_days
    ) ||
      (is.numeric(older_than_days) && older_than_days >= 0),
    "dry_run must be logical" = is.logical(dry_run),
    "verbose must be logical" = is.logical(verbose)
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

  # Strategy 1: Keep only N most recent versions per level
  if (!is.null(keep_latest_n)) {
    data.table::setorder(metadata, territorial_level, -date)
    metadata[, rank := seq_len(.N), by = territorial_level]
    to_remove_by_rank <- metadata[rank > keep_latest_n]
  } else {
    to_remove_by_rank <- metadata[FALSE]
  }

  # Strategy 2: Remove versions older than N days
  if (!is.null(older_than_days)) {
    cutoff_date <- Sys.time() - (older_than_days * 24 * 60 * 60)
    to_remove_by_age <- metadata[download_timestamp < cutoff_date]
  } else {
    to_remove_by_age <- metadata[FALSE]
  }

  # Combine and remove duplicates
  to_remove <- data.table::rbindlist(
    list(to_remove_by_rank, to_remove_by_age),
    fill = TRUE
  )
  to_remove <- unique(to_remove)
  if ("rank" %in% names(to_remove)) {
    to_remove[, rank := NULL]
  }

  if (nrow(to_remove) == 0) {
    if (verbose) {
      message("No files meet the removal criteria.")
    }
    return(invisible(character(0)))
  }

  # Display what will be removed
  if (verbose || dry_run) {
    message(
      ifelse(dry_run, "Would remove", "Removing"),
      " ",
      nrow(to_remove),
      " file(s):"
    )
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
  if ("rank" %in% names(metadata)) {
    metadata[, rank := NULL]
  }
  save_boundaries_metadata(metadata)

  if (verbose) {
    message("Successfully removed ", length(removed_files), " file(s).")
  }

  return(invisible(removed_files))
}
