#' Prepare Geographic Maps of Italian Territorial Units
#'
#' Creates map-ready files by joining SITUAS data with ISTAT shapefiles for any
#' territorial level. Exports multiple formats for R dashboards (RDS, GeoJSON)
#' and Power BI (TopoJSON). Supports municipalities, provinces, regions, and
#' geographic partitions.
#'
#' @param situas_data data.table. SITUAS data to join with shapefile. If provided,
#'   this data will be used instead of downloading from the API. Must contain at
#'   least the appropriate join field for the territorial level. Default is NULL
#'   (download from API).
#' @param pfun Integer or NULL. SITUAS report ID to download. Only used if
#'   \code{situas_data} is NULL. If both \code{situas_data} and \code{pfun} are
#'   NULL, defaults based on \code{territorial_level}. Also used for file naming.
#'   Common values:
#'   \itemize{
#'     \item \strong{61}: Current municipalities - Elenco comuni (default for "comuni")
#'     \item \strong{64}: Provinces/UTS - Elenco Province/Uts (default for "province")
#'     \item \strong{68}: Regions - Elenco Regioni (default for "regioni")
#'     \item \strong{71}: Geographic partitions - Ripartizioni geografiche (default for "ripartizioni")
#'   }
#'   Use \code{search_reports()} to find other available reports.
#' @param territorial_level Character string. Level of territorial unit to map.
#'   Must be one of: "comuni" (municipalities), "province" (provinces/metropolitan
#'   cities), "regioni" (regions), or "ripartizioni" (geographic partitions).
#'   Default is "comuni".
#' @param output_dir Character string. Directory where output files will be saved.
#'   Defaults to "data". Directory will be created if it doesn't exist.
#' @param date Date, POSIXct, or character string. Reference date for SITUAS data.
#'   Only used if \code{situas_data} is NULL and data is downloaded from API.
#'   Defaults to current date. The date determines which territorial configuration
#'   is returned (as territories change over time).
#' @param simplify Logical. If TRUE (default), simplify geometries to reduce file
#'   size while preserving topology. Recommended for web mapping and dashboards.
#' @param tolerance Numeric. Simplification tolerance (in map units, degrees for
#'   WGS84). Default is 0.001. Higher values = more simplification. Only used if
#'   simplify = TRUE and rmapshaper package is installed.
#' @param keep_attributes Character vector. Attributes to keep in the output.
#'   Defaults to NULL (keep all attributes from SITUAS report). Set to a character
#'   vector to filter specific attributes. Adjust based on your chosen report and
#'   territorial level.
#' @param verbose Logical. If TRUE (default), print progress messages.
#'
#' @section Setup:
#' For better performance, extract the shapefile once before first use:
#' \code{source("data-raw/extract_shapefiles.R")}
#'
#' If not pre-extracted, the shapefile will be extracted to a temporary directory
#' each time this function is called (slower but still works).
#'
#' @return Invisibly returns a list with paths to the created files:
#'   \describe{
#'     \item{rds}{Path to RDS file (merged R sf object with SITUAS attributes)}
#'     \item{geojson}{Path to GeoJSON file (for Leaflet/web mapping)}
#'     \item{topojson}{Path to TopoJSON file (for Power BI)}
#'     \item{shapefile_rds}{Path to RDS file (original sf shapefile before merge)}
#'     \item{situas_rds}{Path to RDS file (SITUAS data table before merge)}
#'   }
#'
#' @details
#' \strong{Input Data:}
#' \itemize{
#'   \item Shapefile: \code{data-raw/Limiti01012025_g.zip} containing territorial
#'     boundaries in WGS84 projection for all levels
#'   \item SITUAS data: Appropriate report based on \code{pfun} or \code{territorial_level}
#' }
#'
#' \strong{Join Keys by Territorial Level:}
#' \itemize{
#'   \item \strong{comuni}: PRO_COM - Province-municipality code (6 digits)
#'   \item \strong{province}: COD_UTS - Province/UTS code (3 digits)
#'   \item \strong{regioni}: COD_REG - Region code (2 digits)
#'   \item \strong{ripartizioni}: COD_RIP - Geographic partition code (1 digit)
#' }
#'
#' \strong{Output Files:}
#' \itemize{
#'   \item \strong{RDS (merged)}: Fast loading in R, preserves merged sf object with
#'     SITUAS attributes. Best for R dashboards and analysis.
#'   \item \strong{GeoJSON}: Web standard, works with Leaflet and most mapping
#'     libraries. Human-readable format.
#'   \item \strong{TopoJSON}: Microsoft Power BI standard for Shape Map visual.
#'     More compact than GeoJSON.
#'   \item \strong{Shapefile RDS}: Original sf shapefile with geometries only
#'     (before merge with SITUAS). Useful for custom joins.
#'   \item \strong{SITUAS RDS}: SITUAS data table (before merge). Useful for
#'     separate attribute analysis.
#' }
#'
#' \strong{File Sizes (approximate):}
#' \itemize{
#'   \item Comuni original: ~12 MB, simplified: ~1-2 MB
#'   \item Province original: ~2 MB, simplified: ~0.5 MB
#'   \item Regioni original: ~1 MB, simplified: ~0.3 MB
#'   \item Ripartizioni original: ~0.5 MB, simplified: ~0.2 MB
#'   \item TopoJSON is typically most compact
#' }
#'
#' \strong{Using in Power BI:}
#' \enumerate{
#'   \item Open Power BI Desktop
#'   \item Insert > Shape Map visual
#'   \item Click on the visual, then Format > Shape > Map settings
#'   \item Click "+ Add map" and select the .json (TopoJSON) file
#'   \item Map your data using the appropriate join field
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. MUNICIPALITIES (default)
#' files <- prepare_territorial_maps()
#' # or explicitly
#' files <- prepare_territorial_maps(territorial_level = "comuni")
#'
#' # 2. PROVINCES
#' files <- prepare_territorial_maps(
#'   territorial_level = "province",
#'   keep_attributes = c("COD_UTS", "DEN_UTS", "COD_REG", "DEN_REG")
#' )
#'
#' # 3. REGIONS
#' files <- prepare_territorial_maps(
#'   territorial_level = "regioni",
#'   keep_attributes = c("COD_REG", "DEN_REG")
#' )
#'
#' # 4. GEOGRAPHIC PARTITIONS
#' files <- prepare_territorial_maps(
#'   territorial_level = "ripartizioni",
#'   keep_attributes = c("COD_RIP", "DEN_RIP")
#' )
#'
#' # Pass pre-downloaded SITUAS data (recommended for custom workflows)
#' regioni_data <- get_situas_tables(pfun = 68, date = "2025-01-01")
#' files <- prepare_territorial_maps(
#'   situas_data = regioni_data,
#'   territorial_level = "regioni"
#' )
#'
#' # Use in Leaflet
#' library(leaflet)
#' map_sf <- readRDS(files$rds)
#' leaflet(map_sf) %>%
#'   addTiles() %>%
#'   addPolygons(popup = ~DEN_REG)  # or ~COMUNE, ~DEN_UTS, etc.
#'
#' # Create maps for a specific date (auto-download)
#' files <- prepare_territorial_maps(
#'   territorial_level = "comuni",
#'   date = "2020-01-01"
#' )
#'
#' # No simplification (larger files, more detail)
#' files <- prepare_territorial_maps(
#'   territorial_level = "province",
#'   simplify = FALSE
#' )
#'
#' # Custom report with specific pfun
#' files <- prepare_territorial_maps(
#'   pfun = 73,  # Municipality characteristics
#'   territorial_level = "comuni",
#'   keep_attributes = NULL  # Keep all attributes
#' )
#'
#' # Find available reports
#' reports <- search_reports()
#' print(reports[, .(pfun, title, analysis_type)])
#' }
prepare_territorial_maps <- function(situas_data = NULL,
                                      pfun = NULL,
                                      territorial_level = "comuni",
                                      output_dir = "data",
                                      date = Sys.Date(),
                                      simplify = TRUE,
                                      tolerance = 0.001,
                                      keep_attributes = NULL,
                                      verbose = TRUE) {
  # 1. Input validation -----

  # Validate basic parameters first (before using them)
  stopifnot(
    "territorial_level must be a character string" =
      is.character(territorial_level) && length(territorial_level) == 1,
    "output_dir must be a character string" =
      is.character(output_dir) && length(output_dir) == 1,
    "simplify must be non-NA logical" =
      is.logical(simplify) && length(simplify) == 1 && !is.na(simplify),
    "tolerance must be positive numeric" =
      is.numeric(tolerance) && length(tolerance) == 1 && tolerance > 0,
    "verbose must be logical" =
      is.logical(verbose) && length(verbose) == 1 && !is.na(verbose)
  )

  # Validate territorial_level
  valid_levels <- c("comuni", "province", "regioni", "ripartizioni")
  if (!territorial_level %in% valid_levels) {
    stop(
      "territorial_level must be one of: ", paste(valid_levels, collapse = ", "),
      "\nYou provided: ", territorial_level,
      call. = FALSE
    )
  }

  # Get join field for this territorial level
  join_field <- get_join_field(territorial_level)

  # Validate situas_data parameter
  if (!is.null(situas_data)) {
    stopifnot(
      "situas_data must be a data.frame or data.table" =
        is.data.frame(situas_data)
    )
    if (!join_field %in% names(situas_data)) {
      stop(
        "situas_data must contain a '", join_field, "' column for joining with ",
        territorial_level, " shapefile",
        call. = FALSE
      )
    }
  }

  # Determine if we need to download data
  download_data <- is.null(situas_data)

  # If downloading, pfun must be provided or default based on territorial level
  if (download_data) {
    if (is.null(pfun)) {
      pfun <- get_default_pfun(territorial_level)
      if (verbose) {
        message(
          "No situas_data or pfun provided. Defaulting to pfun = ", pfun,
          " (", territorial_level, ")"
        )
      }
    }
  }

  if (!is.null(keep_attributes)) {
    stopifnot("keep_attributes must be character vector" = is.character(keep_attributes))
  }

  # Validate pfun if provided (needed for file naming even with custom data)
  report_info <- NULL
  if (!is.null(pfun)) {
    stopifnot(
      "pfun must be a single integer" =
        is.numeric(pfun) && length(pfun) == 1 && pfun == as.integer(pfun)
    )

    pfun <- as.integer(pfun)
    report_info <- get_report_details(pfun)

    if (nrow(report_info) == 0 && download_data) {
      warning(
        "Report ID ", pfun, " is not in the list of known SITUAS reports.\n",
        "Use search_reports() to see available reports.\n",
        "Proceeding anyway...",
        call. = FALSE
      )
    }
  } else {
    # If situas_data provided without pfun, use "custom" for file naming
    pfun <- "custom"
  }

  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (verbose) {
      message("Created output directory: ", output_dir)
    }
  }

  # 2. Read shapefile -----
  if (verbose) {
    message("\n=== Step 1/6: Reading ", territorial_level, " shapefile ===")
  }

  shapefile_sf <- read_territorial_shapefile(
    territorial_level = territorial_level,
    verbose = verbose
  )

  if (verbose) {
    message("  Shapefile CRS: ", sf::st_crs(shapefile_sf)$input)
    message("  Number of polygons: ", nrow(shapefile_sf))
  }

  # 3. Get SITUAS data -----
  if (verbose) {
    message("\n=== Step 2/6: Getting SITUAS data ===")
  }

  if (download_data) {
    # Download from API
    if (verbose && !is.null(report_info) && nrow(report_info) > 0) {
      message("  Report: ", report_info$title[1])
    }

    situas_data <- get_situas_tables(pfun = pfun, date = date, verbose = verbose)
  } else {
    # Use provided data
    if (verbose) {
      message("  Using provided situas_data")
    }
    # Ensure it's a data.table for consistency
    if (!data.table::is.data.table(situas_data)) {
      situas_data <- data.table::as.data.table(situas_data)
    }
  }

  if (verbose) {
    message("  Number of records in SITUAS: ", nrow(situas_data))
  }

  # 3.5. Save pre-merge data -----
  if (verbose) {
    message("\n=== Step 3/6: Saving pre-merge data ===")
  }

  # Generate file names for pre-merge data
  date_str <- format(as.Date(date), "%Y%m%d")

  # Save shapefile (sf object with geometries)
  file_shapefile_rds <- file.path(
    output_dir,
    paste0("situa_shapefile_", territorial_level, "_", pfun, "_", date_str, ".rds")
  )
  saveRDS(shapefile_sf, file = file_shapefile_rds, compress = "xz")
  if (verbose) {
    file_size <- file.size(file_shapefile_rds)
    message("  Saved shapefile: ", file_shapefile_rds,
            " (", round(file_size / 1024^2, 2), " MB)")
  }

  # Save SITUAS data table
  file_situas_rds <- file.path(
    output_dir,
    paste0("situa_data_", territorial_level, "_", pfun, "_", date_str, ".rds")
  )
  saveRDS(situas_data, file = file_situas_rds, compress = "xz")
  if (verbose) {
    file_size <- file.size(file_situas_rds)
    message("  Saved SITUAS data: ", file_situas_rds,
            " (", round(file_size / 1024^2, 2), " MB)")
  }

  # 4. Join spatial and attribute data -----
  if (verbose) {
    message("\n=== Step 4/6: Joining spatial and attribute data ===")
    message("  Join field: ", join_field)
  }

  # Convert join field to character for consistent joining
  if (join_field %in% names(shapefile_sf)) {
    shapefile_sf[[join_field]] <- as.character(as.integer(shapefile_sf[[join_field]]))
  }

  if (join_field %in% names(situas_data)) {
    situas_data[[join_field]] <- as.character(as.integer(situas_data[[join_field]]))
  }

  # Create text version of join field with leading zeros if appropriate
  text_field <- paste0(join_field, "_T")
  field_width <- switch(
    territorial_level,
    "comuni" = 6,
    "province" = 3,
    "regioni" = 2,
    "ripartizioni" = 1
  )

  if (!text_field %in% names(situas_data) && join_field %in% names(situas_data)) {
    situas_data[[text_field]] <- sprintf(
      paste0("%0", field_width, "d"),
      as.integer(situas_data[[join_field]])
    )
  }

  # Remove duplicate join field values from SITUAS to avoid conflicts
  situas_data <- unique(situas_data, by = join_field)

  # Identify conflicting columns and keep only SITUAS versions
  shapefile_cols <- names(shapefile_sf)
  situas_cols <- names(situas_data)
  common_cols <- intersect(shapefile_cols, situas_cols)
  common_cols <- setdiff(common_cols, c(join_field, attr(shapefile_sf, "sf_column")))

  if (length(common_cols) > 0 && verbose) {
    message("  Resolving ", length(common_cols), " duplicate columns: ",
            paste(common_cols, collapse = ", "))
  }

  # Remove conflicting columns from shapefile (keep SITUAS versions)
  shapefile_sf <- shapefile_sf[, setdiff(names(shapefile_sf), common_cols)]

  # Join: left join to keep all shapefile features
  territorial_map <- merge(
    shapefile_sf,
    situas_data,
    by = join_field,
    all.x = TRUE
  )

  # Report on join quality
  # Find a name field to check for matches
  name_fields <- c("COMUNE", "DEN_UTS", "DEN_REG", "DEN_RIP")
  check_field <- NULL
  for (field in name_fields) {
    if (field %in% names(territorial_map)) {
      check_field <- field
      break
    }
  }

  if (!is.null(check_field)) {
    n_matched <- sum(!is.na(territorial_map[[check_field]]))
    n_unmatched <- nrow(territorial_map) - n_matched

    if (verbose) {
      message("  Successfully matched: ", n_matched, " ", territorial_level)
      if (n_unmatched > 0) {
        message("  Unmatched polygons: ", n_unmatched)
        unmatched_codes <- territorial_map[[join_field]][is.na(territorial_map[[check_field]])]
        if (length(unmatched_codes) <= 10) {
          message("  Unmatched codes: ", paste(unmatched_codes, collapse = ", "))
        }
      }
    }
  } else {
    if (verbose) {
      message("  Join completed: ", nrow(territorial_map), " features")
    }
  }

  # 5. Filter attributes -----
  if (!is.null(keep_attributes)) {
    # Keep geometry and specified attributes
    all_cols <- names(territorial_map)
    geom_col <- attr(territorial_map, "sf_column")
    cols_to_keep <- c(intersect(keep_attributes, all_cols), geom_col)
    territorial_map <- territorial_map[, cols_to_keep]

    if (verbose) {
      message("  Kept attributes: ", paste(setdiff(cols_to_keep, geom_col), collapse = ", "))
    }
  }

  # 6. Simplify geometries -----
  if (simplify) {
    if (verbose) {
      message("\n=== Step 5/6: Simplifying geometries ===")
    }

    if (requireNamespace("rmapshaper", quietly = TRUE)) {
      # Calculate original size
      size_before <- object.size(territorial_map)

      # Simplify
      territorial_map <- rmapshaper::ms_simplify(
        territorial_map,
        keep = tolerance,
        keep_shapes = TRUE
      )

      size_after <- object.size(territorial_map)
      reduction_pct <- round((1 - as.numeric(size_after) / as.numeric(size_before)) * 100, 1)

      if (verbose) {
        message("  Size before: ", format(size_before, units = "MB"))
        message("  Size after: ", format(size_after, units = "MB"))
        message("  Reduction: ", reduction_pct, "%")
      }
    } else {
      if (verbose) {
        message("  rmapshaper package not installed - skipping simplification")
        message("  Install with: install.packages('rmapshaper')")
      }
    }
  }

  # 7. Export files -----
  if (verbose) {
    message("\n=== Step 6/6: Exporting merged map files ===")
  }

  # Generate file names with territorial level, date, and report ID
  date_str <- format(as.Date(date), "%Y%m%d")
  base_name <- paste0("situas_map_", territorial_level, "_", pfun, "_", date_str)

  # Output file paths
  file_rds <- file.path(output_dir, paste0(base_name, ".rds"))
  file_geojson <- file.path(output_dir, paste0(base_name, ".geojson"))
  file_topojson <- file.path(output_dir, paste0(base_name, "_powerbi.json"))

  # Export RDS
  saveRDS(territorial_map, file = file_rds, compress = "xz")
  if (verbose) {
    file_size <- file.size(file_rds)
    message("  Saved RDS: ", file_rds, " (", round(file_size / 1024^2, 2), " MB)")
  }

  # Export GeoJSON
  sf::st_write(
    territorial_map,
    dsn = file_geojson,
    driver = "GeoJSON",
    delete_dsn = TRUE,
    quiet = !verbose
  )
  if (verbose) {
    file_size <- file.size(file_geojson)
    message("  Saved GeoJSON: ", file_geojson, " (", round(file_size / 1024^2, 2), " MB)")
  }

  # Export TopoJSON
  tryCatch(
    {
      geojsonio::topojson_write(
        territorial_map,
        file = file_topojson,
        object_name = territorial_level,
        overwrite = TRUE
      )

      if (verbose) {
        file_size <- file.size(file_topojson)
        message("  Saved TopoJSON: ", file_topojson, " (", round(file_size / 1024^2, 2), " MB)")
      }
    },
    error = function(e) {
      warning(
        "Failed to create TopoJSON file: ", e$message,
        "\nTrying alternative method via GeoJSON...",
        call. = FALSE
      )

      # Alternative method: write GeoJSON then convert
      temp_geojson <- tempfile(fileext = ".geojson")
      sf::st_write(territorial_map, dsn = temp_geojson, driver = "GeoJSON", quiet = TRUE)

      # Read as text and convert to TopoJSON
      geojson_text <- paste(readLines(temp_geojson), collapse = "\n")
      topo <- geojsonio::geo2topo(geojson_text, object_name = territorial_level)

      # Write TopoJSON
      writeLines(topo, file_topojson)

      # Clean up temp file
      unlink(temp_geojson)

      if (verbose && file.exists(file_topojson)) {
        file_size <- file.size(file_topojson)
        message("  Saved TopoJSON (alternative method): ", file_topojson,
                " (", round(file_size / 1024^2, 2), " MB)")
      }
    }
  )

  if (verbose) {
    message("\n=== Complete! ===")
    all_files <- c(file_shapefile_rds, file_situas_rds, file_rds, file_geojson, file_topojson)
    message("Created ", sum(file.exists(all_files)), " files total")
  }

  # Return file paths
  result <- list(
    rds = file_rds,
    geojson = file_geojson,
    topojson = file_topojson,
    shapefile_rds = file_shapefile_rds,
    situas_rds = file_situas_rds
  )

  invisible(result)
}


#' Get Join Field for Territorial Level
#'
#' Returns the appropriate field name for joining SITUAS data with shapefiles
#' based on the territorial level.
#'
#' @param territorial_level Character string. One of: "comuni", "province",
#'   "regioni", "ripartizioni".
#'
#' @return Character string with the join field name
#'
#' @keywords internal
#' @noRd
get_join_field <- function(territorial_level) {
  switch(
    territorial_level,
    "comuni" = "PRO_COM",
    "province" = "COD_UTS",
    "regioni" = "COD_REG",
    "ripartizioni" = "COD_RIP",
    stop("Unknown territorial_level: ", territorial_level, call. = FALSE)
  )
}


#' Get Default SITUAS Report ID for Territorial Level
#'
#' Returns the default SITUAS report ID (pfun) for a given territorial level.
#' These are the standard reports for listing current territorial units.
#'
#' @param territorial_level Character string. One of: "comuni", "province",
#'   "regioni", "ripartizioni".
#'
#' @return Integer. Default pfun value
#'
#' @keywords internal
#' @noRd
get_default_pfun <- function(territorial_level) {
  switch(
    territorial_level,
    "comuni" = 61L,
    "province" = 64L,
    "regioni" = 68L,
    "ripartizioni" = 71L,
    stop("Unknown territorial_level: ", territorial_level, call. = FALSE)
  )
}


#' Read Italian Territorial Shapefile
#'
#' Internal helper function to read territorial shapefiles from extracted files
#' or from the ISTAT zip archive. Supports all territorial levels.
#'
#' @param territorial_level Character string. One of: "comuni", "province",
#'   "regioni", "ripartizioni".
#' @param verbose Logical. Print progress messages.
#'
#' @return An sf object with territorial boundaries
#'
#' @details
#' This function first looks for pre-extracted shapefiles in
#' \code{data-raw/Limiti01012025_g/}. If not found, it will extract
#' from the zip file to a temporary directory.
#'
#' For better performance, extract the zip file once using:
#' \code{source("data-raw/extract_shapefiles.R")}
#'
#' Shapefile patterns in zip:
#' \itemize{
#'   \item comuni: Com01012025_g/Com01012025_g_WGS84.shp
#'   \item province: ProvCM01012025_g/ProvCM01012025_g_WGS84.shp
#'   \item regioni: Reg01012025_g/Reg01012025_g_WGS84.shp
#'   \item ripartizioni: RipGeo01012025_g/RipGeo01012025_g_WGS84.shp
#' }
#'
#' @keywords internal
#' @noRd
read_territorial_shapefile <- function(territorial_level = "comuni",
                                        verbose = TRUE) {
  # Find package root or use current directory
  pkg_root <- tryCatch(
    {
      find_package_root()
    },
    error = function(e) {
      getwd()
    }
  )

  # Map territorial level to directory name and file pattern
  dir_pattern <- switch(
    territorial_level,
    "comuni" = "Com01012025_g",
    "province" = "ProvCM01012025_g",
    "regioni" = "Reg01012025_g",
    "ripartizioni" = "RipGeo01012025_g",
    stop("Unknown territorial_level: ", territorial_level, call. = FALSE)
  )

  file_pattern <- switch(
    territorial_level,
    "comuni" = "Com.*\\.shp$",
    "province" = "ProvCM.*\\.shp$",
    "regioni" = "Reg.*\\.shp$",
    "ripartizioni" = "RipGeo.*\\.shp$",
    stop("Unknown territorial_level: ", territorial_level, call. = FALSE)
  )

  # First, try to read from pre-extracted directory
  extracted_dir <- file.path(pkg_root, "data-raw", dir_pattern)

  if (dir.exists(extracted_dir)) {
    # Look for shapefile in extracted directory
    shp_files <- list.files(
      extracted_dir,
      pattern = "\\.shp$",
      full.names = TRUE
    )

    if (length(shp_files) > 0) {
      shp_path <- shp_files[1]

      if (verbose) {
        message("  Reading shapefile from extracted files: ", basename(shp_path))
      }

      shapefile_sf <- sf::st_read(shp_path, quiet = !verbose)

      # Verify and transform CRS
      shapefile_sf <- ensure_wgs84_crs(shapefile_sf, verbose)

      return(shapefile_sf)
    }
  }

  # If not found in extracted directory, extract from zip
  if (verbose) {
    message("  Extracted files not found. Extracting from zip...")
    message("  TIP: Run source('data-raw/extract_shapefiles.R') once for better performance")
  }

  # Path to zip file
  zip_path <- file.path(pkg_root, "data-raw", "Limiti01012025_g.zip")

  if (!file.exists(zip_path)) {
    stop(
      "Shapefile not found at: ", zip_path,
      "\nExpected: data-raw/Limiti01012025_g.zip",
      "\nOr pre-extracted files at: ", extracted_dir,
      call. = FALSE
    )
  }

  # Create temporary directory for extraction
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Extract zip
  if (verbose) {
    message("  Extracting shapefile from zip...")
  }

  utils::unzip(zip_path, exdir = temp_dir, junkpaths = FALSE)

  # Find the shapefile
  shp_files <- list.files(
    temp_dir,
    pattern = file_pattern,
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(shp_files) == 0) {
    stop(
      "No ", territorial_level, " shapefile found in zip archive",
      call. = FALSE
    )
  }

  # Read the shapefile (use first match if multiple)
  shp_path <- shp_files[1]

  if (verbose) {
    message("  Reading shapefile: ", basename(shp_path))
  }

  shapefile_sf <- sf::st_read(shp_path, quiet = !verbose)

  # Verify and transform CRS
  shapefile_sf <- ensure_wgs84_crs(shapefile_sf, verbose)

  # Clean up temp directory
  unlink(temp_dir, recursive = TRUE)

  return(shapefile_sf)
}


#' Ensure Shapefile is in WGS84 CRS
#'
#' Internal helper to verify and transform CRS to WGS84 (EPSG:4326)
#'
#' @param sf_obj An sf object
#' @param verbose Logical. Print messages
#'
#' @return sf object with WGS84 CRS
#'
#' @keywords internal
#' @noRd
ensure_wgs84_crs <- function(sf_obj, verbose = TRUE) {
  if (is.na(sf::st_crs(sf_obj))) {
    if (verbose) {
      message("  Warning: Shapefile has no CRS defined. Assuming WGS84 (EPSG:4326)")
    }
    sf::st_crs(sf_obj) <- 4326
  } else if (sf::st_crs(sf_obj)$epsg != 4326) {
    if (verbose) {
      message("  Transforming CRS to WGS84 (EPSG:4326)...")
    }
    sf_obj <- sf::st_transform(sf_obj, crs = 4326)
  }

  return(sf_obj)
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
