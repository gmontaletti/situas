#' Convert SF Object to Power BI-Compatible TopoJSON
#'
#' Converts an sf spatial dataframe to TopoJSON format optimized for Microsoft
#' Power BI Shape Map visualizations. Automatically handles CRS transformation
#' to WGS84 and validates geometry types.
#'
#' @param sf_data sf object. Spatial data to convert. Must contain POLYGON or
#'   MULTIPOLYGON geometries. Required.
#' @param file Character string. Output file path. Must end with .json extension
#'   (Power BI requirement). Default is "output_powerbi.json".
#' @param object_name Character string. Name for the TopoJSON object layer. This
#'   name will be used by Power BI to identify the geographic layer. Default is
#'   "features".
#' @param id_col Character string or NULL. Optional column name to use as the
#'   feature ID in the TopoJSON properties. If specified, this field should
#'   uniquely identify each feature. Default is NULL (no specific ID field).
#' @param name_col Character string or NULL. Optional column name to use as the
#'   feature name in the TopoJSON properties. Useful for labeling features in
#'   Power BI. Default is NULL (no specific name field).
#' @param keep_cols Character vector or NULL. Column names to keep in the output
#'   properties (excluding geometry). If NULL (default), all attribute columns
#'   are kept. Geometry column is always retained.
#' @param simplify Logical. If TRUE, simplify geometries using rmapshaper to
#'   reduce file size while preserving topology. Requires rmapshaper package.
#'   Default is FALSE.
#' @param tolerance Numeric. Simplification tolerance parameter for
#'   \code{rmapshaper::ms_simplify} (0-1). Higher values = more simplification.
#'   This is the "keep" parameter: proportion of points to retain. Only used if
#'   simplify = TRUE. Default is 0.01 (keep 1% of points).
#' @param verbose Logical. If TRUE (default), print progress messages including
#'   CRS transformations, simplification results, and file creation.
#'
#' @return Invisibly returns the output file path. The function is called
#'   primarily for its side effect of creating the TopoJSON file.
#'
#' @details
#' \strong{Power BI Requirements:}
#' Power BI Shape Map visual has specific requirements for custom map files:
#' \itemize{
#'   \item \strong{CRS}: Must use WGS84 (EPSG:4326) coordinate system
#'   \item \strong{Geometry}: Only POLYGON and MULTIPOLYGON supported (no lines/points)
#'   \item \strong{File Extension}: Must be .json (not .topojson)
#'   \item \strong{Properties}: Need feature properties for data binding
#' }
#'
#' This function automatically handles all these requirements:
#' \enumerate{
#'   \item Validates that geometries are polygon types
#'   \item Transforms to WGS84 if needed
#'   \item Ensures .json file extension
#'   \item Preserves attribute columns for data binding
#' }
#'
#' \strong{Column Filtering:}
#' You can control which attribute columns are included using \code{keep_cols},
#' \code{id_col}, and \code{name_col}:
#' \itemize{
#'   \item If \code{keep_cols} is NULL: all columns are kept
#'   \item If \code{keep_cols} is specified: only those columns (plus geometry)
#'   \item If \code{id_col} or \code{name_col} are specified and not in
#'     \code{keep_cols}, they are automatically added
#' }
#'
#' \strong{Simplification:}
#' Setting \code{simplify = TRUE} can dramatically reduce file size for complex
#' geometries (e.g., detailed municipality boundaries). The \code{tolerance}
#' parameter controls how much simplification occurs:
#' \itemize{
#'   \item 0.01 = keep 1% of points (aggressive simplification)
#'   \item 0.1 = keep 10% of points (moderate simplification)
#'   \item 0.5 = keep 50% of points (gentle simplification)
#' }
#'
#' Simplification uses topology-preserving algorithms that maintain shared
#' boundaries between adjacent polygons.
#'
#' \strong{Using in Power BI:}
#' Once you have created the .json file:
#' \enumerate{
#'   \item Open Power BI Desktop
#'   \item Insert > Shape Map visual
#'   \item Click on the visual
#'   \item Format visual > Shape > Map settings
#'   \item Click "+ Add map" and select your .json file
#'   \item Drag your data fields to the visual:
#'     \itemize{
#'       \item Location: field matching \code{id_col} or other identifier
#'       \item Color saturation: your metric field
#'       \item Tooltips: additional fields to display
#'     }
#' }
#'
#' \strong{Export Method:}
#' The function uses \code{geojsonio::topojson_write()} with high quantization
#' (1e5) to create Power BI-compatible TopoJSON files with optimal topology
#' preservation and precision. Optional geometry simplification is available
#' via \code{rmapshaper::ms_simplify()} when \code{simplify = TRUE}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with default settings
#' library(sf)
#' italy_regions <- st_read("regions.shp")
#' sf_to_powerbi_topojson(italy_regions, file = "italy_regions.json")
#'
#' # Specify ID and name columns for Power BI matching
#' sf_to_powerbi_topojson(
#'   sf_data = italy_regions,
#'   file = "italy_regions.json",
#'   id_col = "COD_REG",
#'   name_col = "DEN_REG",
#'   object_name = "regioni"
#' )
#'
#' # Keep only specific columns and simplify geometries
#' sf_to_powerbi_topojson(
#'   sf_data = italy_municipalities,
#'   file = "italy_comuni_simple.json",
#'   keep_cols = c("PRO_COM", "COMUNE", "COD_REG"),
#'   simplify = TRUE,
#'   tolerance = 0.01  # Keep 1% of points
#' )
#'
#' # With aggressive simplification for large datasets
#' sf_to_powerbi_topojson(
#'   sf_data = detailed_boundaries,
#'   file = "boundaries_simplified.json",
#'   simplify = TRUE,
#'   tolerance = 0.005,  # Very aggressive
#'   verbose = TRUE  # See size reduction
#' )
#'
#' # Use with situas package data
#' library(situas)
#' files <- prepare_territorial_maps(territorial_level = "province")
#' province_sf <- readRDS(files$rds)
#'
#' # Convert to custom Power BI format with specific columns
#' sf_to_powerbi_topojson(
#'   sf_data = province_sf,
#'   file = "data/province_custom.json",
#'   id_col = "COD_UTS",
#'   name_col = "DEN_UTS",
#'   keep_cols = c("COD_UTS", "DEN_UTS", "COD_REG"),
#'   object_name = "province"
#' )
#' }
sf_to_powerbi_topojson <- function(sf_data,
                                    file = "output_powerbi.json",
                                    object_name = "features",
                                    id_col = NULL,
                                    name_col = NULL,
                                    keep_cols = NULL,
                                    simplify = FALSE,
                                    tolerance = 0.01,
                                    verbose = TRUE) {
  # 1. Input validation -----

  # Check sf_data
  if (missing(sf_data)) {
    stop("sf_data is required", call. = FALSE)
  }

  if (!inherits(sf_data, "sf")) {
    stop(
      "sf_data must be an sf object.\n",
      "You provided: ", paste(class(sf_data), collapse = ", "),
      call. = FALSE
    )
  }

  # Check basic parameters
  stopifnot(
    "file must be a character string" =
      is.character(file) && length(file) == 1,
    "object_name must be a character string" =
      is.character(object_name) && length(object_name) == 1,
    "simplify must be logical" =
      is.logical(simplify) && length(simplify) == 1,
    "tolerance must be numeric" =
      is.numeric(tolerance) && length(tolerance) == 1,
    "verbose must be logical" =
      is.logical(verbose) && length(verbose) == 1
  )

  # Validate tolerance range
  if (tolerance <= 0 || tolerance > 1) {
    stop(
      "tolerance must be between 0 and 1 (exclusive of 0).\n",
      "You provided: ", tolerance,
      call. = FALSE
    )
  }

  # Check file extension
  if (!grepl("\\.json$", file, ignore.case = TRUE)) {
    stop(
      "file must end with .json extension (Power BI requirement).\n",
      "You provided: ", file,
      "\nExample: 'output.json' or 'maps/italy.json'",
      call. = FALSE
    )
  }

  # Validate optional string parameters
  if (!is.null(id_col)) {
    stopifnot(
      "id_col must be a character string" =
        is.character(id_col) && length(id_col) == 1
    )
  }

  if (!is.null(name_col)) {
    stopifnot(
      "name_col must be a character string" =
        is.character(name_col) && length(name_col) == 1
    )
  }

  if (!is.null(keep_cols)) {
    stopifnot(
      "keep_cols must be a character vector" =
        is.character(keep_cols)
    )
  }

  # Validate geometry types
  geom_types <- unique(sf::st_geometry_type(sf_data))
  valid_types <- c("POLYGON", "MULTIPOLYGON")

  invalid_types <- setdiff(as.character(geom_types), valid_types)
  if (length(invalid_types) > 0) {
    stop(
      "sf_data must contain only POLYGON or MULTIPOLYGON geometries.\n",
      "Found invalid types: ", paste(invalid_types, collapse = ", "),
      "\nPower BI Shape Map only supports polygon geometries.",
      call. = FALSE
    )
  }

  if (verbose) {
    message("=== Converting SF to Power BI TopoJSON ===")
    message("Input features: ", nrow(sf_data))
  }

  # 2. Transform to WGS84 -----
  current_crs <- sf::st_crs(sf_data)

  if (is.na(current_crs)) {
    warning(
      "sf_data has no CRS defined. Assuming WGS84 (EPSG:4326).\n",
      "If this is incorrect, set CRS before calling this function.",
      call. = FALSE
    )
    sf::st_crs(sf_data) <- 4326
  } else if (current_crs$epsg != 4326) {
    if (verbose) {
      message("Transforming CRS: ", current_crs$input, " -> WGS84 (EPSG:4326)")
    }
    sf_data <- sf::st_transform(sf_data, crs = 4326)
  } else {
    if (verbose) {
      message("CRS: WGS84 (EPSG:4326) - OK")
    }
  }

  # 3. Filter columns if specified -----
  geom_col <- attr(sf_data, "sf_column")
  all_cols <- names(sf_data)

  if (!is.null(keep_cols)) {
    # Start with requested columns
    cols_to_keep <- intersect(keep_cols, all_cols)

    # Add id_col and name_col if specified and not already included
    if (!is.null(id_col) && id_col %in% all_cols && !id_col %in% cols_to_keep) {
      cols_to_keep <- c(cols_to_keep, id_col)
    }

    if (!is.null(name_col) && name_col %in% all_cols && !name_col %in% cols_to_keep) {
      cols_to_keep <- c(cols_to_keep, name_col)
    }

    # Always keep geometry
    cols_to_keep <- unique(c(cols_to_keep, geom_col))

    # Warn if requested columns don't exist
    missing_cols <- setdiff(keep_cols, all_cols)
    if (length(missing_cols) > 0 && verbose) {
      warning(
        "Requested columns not found in sf_data: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }

    # Filter
    sf_data <- sf_data[, cols_to_keep]

    if (verbose) {
      kept_attrs <- setdiff(cols_to_keep, geom_col)
      message("Keeping ", length(kept_attrs), " attribute column(s): ",
              paste(kept_attrs, collapse = ", "))
    }
  }

  # Validate id_col and name_col exist in final dataset
  if (!is.null(id_col) && !id_col %in% names(sf_data)) {
    warning(
      "id_col '", id_col, "' not found in sf_data columns.\n",
      "Available columns: ", paste(setdiff(names(sf_data), geom_col), collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(name_col) && !name_col %in% names(sf_data)) {
    warning(
      "name_col '", name_col, "' not found in sf_data columns.\n",
      "Available columns: ", paste(setdiff(names(sf_data), geom_col), collapse = ", "),
      call. = FALSE
    )
  }

  # 4. Simplify geometries if requested -----
  if (simplify) {
    if (verbose) {
      message("Simplifying geometries...")
    }

    if (!requireNamespace("rmapshaper", quietly = TRUE)) {
      warning(
        "rmapshaper package not installed - skipping simplification.\n",
        "Install with: install.packages('rmapshaper')",
        call. = FALSE
      )
    } else {
      # Calculate original size
      size_before <- object.size(sf_data)

      # Simplify with topology preservation
      sf_data <- rmapshaper::ms_simplify(
        sf_data,
        keep = tolerance,
        keep_shapes = TRUE
      )

      size_after <- object.size(sf_data)
      reduction_pct <- round((1 - as.numeric(size_after) / as.numeric(size_before)) * 100, 1)

      if (verbose) {
        message("  Size before: ", format(size_before, units = "auto"))
        message("  Size after: ", format(size_after, units = "auto"))
        message("  Reduction: ", reduction_pct, "%")
      }
    }
  }

  # 5. Export TopoJSON -----
  if (verbose) {
    message("Exporting TopoJSON...")
  }

  # Ensure output directory exists
  output_dir <- dirname(file)
  if (output_dir != "." && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (verbose) {
      message("  Created output directory: ", output_dir)
    }
  }

  # Export TopoJSON using geojsonio with high quantization
  tryCatch(
    {
      # Suppress harmless warnings from geojsonio about class comparisons
      suppressWarnings({
        geojsonio::topojson_write(
          sf_data,
          file = file,
          object_name = object_name,
          quantization = 1e5,  # High quantization for precision (as per user request)
          overwrite = TRUE
        )
      })

      if (verbose && file.exists(file)) {
        file_size <- file.size(file)
        message("  Saved TopoJSON: ", file, " (", round(file_size / 1024^2, 2), " MB)")
      }
    },
    error = function(e) {
      warning(
        "Primary export method failed: ", e$message,
        "\nTrying alternative method via GeoJSON...",
        call. = FALSE
      )

      # Alternative method: write GeoJSON then convert
      temp_geojson <- tempfile(fileext = ".geojson")

      tryCatch(
        {
          sf::st_write(sf_data, dsn = temp_geojson, driver = "GeoJSON", quiet = TRUE)

          # Read as text and convert to TopoJSON with quantization
          geojson_text <- paste(readLines(temp_geojson, warn = FALSE), collapse = "\n")
          topo <- geojsonio::geo2topo(geojson_text, object_name = object_name,
                                      quantization = 1e5)

          # Write TopoJSON
          writeLines(topo, file)

          # Clean up temp file
          unlink(temp_geojson)

          if (verbose && file.exists(file)) {
            file_size <- file.size(file)
            message("  Saved TopoJSON (alternative method): ", file,
                    " (", round(file_size / 1024^2, 2), " MB)")
          }
        },
        error = function(e2) {
          # Clean up temp file if it exists
          if (file.exists(temp_geojson)) {
            unlink(temp_geojson)
          }

          stop(
            "Failed to export TopoJSON using both methods.\n",
            "Primary error: ", e$message, "\n",
            "Secondary error: ", e2$message, "\n",
            "Please check your sf object and try again.",
            call. = FALSE
          )
        }
      )
    }
  )

  # 6. Return file path -----
  if (!file.exists(file)) {
    stop("Export failed: output file was not created", call. = FALSE)
  }

  if (verbose) {
    message("\n=== Conversion complete! ===")
    message("Output file: ", file)
    message("\nTo use in Power BI:")
    message("  1. Insert > Shape Map visual")
    message("  2. Format visual > Shape > Map settings")
    message("  3. Click '+ Add map' and select this .json file")
    if (!is.null(id_col)) {
      message("  4. Use '", id_col, "' to match your data")
    }
  }

  invisible(file)
}
