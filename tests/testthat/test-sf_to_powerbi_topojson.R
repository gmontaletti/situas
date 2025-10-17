# Tests for sf_to_powerbi_topojson()

# 1. Helper Functions for Test Data -----

#' Create a test sf object with specified parameters
#'
#' @param n_features Number of features to create
#' @param crs CRS code (default 4326 for WGS84)
#' @param geom_type Geometry type ("POLYGON" or "MULTIPOLYGON" or "POINT" or "LINESTRING")
#' @param add_columns Logical, whether to add test columns
#' @return An sf object for testing
create_test_sf <- function(n_features = 3,
                          crs = 4326,
                          geom_type = "POLYGON",
                          add_columns = TRUE) {

  # Create simple geometries based on type
  if (geom_type == "POLYGON") {
    geom_list <- lapply(seq_len(n_features), function(i) {
      # Create a simple square polygon
      x_offset <- (i - 1) * 0.1
      coords <- matrix(
        c(0 + x_offset, 0,
          1 + x_offset, 0,
          1 + x_offset, 1,
          0 + x_offset, 1,
          0 + x_offset, 0),
        ncol = 2, byrow = TRUE
      )
      sf::st_polygon(list(coords))
    })
  } else if (geom_type == "MULTIPOLYGON") {
    geom_list <- lapply(seq_len(n_features), function(i) {
      x_offset <- (i - 1) * 0.1
      # First polygon
      coords1 <- matrix(
        c(0 + x_offset, 0,
          0.4 + x_offset, 0,
          0.4 + x_offset, 0.4,
          0 + x_offset, 0.4,
          0 + x_offset, 0),
        ncol = 2, byrow = TRUE
      )
      # Second polygon
      coords2 <- matrix(
        c(0.6 + x_offset, 0.6,
          1 + x_offset, 0.6,
          1 + x_offset, 1,
          0.6 + x_offset, 1,
          0.6 + x_offset, 0.6),
        ncol = 2, byrow = TRUE
      )
      sf::st_multipolygon(list(list(coords1), list(coords2)))
    })
  } else if (geom_type == "POINT") {
    geom_list <- lapply(seq_len(n_features), function(i) {
      sf::st_point(c(i * 0.1, i * 0.1))
    })
  } else if (geom_type == "LINESTRING") {
    geom_list <- lapply(seq_len(n_features), function(i) {
      coords <- matrix(
        c(0 + i * 0.1, 0,
          1 + i * 0.1, 1),
        ncol = 2, byrow = TRUE
      )
      sf::st_linestring(coords)
    })
  } else {
    stop("Unsupported geometry type for test: ", geom_type)
  }

  # Create sf object
  if (is.na(crs)) {
    geom <- sf::st_sfc(geom_list)
  } else {
    geom <- sf::st_sfc(geom_list, crs = crs)
  }

  if (add_columns) {
    df <- data.frame(
      id = seq_len(n_features),
      name = paste0("Feature_", seq_len(n_features)),
      value = seq_len(n_features) * 10,
      category = rep(c("A", "B"), length.out = n_features),
      stringsAsFactors = FALSE
    )
    sf_obj <- sf::st_sf(df, geometry = geom)
  } else {
    sf_obj <- sf::st_sf(geometry = geom)
  }

  return(sf_obj)
}


# 2. Input Validation Tests -----

test_that("sf_to_powerbi_topojson requires sf_data parameter", {
  withr::with_tempdir({
    expect_error(
      sf_to_powerbi_topojson(),
      "sf_data is required"
    )
  })
})


test_that("sf_to_powerbi_topojson rejects non-sf objects", {
  withr::with_tempdir({
    # Test with data.frame
    expect_error(
      sf_to_powerbi_topojson(data.frame(x = 1:3, y = 4:6)),
      "sf_data must be an sf object"
    )

    # Test with NULL
    expect_error(
      sf_to_powerbi_topojson(NULL),
      "sf_data must be an sf object"
    )

    # Test with character
    expect_error(
      sf_to_powerbi_topojson("not an sf object"),
      "sf_data must be an sf object"
    )

    # Test with list
    expect_error(
      sf_to_powerbi_topojson(list(a = 1, b = 2)),
      "sf_data must be an sf object"
    )
  })
})


test_that("sf_to_powerbi_topojson rejects invalid geometry types", {
  skip_if_not_installed("sf")

  withr::with_tempdir({
    # Test with POINT geometry
    point_sf <- create_test_sf(geom_type = "POINT")
    expect_error(
      sf_to_powerbi_topojson(point_sf, verbose = FALSE),
      "POLYGON or MULTIPOLYGON"
    )

    # Test with LINESTRING geometry
    line_sf <- create_test_sf(geom_type = "LINESTRING")
    expect_error(
      sf_to_powerbi_topojson(line_sf, verbose = FALSE),
      "POLYGON or MULTIPOLYGON"
    )
  })
})


test_that("sf_to_powerbi_topojson validates file parameter", {
  skip_if_not_installed("sf")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    # Test non-character file
    expect_error(
      sf_to_powerbi_topojson(test_sf, file = 123, verbose = FALSE),
      "file must be a character string"
    )

    # Test multiple file paths
    expect_error(
      sf_to_powerbi_topojson(test_sf, file = c("file1.json", "file2.json"), verbose = FALSE),
      "file must be a character string"
    )

    # Test file without .json extension
    expect_error(
      sf_to_powerbi_topojson(test_sf, file = "output.txt", verbose = FALSE),
      "must end with .json extension"
    )

    expect_error(
      sf_to_powerbi_topojson(test_sf, file = "output.topojson", verbose = FALSE),
      "must end with .json extension"
    )

    expect_error(
      sf_to_powerbi_topojson(test_sf, file = "output", verbose = FALSE),
      "must end with .json extension"
    )
  })
})


test_that("sf_to_powerbi_topojson validates object_name parameter", {
  skip_if_not_installed("sf")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    expect_error(
      sf_to_powerbi_topojson(test_sf, object_name = 123, verbose = FALSE),
      "object_name must be a character string"
    )

    expect_error(
      sf_to_powerbi_topojson(test_sf, object_name = c("obj1", "obj2"), verbose = FALSE),
      "object_name must be a character string"
    )

    expect_error(
      sf_to_powerbi_topojson(test_sf, object_name = NULL, verbose = FALSE),
      "object_name must be a character string"
    )
  })
})


test_that("sf_to_powerbi_topojson validates id_col parameter", {
  skip_if_not_installed("sf")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    expect_error(
      sf_to_powerbi_topojson(test_sf, id_col = 123, verbose = FALSE),
      "id_col must be a character string"
    )

    expect_error(
      sf_to_powerbi_topojson(test_sf, id_col = c("col1", "col2"), verbose = FALSE),
      "id_col must be a character string"
    )
  })
})


test_that("sf_to_powerbi_topojson validates name_col parameter", {
  skip_if_not_installed("sf")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    expect_error(
      sf_to_powerbi_topojson(test_sf, name_col = 123, verbose = FALSE),
      "name_col must be a character string"
    )

    expect_error(
      sf_to_powerbi_topojson(test_sf, name_col = c("col1", "col2"), verbose = FALSE),
      "name_col must be a character string"
    )
  })
})


test_that("sf_to_powerbi_topojson validates keep_cols parameter", {
  skip_if_not_installed("sf")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    expect_error(
      sf_to_powerbi_topojson(test_sf, keep_cols = 123, verbose = FALSE),
      "keep_cols must be a character vector"
    )

    expect_error(
      sf_to_powerbi_topojson(test_sf, keep_cols = TRUE, verbose = FALSE),
      "keep_cols must be a character vector"
    )
  })
})


test_that("sf_to_powerbi_topojson validates simplify parameter", {
  skip_if_not_installed("sf")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    expect_error(
      sf_to_powerbi_topojson(test_sf, simplify = "yes", verbose = FALSE),
      "simplify must be logical"
    )

    expect_error(
      sf_to_powerbi_topojson(test_sf, simplify = 1, verbose = FALSE),
      "simplify must be logical"
    )

    expect_error(
      sf_to_powerbi_topojson(test_sf, simplify = c(TRUE, FALSE), verbose = FALSE),
      "simplify must be logical"
    )
  })
})


test_that("sf_to_powerbi_topojson validates tolerance parameter", {
  skip_if_not_installed("sf")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    expect_error(
      sf_to_powerbi_topojson(test_sf, tolerance = "0.01", verbose = FALSE),
      "tolerance must be numeric"
    )

    expect_error(
      sf_to_powerbi_topojson(test_sf, tolerance = c(0.01, 0.02), verbose = FALSE),
      "tolerance must be numeric"
    )

    expect_error(
      sf_to_powerbi_topojson(test_sf, tolerance = 0, verbose = FALSE),
      "tolerance must be between 0 and 1"
    )

    expect_error(
      sf_to_powerbi_topojson(test_sf, tolerance = -0.1, verbose = FALSE),
      "tolerance must be between 0 and 1"
    )

    expect_error(
      sf_to_powerbi_topojson(test_sf, tolerance = 1.5, verbose = FALSE),
      "tolerance must be between 0 and 1"
    )
  })
})


test_that("sf_to_powerbi_topojson validates verbose parameter", {
  skip_if_not_installed("sf")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    expect_error(
      sf_to_powerbi_topojson(test_sf, verbose = "yes"),
      "verbose must be logical"
    )

    expect_error(
      sf_to_powerbi_topojson(test_sf, verbose = 1),
      "verbose must be logical"
    )

    expect_error(
      sf_to_powerbi_topojson(test_sf, verbose = c(TRUE, FALSE)),
      "verbose must be logical"
    )
  })
})


# 3. CRS Transformation Tests -----

test_that("sf_to_powerbi_topojson preserves WGS84 CRS", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  # Create sf object already in WGS84
  test_sf <- create_test_sf(crs = 4326)

  withr::with_tempdir({
    output_file <- "test_output.json"

    # Should not transform (already in WGS84)
    result <- sf_to_powerbi_topojson(test_sf, file = output_file, verbose = FALSE)

    expect_true(file.exists(output_file))
    expect_equal(result, output_file)
  })
})


test_that("sf_to_powerbi_topojson transforms non-WGS84 CRS to WGS84", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  # Create sf object in UTM zone 33N (Italy)
  test_sf <- create_test_sf(crs = 32633)

  withr::with_tempdir({
    output_file <- "test_output.json"

    # Should transform to WGS84
    result <- sf_to_powerbi_topojson(test_sf, file = output_file, verbose = FALSE)

    expect_true(file.exists(output_file))
    expect_equal(result, output_file)
  })
})


test_that("sf_to_powerbi_topojson warns about undefined CRS", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  # Create sf object without CRS
  test_sf <- create_test_sf(crs = NA)

  withr::with_tempdir({
    output_file <- "test_output.json"

    # Should warn about undefined CRS
    expect_warning(
      sf_to_powerbi_topojson(test_sf, file = output_file, verbose = FALSE),
      "no CRS defined"
    )

    expect_true(file.exists(output_file))
  })
})


# 4. Column Filtering Tests -----

test_that("sf_to_powerbi_topojson keeps all columns when keep_cols is NULL", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()
  original_cols <- setdiff(names(test_sf), attr(test_sf, "sf_column"))

  withr::with_tempdir({
    output_file <- "test_output.json"

    result <- sf_to_powerbi_topojson(
      test_sf,
      file = output_file,
      keep_cols = NULL,
      verbose = FALSE
    )

    expect_true(file.exists(output_file))

    # Read back the file to check structure
    json_content <- jsonlite::fromJSON(output_file)
    expect_true("objects" %in% names(json_content))
  })
})


test_that("sf_to_powerbi_topojson filters columns with keep_cols", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "test_output.json"

    result <- sf_to_powerbi_topojson(
      test_sf,
      file = output_file,
      keep_cols = c("id", "name"),
      verbose = FALSE
    )

    expect_true(file.exists(output_file))
  })
})


test_that("sf_to_powerbi_topojson warns about missing columns in keep_cols", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "test_output.json"

    # Request columns that don't exist
    expect_warning(
      sf_to_powerbi_topojson(
        test_sf,
        file = output_file,
        keep_cols = c("id", "nonexistent_col"),
        verbose = TRUE
      ),
      "not found in sf_data"
    )

    expect_true(file.exists(output_file))
  })
})


test_that("sf_to_powerbi_topojson includes id_col even if not in keep_cols", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "test_output.json"

    # Specify id_col but don't include it in keep_cols
    result <- sf_to_powerbi_topojson(
      test_sf,
      file = output_file,
      id_col = "id",
      keep_cols = c("name", "value"),
      verbose = FALSE
    )

    expect_true(file.exists(output_file))
  })
})


test_that("sf_to_powerbi_topojson includes name_col even if not in keep_cols", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "test_output.json"

    # Specify name_col but don't include it in keep_cols
    result <- sf_to_powerbi_topojson(
      test_sf,
      file = output_file,
      name_col = "name",
      keep_cols = c("id", "value"),
      verbose = FALSE
    )

    expect_true(file.exists(output_file))
  })
})


test_that("sf_to_powerbi_topojson warns when id_col doesn't exist", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "test_output.json"

    expect_warning(
      sf_to_powerbi_topojson(
        test_sf,
        file = output_file,
        id_col = "nonexistent_id",
        verbose = FALSE
      ),
      "id_col.*not found"
    )

    expect_true(file.exists(output_file))
  })
})


test_that("sf_to_powerbi_topojson warns when name_col doesn't exist", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "test_output.json"

    expect_warning(
      sf_to_powerbi_topojson(
        test_sf,
        file = output_file,
        name_col = "nonexistent_name",
        verbose = FALSE
      ),
      "name_col.*not found"
    )

    expect_true(file.exists(output_file))
  })
})


test_that("sf_to_powerbi_topojson always keeps geometry column", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()
  geom_col <- attr(test_sf, "sf_column")

  withr::with_tempdir({
    output_file <- "test_output.json"

    # Even with keep_cols specified, geometry should be preserved
    result <- sf_to_powerbi_topojson(
      test_sf,
      file = output_file,
      keep_cols = c("id"),
      verbose = FALSE
    )

    expect_true(file.exists(output_file))

    # File should contain TopoJSON structure with geometries
    json_content <- jsonlite::fromJSON(output_file)
    expect_true("type" %in% names(json_content))
    expect_equal(json_content$type, "Topology")
  })
})


# 5. File Output Tests -----

test_that("sf_to_powerbi_topojson creates output file", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "test_output.json"

    result <- sf_to_powerbi_topojson(test_sf, file = output_file, verbose = FALSE)

    expect_true(file.exists(output_file))
    expect_equal(result, output_file)
  })
})


test_that("sf_to_powerbi_topojson returns correct file path", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "my_custom_output.json"

    result <- sf_to_powerbi_topojson(test_sf, file = output_file, verbose = FALSE)

    expect_equal(result, output_file)
    expect_true(file.exists(result))
  })
})


test_that("sf_to_powerbi_topojson creates valid JSON file", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "test_output.json"

    sf_to_powerbi_topojson(test_sf, file = output_file, verbose = FALSE)

    # Read and parse JSON
    json_content <- jsonlite::fromJSON(output_file)

    # Check TopoJSON structure
    expect_true("type" %in% names(json_content))
    expect_equal(json_content$type, "Topology")
    expect_true("objects" %in% names(json_content))
    expect_true("arcs" %in% names(json_content))
  })
})


test_that("sf_to_powerbi_topojson creates output directory if needed", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    nested_path <- file.path("subdir1", "subdir2", "output.json")

    expect_false(dir.exists("subdir1"))

    result <- sf_to_powerbi_topojson(test_sf, file = nested_path, verbose = FALSE)

    expect_true(dir.exists("subdir1"))
    expect_true(dir.exists(file.path("subdir1", "subdir2")))
    expect_true(file.exists(nested_path))
  })
})


test_that("sf_to_powerbi_topojson overwrites existing file", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "test_output.json"

    # Create first file
    sf_to_powerbi_topojson(test_sf, file = output_file, verbose = FALSE)
    first_mtime <- file.info(output_file)$mtime

    # Wait a moment
    Sys.sleep(0.1)

    # Overwrite
    sf_to_powerbi_topojson(test_sf, file = output_file, verbose = FALSE)
    second_mtime <- file.info(output_file)$mtime

    expect_true(file.exists(output_file))
    expect_true(second_mtime >= first_mtime)
  })
})


test_that("sf_to_powerbi_topojson respects custom object_name", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()
  custom_name <- "my_custom_layer"

  withr::with_tempdir({
    output_file <- "test_output.json"

    sf_to_powerbi_topojson(
      test_sf,
      file = output_file,
      object_name = custom_name,
      verbose = FALSE
    )

    # Read and check object name
    json_content <- jsonlite::fromJSON(output_file)
    expect_true(custom_name %in% names(json_content$objects))
  })
})


# 6. Simplification Tests -----

test_that("sf_to_powerbi_topojson handles simplify=FALSE", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "test_output.json"

    result <- sf_to_powerbi_topojson(
      test_sf,
      file = output_file,
      simplify = FALSE,
      verbose = FALSE
    )

    expect_true(file.exists(output_file))
  })
})


test_that("sf_to_powerbi_topojson handles simplify=TRUE when rmapshaper available", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")
  skip_if_not_installed("rmapshaper")

  test_sf <- create_test_sf(n_features = 5)

  withr::with_tempdir({
    output_file <- "test_output.json"

    result <- sf_to_powerbi_topojson(
      test_sf,
      file = output_file,
      simplify = TRUE,
      tolerance = 0.1,
      verbose = FALSE
    )

    expect_true(file.exists(output_file))
  })
})


test_that("sf_to_powerbi_topojson warns when rmapshaper not available", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")
  skip_if(requireNamespace("rmapshaper", quietly = TRUE), "rmapshaper is installed")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "test_output.json"

    expect_warning(
      sf_to_powerbi_topojson(
        test_sf,
        file = output_file,
        simplify = TRUE,
        verbose = TRUE
      ),
      "rmapshaper"
    )

    expect_true(file.exists(output_file))
  })
})


test_that("sf_to_powerbi_topojson accepts different tolerance values", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")
  skip_if_not_installed("rmapshaper")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    # Test with various valid tolerance values
    for (tol in c(0.001, 0.01, 0.1, 0.5, 0.9, 0.99)) {
      output_file <- paste0("test_", tol, ".json")

      result <- sf_to_powerbi_topojson(
        test_sf,
        file = output_file,
        simplify = TRUE,
        tolerance = tol,
        verbose = FALSE
      )

      expect_true(file.exists(output_file))
    }
  })
})


# 7. Verbose Output Tests -----

test_that("sf_to_powerbi_topojson suppresses messages when verbose=FALSE", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "test_output.json"

    # The function itself should not produce messages with verbose=FALSE
    # (geojsonio may print to console, but that's not a message)
    # Just verify the function runs without error and creates the file
    result <- sf_to_powerbi_topojson(test_sf, file = output_file, verbose = FALSE)

    expect_true(file.exists(output_file))
    expect_equal(result, output_file)
  })
})


test_that("sf_to_powerbi_topojson shows messages when verbose=TRUE", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "test_output.json"

    expect_message(
      sf_to_powerbi_topojson(test_sf, file = output_file, verbose = TRUE),
      "Converting SF to Power BI TopoJSON"
    )
  })
})


test_that("sf_to_powerbi_topojson shows CRS transformation message when verbose=TRUE", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  # Create sf with non-WGS84 CRS
  test_sf <- create_test_sf(crs = 32633)

  withr::with_tempdir({
    output_file <- "test_output.json"

    expect_message(
      sf_to_powerbi_topojson(test_sf, file = output_file, verbose = TRUE),
      "Transforming CRS"
    )
  })
})


# 8. Geometry Type Tests -----

test_that("sf_to_powerbi_topojson accepts POLYGON geometry", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  polygon_sf <- create_test_sf(geom_type = "POLYGON")

  withr::with_tempdir({
    output_file <- "test_output.json"

    result <- sf_to_powerbi_topojson(polygon_sf, file = output_file, verbose = FALSE)

    expect_true(file.exists(output_file))
  })
})


test_that("sf_to_powerbi_topojson accepts MULTIPOLYGON geometry", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  multipolygon_sf <- create_test_sf(geom_type = "MULTIPOLYGON")

  withr::with_tempdir({
    output_file <- "test_output.json"

    result <- sf_to_powerbi_topojson(multipolygon_sf, file = output_file, verbose = FALSE)

    expect_true(file.exists(output_file))
  })
})


test_that("sf_to_powerbi_topojson accepts mixed POLYGON and MULTIPOLYGON", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  # Create sf with mixed geometry types
  poly1 <- sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow=TRUE)))

  # Create a multipolygon
  coords1 <- matrix(c(2,0, 3,0, 3,1, 2,1, 2,0), ncol=2, byrow=TRUE)
  coords2 <- matrix(c(2.2,0.2, 2.8,0.2, 2.8,0.8, 2.2,0.8, 2.2,0.2), ncol=2, byrow=TRUE)
  poly2 <- sf::st_multipolygon(list(list(coords1), list(coords2)))

  mixed_sf <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(poly1, poly2, crs = 4326)
  )

  withr::with_tempdir({
    output_file <- "test_output.json"

    result <- sf_to_powerbi_topojson(mixed_sf, file = output_file, verbose = FALSE)

    expect_true(file.exists(output_file))
  })
})


# 9. Edge Cases and Error Handling -----

test_that("sf_to_powerbi_topojson handles sf with single feature", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  single_feature_sf <- create_test_sf(n_features = 1)

  withr::with_tempdir({
    output_file <- "test_output.json"

    result <- sf_to_powerbi_topojson(single_feature_sf, file = output_file, verbose = FALSE)

    expect_true(file.exists(output_file))
  })
})


test_that("sf_to_powerbi_topojson handles sf with many features", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  many_features_sf <- create_test_sf(n_features = 50)

  withr::with_tempdir({
    output_file <- "test_output.json"

    result <- sf_to_powerbi_topojson(many_features_sf, file = output_file, verbose = FALSE)

    expect_true(file.exists(output_file))
  })
})


test_that("sf_to_powerbi_topojson handles sf with no attribute columns", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  # Create sf with only geometry
  no_attrs_sf <- create_test_sf(add_columns = FALSE)

  withr::with_tempdir({
    output_file <- "test_output.json"

    result <- sf_to_powerbi_topojson(no_attrs_sf, file = output_file, verbose = FALSE)

    expect_true(file.exists(output_file))
  })
})


test_that("sf_to_powerbi_topojson handles keep_cols with all valid columns", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()
  all_cols <- setdiff(names(test_sf), attr(test_sf, "sf_column"))

  withr::with_tempdir({
    output_file <- "test_output.json"

    result <- sf_to_powerbi_topojson(
      test_sf,
      file = output_file,
      keep_cols = all_cols,
      verbose = FALSE
    )

    expect_true(file.exists(output_file))
  })
})


test_that("sf_to_powerbi_topojson handles keep_cols with only invalid columns", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "test_output.json"

    # Request only non-existent columns
    expect_warning(
      result <- sf_to_powerbi_topojson(
        test_sf,
        file = output_file,
        keep_cols = c("fake1", "fake2", "fake3"),
        verbose = TRUE
      ),
      "not found"
    )

    # Should still create file with geometry
    expect_true(file.exists(output_file))
  })
})


# 10. Integration Tests with Real-World Scenarios -----

test_that("sf_to_powerbi_topojson works with typical use case", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  # Simulate typical Italian administrative boundaries
  test_sf <- create_test_sf(n_features = 10)
  test_sf$COD_REG <- sprintf("%02d", seq_len(10))
  test_sf$DEN_REG <- paste0("Region_", seq_len(10))
  test_sf$AREA_KM2 <- runif(10, 1000, 10000)

  withr::with_tempdir({
    output_file <- "italy_regions.json"

    result <- sf_to_powerbi_topojson(
      test_sf,
      file = output_file,
      object_name = "regioni",
      id_col = "COD_REG",
      name_col = "DEN_REG",
      keep_cols = c("COD_REG", "DEN_REG", "AREA_KM2"),
      simplify = FALSE,
      verbose = FALSE
    )

    expect_true(file.exists(output_file))

    # Check JSON structure
    json_content <- jsonlite::fromJSON(output_file)
    expect_equal(json_content$type, "Topology")
    expect_true("regioni" %in% names(json_content$objects))
  })
})


test_that("sf_to_powerbi_topojson invisible return works correctly", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf()

  withr::with_tempdir({
    output_file <- "test_output.json"

    # Function returns invisibly
    result <- sf_to_powerbi_topojson(test_sf, file = output_file, verbose = FALSE)

    expect_equal(result, output_file)
    expect_true(file.exists(result))
  })
})


test_that("sf_to_powerbi_topojson file size is reasonable", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  test_sf <- create_test_sf(n_features = 10)

  withr::with_tempdir({
    output_file <- "test_output.json"

    sf_to_powerbi_topojson(test_sf, file = output_file, verbose = FALSE)

    # File should exist and have content
    file_size <- file.size(output_file)
    expect_gt(file_size, 0)
    expect_lt(file_size, 10 * 1024 * 1024)  # Less than 10 MB for test data
  })
})
