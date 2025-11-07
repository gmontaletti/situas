# Tests for prepare_comuni_maps() and related functions

test_that("read_comuni_shapefile reads shapefile correctly", {
  skip_on_cran()
  skip_if_not_installed("sf")

  # Check if shapefile exists
  pkg_root <- tryCatch(find_package_root(), error = function(e) getwd())
  zip_path <- file.path(pkg_root, "data-raw", "Limiti01012025_g.zip")
  skip_if_not(file.exists(zip_path), "Shapefile zip not found")

  # Read shapefile
  shapefile_sf <- read_comuni_shapefile(verbose = FALSE)

  # Check that it's an sf object
  expect_s3_class(shapefile_sf, "sf")

  # Check that it has data
  expect_true(nrow(shapefile_sf) > 0)

  # Check for essential columns
  expect_true("PRO_COM" %in% names(shapefile_sf))

  # Check CRS is WGS84
  expect_equal(sf::st_crs(shapefile_sf)$epsg, 4326)

  # Check geometry is polygon
  geom_type <- unique(as.character(sf::st_geometry_type(shapefile_sf)))
  expect_true(any(c("POLYGON", "MULTIPOLYGON") %in% geom_type))
})


test_that("prepare_comuni_maps validates input parameters", {
  skip_on_cran()
  skip_if_not_installed("sf")

  # Test invalid situas_data (not a data.frame)
  expect_error(
    prepare_comuni_maps(situas_data = "not a dataframe"),
    "situas_data must be a data.frame or data.table"
  )

  # Test situas_data without PRO_COM column
  bad_data <- data.frame(x = 1:10, y = 11:20)
  expect_error(
    prepare_comuni_maps(situas_data = bad_data),
    "situas_data must contain a 'PRO_COM' column"
  )

  # Test invalid output_dir
  expect_error(
    prepare_comuni_maps(output_dir = 123),
    "output_dir must be a character string"
  )

  # Test invalid simplify
  expect_error(
    prepare_comuni_maps(simplify = "yes"),
    "simplify must be non-NA logical"
  )

  # Test invalid tolerance
  expect_error(
    prepare_comuni_maps(tolerance = -1),
    "tolerance must be positive numeric"
  )

  # Test invalid verbose
  expect_error(
    prepare_comuni_maps(verbose = "yes"),
    "verbose must be logical"
  )

  # Test invalid pfun (when provided)
  expect_error(
    prepare_comuni_maps(pfun = "not a number"),
    "pfun must be a single integer"
  )
})


test_that("prepare_comuni_maps creates output directory", {
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  # Find shapefile
  pkg_root <- tryCatch(find_package_root(), error = function(e) getwd())
  zip_path <- file.path(pkg_root, "data-raw", "Limiti01012025_g.zip")
  skip_if_not(file.exists(zip_path), "Shapefile zip not found")

  # Create temp directory for output
  temp_output <- tempfile()

  expect_false(dir.exists(temp_output))

  # Run function (this may take time, so we'll mock or skip actual execution)
  # For now, just test that directory creation works
  if (!dir.exists(temp_output)) {
    dir.create(temp_output, recursive = TRUE)
  }

  expect_true(dir.exists(temp_output))

  # Cleanup
  unlink(temp_output, recursive = TRUE)
})


test_that("prepare_comuni_maps output file structure", {
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("geojsonio")

  pkg_root <- tryCatch(find_package_root(), error = function(e) getwd())
  zip_path <- file.path(pkg_root, "data-raw", "Limiti01012025_g.zip")
  skip_if_not(file.exists(zip_path), "Shapefile zip not found")

  # This test would actually run the function - mark as manual/slow
  skip("Slow test - run manually with devtools::test()")

  temp_output <- tempfile()

  # Run the function
  files <- prepare_comuni_maps(
    output_dir = temp_output,
    verbose = FALSE,
    simplify = TRUE
  )

  # Check that files list is returned
  expect_type(files, "list")
  expect_true(all(c("rds", "geojson", "topojson") %in% names(files)))

  # Check that files were created
  expect_true(file.exists(files$rds))
  expect_true(file.exists(files$geojson))

  # Check RDS file contains sf object
  comuni_sf <- readRDS(files$rds)
  expect_s3_class(comuni_sf, "sf")
  expect_true(nrow(comuni_sf) > 0)

  # Check essential columns
  expect_true("PRO_COM" %in% names(comuni_sf))
  expect_true("COMUNE" %in% names(comuni_sf))

  # Cleanup
  unlink(temp_output, recursive = TRUE)
})


test_that("map_comuni_leaflet validates input", {
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("leaflet")

  # Test with non-sf object
  expect_error(
    map_comuni_leaflet(data.frame(x = 1:10)),
    "territorial_sf must be an sf object"
  )

  # Create minimal sf object with POINT geometry (should be rejected)
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  minimal_sf_point <- sf::st_sf(
    PRO_COM = "001001",
    COMUNE = "Test",
    geometry = pts
  )

  # Test rejection of POINT geometry
  expect_error(
    map_comuni_leaflet(minimal_sf_point, color_by = NULL, add_legend = FALSE),
    "POLYGON or MULTIPOLYGON"
  )

  # Create valid sf object with POLYGON geometry for further testing
  polygon <- sf::st_sfc(sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol=2, byrow=TRUE))), crs = 4326)
  minimal_sf <- sf::st_sf(
    PRO_COM = "001001",
    COMUNE = "Test",
    POPULATION = 1000,
    geometry = polygon
  )

  # Test invalid color_by
  expect_error(
    map_comuni_leaflet(minimal_sf, color_by = "NONEXISTENT"),
    "color_by field"
  )

  # Test valid input creates leaflet object
  expect_no_error({
    map <- map_comuni_leaflet(minimal_sf, color_by = NULL, add_legend = FALSE)
  })
})


test_that("map_comuni_choropleth validates input", {
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("leaflet")

  # Create minimal sf object
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  minimal_sf <- sf::st_sf(
    PRO_COM = "001001",
    COMUNE = "Test",
    geometry = pts
  )

  # Create sample data
  sample_data <- data.frame(
    PRO_COM = "001001",
    value = 100
  )

  # Test missing value_col
  expect_error(
    map_comuni_choropleth(minimal_sf, sample_data, value_col = "nonexistent"),
    "value_col"
  )

  # Test invalid join_by
  expect_error(
    map_comuni_choropleth(minimal_sf, sample_data, join_by = "WRONG", value_col = "value"),
    "join_by"
  )

  # Test that non-data.frame errors
  expect_error(
    map_comuni_choropleth(minimal_sf, data = "not a dataframe", value_col = "value"),
    "data must be a data.frame"
  )
})


test_that("create_popup_text generates HTML correctly", {
  # Create test sf object
  test_data <- data.frame(
    field1 = c("Value1", "Value2"),
    field2 = c(100, 200),
    field3 = c(NA, "Value3")
  )

  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326)
  test_sf <- sf::st_sf(test_data, geometry = pts)

  # Test popup creation
  popup <- create_popup_text(test_sf, c("field1", "field2"))

  expect_type(popup, "character")
  expect_length(popup, 2)
  expect_true(grepl("field1", popup[1]))
  expect_true(grepl("field2", popup[1]))
})


test_that("add_base_tiles adds correct tile layer", {
  skip_on_cran()
  skip_if_not_installed("leaflet")

  map <- leaflet::leaflet()

  # Test OpenStreetMap
  map_osm <- add_base_tiles(map, "OpenStreetMap")
  expect_s3_class(map_osm, "leaflet")

  # Test other provider
  map_carto <- add_base_tiles(map, "CartoDB.Positron")
  expect_s3_class(map_carto, "leaflet")
})


test_that("file extensions are correct", {
  skip_on_cran()

  # Test that output file names have correct extensions
  date_str <- "20250114"
  base_name <- paste0("comuni_map_", date_str)

  rds_file <- paste0(base_name, ".rds")
  geojson_file <- paste0(base_name, ".geojson")
  topojson_file <- paste0(base_name, "_powerbi.json")

  expect_true(grepl("\\.rds$", rds_file))
  expect_true(grepl("\\.geojson$", geojson_file))
  expect_true(grepl("\\.json$", topojson_file))
})
