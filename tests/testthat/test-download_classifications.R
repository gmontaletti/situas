test_that("parse_classification_version extracts version from standard filename", {
  result <- situas:::parse_classification_version("Rev.090-ST-Classificazioni-Standard.xlsx")

  expect_type(result, "list")
  expect_equal(result$version, 90L)
  expect_equal(result$type, "classificazioni_standard")
})

test_that("parse_classification_version handles single-digit version", {
  result <- situas:::parse_classification_version("Rev.005-ST-Classificazioni-Standard-1.xlsx")

  expect_equal(result$version, 5L)
  expect_equal(result$type, "classificazioni_standard")
})

test_that("parse_classification_version handles allegato files", {
  result <- situas:::parse_classification_version("AllegatoA_AnalisiMigrazioneCp2011-Cp2021.xlsx")

  expect_true(is.na(result$version))
  expect_equal(result$type, "allegato")
})

test_that("parse_classification_version handles files without version", {
  result <- situas:::parse_classification_version("SomeOtherFile.xlsx")

  expect_true(is.na(result$version))
  expect_equal(result$type, "unknown")
})

test_that("parse_classification_version handles migrazione files", {
  result <- situas:::parse_classification_version("Rev.001-Migrazione-Codici.xlsx")

  expect_equal(result$version, 1L)
  expect_equal(result$type, "migrazione")
})

test_that("scrape_classification_links requires rvest", {
  skip_if_not_installed("mockery")

  mockery::stub(
    situas:::scrape_classification_links,
    "requireNamespace",
    FALSE
  )

  expect_error(
    situas:::scrape_classification_links("http://example.com"),
    "Package 'rvest' is required"
  )
})

test_that("scrape_classification_links returns empty data.table when no links found", {
  skip_if_not_installed("rvest")
  skip_if_not_installed("polite")
  skip_if_not_installed("mockery")

  # Mock the scraping to return no links
  mock_nodes <- list()
  class(mock_nodes) <- "xml_nodeset"

  mockery::stub(
    situas:::scrape_classification_links,
    "rvest::html_nodes",
    mock_nodes
  )

  mockery::stub(
    situas:::scrape_classification_links,
    "polite::bow",
    structure(list(url = "http://example.com"), class = "polite")
  )

  mockery::stub(
    situas:::scrape_classification_links,
    "polite::scrape",
    structure(list(), class = "xml_document")
  )

  result <- situas:::scrape_classification_links(
    "http://example.com",
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("filename", "url", "link_text"))
})

test_that("download_classification_file validates inputs", {
  temp_dir <- tempdir()

  # Should work with valid inputs (but fail at download since URL is fake)
  expect_error(
    situas:::download_classification_file(
      url = "http://example.com/fake.xlsx",
      filename = "test.xlsx",
      output_dir = temp_dir,
      verbose = FALSE
    ),
    "Failed to download"
  )
})

test_that("download_classification_file skips existing files", {
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "existing_test.xlsx")

  # Create a dummy file
  writeLines("test", test_file)

  result <- situas:::download_classification_file(
    url = "http://example.com/test.xlsx",
    filename = "existing_test.xlsx",
    output_dir = temp_dir,
    force = FALSE,
    verbose = FALSE
  )

  expect_equal(result, test_file)
  expect_true(file.exists(result))

  # Clean up
  unlink(test_file)
})

test_that("download_classification_file handles relative URLs", {
  skip_if_not_installed("mockery")

  temp_dir <- tempdir()

  # Mock download.file to capture the URL
  captured_url <- NULL
  mock_download <- function(url, destfile, ...) {
    captured_url <<- url
    # Create a dummy file
    writeLines("test", destfile)
  }

  mockery::stub(
    situas:::download_classification_file,
    "download.file",
    mock_download
  )

  result <- situas:::download_classification_file(
    url = "/sfc/servlet/example.xlsx",
    filename = "test.xlsx",
    output_dir = temp_dir,
    verbose = FALSE
  )

  expect_match(captured_url, "^https://urponline.lavoro.gov.it")

  # Clean up
  unlink(result)
})

test_that("download_classification_standards validates inputs", {
  expect_error(
    download_classification_standards(output_dir = c("dir1", "dir2")),
    "output_dir must be a single character string"
  )

  expect_error(
    download_classification_standards(url = 123),
    "url must be a single character string"
  )

  expect_error(
    download_classification_standards(download_all = "yes"),
    "download_all must be logical"
  )

  expect_error(
    download_classification_standards(force_refresh = 1),
    "force_refresh must be logical"
  )

  expect_error(
    download_classification_standards(verbose = c(TRUE, FALSE)),
    "verbose must be logical"
  )

  expect_error(
    download_classification_standards(classification_types = 123),
    "classification_types must be character vector"
  )
})

test_that("download_classification_standards creates output directory", {
  skip_if_not_installed("rvest")
  skip_if_not_installed("polite")
  skip_if_not_installed("mockery")

  temp_dir <- file.path(tempdir(), "test_classifications")

  # Clean up if exists
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }

  # Mock scraping to return no files (to avoid actual download)
  mockery::stub(
    download_classification_standards,
    "scrape_classification_links",
    data.table::data.table(
      filename = character(0),
      url = character(0),
      link_text = character(0)
    )
  )

  expect_error(
    download_classification_standards(
      output_dir = temp_dir,
      verbose = FALSE
    ),
    "No Excel files found"
  )

  # Directory should still be created even though download failed
  expect_true(dir.exists(temp_dir))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("download_classification_standards filters by type", {
  skip_if_not_installed("rvest")
  skip_if_not_installed("polite")
  skip_if_not_installed("mockery")

  temp_dir <- file.path(tempdir(), "test_filter")

  # Mock scraping to return multiple types
  mock_links <- data.table::data.table(
    filename = c("Rev.090-ST-Classificazioni-Standard.xlsx", "AllegatoA.xlsx"),
    url = c("http://example.com/file1.xlsx", "http://example.com/file2.xlsx"),
    link_text = c("Standard Classifications", "Annex A")
  )

  mockery::stub(
    download_classification_standards,
    "scrape_classification_links",
    mock_links
  )

  # Mock download to avoid actual download
  mockery::stub(
    download_classification_standards,
    "download_classification_file",
    function(url, filename, output_dir, force, verbose) {
      path <- file.path(output_dir, filename)
      writeLines("test", path)
      return(path)
    }
  )

  result <- download_classification_standards(
    output_dir = temp_dir,
    classification_types = "classificazioni_standard",
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$type, "classificazioni_standard")

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("download_classification_standards selects latest version", {
  skip_if_not_installed("rvest")
  skip_if_not_installed("polite")
  skip_if_not_installed("mockery")

  temp_dir <- file.path(tempdir(), "test_latest")

  # Mock scraping to return multiple versions
  mock_links <- data.table::data.table(
    filename = c(
      "Rev.090-ST-Classificazioni-Standard.xlsx",
      "Rev.089-ST-Classificazioni-Standard.xlsx",
      "Rev.088-ST-Classificazioni-Standard.xlsx"
    ),
    url = c(
      "http://example.com/file1.xlsx",
      "http://example.com/file2.xlsx",
      "http://example.com/file3.xlsx"
    ),
    link_text = c("Standard 090", "Standard 089", "Standard 088")
  )

  mockery::stub(
    download_classification_standards,
    "scrape_classification_links",
    mock_links
  )

  # Mock download
  mockery::stub(
    download_classification_standards,
    "download_classification_file",
    function(url, filename, output_dir, force, verbose) {
      path <- file.path(output_dir, filename)
      writeLines("test", path)
      return(path)
    }
  )

  result <- download_classification_standards(
    output_dir = temp_dir,
    download_all = FALSE,
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$version, 90L)
  expect_equal(result$filename, "Rev.090-ST-Classificazioni-Standard.xlsx")

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("download_classification_standards downloads all versions when requested", {
  skip_if_not_installed("rvest")
  skip_if_not_installed("polite")
  skip_if_not_installed("mockery")

  temp_dir <- file.path(tempdir(), "test_all")

  # Mock scraping to return multiple versions
  mock_links <- data.table::data.table(
    filename = c(
      "Rev.090-ST-Classificazioni-Standard.xlsx",
      "Rev.089-ST-Classificazioni-Standard.xlsx"
    ),
    url = c(
      "http://example.com/file1.xlsx",
      "http://example.com/file2.xlsx"
    ),
    link_text = c("Standard 090", "Standard 089")
  )

  mockery::stub(
    download_classification_standards,
    "scrape_classification_links",
    mock_links
  )

  # Mock download
  mockery::stub(
    download_classification_standards,
    "download_classification_file",
    function(url, filename, output_dir, force, verbose) {
      path <- file.path(output_dir, filename)
      writeLines("test", path)
      return(path)
    }
  )

  result <- download_classification_standards(
    output_dir = temp_dir,
    download_all = TRUE,
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_equal(result$version, c(90L, 89L))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("download_classification_standards saves metadata", {
  skip_if_not_installed("rvest")
  skip_if_not_installed("polite")
  skip_if_not_installed("mockery")

  temp_dir <- file.path(tempdir(), "test_metadata")

  # Mock scraping
  mock_links <- data.table::data.table(
    filename = "Rev.090-ST-Classificazioni-Standard.xlsx",
    url = "http://example.com/file1.xlsx",
    link_text = "Standard Classifications"
  )

  mockery::stub(
    download_classification_standards,
    "scrape_classification_links",
    mock_links
  )

  # Mock download
  mockery::stub(
    download_classification_standards,
    "download_classification_file",
    function(url, filename, output_dir, force, verbose) {
      path <- file.path(output_dir, filename)
      writeLines("test", path)
      return(path)
    }
  )

  result <- download_classification_standards(
    output_dir = temp_dir,
    verbose = FALSE
  )

  # Check metadata file exists
  metadata_path <- file.path(temp_dir, "download_metadata.rds")
  expect_true(file.exists(metadata_path))

  # Check metadata content
  metadata <- readRDS(metadata_path)
  expect_type(metadata, "list")
  expect_true("download_date" %in% names(metadata))
  expect_true("source_url" %in% names(metadata))
  expect_true("files" %in% names(metadata))
  expect_s3_class(metadata$files, "data.table")

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})
