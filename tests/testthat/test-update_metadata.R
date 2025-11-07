# Tests for update_reports_metadata()

test_that("update_reports_metadata reads Excel file correctly", {
  skip_on_cran()
  skip_if_not_installed("openxlsx")

  # Find package root
  pkg_root <- testthat::test_path("../..")
  excel_path <- file.path(pkg_root, "data-raw", "situas_api.xlsx")

  skip_if_not(file.exists(excel_path), "Excel file not found")

  # Read without saving
  result <- update_reports_metadata(
    excel_path = excel_path,
    save_to_package = FALSE,
    verbose = FALSE
  )

  # Check structure
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)

  # Check required columns
  expect_true(all(c("pfun", "title", "date_range", "analysis_type") %in% names(result)))

  # Check pfun is integer
  expect_type(result$pfun, "integer")

  # Check pfun values are unique and sorted
  expect_equal(result$pfun, sort(unique(result$pfun)))

  # Check analysis_type values are valid
  valid_types <- c("DATA", "PERIODO", "ATTUALIZZAZIONE")
  expect_true(all(result$analysis_type %in% valid_types))
})


test_that("update_reports_metadata handles missing file gracefully", {
  expect_error(
    update_reports_metadata(
      excel_path = "nonexistent_file.xlsx",
      save_to_package = FALSE,
      verbose = FALSE
    ),
    "Excel file not found"
  )
})


test_that("update_reports_metadata validates input parameters", {
  # Test invalid save_to_package
  expect_error(
    update_reports_metadata(save_to_package = "yes"),
    "save_to_package must be logical"
  )

  # Test invalid verbose
  expect_error(
    update_reports_metadata(verbose = "yes"),
    "verbose must be logical"
  )

  # Test invalid excel_path type
  expect_error(
    update_reports_metadata(excel_path = 123),
    "excel_path must be a character string"
  )
})


test_that("update_reports_metadata processes data correctly", {
  skip_on_cran()
  skip_if_not_installed("openxlsx")

  pkg_root <- testthat::test_path("../..")
  excel_path <- file.path(pkg_root, "data-raw", "situas_api.xlsx")

  skip_if_not(file.exists(excel_path), "Excel file not found")

  result <- update_reports_metadata(
    excel_path = excel_path,
    save_to_package = FALSE,
    verbose = FALSE
  )

  # Check that whitespace is trimmed
  expect_true(all(!grepl("^\\s|\\s$", result$title[!is.na(result$title)])))

  # Check that pfun values are positive
  expect_true(all(result$pfun > 0))

  # Check no duplicate pfun values
  expect_equal(nrow(result), length(unique(result$pfun)))
})


test_that("find_package_root finds the package directory", {
  skip_on_cran()
  skip_if(identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "situas"),
          "Skip during R CMD check (runs from temp directory)")

  # This should work when running tests from within the package
  expect_no_error({
    pkg_root <- find_package_root()
    expect_type(pkg_root, "character")
    expect_true(file.exists(file.path(pkg_root, "DESCRIPTION")))
  })
})


test_that("update_reports_metadata detects changes", {
  skip_on_cran()
  skip_if_not_installed("openxlsx")

  pkg_root <- testthat::test_path("../..")
  excel_path <- file.path(pkg_root, "data-raw", "situas_api.xlsx")

  skip_if_not(file.exists(excel_path), "Excel file not found")

  # Capture messages
  expect_message(
    update_reports_metadata(
      excel_path = excel_path,
      save_to_package = FALSE,
      verbose = TRUE
    ),
    "Processed metadata summary"
  )
})
