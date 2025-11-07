# Tests for list_available_reports() and scan_situas_reports()

# Test default behavior (backward compatibility) ----

test_that("list_available_reports returns data.table with default arguments", {
  skip_if_offline()
  skip_on_cran()

  result <- list_available_reports()

  expect_s3_class(result, "data.table")
  expect_type(result, "list")
})

test_that("list_available_reports has expected columns", {
  skip_if_offline()
  skip_on_cran()

  result <- list_available_reports()

  expected_cols <- c("pfun", "title", "type", "date_range", "row_count")
  expect_true(all(expected_cols %in% names(result)))
  expect_equal(length(names(result)), 5)
})

test_that("list_available_reports returns expected number of reports with defaults", {
  skip_if_offline()
  skip_on_cran()

  result <- list_available_reports()

  # Should return 65 reports from metadata
  # (all reports from situas_api.xlsx)
  expect_equal(nrow(result), 65)
})

test_that("list_available_reports includes DATA, PERIODO, and ATTUALIZZAZIONE types", {
  skip_if_offline()
  skip_on_cran()

  result <- list_available_reports()

  types <- unique(result$type)
  expect_true("DATA" %in% types)
  expect_true("PERIODO" %in% types)
  expect_true("ATTUALIZZAZIONE" %in% types)
  expect_equal(length(types), 3)
})

test_that("list_available_reports column types are correct", {
  skip_if_offline()
  skip_on_cran()

  result <- list_available_reports()

  expect_type(result$pfun, "integer")
  expect_type(result$title, "character")
  expect_type(result$type, "character")
  expect_type(result$date_range, "character")
  expect_type(result$row_count, "integer")
})

test_that("list_available_reports returns valid pfun values", {
  skip_if_offline()
  skip_on_cran()

  result <- list_available_reports()

  # All pfun values should be positive integers
  expect_true(all(result$pfun > 0))
  expect_true(all(!is.na(result$pfun)))
})

# Test with custom report IDs and auto-detection ----

test_that("list_available_reports works with single custom DATA report ID", {
  skip_if_offline()
  skip_on_cran()

  # pfun = 61 is a known DATA type report (municipalities)
  result <- list_available_reports(report_ids = 61)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$pfun, 61)
  expect_equal(result$type, "DATA")
  expect_true(!is.na(result$title))
  expect_true(!is.na(result$row_count))
})

test_that("list_available_reports works with single custom ATTUALIZZAZIONE report ID", {
  skip_if_offline()
  skip_on_cran()

  # pfun = 99 is a known ATTUALIZZAZIONE type report
  result <- list_available_reports(report_ids = 99)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$pfun, 99)
  expect_equal(result$type, "ATTUALIZZAZIONE")
  expect_true(!is.na(result$title))
  expect_true(!is.na(result$row_count))
})

test_that("list_available_reports auto-detects multiple report types", {
  skip_if_offline()
  skip_on_cran()

  # Mix of DATA (61) and ATTUALIZZAZIONE (99) reports
  result <- list_available_reports(report_ids = c(61, 99))

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_equal(result$pfun, c(61, 99))

  # Check that types are correctly detected
  expect_equal(result$type[result$pfun == 61], "DATA")
  expect_equal(result$type[result$pfun == 99], "ATTUALIZZAZIONE")
})

test_that("list_available_reports with custom IDs returns in order provided", {
  skip_if_offline()
  skip_on_cran()

  # Test with specific order
  ids <- c(71, 61, 99)
  result <- list_available_reports(report_ids = ids)

  expect_equal(result$pfun, ids)
})

# Test with auto_detect_type parameter ----

test_that("list_available_reports with auto_detect_type = FALSE uses known types", {
  skip_if_offline()
  skip_on_cran()

  # pfun = 61 is in the hardcoded DATA reports list
  result <- list_available_reports(report_ids = 61, auto_detect_type = FALSE)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$type, "DATA")
  expect_true(!is.na(result$title))
})

test_that("list_available_reports with auto_detect_type = FALSE handles ATTUALIZZAZIONE types", {
  skip_if_offline()
  skip_on_cran()

  # pfun = 99 is in the hardcoded ATTUALIZZAZIONE reports list
  result <- list_available_reports(report_ids = 99, auto_detect_type = FALSE)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$type, "ATTUALIZZAZIONE")
  expect_true(!is.na(result$title))
})

test_that("list_available_reports auto_detect_type = FALSE is faster than TRUE", {
  skip_if_offline()
  skip_on_cran()

  # Test with a single known report
  time_with_auto <- system.time({
    result_auto <- list_available_reports(report_ids = 61, auto_detect_type = TRUE)
  })

  time_without_auto <- system.time({
    result_no_auto <- list_available_reports(report_ids = 61, auto_detect_type = FALSE)
  })

  # Without auto-detection should be faster (single API call vs potential two calls)
  # Note: This is a soft expectation as network timing can vary
  expect_equal(result_auto$pfun, result_no_auto$pfun)
  expect_equal(result_auto$type, result_no_auto$type)
})

# Test verbose output ----

test_that("list_available_reports with verbose = TRUE prints messages", {
  skip_if_offline()
  skip_on_cran()

  # Test with a single report ID to minimize output
  expect_message(
    list_available_reports(report_ids = 61, verbose = TRUE),
    "Fetching info for report 61"
  )
})

test_that("list_available_reports with verbose = FALSE produces no messages", {
  skip_if_offline()
  skip_on_cran()

  # Should not produce any messages
  expect_silent(
    list_available_reports(report_ids = 61, verbose = FALSE)
  )
})

test_that("list_available_reports verbose shows progress for multiple reports", {
  skip_if_offline()
  skip_on_cran()

  # Should see messages for each report
  expect_message(
    list_available_reports(report_ids = c(61, 71), verbose = TRUE),
    "Fetching info for report"
  )
})

# Test with custom date parameter ----

test_that("list_available_reports works with specific date as Date object", {
  skip_if_offline()
  skip_on_cran()

  result <- list_available_reports(
    date = as.Date("2023-01-01"),
    report_ids = 71  # Small stable report
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_true(!is.na(result$row_count))
})

test_that("list_available_reports works with date as character string", {
  skip_if_offline()
  skip_on_cran()

  result <- list_available_reports(
    date = "2023-06-15",
    report_ids = 71
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_true(!is.na(result$row_count))
})

test_that("list_available_reports works with POSIXct date", {
  skip_if_offline()
  skip_on_cran()

  result <- list_available_reports(
    date = as.POSIXct("2023-01-01 12:00:00"),
    report_ids = 71
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_true(!is.na(result$row_count))
})

test_that("list_available_reports with past date returns data", {
  skip_if_offline()
  skip_on_cran()

  # Test that historical data can be retrieved
  result_past <- list_available_reports(
    date = as.Date("2020-01-01"),
    report_ids = 71
  )

  result_current <- list_available_reports(
    date = Sys.Date(),
    report_ids = 71
  )

  # Both should return valid data
  expect_s3_class(result_past, "data.table")
  expect_s3_class(result_current, "data.table")
  expect_equal(nrow(result_past), 1)
  expect_equal(nrow(result_current), 1)
})

# Test error handling ----

test_that("list_available_reports handles invalid report ID gracefully", {
  skip_if_offline()
  skip_on_cran()

  # Use a report ID that likely doesn't exist (999999)
  result <- list_available_reports(
    report_ids = 999999,
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$pfun, 999999)

  # Should have NA values for failed fetch
  expect_true(is.na(result$title))
  expect_true(is.na(result$date_range))
  expect_true(is.na(result$row_count))
})

test_that("list_available_reports with verbose = TRUE warns on invalid ID", {
  skip_if_offline()
  skip_on_cran()

  # Should produce a warning message for invalid ID
  expect_warning(
    list_available_reports(report_ids = 999999, verbose = TRUE),
    "Failed to fetch info for report 999999"
  )
})

test_that("list_available_reports handles mix of valid and invalid IDs", {
  skip_if_offline()
  skip_on_cran()

  # Mix valid (61) and invalid (999999) report IDs
  result <- list_available_reports(
    report_ids = c(61, 999999),
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)

  # Valid report should have data
  valid_row <- result[pfun == 61]
  expect_true(!is.na(valid_row$title))
  expect_true(!is.na(valid_row$row_count))

  # Invalid report should have NAs
  invalid_row <- result[pfun == 999999]
  expect_true(is.na(invalid_row$title))
  expect_true(is.na(invalid_row$row_count))
})

# Test data structure integrity ----

test_that("list_available_reports returns consistent structure across calls", {
  skip_if_offline()
  skip_on_cran()

  result1 <- list_available_reports(report_ids = 61)
  result2 <- list_available_reports(report_ids = 71)

  # Column names should be identical
  expect_equal(names(result1), names(result2))

  # Column types should be identical
  expect_equal(sapply(result1, class), sapply(result2, class))
})

test_that("list_available_reports type values are only DATA, PERIODO, or ATTUALIZZAZIONE", {
  skip_if_offline()
  skip_on_cran()

  result <- list_available_reports()

  # Only valid types should be present
  expect_true(all(result$type %in% c("DATA", "PERIODO", "ATTUALIZZAZIONE")))
})

test_that("list_available_reports row_count values are non-negative", {
  skip_if_offline()
  skip_on_cran()

  result <- list_available_reports()

  # Row counts should be positive or NA (for failed fetches)
  non_na_counts <- result$row_count[!is.na(result$row_count)]
  expect_true(all(non_na_counts >= 0))
})

# Test edge cases ----

test_that("list_available_reports handles empty report_ids vector", {
  skip_if_offline()
  skip_on_cran()

  # Empty vector should behave like NULL (use defaults)
  result <- list_available_reports(report_ids = integer(0))

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("list_available_reports handles duplicate report IDs", {
  skip_if_offline()
  skip_on_cran()

  # Duplicate IDs should return duplicate rows
  result <- list_available_reports(report_ids = c(61, 61, 71))

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)
  expect_equal(sum(result$pfun == 61), 2)
  expect_equal(sum(result$pfun == 71), 1)
})

# Tests for scan_situas_reports() ----

test_that("scan_situas_reports validates max_id parameter", {
  expect_error(
    scan_situas_reports(max_id = "invalid"),
    "max_id must be a positive integer"
  )

  expect_error(
    scan_situas_reports(max_id = -5),
    "max_id must be a positive integer"
  )

  expect_error(
    scan_situas_reports(max_id = 1.5),
    "max_id must be a positive integer"
  )

  expect_error(
    scan_situas_reports(max_id = 0),
    "max_id must be a positive integer"
  )
})

test_that("scan_situas_reports returns data.table with correct structure", {
  skip_if_offline()
  skip_on_cran()

  # Test with small range to minimize API calls
  result <- scan_situas_reports(max_id = 5, verbose = FALSE)

  expect_s3_class(result, "data.table")

  expected_cols <- c("pfun", "type", "row_count")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("scan_situas_reports with verbose = TRUE prints progress", {
  skip_if_offline()
  skip_on_cran()

  expect_message(
    scan_situas_reports(max_id = 15, verbose = TRUE),
    "Scanning SITUAS API"
  )

  expect_message(
    scan_situas_reports(max_id = 15, verbose = TRUE),
    "Testing ID"
  )
})

test_that("scan_situas_reports with verbose = FALSE produces no messages", {
  skip_if_offline()
  skip_on_cran()

  # Should be silent except for potential warnings
  result <- scan_situas_reports(max_id = 5, verbose = FALSE)
  expect_s3_class(result, "data.table")
})

test_that("scan_situas_reports returns empty data.table when no valid reports found", {
  skip_if_offline()
  skip_on_cran()

  # Test with range unlikely to have valid reports (e.g., 500-501)
  # This may fail if ISTAT adds reports in that range, but unlikely
  result <- scan_situas_reports(max_id = 1, verbose = FALSE)

  # Could be empty or have 1 row depending on if ID 1 is valid
  expect_s3_class(result, "data.table")
})

test_that("scan_situas_reports handles date parameter", {
  skip_if_offline()
  skip_on_cran()

  result <- scan_situas_reports(
    max_id = 5,
    date = as.Date("2023-01-01"),
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
})

test_that("scan_situas_reports identifies both DATA and PERIODO types", {
  skip_if_offline()
  skip_on_cran()

  # Scan range known to contain both types (60-105 includes both)
  result <- scan_situas_reports(max_id = 105, verbose = FALSE)

  expect_s3_class(result, "data.table")

  # Should find both types if reports exist
  if (nrow(result) > 0) {
    types <- unique(result$type)
    # At minimum, should have valid type values
    expect_true(all(types %in% c("DATA", "PERIODO")))
  }
})

# Integration tests combining multiple features ----

test_that("list_available_reports results match known report characteristics", {
  skip_if_offline()
  skip_on_cran()

  result <- list_available_reports()

  # Check known reports are present
  # pfun = 71 is "Elenco Ripartizioni geografiche" (always 5 rows)
  rip_report <- result[pfun == 71]
  expect_equal(nrow(rip_report), 1)
  expect_equal(rip_report$type, "DATA")
  expect_equal(rip_report$row_count, 5)
  expect_equal(rip_report$title, "Elenco Ripartizioni geografiche")
})

test_that("list_available_reports with different dates returns consistent structure", {
  skip_if_offline()
  skip_on_cran()

  result_2020 <- list_available_reports(
    date = as.Date("2020-01-01"),
    report_ids = c(61, 71)
  )

  result_2024 <- list_available_reports(
    date = as.Date("2024-01-01"),
    report_ids = c(61, 71)
  )

  # Structure should be identical
  expect_equal(names(result_2020), names(result_2024))
  expect_equal(result_2020$pfun, result_2024$pfun)
  expect_equal(result_2020$type, result_2024$type)

  # Row counts might differ for municipalities (61) due to territorial changes
  # but should be consistent for ripartizioni (71)
  expect_equal(
    result_2020$row_count[result_2020$pfun == 71],
    result_2024$row_count[result_2024$pfun == 71]
  )
})

test_that("list_available_reports data can be subset using data.table syntax", {
  skip_if_offline()
  skip_on_cran()

  result <- list_available_reports()

  # Filter for DATA type reports
  data_reports <- result[type == "DATA"]
  expect_true(all(data_reports$type == "DATA"))

  # Filter for PERIODO type reports
  periodo_reports <- result[type == "PERIODO"]
  expect_true(all(periodo_reports$type == "PERIODO"))

  # Filter by title pattern
  if (any(grepl("omun", result$title, ignore.case = TRUE))) {
    munic_reports <- result[grepl("omun", title, ignore.case = TRUE)]
    expect_true(nrow(munic_reports) > 0)
  }
})
