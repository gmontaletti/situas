# Test suite for get_tables.R
# Tests for get_situas_tables() - the main exported function

# Tests for pfun validation

test_that("get_situas_tables() validates pfun against metadata", {
  skip_if_offline()
  skip_on_cran()

  # Test with invalid pfun
  expect_error(
    get_situas_tables(pfun = 999999, verbose = FALSE),
    "Report ID 999999 is not in the list of known SITUAS reports"
  )
})

test_that("get_situas_tables() suggests nearby report IDs for invalid pfun", {
  skip_if_offline()
  skip_on_cran()

  # Test with pfun close to valid ones
  expect_error(
    get_situas_tables(pfun = 60, verbose = FALSE),
    "Nearby report IDs"
  )
})

test_that("get_situas_tables() shows report information when verbose", {
  skip_if_offline()
  skip_on_cran()

  # Test with valid pfun - check for correct report title
  expect_message(
    get_situas_tables(pfun = 61, verbose = TRUE),
    "Report 61:.*Elenco dei codici"
  )
})

test_that("get_situas_tables() validates pfun parameter type", {
  expect_error(
    get_situas_tables(pfun = "61"),
    "pfun must be a single integer"
  )

  expect_error(
    get_situas_tables(pfun = 61.5),
    "pfun must be a single integer"
  )

  expect_error(
    get_situas_tables(pfun = c(61, 62)),
    "pfun must be a single integer"
  )
})

# Helper function to create mock API response
create_mock_response <- function(n_items = 3) {
  list(
    items = lapply(seq_len(n_items), function(i) {
      list(
        id = i,
        name = paste0("Table", i),
        description = paste0("Description for table ", i)
      )
    })
  )
}

# Tests for parameter validation

test_that("get_situas_tables() validates force_refresh parameter", {
  expect_error(
    get_situas_tables(force_refresh = "TRUE"),
    "force_refresh must be logical"
  )

  expect_error(
    get_situas_tables(force_refresh = 1),
    "force_refresh must be logical"
  )

  expect_error(
    get_situas_tables(force_refresh = c(TRUE, FALSE)),
    "force_refresh must be logical"
  )

  expect_error(
    get_situas_tables(force_refresh = NA),
    "force_refresh must be logical"
  )
})

test_that("get_situas_tables() validates max_age_hours parameter", {
  expect_error(
    get_situas_tables(max_age_hours = "24"),
    "max_age_hours must be numeric and positive"
  )

  expect_error(
    get_situas_tables(max_age_hours = -1),
    "max_age_hours must be numeric and positive"
  )

  expect_error(
    get_situas_tables(max_age_hours = 0),
    "max_age_hours must be numeric and positive"
  )

  expect_error(
    get_situas_tables(max_age_hours = c(24, 48)),
    "max_age_hours must be numeric and positive"
  )
})

test_that("get_situas_tables() validates verbose parameter", {
  expect_error(
    get_situas_tables(verbose = "TRUE"),
    "verbose must be logical"
  )

  expect_error(
    get_situas_tables(verbose = 1),
    "verbose must be logical"
  )

  expect_error(
    get_situas_tables(verbose = c(TRUE, FALSE)),
    "verbose must be logical"
  )

  expect_error(
    get_situas_tables(verbose = NA),
    "verbose must be logical"
  )
})

# Tests for basic functionality with mocked API

test_that("get_situas_tables() downloads data from API when no cache exists", {
  skip_if_not_installed("mockery")

  withr::local_tempdir()

  # Mock cache directory to use temp
  temp_cache <- tempdir()

  # Mock the cache functions
  mockery::stub(get_situas_tables, "get_cache_dir", temp_cache)
  mockery::stub(get_situas_tables, "is_cache_valid", FALSE)
  mockery::stub(get_situas_tables, "load_from_cache", NULL)

  # Mock API call
  mock_api_response <- create_mock_response(3)
  mockery::stub(get_situas_tables, "situas_api_call", mock_api_response)

  # Call function
  result <- get_situas_tables(verbose = FALSE)

  # Check result
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)
  expect_true(all(c("id", "name", "description") %in% names(result)))
})

test_that("get_situas_tables() uses cache when valid", {
  skip_if_not_installed("mockery")

  withr::local_tempdir()

  # Create cached data
  cached_data <- data.table::data.table(
    id = 1:2,
    name = c("Cached1", "Cached2"),
    description = c("Desc1", "Desc2")
  )
  attr(cached_data, "cache_timestamp") <- Sys.time()

  temp_cache <- tempdir()

  # Mock cache functions to return valid cached data
  mockery::stub(get_situas_tables, "is_cache_valid", TRUE)
  mockery::stub(get_situas_tables, "load_from_cache", cached_data)

  # Mock API call - should NOT be called
  mock_api_call <- mockery::mock(stop("API should not be called"))
  mockery::stub(get_situas_tables, "situas_api_call", mock_api_call)

  # Call function
  result <- get_situas_tables(verbose = FALSE)

  # Check that cached data was returned
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_equal(result$name, c("Cached1", "Cached2"))

  # Verify API was NOT called
  mockery::expect_called(mock_api_call, 0)
})

test_that("get_situas_tables() respects force_refresh parameter", {
  skip_if_not_installed("mockery")

  withr::local_tempdir()

  # Create cached data
  cached_data <- data.table::data.table(
    id = 1:2,
    name = c("Cached1", "Cached2")
  )

  temp_cache <- tempdir()

  # Mock cache as valid but should be ignored due to force_refresh
  mockery::stub(get_situas_tables, "is_cache_valid", TRUE)
  mockery::stub(get_situas_tables, "load_from_cache", cached_data)
  mockery::stub(get_situas_tables, "get_cache_dir", temp_cache)

  # Mock API call - SHOULD be called
  mock_api_response <- create_mock_response(3)
  mock_api_call <- mockery::mock(mock_api_response)
  mockery::stub(get_situas_tables, "situas_api_call", mock_api_call)

  # Call with force_refresh = TRUE
  result <- get_situas_tables(force_refresh = TRUE, verbose = FALSE)

  # Check that fresh data was returned (3 rows, not 2)
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)

  # Verify API WAS called
  mockery::expect_called(mock_api_call, 1)
})

test_that("get_situas_tables() refreshes when cache is invalid", {
  skip_if_not_installed("mockery")

  withr::local_tempdir()

  temp_cache <- tempdir()

  # Mock cache as invalid
  mockery::stub(get_situas_tables, "is_cache_valid", FALSE)
  mockery::stub(get_situas_tables, "load_from_cache", NULL)
  mockery::stub(get_situas_tables, "get_cache_dir", temp_cache)

  # Mock API call
  mock_api_response <- create_mock_response(5)
  mock_api_call <- mockery::mock(mock_api_response)
  mockery::stub(get_situas_tables, "situas_api_call", mock_api_call)

  # Call function
  result <- get_situas_tables(verbose = FALSE)

  # Check result
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 5)

  # Verify API was called
  mockery::expect_called(mock_api_call, 1)
})

test_that("get_situas_tables() respects max_age_hours parameter", {
  skip_if_not_installed("mockery")

  withr::local_tempdir()

  # Test that max_age_hours is passed to is_cache_valid
  mock_is_cache_valid <- mockery::mock(TRUE)
  mockery::stub(get_situas_tables, "is_cache_valid", mock_is_cache_valid)

  cached_data <- data.table::data.table(id = 1, name = "Test")
  mockery::stub(get_situas_tables, "load_from_cache", cached_data)

  # Call with custom max_age_hours
  result <- get_situas_tables(max_age_hours = 48, verbose = FALSE)

  # Check that is_cache_valid was called with correct max_age_hours
  mockery::expect_called(mock_is_cache_valid, 1)
  call_args <- mockery::mock_args(mock_is_cache_valid)[[1]]
  expect_equal(call_args[[2]], 48)
})

test_that("get_situas_tables() saves data to cache after download", {
  skip_if_not_installed("mockery")

  withr::local_tempdir()

  temp_cache <- tempdir()

  # Mock cache as invalid
  mockery::stub(get_situas_tables, "is_cache_valid", FALSE)
  mockery::stub(get_situas_tables, "load_from_cache", NULL)
  mockery::stub(get_situas_tables, "get_cache_dir", temp_cache)

  # Mock API call
  mock_api_response <- create_mock_response(3)
  mockery::stub(get_situas_tables, "situas_api_call", mock_api_response)

  # Mock save_to_cache to track calls
  mock_save <- mockery::mock(TRUE)
  mockery::stub(get_situas_tables, "save_to_cache", mock_save)

  # Call function
  result <- get_situas_tables(verbose = FALSE)

  # Verify save_to_cache was called
  mockery::expect_called(mock_save, 1)

  # Check the arguments to save_to_cache
  call_args <- mockery::mock_args(mock_save)[[1]]
  expect_s3_class(call_args[[1]], "data.table")
  expect_equal(call_args[[2]], "situas_tables")
})

# Tests for verbose messages

test_that("get_situas_tables() shows messages when verbose = TRUE", {
  skip_if_not_installed("mockery")

  withr::local_tempdir()

  temp_cache <- tempdir()

  mockery::stub(get_situas_tables, "is_cache_valid", FALSE)
  mockery::stub(get_situas_tables, "load_from_cache", NULL)
  mockery::stub(get_situas_tables, "get_cache_dir", temp_cache)

  mock_api_response <- create_mock_response(3)
  mockery::stub(get_situas_tables, "situas_api_call", mock_api_response)

  expect_message(
    get_situas_tables(verbose = TRUE),
    "Downloading available tables from SITUAS API"
  )

  expect_message(
    get_situas_tables(verbose = TRUE),
    "Successfully downloaded and cached"
  )
})

test_that("get_situas_tables() shows cache message when using cached data", {
  skip_if_not_installed("mockery")

  cached_data <- data.table::data.table(id = 1, name = "Test")
  attr(cached_data, "cache_timestamp") <- Sys.time()

  mockery::stub(get_situas_tables, "is_cache_valid", TRUE)
  mockery::stub(get_situas_tables, "load_from_cache", cached_data)

  # Note: get_cache_info is called but might not exist
  # Mock it to avoid errors
  mock_cache_info <- list(timestamp = Sys.time())
  mockery::stub(get_situas_tables, "get_cache_info", mock_cache_info)

  expect_message(
    get_situas_tables(verbose = TRUE),
    "Using cached data"
  )
})

test_that("get_situas_tables() suppresses messages when verbose = FALSE", {
  skip_if_not_installed("mockery")

  withr::local_tempdir()

  temp_cache <- tempdir()

  mockery::stub(get_situas_tables, "is_cache_valid", FALSE)
  mockery::stub(get_situas_tables, "load_from_cache", NULL)
  mockery::stub(get_situas_tables, "get_cache_dir", temp_cache)

  mock_api_response <- create_mock_response(3)
  mockery::stub(get_situas_tables, "situas_api_call", mock_api_response)

  expect_silent(
    get_situas_tables(verbose = FALSE)
  )
})

# Tests for error handling

test_that("get_situas_tables() handles API errors gracefully", {
  skip_if_not_installed("mockery")

  withr::local_tempdir()

  temp_cache <- tempdir()

  mockery::stub(get_situas_tables, "is_cache_valid", FALSE)
  mockery::stub(get_situas_tables, "load_from_cache", NULL)
  mockery::stub(get_situas_tables, "get_cache_dir", temp_cache)

  # Mock API call to throw error
  mockery::stub(get_situas_tables, "situas_api_call", function(...) {
    stop("Network timeout")
  })

  expect_error(
    get_situas_tables(verbose = FALSE),
    "Failed to retrieve SITUAS tables"
  )
})

test_that("get_situas_tables() handles parsing errors", {
  skip_if_not_installed("mockery")

  withr::local_tempdir()

  temp_cache <- tempdir()

  mockery::stub(get_situas_tables, "is_cache_valid", FALSE)
  mockery::stub(get_situas_tables, "load_from_cache", NULL)
  mockery::stub(get_situas_tables, "get_cache_dir", temp_cache)

  # Mock successful API call
  mock_api_response <- create_mock_response(3)
  mockery::stub(get_situas_tables, "situas_api_call", mock_api_response)

  # Mock parse_funzione_response to throw error
  mockery::stub(get_situas_tables, "parse_funzione_response", function(...) {
    stop("Cannot parse response")
  })

  expect_error(
    get_situas_tables(verbose = FALSE),
    "Failed to retrieve SITUAS tables"
  )
})

test_that("get_situas_tables() returns data.table with correct structure", {
  skip_if_not_installed("mockery")

  withr::local_tempdir()

  temp_cache <- tempdir()

  mockery::stub(get_situas_tables, "is_cache_valid", FALSE)
  mockery::stub(get_situas_tables, "load_from_cache", NULL)
  mockery::stub(get_situas_tables, "get_cache_dir", temp_cache)

  mock_api_response <- create_mock_response(3)
  mockery::stub(get_situas_tables, "situas_api_call", mock_api_response)

  result <- get_situas_tables(verbose = FALSE)

  expect_s3_class(result, "data.table")
  expect_true(inherits(result, "data.frame"))
  expect_true(nrow(result) > 0)
})

test_that("get_situas_tables() handles empty API response", {
  skip_if_not_installed("mockery")

  withr::local_tempdir()

  temp_cache <- tempdir()

  mockery::stub(get_situas_tables, "is_cache_valid", FALSE)
  mockery::stub(get_situas_tables, "load_from_cache", NULL)
  mockery::stub(get_situas_tables, "get_cache_dir", temp_cache)

  # Empty response
  mock_api_response <- list(items = list())
  mockery::stub(get_situas_tables, "situas_api_call", mock_api_response)

  result <- get_situas_tables(verbose = FALSE)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

# Integration-style tests (without external dependencies)

test_that("get_situas_tables() complete workflow: no cache -> download -> cache", {
  skip_if_not_installed("mockery")

  withr::local_tempdir()

  temp_cache <- tempdir()
  cache_key <- "situas_tables"

  # Initially no cache
  mockery::stub(get_situas_tables, "get_cache_dir", temp_cache)

  # First call - should check cache validity
  mock_is_valid <- mockery::mock(FALSE)
  mockery::stub(get_situas_tables, "is_cache_valid", mock_is_valid)

  # Load should return NULL
  mock_load <- mockery::mock(NULL)
  mockery::stub(get_situas_tables, "load_from_cache", mock_load)

  # API should be called
  mock_api_response <- create_mock_response(3)
  mock_api <- mockery::mock(mock_api_response)
  mockery::stub(get_situas_tables, "situas_api_call", mock_api)

  # Save should be called
  mock_save <- mockery::mock(TRUE)
  mockery::stub(get_situas_tables, "save_to_cache", mock_save)

  result <- get_situas_tables(verbose = FALSE)

  # Verify workflow
  mockery::expect_called(mock_is_valid, 1)
  mockery::expect_called(mock_api, 1)
  mockery::expect_called(mock_save, 1)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)
})

# Tests for report type detection and date_end parameter

test_that("get_situas_tables() requires date_end for PERIODO reports", {
  skip_if_offline()
  skip_on_cran()

  # Test with PERIODO report without date_end
  # First, find a PERIODO type report from metadata
  periodo_reports <- situas_reports_metadata[analysis_type == "PERIODO"]

  if (nrow(periodo_reports) > 0) {
    periodo_pfun <- periodo_reports$pfun[1]

    expect_error(
      get_situas_tables(pfun = periodo_pfun, date = Sys.Date(), verbose = FALSE),
      "is a PERIODO type report and requires both date \\(start\\) and date_end parameters"
    )
  } else {
    skip("No PERIODO type reports available in metadata")
  }
})

test_that("get_situas_tables() requires date_end for ATTUALIZZAZIONE reports", {
  skip_if_offline()
  skip_on_cran()

  # Test with ATTUALIZZAZIONE report without date_end
  attualizzazione_reports <- situas_reports_metadata[analysis_type == "ATTUALIZZAZIONE"]

  if (nrow(attualizzazione_reports) > 0) {
    attualizzazione_pfun <- attualizzazione_reports$pfun[1]

    expect_error(
      get_situas_tables(pfun = attualizzazione_pfun, date = Sys.Date(), verbose = FALSE),
      "is a ATTUALIZZAZIONE type report and requires both date \\(start\\) and date_end parameters"
    )
  } else {
    skip("No ATTUALIZZAZIONE type reports available in metadata")
  }
})

test_that("get_situas_tables() warns when date_end provided for DATA reports", {
  skip_if_offline()
  skip_on_cran()

  # Test with DATA report with date_end
  # Report 61 (municipalities) is a DATA type report
  expect_warning(
    get_situas_tables(
      pfun = 61,
      date = Sys.Date(),
      date_end = Sys.Date() + 30,
      verbose = FALSE
    ),
    "Report 61 is a DATA type report. The date_end parameter will be ignored."
  )
})

test_that("get_situas_tables() accepts date_end for PERIODO reports", {
  skip_if_offline()
  skip_on_cran()

  periodo_reports <- situas_reports_metadata[analysis_type == "PERIODO"]

  if (nrow(periodo_reports) > 0) {
    periodo_pfun <- periodo_reports$pfun[1]

    # Should not throw error when date_end is provided
    expect_error(
      get_situas_tables(
        pfun = periodo_pfun,
        date = as.Date("2020-01-01"),
        date_end = as.Date("2025-01-01"),
        verbose = FALSE
      ),
      NA  # Expect no error
    )
  } else {
    skip("No PERIODO type reports available in metadata")
  }
})

test_that("get_situas_tables() creates correct cache key for DATA reports", {
  skip_if_not_installed("mockery")

  # Mock functions
  mockery::stub(get_situas_tables, "is_cache_valid", FALSE)

  # Track the cache key used
  cache_keys <- character(0)
  mock_save <- function(data, key) {
    cache_keys <<- c(cache_keys, key)
    TRUE
  }

  mockery::stub(get_situas_tables, "save_to_cache", mock_save)
  mockery::stub(get_situas_tables, "situas_get_report_data",
                data.table::data.table(id = 1, name = "Test"))

  # Call with DATA report (pfun 61)
  test_date <- as.Date("2020-01-01")
  get_situas_tables(pfun = 61, date = test_date, verbose = FALSE)

  # Check cache key format
  expected_key <- "situas_report_61_20200101"
  expect_equal(cache_keys[1], expected_key)
})

test_that("get_situas_tables() creates correct cache key for PERIODO reports", {
  skip_if_not_installed("mockery")

  # Find a PERIODO report
  periodo_reports <- situas_reports_metadata[analysis_type == "PERIODO"]

  if (nrow(periodo_reports) > 0) {
    periodo_pfun <- periodo_reports$pfun[1]

    # Mock functions
    mockery::stub(get_situas_tables, "is_cache_valid", FALSE)

    # Track the cache key used
    cache_keys <- character(0)
    mock_save <- function(data, key) {
      cache_keys <<- c(cache_keys, key)
      TRUE
    }

    mockery::stub(get_situas_tables, "save_to_cache", mock_save)
    mockery::stub(get_situas_tables, "situas_get_report_data",
                  data.table::data.table(id = 1, name = "Test"))

    # Call with PERIODO report
    test_date_start <- as.Date("2020-01-01")
    test_date_end <- as.Date("2025-01-01")
    get_situas_tables(
      pfun = periodo_pfun,
      date = test_date_start,
      date_end = test_date_end,
      verbose = FALSE
    )

    # Check cache key format includes both dates
    expected_key <- paste0("situas_report_", periodo_pfun, "_20200101_20250101")
    expect_equal(cache_keys[1], expected_key)
  } else {
    skip("No PERIODO type reports available in metadata")
  }
})

test_that("get_situas_tables() shows correct verbose message for PERIODO reports", {
  skip_if_not_installed("mockery")

  periodo_reports <- situas_reports_metadata[analysis_type == "PERIODO"]

  if (nrow(periodo_reports) > 0) {
    periodo_pfun <- periodo_reports$pfun[1]

    # Mock functions
    mockery::stub(get_situas_tables, "is_cache_valid", FALSE)
    mockery::stub(get_situas_tables, "save_to_cache", TRUE)
    mockery::stub(get_situas_tables, "situas_get_report_data",
                  data.table::data.table(id = 1, name = "Test"))

    # Call with PERIODO report and verbose = TRUE
    expect_message(
      get_situas_tables(
        pfun = periodo_pfun,
        date = as.Date("2020-01-01"),
        date_end = as.Date("2025-01-01"),
        verbose = TRUE
      ),
      "Downloading data for period 2020-01-01 to 2025-01-01"
    )
  } else {
    skip("No PERIODO type reports available in metadata")
  }
})

test_that("get_situas_tables() passes date_end to situas_get_report_data", {
  skip_if_not_installed("mockery")

  periodo_reports <- situas_reports_metadata[analysis_type == "PERIODO"]

  if (nrow(periodo_reports) > 0) {
    periodo_pfun <- periodo_reports$pfun[1]

    # Mock functions
    mockery::stub(get_situas_tables, "is_cache_valid", FALSE)
    mockery::stub(get_situas_tables, "save_to_cache", TRUE)

    # Create mock to track calls
    mock_get_report <- mockery::mock(data.table::data.table(id = 1, name = "Test"))
    mockery::stub(get_situas_tables, "situas_get_report_data", mock_get_report)

    # Call with PERIODO report
    test_date_start <- as.Date("2020-01-01")
    test_date_end <- as.Date("2025-01-01")
    get_situas_tables(
      pfun = periodo_pfun,
      date = test_date_start,
      date_end = test_date_end,
      verbose = FALSE
    )

    # Check that situas_get_report_data was called with correct arguments
    mockery::expect_called(mock_get_report, 1)
    call_args <- mockery::mock_args(mock_get_report)[[1]]
    expect_equal(call_args$pfun, periodo_pfun)
    expect_equal(call_args$date, test_date_start)
    expect_equal(call_args$date_end, test_date_end)
  } else {
    skip("No PERIODO type reports available in metadata")
  }
})
