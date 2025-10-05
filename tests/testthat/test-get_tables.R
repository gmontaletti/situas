# Test suite for get_tables.R
# Tests for get_situas_tables() - the main exported function

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
