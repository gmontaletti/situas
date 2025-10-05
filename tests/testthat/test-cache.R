# Test suite for cache.R functions
# Tests for get_cache_dir(), save_to_cache(), load_from_cache(), is_cache_valid()

test_that("get_cache_dir() returns a valid path", {
  cache_dir <- get_cache_dir()

  expect_type(cache_dir, "character")
  expect_true(nzchar(cache_dir))
  expect_true(grepl("situas", cache_dir, fixed = TRUE))
})

test_that("get_cache_dir() creates directory if it doesn't exist", {
  cache_dir <- get_cache_dir()

  expect_true(dir.exists(cache_dir))
})

test_that("save_to_cache() saves data correctly with timestamp", {
  withr::local_tempdir()

  # Create test data
  test_data <- data.table::data.table(
    id = 1:3,
    name = c("A", "B", "C")
  )

  # Mock get_cache_dir to use temp directory
  temp_cache <- tempdir()
  mockery::stub(save_to_cache, "get_cache_dir", temp_cache)

  # Save to cache
  cache_key <- "test_save"
  result <- save_to_cache(test_data, cache_key)

  # Check return value
  expect_true(result)

  # Check file exists
  cache_file <- file.path(temp_cache, paste0(cache_key, ".rds"))
  expect_true(file.exists(cache_file))

  # Load and verify data
  loaded <- readRDS(cache_file)
  expect_identical(loaded[, .(id, name)], test_data)

  # Check timestamp attribute exists
  expect_true(!is.null(attr(loaded, "cache_timestamp")))
  expect_s3_class(attr(loaded, "cache_timestamp"), "POSIXct")
})

test_that("save_to_cache() validates inputs", {
  expect_error(
    save_to_cache(cache_key = "test"),
    "data must be provided"
  )

  expect_error(
    save_to_cache(data = data.table::data.table(), cache_key = ""),
    "cache_key must be a non-empty character string"
  )

  expect_error(
    save_to_cache(data = data.table::data.table(), cache_key = 123),
    "cache_key must be a non-empty character string"
  )

  expect_error(
    save_to_cache(data = data.table::data.table(), cache_key = c("a", "b")),
    "cache_key must be a non-empty character string"
  )
})

test_that("save_to_cache() handles errors gracefully", {
  # Create invalid cache directory path to trigger error
  temp_dir <- tempdir()
  invalid_dir <- file.path(temp_dir, "nonexistent", "deeply", "nested")

  # Mock get_cache_dir to return invalid path
  mockery::stub(save_to_cache, "get_cache_dir", invalid_dir)

  # Attempt to save should return FALSE and warn
  test_data <- data.table::data.table(x = 1)
  expect_warning(
    result <- save_to_cache(test_data, "test"),
    "Failed to save data to cache"
  )
  expect_false(result)
})

test_that("load_from_cache() retrieves saved data", {
  withr::local_tempdir()

  # Create and save test data
  test_data <- data.table::data.table(
    id = 1:5,
    value = letters[1:5]
  )

  temp_cache <- tempdir()
  cache_key <- "test_load"

  # Mock get_cache_dir for both save and load
  mockery::stub(save_to_cache, "get_cache_dir", temp_cache)
  mockery::stub(load_from_cache, "get_cache_dir", temp_cache)

  save_to_cache(test_data, cache_key)

  # Load from cache
  loaded <- load_from_cache(cache_key)

  expect_false(is.null(loaded))
  expect_s3_class(loaded, "data.table")
  expect_identical(loaded[, .(id, value)], test_data)
})

test_that("load_from_cache() returns NULL for non-existent cache", {
  temp_cache <- tempdir()
  mockery::stub(load_from_cache, "get_cache_dir", temp_cache)

  result <- load_from_cache("nonexistent_cache_key")

  expect_null(result)
})

test_that("load_from_cache() validates inputs", {
  expect_error(
    load_from_cache(cache_key = ""),
    "cache_key must be a non-empty character string"
  )

  expect_error(
    load_from_cache(cache_key = 123),
    "cache_key must be a non-empty character string"
  )

  expect_error(
    load_from_cache(cache_key = c("a", "b")),
    "cache_key must be a non-empty character string"
  )
})

test_that("load_from_cache() handles corrupted cache gracefully", {
  withr::local_tempdir()

  temp_cache <- tempdir()
  cache_key <- "corrupted"
  cache_file <- file.path(temp_cache, paste0(cache_key, ".rds"))

  # Create corrupted file
  writeLines("corrupted data", cache_file)

  mockery::stub(load_from_cache, "get_cache_dir", temp_cache)

  # Should return NULL and warn
  expect_warning(
    result <- load_from_cache(cache_key),
    "Failed to load cache"
  )
  expect_null(result)
})

test_that("is_cache_valid() correctly identifies valid cache", {
  withr::local_tempdir()

  # Create fresh cache
  test_data <- data.table::data.table(x = 1:3)
  temp_cache <- tempdir()
  cache_key <- "valid_cache"

  mockery::stub(save_to_cache, "get_cache_dir", temp_cache)
  mockery::stub(is_cache_valid, "get_cache_dir", temp_cache)

  save_to_cache(test_data, cache_key)

  # Cache should be valid with default max_age_hours
  result <- is_cache_valid(cache_key)
  expect_true(result)

  # Cache should be valid with longer max_age
  result <- is_cache_valid(cache_key, max_age_hours = 48)
  expect_true(result)
})

test_that("is_cache_valid() correctly identifies invalid cache", {
  withr::local_tempdir()

  temp_cache <- tempdir()
  cache_key <- "invalid_cache"

  mockery::stub(is_cache_valid, "get_cache_dir", temp_cache)

  # Non-existent cache should be invalid
  result <- is_cache_valid(cache_key)
  expect_false(result)
})

test_that("is_cache_valid() detects expired cache", {
  withr::local_tempdir()

  # Create old cache
  test_data <- data.table::data.table(x = 1:3)
  temp_cache <- tempdir()
  cache_key <- "old_cache"

  # Set timestamp to 2 days ago
  old_timestamp <- Sys.time() - as.difftime(48, units = "hours")
  attr(test_data, "cache_timestamp") <- old_timestamp

  cache_file <- file.path(temp_cache, paste0(cache_key, ".rds"))
  saveRDS(test_data, cache_file)

  mockery::stub(is_cache_valid, "get_cache_dir", temp_cache)

  # Cache should be invalid with max_age_hours = 24
  result <- is_cache_valid(cache_key, max_age_hours = 24)
  expect_false(result)

  # Cache should be valid with max_age_hours = 72
  result <- is_cache_valid(cache_key, max_age_hours = 72)
  expect_true(result)
})

test_that("is_cache_valid() returns FALSE for cache without timestamp", {
  withr::local_tempdir()

  # Create cache without timestamp attribute
  test_data <- data.table::data.table(x = 1:3)
  temp_cache <- tempdir()
  cache_key <- "no_timestamp"

  cache_file <- file.path(temp_cache, paste0(cache_key, ".rds"))
  saveRDS(test_data, cache_file)

  mockery::stub(is_cache_valid, "get_cache_dir", temp_cache)

  result <- is_cache_valid(cache_key)
  expect_false(result)
})

test_that("is_cache_valid() validates inputs", {
  expect_error(
    is_cache_valid(cache_key = ""),
    "cache_key must be a non-empty character string"
  )

  expect_error(
    is_cache_valid(cache_key = 123),
    "cache_key must be a non-empty character string"
  )

  expect_error(
    is_cache_valid(cache_key = "test", max_age_hours = -1),
    "max_age_hours must be a positive numeric value"
  )

  expect_error(
    is_cache_valid(cache_key = "test", max_age_hours = 0),
    "max_age_hours must be a positive numeric value"
  )

  expect_error(
    is_cache_valid(cache_key = "test", max_age_hours = "24"),
    "max_age_hours must be a positive numeric value"
  )
})

test_that("is_cache_valid() handles corrupted cache files", {
  withr::local_tempdir()

  temp_cache <- tempdir()
  cache_key <- "corrupted_valid"
  cache_file <- file.path(temp_cache, paste0(cache_key, ".rds"))

  # Create corrupted file
  writeLines("corrupted", cache_file)

  mockery::stub(is_cache_valid, "get_cache_dir", temp_cache)

  result <- is_cache_valid(cache_key)
  expect_false(result)
})
