# Test Suite for situas Package

This directory contains comprehensive unit tests for the situas R package using the testthat framework (edition 3).

## Test Files

### test-cache.R
Tests for cache management functions (`R/cache.R`):
- `get_cache_dir()` - Returns and creates cache directory
- `save_to_cache()` - Saves data with timestamps
- `load_from_cache()` - Retrieves cached data
- `is_cache_valid()` - Validates cache age and existence

**Test Coverage:**
- Directory creation and path validation
- Data saving with timestamp attributes
- Data retrieval from cache
- Cache expiration detection
- Input validation for all functions
- Error handling for corrupted cache files
- Cache files without timestamps

### test-api_client.R
Tests for API client functions (`R/api_client.R`):
- `situas_base_url()` - Returns API base URL
- `situas_api_call()` - Makes HTTP requests to SITUAS API (mocked)
- `parse_funzione_response()` - Parses API responses

**Test Coverage:**
- URL construction and consistency
- Response parsing for various data structures
- Empty and NULL response handling
- Nested data structure handling
- Network error handling (mocked)
- HTTP error handling (404, 500, etc., mocked)
- JSON parsing errors (mocked)
- User agent inclusion
- Additional query parameters

### test-get_tables.R
Tests for main exported function (`R/get_tables.R`):
- `get_situas_tables()` - Main function to retrieve SITUAS tables

**Test Coverage:**
- Parameter validation (force_refresh, max_age_hours, verbose)
- Cache usage when valid cache exists
- API calls when cache is invalid or missing
- Force refresh behavior
- Cache expiration with custom max_age_hours
- Data saving to cache after download
- Verbose messaging
- Error handling for API and parsing errors
- Empty API responses
- Complete workflow integration

### helper-mocks.R
Utility functions and mock data for testing:
- `mock_funzione_response()` - Creates mock API responses
- `mock_empty_response()` - Creates empty API responses
- `mock_cached_data()` - Creates cached data.tables
- `mock_http_response()` - Creates HTTP response objects
- `sample_tables_data()` - Sample SITUAS table data
- `setup_temp_cache()` - Sets up temporary cache directories
- `save_mock_cache()` - Saves mock data to cache
- Error message constants

## Running Tests

### Run All Tests
```r
# From package root directory
devtools::test()

# Or using testthat directly
testthat::test_dir("tests/testthat")
```

### Run Specific Test File
```r
testthat::test_file("tests/testthat/test-cache.R")
testthat::test_file("tests/testthat/test-api_client.R")
testthat::test_file("tests/testthat/test-get_tables.R")
```

### Check Test Coverage
```r
# Requires covr package
covr::package_coverage()
```

## Test Dependencies

The test suite requires the following packages (listed in DESCRIPTION under Suggests):
- **testthat** (>= 3.0.0) - Testing framework
- **mockery** - For mocking functions and HTTP calls
- **withr** - For temporary directory management

Install test dependencies:
```r
install.packages(c("testthat", "mockery", "withr"))
```

## Testing Approach

### Mocking Strategy
All tests use mocking to avoid external dependencies:
- **No real API calls** - All HTTP requests are mocked using mockery
- **Temporary cache directories** - Tests use isolated temp directories via withr
- **Deterministic timestamps** - Cache age tests use fixed time offsets

### Test Isolation
Each test is isolated:
- Tests clean up temporary files automatically
- Mock functions are scoped to individual tests
- No test dependencies on execution order

### Test Organization
Tests follow the AAA pattern:
1. **Arrange** - Set up mocks, test data, and preconditions
2. **Act** - Execute the function under test
3. **Assert** - Verify expected outcomes using expect_* functions

## Test Statistics

### test-cache.R
- 14 test cases
- Tests cover all cache functions and edge cases
- Validates input parameters, error handling, and cache lifecycle

### test-api_client.R
- 17 test cases
- Comprehensive mocking of HTTP interactions
- Tests URL construction, parsing, and error scenarios

### test-get_tables.R
- 21 test cases
- Integration-style tests with mocked dependencies
- Tests complete workflows and parameter interactions

**Total: 52 test cases**

## Notes

1. **get_cache_info() function**: The main function references `get_cache_info()` which is not currently defined in the package. This may need to be implemented or removed from `get_situas_tables()`.

2. **Skipping tests**: Some tests use `skip_if_not_installed("mockery")` to gracefully skip if mockery is not available.

3. **CRAN compliance**: All tests follow CRAN standards:
   - No network calls
   - Temporary files cleaned up
   - No writing to user directories (except designated temp locations)

## Extending the Tests

When adding new functions to the package:
1. Create a new `test-*.R` file for the function
2. Follow existing test patterns
3. Use mocking for external dependencies
4. Add helper functions to `helper-mocks.R` if needed
5. Update this README with new test coverage information
