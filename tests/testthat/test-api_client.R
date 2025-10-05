# Test suite for api_client.R functions
# Tests for situas_base_url(), situas_api_call(), parse_funzione_response()

test_that("situas_base_url() returns correct URL", {
  url <- situas_base_url()

  expect_type(url, "character")
  expect_equal(url, "https://situas.istat.it/ShibO2Module")
  expect_true(nzchar(url))
  expect_true(grepl("^https://", url))
})

test_that("situas_base_url() returns consistent URL", {
  url1 <- situas_base_url()
  url2 <- situas_base_url()

  expect_identical(url1, url2)
})

# Tests for parse_funzione_response()

test_that("parse_funzione_response() handles NULL input", {
  result <- parse_funzione_response(NULL)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("parse_funzione_response() handles empty response", {
  # Response with NULL items
  response <- list(items = NULL)
  result <- parse_funzione_response(response)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("parse_funzione_response() handles response with empty items list", {
  response <- list(items = list())
  result <- parse_funzione_response(response)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("parse_funzione_response() converts valid response to data.table", {
  # Create mock response
  response <- list(
    items = list(
      list(id = 1, name = "Table1", description = "First table"),
      list(id = 2, name = "Table2", description = "Second table"),
      list(id = 3, name = "Table3", description = "Third table")
    )
  )

  result <- parse_funzione_response(response)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)
  expect_true(all(c("id", "name", "description") %in% names(result)))
  expect_equal(result$id, c(1, 2, 3))
  expect_equal(result$name, c("Table1", "Table2", "Table3"))
})

test_that("parse_funzione_response() handles single item response", {
  response <- list(
    items = list(
      list(id = 1, name = "SingleTable")
    )
  )

  result <- parse_funzione_response(response)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$id, 1)
  expect_equal(result$name, "SingleTable")
})

test_that("parse_funzione_response() handles responses with nested structures", {
  response <- list(
    items = list(
      list(
        id = 1,
        name = "Complex",
        metadata = list(created = "2024-01-01", updated = "2024-01-02")
      )
    )
  )

  result <- parse_funzione_response(response)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_true("id" %in% names(result))
})

test_that("parse_funzione_response() handles responses without items field", {
  response <- list(other_field = "value")

  result <- parse_funzione_response(response)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

# Tests for situas_api_call() with mocking

test_that("situas_api_call() constructs correct URL", {
  skip_if_not_installed("mockery")

  # Mock httr::GET to capture the URL
  mock_response <- structure(
    list(
      status_code = 200,
      headers = list(`content-type` = "application/json")
    ),
    class = "response"
  )

  mock_get <- mockery::mock(mock_response)
  mock_content <- mockery::mock('{"items":[]}')

  mockery::stub(situas_api_call, "httr::GET", mock_get)
  mockery::stub(situas_api_call, "httr::stop_for_status", function(x) invisible(x))
  mockery::stub(situas_api_call, "httr::content", mock_content)
  mockery::stub(situas_api_call, "jsonlite::fromJSON", function(x, ...) list())

  situas_api_call("/api/Report/GetFunzione")

  # Check that GET was called
  mockery::expect_called(mock_get, 1)

  # Get the first argument of the first call (the URL)
  call_args <- mockery::mock_args(mock_get)[[1]]
  expect_match(call_args[[1]], "https://situas.istat.it/ShibO2Module/api/Report/GetFunzione")
})

test_that("situas_api_call() includes user agent", {
  skip_if_not_installed("mockery")

  mock_response <- structure(
    list(
      status_code = 200,
      headers = list(`content-type` = "application/json")
    ),
    class = "response"
  )

  mock_get <- mockery::mock(mock_response)
  mock_content <- mockery::mock('{"items":[]}')

  mockery::stub(situas_api_call, "httr::GET", mock_get)
  mockery::stub(situas_api_call, "httr::stop_for_status", function(x) invisible(x))
  mockery::stub(situas_api_call, "httr::content", mock_content)
  mockery::stub(situas_api_call, "jsonlite::fromJSON", function(x, ...) list())

  situas_api_call("/api/test")

  # Check that user_agent was in the call
  call_args <- mockery::mock_args(mock_get)[[1]]
  # Second argument should be user_agent
  expect_true(length(call_args) >= 2)
})

test_that("situas_api_call() handles network errors", {
  skip_if_not_installed("mockery")

  # Mock httr::GET to throw network error
  mock_get <- mockery::mock(stop("Connection timeout"))

  mockery::stub(situas_api_call, "httr::GET", mock_get)

  expect_error(
    situas_api_call("/api/test"),
    "Network error when calling SITUAS API"
  )
})

test_that("situas_api_call() handles HTTP errors", {
  skip_if_not_installed("mockery")

  # Mock response with 404 status
  mock_response <- structure(
    list(status_code = 404),
    class = "response"
  )

  mock_get <- mockery::mock(mock_response)
  mock_status_code <- mockery::mock(404)
  mock_http_status <- mockery::mock(list(message = "Not Found"))

  mockery::stub(situas_api_call, "httr::GET", mock_get)
  mockery::stub(situas_api_call, "httr::stop_for_status", function(x) {
    stop("HTTP 404")
  })
  mockery::stub(situas_api_call, "httr::status_code", mock_status_code)
  mockery::stub(situas_api_call, "httr::http_status", mock_http_status)

  expect_error(
    situas_api_call("/api/notfound"),
    "SITUAS API returned an error"
  )
})

test_that("situas_api_call() handles JSON parsing errors", {
  skip_if_not_installed("mockery")

  mock_response <- structure(
    list(
      status_code = 200,
      headers = list(`content-type` = "application/json")
    ),
    class = "response"
  )

  mock_get <- mockery::mock(mock_response)
  mock_content <- mockery::mock("invalid json {{{")

  mockery::stub(situas_api_call, "httr::GET", mock_get)
  mockery::stub(situas_api_call, "httr::stop_for_status", function(x) invisible(x))
  mockery::stub(situas_api_call, "httr::content", mock_content)
  mockery::stub(situas_api_call, "jsonlite::fromJSON", function(x, ...) {
    stop("Invalid JSON")
  })

  expect_error(
    situas_api_call("/api/test"),
    "Failed to parse JSON response"
  )
})

test_that("situas_api_call() successfully returns parsed response", {
  skip_if_not_installed("mockery")

  mock_response <- structure(
    list(
      status_code = 200,
      headers = list(`content-type` = "application/json")
    ),
    class = "response"
  )

  expected_data <- list(
    items = list(
      list(id = 1, name = "Test")
    )
  )

  mock_get <- mockery::mock(mock_response)
  mock_content <- mockery::mock('{"items":[{"id":1,"name":"Test"}]}')
  mock_from_json <- mockery::mock(expected_data)

  mockery::stub(situas_api_call, "httr::GET", mock_get)
  mockery::stub(situas_api_call, "httr::stop_for_status", function(x) invisible(x))
  mockery::stub(situas_api_call, "httr::content", mock_content)
  mockery::stub(situas_api_call, "jsonlite::fromJSON", mock_from_json)

  result <- situas_api_call("/api/test")

  expect_type(result, "list")
  expect_equal(result, expected_data)
})

test_that("situas_api_call() handles content extraction errors", {
  skip_if_not_installed("mockery")

  mock_response <- structure(
    list(
      status_code = 200,
      headers = list(`content-type` = "application/json")
    ),
    class = "response"
  )

  mock_get <- mockery::mock(mock_response)

  mockery::stub(situas_api_call, "httr::GET", mock_get)
  mockery::stub(situas_api_call, "httr::stop_for_status", function(x) invisible(x))
  mockery::stub(situas_api_call, "httr::content", function(...) {
    stop("Cannot extract content")
  })

  expect_error(
    situas_api_call("/api/test"),
    "Failed to extract content from API response"
  )
})

test_that("situas_api_call() passes additional arguments to httr::GET", {
  skip_if_not_installed("mockery")

  mock_response <- structure(
    list(
      status_code = 200,
      headers = list(`content-type` = "application/json")
    ),
    class = "response"
  )

  mock_get <- mockery::mock(mock_response)
  mock_content <- mockery::mock('{"items":[]}')

  mockery::stub(situas_api_call, "httr::GET", mock_get)
  mockery::stub(situas_api_call, "httr::stop_for_status", function(x) invisible(x))
  mockery::stub(situas_api_call, "httr::content", mock_content)
  mockery::stub(situas_api_call, "jsonlite::fromJSON", function(x, ...) list())

  # Call with additional query parameter
  situas_api_call("/api/test", query = list(param = "value"))

  mockery::expect_called(mock_get, 1)
  call_args <- mockery::mock_args(mock_get)[[1]]
  # Should have at least 3 arguments: URL, user_agent, query
  expect_true(length(call_args) >= 3)
})
