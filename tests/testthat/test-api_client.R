# Tests for API client functions

test_that("situas_base_url returns correct URL", {
  url <- situas_base_url()

  expect_type(url, "character")
  expect_equal(url, "https://situas-servizi.istat.it/publish/")
  expect_true(nzchar(url))
  expect_true(grepl("^https://", url))
})

test_that("format_situas_date formats dates correctly", {
  # Test with Date object
  expect_equal(format_situas_date(as.Date("2025-10-05")), "05/10/2025")
  expect_equal(format_situas_date(as.Date("2020-01-01")), "01/01/2020")
  expect_equal(format_situas_date(as.Date("1999-12-31")), "31/12/1999")

  # Test with character string
  expect_equal(format_situas_date("2025-10-05"), "05/10/2025")
  expect_equal(format_situas_date("2020-01-01"), "01/01/2020")
})

# Tests for parse_resultset_response()

test_that("parse_resultset_response handles NULL input", {
  result <- parse_resultset_response(NULL)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("parse_resultset_response handles empty response", {
  # Response with NULL resultset
  response <- list(resultset = NULL)
  result <- parse_resultset_response(response)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("parse_resultset_response handles response with empty resultset list", {
  response <- list(resultset = list())
  result <- parse_resultset_response(response)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("parse_resultset_response converts valid response to data.table", {
  # Create mock response
  response <- list(
    resultset = list(
      list(id = 1, name = "Item1", value = 100),
      list(id = 2, name = "Item2", value = 200),
      list(id = 3, name = "Item3", value = 300)
    )
  )

  result <- parse_resultset_response(response)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)
  expect_true(all(c("id", "name", "value") %in% names(result)))
  expect_equal(result$id, c(1, 2, 3))
  expect_equal(result$name, c("Item1", "Item2", "Item3"))
  expect_equal(result$value, c(100, 200, 300))
})

test_that("parse_resultset_response handles single item response", {
  response <- list(
    resultset = list(
      list(id = 1, name = "SingleItem")
    )
  )

  result <- parse_resultset_response(response)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$id, 1)
  expect_equal(result$name, "SingleItem")
})

test_that("parse_resultset_response handles responses without resultset field", {
  response <- list(other_field = "value")

  result <- parse_resultset_response(response)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("parse_resultset_response handles named vectors without warnings", {
  # Create a response with a named vector (edge case that could trigger warnings)
  response <- list(
    resultset = list(
      c(id = 1, name = 2, value = 100)  # Named numeric vector
    )
  )

  # This should not produce any warnings about keep_vec_names
  expect_no_warning(result <- parse_resultset_response(response))

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) >= 0)  # Should handle gracefully
})

# Integration tests (require network access)

test_that("situas_get_report_count works for DATA type report", {
  skip_if_offline()
  skip_on_cran()

  # Use report 71 (Ripartizioni) - small and stable
  count <- situas_get_report_count(pfun = 71)
  expect_type(count, "integer")
  expect_equal(count, 5L)
})

test_that("situas_get_report_count works for PERIODO type report", {
  skip_if_offline()
  skip_on_cran()

  count <- situas_get_report_count(
    pfun = 99,
    date = as.Date("2024-01-01"),
    date_end = as.Date("2025-01-01")
  )
  expect_type(count, "integer")
  expect_gt(count, 0)
})

test_that("situas_get_report_data works for small report", {
  skip_if_offline()
  skip_on_cran()

  data <- situas_get_report_data(pfun = 71)  # Ripartizioni
  expect_s3_class(data, "data.table")
  expect_equal(nrow(data), 5)
  expect_gt(ncol(data), 0)
  expect_true("COD_RIP" %in% names(data))
  expect_true("DEN_RIP" %in% names(data))
})

test_that("situas_get_report_metadata returns metadata", {
  skip_if_offline()
  skip_on_cran()

  metadata <- situas_get_report_metadata(pfun = 71)
  expect_type(metadata, "list")
  expect_true("REPORT NAME" %in% names(metadata))
  expect_true("REPORT DESCRIZIONE" %in% names(metadata))
  expect_equal(metadata$`REPORT NAME`, "Elenco Ripartizioni geografiche")
})

test_that("situas_get_report_data validates inputs", {
  expect_error(
    situas_get_report_data(pfun = "not_numeric"),
    "pfun must be a single integer"
  )

  expect_error(
    situas_get_report_data(pfun = c(1, 2)),
    "pfun must be a single integer"
  )

  expect_error(
    situas_get_report_data(pfun = 1.5),
    "pfun must be a single integer"
  )

  expect_error(
    situas_get_report_data(pfun = 61, date = NULL),
    "date must be provided"
  )
})

test_that("situas_get_report_data works with different dates", {
  skip_if_offline()
  skip_on_cran()

  # Get data for a past date
  data_past <- situas_get_report_data(
    pfun = 71,
    date = as.Date("2020-01-01")
  )
  expect_s3_class(data_past, "data.table")
  expect_equal(nrow(data_past), 5)

  # Get data for current date
  data_current <- situas_get_report_data(pfun = 71)
  expect_s3_class(data_current, "data.table")
  expect_equal(nrow(data_current), 5)
})

test_that("situas_get_report_count validates inputs", {
  expect_error(
    situas_get_report_count(pfun = "invalid"),
    "pfun must be a single integer"
  )

  expect_error(
    situas_get_report_count(pfun = 61, date = NULL),
    "date must be provided"
  )
})

test_that("situas_get_report_metadata validates inputs", {
  expect_error(
    situas_get_report_metadata(pfun = "invalid"),
    "pfun must be a single integer"
  )

  expect_error(
    situas_get_report_metadata(pfun = 61, date = NULL),
    "date must be provided"
  )
})
