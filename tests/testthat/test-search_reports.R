# Tests for search_reports() and related functions

test_that("search_reports returns all reports with no filters", {
  result <- search_reports()

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  expect_true(all(c("pfun", "title", "analysis_type", "date_range") %in% names(result)))
})


test_that("search_reports filters by keywords correctly", {
  # Search for comuni (municipalities)
  result <- search_reports(keywords = "comuni")

  expect_s3_class(result, "data.table")

  if (nrow(result) > 0) {
    # All results should contain "comuni" in title (case-insensitive)
    expect_true(all(grepl("comuni", result$title, ignore.case = TRUE)))
  }
})


test_that("search_reports handles multiple keywords with OR logic", {
  result <- search_reports(keywords = c("province", "regioni"))

  expect_s3_class(result, "data.table")

  if (nrow(result) > 0) {
    # Each result should contain at least one keyword
    has_keyword <- sapply(result$title, function(title) {
      grepl("province|regioni", title, ignore.case = TRUE)
    })
    expect_true(all(has_keyword))
  }
})


test_that("search_reports filters by analysis_type correctly", {
  # Test DATA type
  result_data <- search_reports(analysis_type = "DATA")
  expect_true(all(result_data$analysis_type == "DATA"))

  # Test PERIODO type
  result_periodo <- search_reports(analysis_type = "PERIODO")
  expect_true(all(result_periodo$analysis_type == "PERIODO"))

  # Test multiple types
  result_both <- search_reports(analysis_type = c("DATA", "PERIODO"))
  expect_true(all(result_both$analysis_type %in% c("DATA", "PERIODO")))
})


test_that("search_reports validates analysis_type values", {
  expect_error(
    search_reports(analysis_type = "INVALID_TYPE"),
    "Invalid analysis_type values"
  )

  expect_error(
    search_reports(analysis_type = c("DATA", "INVALID")),
    "Invalid analysis_type values"
  )
})


test_that("search_reports filters by pfun_range correctly", {
  result <- search_reports(pfun_range = c(60, 70))

  expect_s3_class(result, "data.table")

  if (nrow(result) > 0) {
    expect_true(all(result$pfun >= 60 & result$pfun <= 70))
  }
})


test_that("search_reports validates pfun_range input", {
  expect_error(
    search_reports(pfun_range = c(60)),
    "pfun_range must be an integer vector of length 2"
  )

  expect_error(
    search_reports(pfun_range = c(-10, 50)),
    "pfun_range values must be positive"
  )
})


test_that("search_reports exact_match works correctly", {
  # Find a known exact title
  all_reports <- search_reports()

  if (nrow(all_reports) > 0) {
    exact_title <- all_reports$title[1]

    result <- search_reports(keywords = exact_title, exact_match = TRUE)

    expect_s3_class(result, "data.table")
    expect_true(nrow(result) >= 1)
    expect_true(tolower(exact_title) %in% tolower(result$title))
  }
})


test_that("search_reports combines multiple filters correctly", {
  result <- search_reports(
    keywords = "comuni",
    analysis_type = "DATA",
    pfun_range = c(50, 100)
  )

  expect_s3_class(result, "data.table")

  if (nrow(result) > 0) {
    expect_true(all(grepl("comuni", result$title, ignore.case = TRUE)))
    expect_true(all(result$analysis_type == "DATA"))
    expect_true(all(result$pfun >= 50 & result$pfun <= 100))
  }
})


test_that("search_reports returns sorted results", {
  result <- search_reports()

  if (nrow(result) > 1) {
    expect_equal(result$pfun, sort(result$pfun))
  }
})


# Tests for get_report_details()

test_that("get_report_details returns correct report", {
  result <- get_report_details(61)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$pfun, 61)
})


test_that("get_report_details returns empty for invalid ID", {
  result <- get_report_details(99999)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})


test_that("get_report_details validates input", {
  expect_error(
    get_report_details("not_a_number"),
    "pfun must be a single integer"
  )

  expect_error(
    get_report_details(c(1, 2)),
    "pfun must be a single integer"
  )
})


# Tests for list_report_categories()

test_that("list_report_categories returns proper structure", {
  result <- list_report_categories()

  expect_type(result, "list")
  expect_true(length(result) > 0)

  # Check that each category is a data.table
  for (category in result) {
    expect_s3_class(category, "data.table")
    expect_true(all(c("pfun", "title", "analysis_type") %in% names(category)))
  }
})


test_that("list_report_categories contains expected categories", {
  result <- list_report_categories()

  expected_categories <- c(
    "municipalities",
    "provinces",
    "regions",
    "municipality_changes",
    "labor_systems",
    "translations"
  )

  for (cat in expected_categories) {
    expect_true(cat %in% names(result),
                info = paste("Missing category:", cat))
  }
})


test_that("list_report_categories filters correctly", {
  result <- list_report_categories()

  # Check that municipality_changes only contains PERIODO or ATTUALIZZAZIONE
  if (nrow(result$municipality_changes) > 0) {
    expect_true(
      all(result$municipality_changes$analysis_type %in% c("PERIODO", "ATTUALIZZAZIONE"))
    )
  }

  # Check that municipalities contains DATA type
  if (nrow(result$municipalities) > 0) {
    expect_true(all(result$municipalities$analysis_type == "DATA"))
  }
})
