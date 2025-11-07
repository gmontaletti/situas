# Test script to verify date_end functionality
# This script tests the new date_end parameter implementation

library(situas)

cat("Testing get_situas_tables() with date_end parameter\n")
cat("===================================================\n\n")

# Test 1: DATA type report (should work normally)
cat("Test 1: DATA type report (pfun = 61)\n")
cat("--------------------------------------\n")
tryCatch({
  result <- get_situas_tables(pfun = 61, date = Sys.Date())
  cat("SUCCESS: Retrieved", nrow(result), "rows\n\n")
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n\n")
})

# Test 2: DATA type report with date_end (should warn)
cat("Test 2: DATA type report with date_end (should warn)\n")
cat("-----------------------------------------------------\n")
tryCatch({
  result <- get_situas_tables(
    pfun = 61,
    date = Sys.Date(),
    date_end = Sys.Date() + 30,
    force_refresh = TRUE
  )
  cat("SUCCESS: Retrieved", nrow(result), "rows (warning should have appeared)\n\n")
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n\n")
})

# Test 3: Find PERIODO reports
cat("Test 3: Finding PERIODO type reports\n")
cat("-------------------------------------\n")
periodo_reports <- situas_reports_metadata[analysis_type == "PERIODO"]
if (nrow(periodo_reports) > 0) {
  cat("Found", nrow(periodo_reports), "PERIODO type reports:\n")
  print(periodo_reports[1:min(5, nrow(periodo_reports)), .(pfun, title)])
  cat("\n")

  # Test 4: PERIODO report without date_end (should error)
  cat("Test 4: PERIODO report without date_end (should error)\n")
  cat("-------------------------------------------------------\n")
  periodo_pfun <- periodo_reports$pfun[1]
  cat("Testing with report", periodo_pfun, "\n")
  tryCatch({
    result <- get_situas_tables(pfun = periodo_pfun, date = Sys.Date())
    cat("UNEXPECTED: Should have errored\n\n")
  }, error = function(e) {
    cat("EXPECTED ERROR:", conditionMessage(e), "\n\n")
  })

  # Test 5: PERIODO report with date_end (should work)
  cat("Test 5: PERIODO report with date_end (should work)\n")
  cat("---------------------------------------------------\n")
  cat("Testing with report", periodo_pfun, "\n")
  tryCatch({
    result <- get_situas_tables(
      pfun = periodo_pfun,
      date = as.Date("2020-01-01"),
      date_end = as.Date("2025-01-01"),
      force_refresh = TRUE
    )
    cat("SUCCESS: Retrieved", nrow(result), "rows\n\n")
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n\n")
  })
} else {
  cat("No PERIODO type reports found in metadata\n\n")
}

# Test 6: Find ATTUALIZZAZIONE reports
cat("Test 6: Finding ATTUALIZZAZIONE type reports\n")
cat("---------------------------------------------\n")
attualizzazione_reports <- situas_reports_metadata[analysis_type == "ATTUALIZZAZIONE"]
if (nrow(attualizzazione_reports) > 0) {
  cat("Found", nrow(attualizzazione_reports), "ATTUALIZZAZIONE type reports:\n")
  print(attualizzazione_reports[1:min(5, nrow(attualizzazione_reports)), .(pfun, title)])
  cat("\n")

  # Test 7: ATTUALIZZAZIONE report without date_end (should error)
  cat("Test 7: ATTUALIZZAZIONE report without date_end (should error)\n")
  cat("---------------------------------------------------------------\n")
  attualizzazione_pfun <- attualizzazione_reports$pfun[1]
  cat("Testing with report", attualizzazione_pfun, "\n")
  tryCatch({
    result <- get_situas_tables(pfun = attualizzazione_pfun, date = Sys.Date())
    cat("UNEXPECTED: Should have errored\n\n")
  }, error = function(e) {
    cat("EXPECTED ERROR:", conditionMessage(e), "\n\n")
  })

  # Test 8: ATTUALIZZAZIONE report with date_end (should work)
  cat("Test 8: ATTUALIZZAZIONE report with date_end (should work)\n")
  cat("-----------------------------------------------------------\n")
  cat("Testing with report", attualizzazione_pfun, "\n")
  tryCatch({
    result <- get_situas_tables(
      pfun = attualizzazione_pfun,
      date = as.Date("2020-01-01"),
      date_end = as.Date("2025-01-01"),
      force_refresh = TRUE
    )
    cat("SUCCESS: Retrieved", nrow(result), "rows\n\n")
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n\n")
  })
} else {
  cat("No ATTUALIZZAZIONE type reports found in metadata\n\n")
}

cat("All tests completed\n")
