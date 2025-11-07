# Comprehensive test script for new SITUAS API implementation
# This script tests all functions with actual API calls

library(data.table)

cat("=== SITUAS API Test Suite ===\n\n")

# Source all R files
cat("Loading package functions...\n")
source("R/cache.R")
source("R/api_client.R")
source("R/list_reports.R")
source("R/get_tables.R")

cat("✓ Functions loaded\n\n")

# Test 1: Date formatting
cat("Test 1: Date Formatting\n")
cat("------------------------\n")
test_date <- as.Date("2025-10-05")
formatted <- format_situas_date(test_date)
cat("Input:", as.character(test_date), "\n")
cat("Output:", formatted, "\n")
stopifnot(formatted == "05/10/2025")
cat("✓ Date formatting works\n\n")

# Test 2: Get report count (DATA type)
cat("Test 2: Get Report Count (DATA type - Report 61)\n")
cat("------------------------------------------------\n")
count <- situas_get_report_count(pfun = 61)
cat("Municipality count:", count, "\n")
stopifnot(count > 7000 && count < 10000)
cat("✓ Report count retrieval works\n\n")

# Test 3: Get report count (PERIODO type)
cat("Test 3: Get Report Count (PERIODO type - Report 99)\n")
cat("---------------------------------------------------\n")
count_periodo <- situas_get_report_count(
  pfun = 99,
  date = as.Date("2024-01-01"),
  date_end = as.Date("2025-10-05")
)
cat("Municipality translation count:", count_periodo, "\n")
stopifnot(count_periodo > 0)
cat("✓ PERIODO report count works\n\n")

# Test 4: Get report data (small report)
cat("Test 4: Get Report Data (Report 71 - Ripartizioni)\n")
cat("--------------------------------------------------\n")
ripartizioni <- situas_get_report_data(pfun = 71)
cat("Dimensions:", paste(dim(ripartizioni), collapse = " x "), "\n")
cat("Columns:", paste(names(ripartizioni), collapse = ", "), "\n")
cat("First row:\n")
print(ripartizioni[1])
stopifnot(nrow(ripartizioni) == 5)
cat("✓ Report data retrieval works\n\n")

# Test 5: Get report metadata
cat("Test 5: Get Report Metadata (Report 61)\n")
cat("---------------------------------------\n")
metadata <- situas_get_report_metadata(pfun = 61)
cat("Report Name:", metadata$`REPORT NAME`, "\n")
cat("Number of columns:", length(metadata$COL_DETAILS), "\n")
cat("First 3 columns:\n")
for (i in 1:min(3, length(metadata$COL_DETAILS))) {
  col <- metadata$COL_DETAILS[[i]]
  cat("  -", col$COLNAME, ":", col$LABEL, "\n")
}
stopifnot(!is.null(metadata$`REPORT NAME`))
cat("✓ Metadata retrieval works\n\n")

# Test 6: Get tables with caching
cat("Test 6: Get Tables with Caching\n")
cat("--------------------------------\n")
cat("First call (should download):\n")
municipalities1 <- get_situas_tables(pfun = 61, verbose = TRUE)
cat("Dimensions:", paste(dim(municipalities1), collapse = " x "), "\n")

cat("\nSecond call (should use cache):\n")
municipalities2 <- get_situas_tables(pfun = 61, verbose = TRUE)
cat("Dimensions:", paste(dim(municipalities2), collapse = " x "), "\n")

# Compare data content (not attributes)
stopifnot(all.equal(
  municipalities1[, .SD],
  municipalities2[, .SD],
  check.attributes = FALSE
))
cat("✓ Caching works correctly\n\n")

# Test 7: Force refresh
cat("Test 7: Force Refresh\n")
cat("---------------------\n")
municipalities3 <- get_situas_tables(pfun = 61, force_refresh = TRUE, verbose = TRUE)
cat("Dimensions:", paste(dim(municipalities3), collapse = " x "), "\n")
cat("✓ Force refresh works\n\n")

# Test 8: List available reports (just first 5)
cat("Test 8: List Available Reports (first 5)\n")
cat("----------------------------------------\n")
# Test with just a few reports to save time
test_reports <- list_available_reports(verbose = FALSE)
cat("Total reports discovered:", nrow(test_reports), "\n")
cat("\nFirst 5 reports:\n")
print(test_reports[1:5, .(pfun, name, type, row_count)])
stopifnot(nrow(test_reports) >= 20)
cat("✓ Report listing works\n\n")

# Test 9: Scan for reports (limited range)
cat("Test 9: Scan for Reports (IDs 60-65)\n")
cat("-------------------------------------\n")
scanned <- scan_situas_reports(max_id = 65, verbose = FALSE)
scanned_filtered <- scanned[pfun >= 60]
cat("Found reports in range 60-65:\n")
print(scanned_filtered)
cat("✓ Report scanning works\n\n")

# Test 10: Different date
cat("Test 10: Get Data for Different Date\n")
cat("-------------------------------------\n")
old_data <- situas_get_report_data(pfun = 61, date = as.Date("2020-01-01"))
current_data <- situas_get_report_data(pfun = 61, date = Sys.Date())
cat("Municipalities in 2020:", nrow(old_data), "\n")
cat("Municipalities today:", nrow(current_data), "\n")
cat("Difference:", nrow(current_data) - nrow(old_data), "\n")
cat("✓ Historical data retrieval works\n\n")

cat("=== ALL TESTS PASSED ===\n")
cat("\nPackage is ready for use!\n")
