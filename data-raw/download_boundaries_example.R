# download_boundaries_example.R
# Example script demonstrating how to download ISTAT boundary shapefiles
# using the situas package download functions

library(situas)

# 1. List Available Versions -----
# Check what boundary versions are available from the OnData repository

cat("\n=== Available Boundary Versions ===\n")
versions <- list_istat_boundary_versions(since_year = 2020)
print(versions)

# 2. Check for Updates -----
# See if there are newer boundaries available than what you have cached

cat("\n=== Checking for Updates ===\n")
updates <- check_boundary_updates()
print(updates)

# 3. Download All Territorial Levels -----
# Download boundaries for all territorial levels (comuni, province, regioni, ripartizioni)

cat("\n=== Downloading All Boundaries for 2025 ===\n")
result_all <- download_istat_boundaries(
  date = "2025-01-01",
  territorial_levels = c("comuni", "province", "regioni", "ripartizioni"),
  force_refresh = FALSE,  # Use cache if available
  verbose = TRUE
)
print(result_all)

# 4. Download Specific Territorial Level -----
# Download only municipalities for a specific date

cat("\n=== Downloading Only Municipalities ===\n")
result_comuni <- download_istat_boundaries(
  date = "2024-01-01",
  territorial_levels = "comuni",
  verbose = TRUE
)
print(result_comuni)

# 5. View Cached Boundaries Info -----
# See what boundaries are currently cached

cat("\n=== Cached Boundaries Info ===\n")
cached_info <- get_cached_boundaries_info(territorial_level = "all")
print(cached_info)

# View info for specific territorial level
cached_comuni <- get_cached_boundaries_info(territorial_level = "comuni")
print(cached_comuni)

# 6. Clean Old Boundaries -----
# Remove old boundary versions to free up disk space

# Dry run first - see what would be removed
cat("\n=== Cleaning Cache (Dry Run) ===\n")
would_remove <- clean_boundary_cache(
  keep_latest_n = 1,      # Keep only the most recent version
  dry_run = TRUE,          # Don't actually remove
  verbose = TRUE
)

# Actually clean the cache
cat("\n=== Cleaning Cache (For Real) ===\n")
removed <- clean_boundary_cache(
  keep_latest_n = 2,       # Keep 2 most recent versions of each level
  dry_run = FALSE,
  verbose = TRUE
)

# 7. Force Re-download -----
# Force re-download even if already cached (useful if files are corrupted)

cat("\n=== Force Re-download ===\n")
result_force <- download_istat_boundaries(
  date = "2025-01-01",
  territorial_levels = "ripartizioni",  # Smallest file for quick test
  force_refresh = TRUE,                 # Ignore cache
  verbose = TRUE
)
print(result_force)

# 8. Use with prepare_territorial_maps() -----
# Downloaded boundaries are automatically detected by prepare_territorial_maps()

cat("\n=== Creating Map with Downloaded Boundaries ===\n")
# The function will automatically use cached boundaries
map_files <- prepare_territorial_maps(
  territorial_level = "regioni",
  output_dir = "data",
  verbose = TRUE
)
print(map_files)

# 9. Clean Up Old Versions by Date -----
# Remove boundaries older than 1 year

cat("\n=== Remove Boundaries Older Than 1 Year ===\n")
removed_old <- clean_boundary_cache(
  older_than_days = 365,
  keep_latest_n = NULL,
  dry_run = TRUE,
  verbose = TRUE
)

# 10. Download Historical Boundaries -----
# Download boundaries for historical analysis

cat("\n=== Downloading Historical Boundaries ===\n")
result_2023 <- download_istat_boundaries(
  date = "2023-01-01",
  territorial_levels = c("comuni", "province", "regioni"),
  verbose = TRUE
)
print(result_2023)

# Final info
cat("\n=== Final Cache Status ===\n")
final_info <- get_cached_boundaries_info()
print(final_info)

cat("\n=== Summary ===\n")
cat("Total cached files:", nrow(final_info), "\n")
cat("Total disk space:", sum(final_info$file_size_mb, na.rm = TRUE), "MB\n")
cat("Cache location:", tools::R_user_dir("situas", which = "data"), "\n")

# Tips:
# - Downloaded boundaries are cached in: tools::R_user_dir("situas", which = "data")
# - Cache persists across R sessions
# - prepare_territorial_maps() automatically uses cached boundaries
# - Use check_boundary_updates() periodically to get latest boundaries
# - Use clean_boundary_cache() to manage disk space
