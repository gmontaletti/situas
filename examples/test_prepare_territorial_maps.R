# Test script for prepare_territorial_maps()
# Demonstrates usage for all territorial levels

library(situas)

# Ensure output directory exists
if (!dir.exists("data/examples")) {
  dir.create("data/examples", recursive = TRUE)
}

# 1. MUNICIPALITIES (default)
cat("\n========================================\n")
cat("1. Testing MUNICIPALITIES (comuni)\n")
cat("========================================\n")
files_comuni <- prepare_territorial_maps(
  territorial_level = "comuni",
  output_dir = "data/examples",
  simplify = TRUE,
  tolerance = 0.001,
  verbose = TRUE
)
cat("Files created:\n")
print(unlist(files_comuni))

# 2. PROVINCES
cat("\n========================================\n")
cat("2. Testing PROVINCES (province)\n")
cat("========================================\n")
files_province <- prepare_territorial_maps(
  territorial_level = "province",
  output_dir = "data/examples",
  keep_attributes = c("COD_UTS", "DEN_UTS", "COD_REG", "DEN_REG", "SIGLA"),
  verbose = TRUE
)
cat("Files created:\n")
print(unlist(files_province))

# 3. REGIONS
cat("\n========================================\n")
cat("3. Testing REGIONS (regioni)\n")
cat("========================================\n")
files_regioni <- prepare_territorial_maps(
  territorial_level = "regioni",
  output_dir = "data/examples",
  keep_attributes = c("COD_REG", "DEN_REG"),
  verbose = TRUE
)
cat("Files created:\n")
print(unlist(files_regioni))

# 4. GEOGRAPHIC PARTITIONS
cat("\n========================================\n")
cat("4. Testing GEOGRAPHIC PARTITIONS (ripartizioni)\n")
cat("========================================\n")
files_ripartizioni <- prepare_territorial_maps(
  territorial_level = "ripartizioni",
  output_dir = "data/examples",
  keep_attributes = c("COD_RIP", "DEN_RIP"),
  verbose = TRUE
)
cat("Files created:\n")
print(unlist(files_ripartizioni))

# 5. USING PRE-DOWNLOADED DATA
cat("\n========================================\n")
cat("5. Testing with PRE-DOWNLOADED DATA\n")
cat("========================================\n")
regioni_data <- get_situas_tables(pfun = 68, verbose = TRUE)
files_custom <- prepare_territorial_maps(
  situas_data = regioni_data,
  territorial_level = "regioni",
  output_dir = "data/examples",
  verbose = TRUE
)
cat("Files created:\n")
print(unlist(files_custom))

# Verify all files exist and show sizes
cat("\n========================================\n")
cat("SUMMARY: Created files\n")
cat("========================================\n")
all_files <- c(
  unlist(files_comuni),
  unlist(files_province),
  unlist(files_regioni),
  unlist(files_ripartizioni)
)

for (f in all_files) {
  if (file.exists(f)) {
    size_mb <- round(file.size(f) / 1024^2, 2)
    cat(sprintf("✓ %s (%.2f MB)\n", basename(f), size_mb))
  } else {
    cat(sprintf("✗ %s (NOT FOUND)\n", basename(f)))
  }
}

cat("\nDone!\n")
