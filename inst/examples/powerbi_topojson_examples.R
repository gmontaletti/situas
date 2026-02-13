# ============================================================================
# Examples: Converting SF DataFrames to Power BI TopoJSON
# ============================================================================
#
# This script demonstrates how to use sf_to_powerbi_topojson() to convert
# sf spatial dataframes to Microsoft Power BI-compatible TopoJSON format.
#
# Author: Giampaolo Montaletti
# Package: situas
#
# ============================================================================

library(situas)
library(sf)

# 1. Basic Example: Italian Regions -----

# Prepare Italian regions data with SITUAS
files <- prepare_territorial_maps(
  territorial_level = "regioni",
  keep_attributes = c("COD_REG", "DEN_REG")
)

# Load the prepared sf object
regions_sf <- readRDS(files$rds)

# Convert to Power BI TopoJSON (basic usage)
sf_to_powerbi_topojson(
  sf_data = regions_sf,
  file = "examples/italy_regions_powerbi.json",
  id_col = "COD_REG",
  name_col = "DEN_REG"
)

# The output file can now be imported into Power BI:
# 1. Open Power BI Desktop
# 2. Insert > Shape Map visual
# 3. Format visual > Shape > Map settings
# 4. Click "+ Add map" and select italy_regions_powerbi.json
# 5. Map your data using COD_REG as the join field


# 2. Municipalities with Column Filtering -----

# Prepare Italian municipalities (comuni)
comuni_files <- prepare_territorial_maps(
  territorial_level = "comuni"
)

# Load the prepared sf object
comuni_sf <- readRDS(comuni_files$rds)

# Convert to Power BI TopoJSON, keeping only essential columns
sf_to_powerbi_topojson(
  sf_data = comuni_sf,
  file = "examples/italy_comuni_powerbi.json",
  id_col = "PRO_COM",
  name_col = "COMUNE",
  keep_cols = c("PRO_COM", "COMUNE", "COD_REG", "DEN_REG"),
  verbose = TRUE
)


# 3. Simplified Geometries for Better Performance -----

# For large datasets, simplification reduces file size
# while maintaining topology

# Load provinces
province_files <- prepare_territorial_maps(
  territorial_level = "province"
)
province_sf <- readRDS(province_files$rds)

# Create simplified version for Power BI
sf_to_powerbi_topojson(
  sf_data = province_sf,
  file = "examples/italy_province_simplified.json",
  id_col = "COD_UTS",
  name_col = "DEN_UTS",
  simplify = TRUE,           # Enable simplification
  tolerance = 0.01,          # Higher = more simplification (range: 0-1)
  verbose = TRUE
)


# 4. Custom SF Object from External Source -----

# You can use sf_to_powerbi_topojson() with any sf object,
# not just SITUAS data

# Example: Create a custom sf object
custom_polygons <- st_sf(
  id = 1:3,
  name = c("Area A", "Area B", "Area C"),
  value = c(100, 200, 150),
  geometry = st_sfc(
    st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
    st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0)))),
    st_polygon(list(rbind(c(0,1), c(1,1), c(1,2), c(0,2), c(0,1))))
  ),
  crs = 4326
)

# Convert to Power BI TopoJSON
sf_to_powerbi_topojson(
  sf_data = custom_polygons,
  file = "examples/custom_areas.json",
  id_col = "id",
  name_col = "name",
  object_name = "custom_areas"  # Custom layer name in TopoJSON
)


# 5. Handling Different CRS -----

# The function automatically transforms to WGS84 (EPSG:4326)
# which is required by Power BI

# Example: Load data in UTM projection
utm_data <- st_transform(regions_sf, crs = 32633)  # UTM Zone 33N

# Function will automatically transform to WGS84
sf_to_powerbi_topojson(
  sf_data = utm_data,
  file = "examples/italy_regions_from_utm.json",
  id_col = "COD_REG",
  name_col = "DEN_REG",
  verbose = TRUE  # Will show "Transforming CRS..." message
)


# 6. Minimal Output (Geometries Only) -----

# Sometimes you only need geometries without attributes
# Keep only ID for joining

sf_to_powerbi_topojson(
  sf_data = regions_sf,
  file = "examples/italy_regions_minimal.json",
  keep_cols = c("COD_REG"),  # Only keep the ID column
  verbose = FALSE             # Silent mode
)


# 7. Complete Workflow: SITUAS to Power BI -----

# Complete example: Download specific report, prepare map, export for Power BI

# Step 1: Get territorial data for a specific date
region_data <- get_situas_tables(
  pfun = 68,              # Report ID for regions
  date = "2025-01-01"
)

# Step 2: Prepare map with SITUAS data
map_files <- prepare_territorial_maps(
  situas_data = region_data,
  territorial_level = "regioni",
  simplify = TRUE,
  tolerance = 0.001
)

# Step 3: Load and convert to Power BI format
final_sf <- readRDS(map_files$rds)

sf_to_powerbi_topojson(
  sf_data = final_sf,
  file = "examples/italy_regions_2025_powerbi.json",
  id_col = "COD_REG",
  name_col = "DEN_REG",
  keep_cols = c("COD_REG", "DEN_REG"),
  simplify = FALSE,  # Already simplified in prepare_territorial_maps
  verbose = TRUE
)


# 8. Batch Processing Multiple Territorial Levels -----

# Create Power BI maps for all territorial levels

territorial_levels <- c("comuni", "province", "regioni", "ripartizioni")

for (level in territorial_levels) {

  cat("\n=== Processing", level, "===\n")

  # Prepare maps
  files <- prepare_territorial_maps(
    territorial_level = level,
    simplify = TRUE,
    verbose = FALSE
  )

  # Load sf object
  sf_obj <- readRDS(files$rds)

  # Determine columns based on level
  columns <- switch(
    level,
    "comuni" = c("PRO_COM", "COMUNE", "COD_REG", "DEN_REG"),
    "province" = c("COD_UTS", "DEN_UTS", "COD_REG", "DEN_REG"),
    "regioni" = c("COD_REG", "DEN_REG"),
    "ripartizioni" = c("COD_RIP", "DEN_RIP")
  )

  id_field <- columns[1]
  name_field <- columns[2]

  # Convert to Power BI TopoJSON
  output_file <- paste0("examples/italy_", level, "_powerbi.json")

  sf_to_powerbi_topojson(
    sf_data = sf_obj,
    file = output_file,
    id_col = id_field,
    name_col = name_field,
    keep_cols = columns,
    verbose = FALSE
  )

  cat("Created:", output_file, "\n")
}


# 9. Comparing File Sizes: Original vs Simplified -----

# Compare file sizes with and without simplification

# Without simplification
sf_to_powerbi_topojson(
  sf_data = comuni_sf,
  file = "examples/comuni_full.json",
  simplify = FALSE,
  verbose = FALSE
)

# With simplification
sf_to_powerbi_topojson(
  sf_data = comuni_sf,
  file = "examples/comuni_simplified.json",
  simplify = TRUE,
  tolerance = 0.05,  # Moderate simplification
  verbose = FALSE
)

# Check file sizes
size_full <- file.size("examples/comuni_full.json")
size_simplified <- file.size("examples/comuni_simplified.json")

cat("\nFile Size Comparison:\n")
cat("  Full detail:  ", round(size_full / 1024^2, 2), "MB\n")
cat("  Simplified:   ", round(size_simplified / 1024^2, 2), "MB\n")
cat("  Reduction:    ", round((1 - size_simplified/size_full) * 100, 1), "%\n")


# 10. Error Handling Examples -----

# The function provides clear error messages for invalid inputs

# Example: Non-polygon geometries (will error)
tryCatch({
  points <- st_sf(
    id = 1:3,
    geometry = st_sfc(st_point(c(0,0)), st_point(c(1,1)), st_point(c(2,2))),
    crs = 4326
  )

  sf_to_powerbi_topojson(
    sf_data = points,
    file = "examples/test.json"
  )
}, error = function(e) {
  cat("Expected error:", e$message, "\n")
})

# Example: Invalid file extension (will error)
tryCatch({
  sf_to_powerbi_topojson(
    sf_data = regions_sf,
    file = "examples/test.geojson"  # Must end with .json
  )
}, error = function(e) {
  cat("Expected error:", e$message, "\n")
})


# ============================================================================
# Additional Resources
# ============================================================================

# For more information:
#
# - Function documentation: ?sf_to_powerbi_topojson
# - Package documentation: ?situas
# - SITUAS API: https://situas.istat.it/
# - Power BI Shape Maps: https://learn.microsoft.com/en-us/power-bi/visuals/desktop-shape-map
# - TopoJSON specification: https://github.com/topojson/topojson-specification
#
# ============================================================================

cat("\n=== All examples completed! ===\n")
cat("Check the 'examples/' directory for generated files.\n")
