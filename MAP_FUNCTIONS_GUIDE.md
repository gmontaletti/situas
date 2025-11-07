# Geographic Mapping Functions - User Guide

## Overview

The situas package now includes comprehensive geographic mapping functions that combine SITUAS Report 61 (municipalities data) with ISTAT shapefiles to create map-ready files for both R dashboards and Microsoft Power BI.

## Quick Start

```r
library(situas)

# 0. Extract shapefiles once (recommended for better performance)
source("data-raw/extract_shapefiles.R")

# 1. Prepare map files for municipalities
files <- prepare_comuni_maps(pfun = 61)

# 2. Load the data
comuni <- readRDS(files$rds)

# 3. Create an interactive map
map <- map_comuni_leaflet(comuni)
map  # Display in RStudio viewer or browser
```

### Working with Different Territorial Levels

```r
# Provinces
province_files <- prepare_comuni_maps(
  pfun = 64,
  keep_attributes = c("COD_UTS", "DEN_UTS", "COD_REG", "DEN_REG")
)

# Regions
region_files <- prepare_comuni_maps(
  pfun = 68,
  keep_attributes = c("COD_REG", "DEN_REG")
)
```

## Setup (One-Time) ⚡

Before first use, extract the shapefile from the zip archive for better performance:

```r
source("data-raw/extract_shapefiles.R")
```

**What this does:**
- Extracts shapefiles from `Limiti01012025_g.zip` to `data-raw/Com01012025_g/`
- Creates directories for municipalities, provinces, regions, and geographic partitions
- Shapefiles are then ready for immediate use (no re-extraction needed)



**Performance comparison:**
- ✅ **With extraction**: ~5-10 seconds per run (reads directly from disk)
- ⚠️ **Without extraction**: ~15-20 seconds per run (extracts to temp each time)

**Note:** If you skip this step, the function will still work by extracting to a temporary directory each time, but you'll see a tip message suggesting the one-time extraction.

## Functions

### `prepare_comuni_maps()` - Main Data Preparation

Creates map-ready files from SITUAS data + shapefiles for different territorial levels.

**Output Files:**
- **RDS** (`situas_map_61_YYYYMMDD.rds`) - Fast R loading, 0.4-6 MB
- **GeoJSON** (`situas_map_61_YYYYMMDD.geojson`) - Web standard, 4-35 MB
- **TopoJSON** (`situas_map_61_YYYYMMDD_powerbi.json`) - Power BI format, 2-12 MB

**Key Parameters:**
```r
prepare_comuni_maps(
  pfun = 61,                     # SITUAS report ID (see below)
  output_dir = "data",           # Where to save files
  date = Sys.Date(),             # Reference date for SITUAS data
  simplify = TRUE,               # Reduce file size (recommended)
  tolerance = 0.001,             # Simplification level (higher = smaller)
  keep_attributes = c(           # Which fields to keep (adjust per report)
    "PRO_COM", "PRO_COM_T", "COMUNE",
    "COD_REG", "DEN_REG", "COD_UTS",
    "DEN_UTS", "SIGLA_AUTOMOBILISTICA"
  ),
  verbose = TRUE                 # Show progress messages
)
```

**Common Report IDs:**
| pfun | Description | Records | Best For |
|------|-------------|---------|----------|
| **61** | Current municipalities | ~7,900 | Most common use case |
| **64** | Provinces/UTS | ~107 | Provincial analysis |
| **68** | Regions | 20 | Regional analysis |
| **73** | Municipality characteristics | ~7,900 | Extended data |

Use `search_reports()` to find other available reports.

**Performance:**
- Original shapefile: ~12 MB
- After simplification: ~1-2 MB (60-80% reduction)
- Processing time: ~10-30 seconds

**Join Results:**
- Successfully matched: **7,519 municipalities** (95.2%)
- Unmatched: 377 (likely historical municipalities)

---

### `map_comuni_leaflet()` - Interactive R Maps

Creates interactive Leaflet maps for R dashboards and Shiny apps.

**Basic Usage:**
```r
# Color by region
map_comuni_leaflet(comuni, color_by = "COD_REG")

# Color by province
map_comuni_leaflet(comuni, color_by = "COD_UTS")

# Uniform color
map_comuni_leaflet(comuni, color_by = NULL)

# Custom tile provider
map_comuni_leaflet(comuni, tile_provider = "CartoDB.Positron")
```

**Parameters:**
- `color_by`: Attribute for coloring (e.g., "COD_REG", "COD_UTS", or NULL)
- `palette`: Color palette name from RColorBrewer (default: "Set3")
- `popup_fields`: Fields to show in popup (default: COMUNE, DEN_REG, DEN_UTS, PRO_COM)
- `tile_provider`: Base map tiles (default: "OpenStreetMap")
- `add_legend`: Show legend (default: TRUE)

---

### `map_comuni_choropleth()` - Data Visualization

Creates choropleth maps by joining external data.

**Example:**
```r
# Your data with municipality-level statistics
my_data <- data.frame(
  PRO_COM = c("1001", "1002", "1003"),
  population = c(50000, 30000, 20000)
)

# Create choropleth
map_comuni_choropleth(
  comuni,
  data = my_data,
  value_col = "population",
  legend_title = "Population",
  palette = "YlOrRd"
)
```

---

## Using in Shiny Dashboards

```r
library(shiny)
library(leaflet)
library(situas)

ui <- fluidPage(
  titlePanel("Italian Municipalities"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Select Region:",
                  choices = c("All", unique(comuni$DEN_REG)))
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

server <- function(input, output) {
  # Load data once
  comuni <- readRDS("data/comuni_map_20251014.rds")

  # Filter based on selection
  filtered_data <- reactive({
    if (input$region == "All") {
      comuni
    } else {
      comuni[comuni$DEN_REG == input$region, ]
    }
  })

  # Render map
  output$map <- renderLeaflet({
    map_comuni_leaflet(filtered_data())
  })
}

shinyApp(ui, server)
```

---

## Using in Power BI

### Step-by-Step Instructions

1. **Prepare the TopoJSON file:**
   ```r
   files <- prepare_comuni_maps(output_dir = "powerbi")
   # Use the file ending in _powerbi.json
   ```

2. **In Power BI Desktop:**
   - Insert → Shape Map visual
   - Click on the visual
   - Format visual → Shape → Map settings
   - Click "+ Add map"
   - Select your `.json` file (TopoJSON)

3. **Map your data:**
   - Add your data table to Power BI
   - Ensure it has a PRO_COM or COMUNE field
   - Drag PRO_COM to the "Location" field in Shape Map
   - Drag your metric to the "Color saturation" field

4. **Tips:**
   - Use PRO_COM for matching (more reliable than names)
   - If using COMUNE names, ensure exact spelling
   - The map includes all 7,519 current Italian municipalities

---

## Data Dictionary

| Field | Description | Example |
|-------|-------------|---------|
| **PRO_COM** | Municipality code (numeric string) | "1001" |
| **PRO_COM_T** | Municipality code (with leading zeros) | "001001" |
| **COMUNE** | Municipality name | "Torino" |
| **COD_REG** | Region code | "1" |
| **DEN_REG** | Region name | "Piemonte" |
| **COD_UTS** | Province/UTS code | "001" |
| **DEN_UTS** | Province/UTS name | "Torino" |
| **SIGLA_AUTOMOBILISTICA** | Car registration code | "TO" |

---

## Advanced Examples

### Example 1: Working with Different Territorial Levels

```r
# Create maps for all three levels
munic_files <- prepare_comuni_maps(pfun = 61)  # Municipalities
prov_files <- prepare_comuni_maps(pfun = 64)   # Provinces
reg_files <- prepare_comuni_maps(pfun = 68)    # Regions

# Load all three
municipalities <- readRDS(munic_files$rds)
provinces <- readRDS(prov_files$rds)
regions <- readRDS(reg_files$rds)

# Create layered map
library(leaflet)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = regions, color = "red", weight = 3, fill = FALSE) %>%
  addPolygons(data = provinces, color = "blue", weight = 1, fill = FALSE)
```

### Example 2: Multiple Date Comparison

```r
# Create maps for different dates
files_2020 <- prepare_comuni_maps(
  pfun = 61,
  date = "2020-01-01",
  output_dir = "maps2020"
)
files_2025 <- prepare_comuni_maps(
  pfun = 61,
  date = "2025-01-01",
  output_dir = "maps2025"
)

# Load both
comuni_2020 <- readRDS(files_2020$rds)
comuni_2025 <- readRDS(files_2025$rds)

# Compare counts
cat("2020:", nrow(comuni_2020), "municipalities\n")
cat("2025:", nrow(comuni_2025), "municipalities\n")
```

### Example 2: Region-Specific Maps

```r
# Filter to a specific region
tuscany <- comuni[comuni$DEN_REG == "Toscana", ]

# Create focused map
map_comuni_leaflet(tuscany, color_by = "COD_UTS")
```

### Example 3: Custom Styling

```r
# Dark theme map
map_comuni_leaflet(
  comuni,
  color_by = "COD_REG",
  palette = "Dark2",
  tile_provider = "CartoDB.DarkMatter"
)
```

### Example 4: Save Map as HTML

```r
library(htmlwidgets)

map <- map_comuni_leaflet(comuni)
saveWidget(map, "italian_municipalities.html")
```

---

## Troubleshooting

### Map Not Showing in Power BI

- **Issue**: Shape Map visual is empty
- **Solution**:
  - Ensure you're using the `_powerbi.json` file (TopoJSON format)
  - Check that your data's PRO_COM matches the format in the map
  - Try mapping with COMUNE name instead

### File Size Too Large

- **Issue**: Files are too big for your use case
- **Solution**:
  ```r
  # Increase simplification (smaller files, less detail)
  prepare_comuni_maps(
    simplify = TRUE,
    tolerance = 0.01  # Higher = more simplification
  )
  ```

### Some Municipalities Missing

- **Issue**: 377 municipalities unmatched
- **Cause**: Historical municipalities in shapefile but not in current SITUAS data
- **Solution**: These are expected. If you need historical data, use a different date:
  ```r
  prepare_comuni_maps(date = "2020-01-01")
  ```

### RColorBrewer Warning

- **Issue**: Warning about palette size
- **Cause**: Italy has 20 regions but some palettes have fewer colors
- **Solution**: Use a different palette:
  ```r
  map_comuni_leaflet(comuni, palette = "Paired")  # Has 12 colors
  ```

---

## Performance Tips

1. **Use RDS format in R** - Fastest loading
2. **Enable simplification** - Reduces file size by 60-80%
3. **Keep only needed attributes** - Smaller files load faster
4. **Cache prepared files** - Don't regenerate every time

---

## Files Created

When you run `prepare_comuni_maps()`, you get:

```
data/
├── comuni_map_20251014.rds          # R format (0.45 MB)
├── comuni_map_20251014.geojson      # Web format (4.57 MB)
└── comuni_map_20251014_powerbi.json # Power BI format (3.02 MB)
```

---

## Technical Details

### Coordinate Reference System
- **Input**: WGS 84 / UTM zone 32N (EPSG:32632)
- **Output**: WGS 84 (EPSG:4326) - Standard for web mapping

### Simplification Algorithm
- Uses `rmapshaper::ms_simplify()`
- Topology-aware (borders remain connected)
- Default tolerance: 0.001 degrees (~100 meters)

### Join Method
- Joins on PRO_COM (municipality code)
- Left join (keeps all shapefile features)
- Resolves duplicate columns automatically

---

## Need Help?

- Package documentation: `?prepare_comuni_maps`
- SITUAS API docs: https://situas.istat.it
- Leaflet docs: https://rstudio.github.io/leaflet/
- Power BI Shape Map: https://learn.microsoft.com/power-bi/visuals/desktop-shape-map

---

**Package Version**: 0.1.0
**Last Updated**: 2025-10-14
**Author**: Giampaolo Montaletti
