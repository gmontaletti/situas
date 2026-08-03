# situas - R Client for SITUAS API

[![Version](https://img.shields.io/badge/version-0.6.0-blue.svg)](https://github.com/gmontaletti/situas)

R package for accessing the SITUAS (Sistema Informativo Territoriale delle Unità Amministrative e Statistiche) API - a set of APIs built by ISTAT (Italian National Institute for Statistics) to retrieve territorial codes and classifications.

Website: https://situas.istat.it/

## Overview

The scope of this R package is:

- provide a set of functions that retrieve the appropriate tables;
- make possible a reclassification process: when an old code is present in a dataset, recode it to the new one
- provide batch detection of territorial codes in datasets and batch cleaning / recoding.

The package is structured as a standard R library with CRAN standards:
- use "testthat" library  to develop unit tests
- use "devtools" for testing and developemnt
- use "roxygen" for documentation
- use "renv" for installation of the environment and dependencies
- use "data.table" for data management and wrangling
- use vectorized code for speed and avoid for loops
- use caching for off line use

## Installation

```r
# Install from GitHub
pak::pak("gmontaletti/situas")

# Force a reinstall when pak reports the package as already up to date
pak::pak("gmontaletti/situas?reinstall")

# Restore package dependencies
renv::restore()
```

`devtools::install_github()` also works but is deprecated since devtools 2.5.0,
which recommends `pak` instead.

## Quick Start

### 1. Get SITUAS Data

```r
library(situas)

# Get current municipalities
comuni <- get_situas_tables(pfun = 61, date = Sys.Date())

# Get provinces
province <- get_situas_tables(pfun = 64, date = Sys.Date())

# Get regions
regioni <- get_situas_tables(pfun = 68, date = Sys.Date())
```

### 2. Download ISTAT Boundary Shapefiles

The package provides functions to automatically download ISTAT administrative boundary shapefiles from the [OnData repository](https://www.confini-amministrativi.it) (with fallback to the [official ISTAT source](https://www.istat.it/it/archivio/222527)).

```r
# Download all territorial levels for the current release
download_istat_boundaries()

# Download specific territorial levels
download_istat_boundaries(
  date = "2026-01-01",
  territorial_levels = c("comuni", "province")
)

# List the releases actually published (27 releases, from 1991 onwards)
list_istat_boundary_versions(since_year = 2020)

# Restrict the download to a single source
download_istat_boundaries(date = "2026-01-01", source = "istat")

# Check for updates
check_boundary_updates()

# View cached boundaries
get_cached_boundaries_info()

# Clean old versions
clean_boundary_cache(keep_latest_n = 2)
```

**Data Sources:**
- Primary: [OnData repository](https://www.confini-amministrativi.it) - community-maintained, multiple formats, releases from 1991 onwards
- Fallback: [ISTAT official](https://www.istat.it/it/archivio/222527) - January 1st releases from 2022 onwards, one bundle covering all territorial levels

Attribute names differ between the two distributions (OnData ships lowercase
names, ISTAT uppercase ones). They are normalised to the ISTAT convention on
read, so `prepare_territorial_maps()` behaves identically regardless of source.

**Cache Location:**
Downloaded boundaries are cached in `tools::R_user_dir("situas", which = "data")` and persist across R sessions.

### 3. Create Maps

```r
# Create map files for Power BI and Leaflet
prepare_territorial_maps(
  territorial_level = "comuni",
  output_dir = "data"
)

# Create interactive Leaflet map
library(leaflet)
map_data <- readRDS("data/situas_map_comuni_61_20250101.rds")
map_territorial_units(map_data)
```

## Features

### Data Retrieval
- `get_situas_tables()` - Download territorial data from SITUAS API
- `list_available_reports()` - Browse available SITUAS reports
- `search_reports()` - Search for specific reports

### Boundary Shapefiles
- `download_istat_boundaries()` - Automatically download ISTAT boundary shapefiles
- `list_istat_boundary_versions()` - List available boundary versions
- `check_boundary_updates()` - Check for newer boundary versions
- `get_cached_boundaries_info()` - View cached boundaries
- `clean_boundary_cache()` - Manage cached files

### Mapping
- `prepare_territorial_maps()` - Create map-ready files (RDS, GeoJSON, TopoJSON)
- `map_territorial_units()` - Create interactive Leaflet maps
- `map_territorial_choropleth()` - Create choropleth maps with custom data
- `sf_to_powerbi_topojson()` - Export maps for Microsoft Power BI

### Utilities
- Automatic caching for offline use
- Cache management functions
- Data validation and error handling

## Citation

If you use this package in your research, please cite it as:

```
Montaletti, G. (2026). situas: Client for the SITUAS API - Italian Territorial Codes and Classifications.
R package version 0.6.0. https://github.com/gmontaletti/situas
```

## Author

Giampaolo Montaletti
Email: giampaolo.montaletti@gmail.com
GitHub: https://github.com/gmontaletti
ORCID: https://orcid.org/0009-0002-5327-1122

