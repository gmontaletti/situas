# situas 0.6.0

## Bug fixes

* `download_istat_boundaries()` failed with `Client error: (404) Not Found` for every territorial level and every date. The OnData API v2 addresses bulk downloads by division name (`comuni`, `unita-territoriali-sovracomunali`, `regioni`, `ripartizioni-geografiche`), not by the ISTAT short code with a `_gen` suffix. Download URLs are now built correctly.
* Shapefile extraction handles both distribution layouts: the OnData per-division archive and the nested ISTAT bundle.
* Boundary attribute names are normalised to the ISTAT convention on read. The OnData distribution ships lowercase names (`pro_com`, `cod_reg`), which prevented `prepare_territorial_maps()` from joining SITUAS data.
* The download progress bar is no longer printed when `verbose = FALSE`.

## New features

* `download_istat_boundaries()` gains a `source` argument (`"auto"`, `"ondata"`, `"istat"`). The default tries OnData and falls back to the ISTAT archive; the ISTAT bundle covers all territorial levels and is downloaded only once per call.
* The ISTAT fallback is implemented and serves January 1st releases from 2022 onwards. It previously returned `NULL` with a warning.
* When a download fails because no release exists for the requested date, the error lists the releases actually available.

## Enhancements

* `list_istat_boundary_versions()` reads the published OnData index instead of assuming an annual release on January 1st since 2020. The series covers 27 releases from 1991 onwards, including dates that do not fall on January 1st. If the index is unreachable, the function warns and falls back to the previous behaviour.
* `build_ondata_url()` supports the alternative distribution formats offered by the API (`geo.json`, `topo.json`, `gpkg`, `geo.parquet`, `csv`).

# situas 0.5.0

## Package structure

* Package brought to CRAN-compliant structure: development artifacts moved out of the package root, `MAP_FUNCTIONS_GUIDE.md` and `DOWNLOAD_CLASSIFICATIONS_README.md` converted into vignettes (`map-functions-guide`, `download-classifications`).
* pkgdown site published to GitHub Pages.
* All `R CMD check` WARNINGs and NOTEs resolved (missing imports for `magrittr`, `utils::object.size`, `data.table` globals, Rd formatting issues).

# situas 0.4.0

## New features

* `download_istat_boundaries()`: download ISTAT administrative boundary shapefiles (OnData/ISTAT source) with automatic caching.
* `list_istat_boundary_versions()`: list available boundary shapefile versions.
* `check_boundary_updates()`: check whether newer boundary versions are available.
* `get_cached_boundaries_info()`: inspect cached boundary files.
* `clean_boundary_cache()`: remove cached boundary files.
* `read_territorial_shapefile()` now checks the local cache before downloading.

# situas 0.3.0

## New features

* Classification standard downloads with version management.
* Report search and discovery functionality.
* Interactive territorial mapping for comuni, province, regioni, and ripartizioni, including choropleth visualization support.
* Metadata management and update capabilities.

## Enhancements

* API client validates report types (`DATA`, `PERIODO`, `ATTUALIZZAZIONE`).
* Intelligent date parameter handling for `PERIODO`/`ATTUALIZZAZIONE` reports.
* Stricter parameter validation (non-NA checks) and geometry type validation (POLYGON/MULTIPOLYGON) for mapping functions.

## Bug fixes

* Mapping functions correctly reject invalid geometry types instead of failing silently.

# situas 0.2.0

## Enhancements

* Enhanced API client and table retrieval functionality.
* Expanded test coverage.

# situas 0.1.1

## New features

* `sf_to_powerbi_topojson()`: convert `sf` data frames to Power BI-compatible TopoJSON, with automatic WGS84 transformation, geometry validation, and optional topology-preserving simplification.

# situas 0.1.0

* Initial release.
* Core API client with authentication and request handling.
* Cache management for API responses.
* `get_situas_tables()` and related functions for retrieving SITUAS territorial codes and classifications.
