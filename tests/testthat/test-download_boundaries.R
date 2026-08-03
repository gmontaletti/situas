# test-download_boundaries.R
# Tests for ISTAT boundary shapefile download functions

# 1. Helper Functions Tests -----

test_that("get_boundaries_cache_dir creates directory", {
  # Use withr to create temp directory for testing
  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  cache_dir <- get_boundaries_cache_dir()

  expect_true(dir.exists(cache_dir))
  expect_match(cache_dir, "boundaries$")
})

test_that("get_territorial_code returns correct codes", {
  expect_equal(get_territorial_code("comuni"), "Com")
  expect_equal(get_territorial_code("province"), "ProvCM")
  expect_equal(get_territorial_code("regioni"), "Reg")
  expect_equal(get_territorial_code("ripartizioni"), "RipGeo")

  expect_error(get_territorial_code("invalid"), "Invalid territorial level")
})

test_that("get_ondata_division returns the API v2 division slugs", {
  expect_equal(get_ondata_division("comuni"), "comuni")
  expect_equal(
    get_ondata_division("province"),
    "unita-territoriali-sovracomunali"
  )
  expect_equal(get_ondata_division("regioni"), "regioni")
  expect_equal(get_ondata_division("ripartizioni"), "ripartizioni-geografiche")

  expect_error(get_ondata_division("invalid"), "Invalid territorial level")
})

test_that("format_boundary_date formats correctly", {
  expect_equal(format_boundary_date("2025-01-01"), "20250101")
  expect_equal(format_boundary_date(as.Date("2024-06-15")), "20240615")
})

test_that("build_ondata_url constructs API v2 division URLs", {
  url <- build_ondata_url("2025-01-01", "comuni")
  expect_equal(
    url,
    "https://www.confini-amministrativi.it/api/v2/it/20250101/comuni.zip"
  )

  # Province are addressed by division slug, not by the ISTAT short code
  url <- build_ondata_url("2024-01-01", "province")
  expect_equal(
    url,
    paste0(
      "https://www.confini-amministrativi.it/api/v2/it/20240101/",
      "unita-territoriali-sovracomunali.zip"
    )
  )

  # The pre-v2 "_gen" naming must not reappear
  for (level in c("comuni", "province", "regioni", "ripartizioni")) {
    expect_false(grepl("_gen\\.zip$", build_ondata_url("2026-01-01", level)))
  }
})

test_that("build_ondata_url supports alternative formats", {
  expect_match(
    build_ondata_url("2026-01-01", "comuni", format = "geo.json"),
    "comuni\\.geo\\.json$"
  )
  expect_match(
    build_ondata_url("2026-01-01", "comuni", format = "gpkg"),
    "comuni\\.gpkg$"
  )
  expect_error(build_ondata_url("2026-01-01", "comuni", format = "shp"))
})

test_that("build_istat_url builds archive URLs and rejects unserved dates", {
  expect_equal(
    build_istat_url("2026-01-01"),
    paste0(
      "https://www.istat.it/storage/cartografia/confini_amministrativi/",
      "generalizzati/2026/Limiti01012026_g.zip"
    )
  )

  expect_match(
    build_istat_url("2025-01-01", generalized = FALSE),
    "non_generalizzati/2025/Limiti01012025\\.zip$"
  )

  # ISTAT storage only serves January 1st releases from 2022 onwards
  expect_null(build_istat_url("2021-01-01"))
  expect_null(build_istat_url("2026-06-15"))
})

test_that("normalize_boundary_fields maps OnData names to ISTAT names", {
  ondata <- data.frame(
    pro_com = 1L,
    cod_reg = 1L,
    comune = "Agliè",
    shape_area = 1.0,
    ontopia = "x",
    stringsAsFactors = FALSE
  )

  normalized <- normalize_boundary_fields(ondata)

  expect_equal(
    names(normalized),
    c("PRO_COM", "COD_REG", "COMUNE", "Shape_Area", "ONTOPIA")
  )
})

test_that("normalize_boundary_fields leaves ISTAT names unchanged", {
  istat <- data.frame(
    PRO_COM = 1L,
    COD_REG = 1L,
    COMUNE = "Agliè",
    Shape_Area = 1.0,
    stringsAsFactors = FALSE
  )

  expect_equal(names(normalize_boundary_fields(istat)), names(istat))

  # Idempotent: normalising twice changes nothing
  once <- normalize_boundary_fields(istat)
  expect_equal(names(normalize_boundary_fields(once)), names(once))
})

test_that("normalize_boundary_fields preserves the geometry column", {
  skip_if_not_installed("sf")

  obj <- sf::st_sf(
    pro_com = 1L,
    geometry = sf::st_sfc(sf::st_point(c(9, 45)), crs = 4326)
  )

  normalized <- normalize_boundary_fields(obj)

  expect_true(inherits(normalized, "sf"))
  expect_true("PRO_COM" %in% names(normalized))
  expect_true(attr(normalized, "sf_column") %in% names(normalized))
})

test_that("boundary_shp_pattern matches the archive layout of each source", {
  ondata_names <- c("comuni.shp", "comuni.dbf", "comuni.prj")
  expect_length(
    grep(boundary_shp_pattern("comuni", "OnData"), ondata_names),
    1
  )

  istat_names <- c(
    "Com01012026_g/Com01012026_g_WGS84.shp",
    "ProvCM01012026_g/ProvCM01012026_g_WGS84.shp",
    "Reg01012026_g/Reg01012026_g_WGS84.shp"
  )
  expect_equal(
    grep(boundary_shp_pattern("province", "ISTAT"), istat_names, value = TRUE),
    "ProvCM01012026_g/ProvCM01012026_g_WGS84.shp"
  )
  expect_equal(
    grep(boundary_shp_pattern("regioni", "ISTAT"), istat_names, value = TRUE),
    "Reg01012026_g/Reg01012026_g_WGS84.shp"
  )
})

# 2. list_istat_boundary_versions Tests -----

test_that("list_istat_boundary_versions returns data.table", {
  skip_on_cran()

  versions <- list_istat_boundary_versions(since_year = 2023, verbose = FALSE)

  expect_s3_class(versions, "data.table")
  expect_true(all(c("date", "year", "source", "base_url") %in% names(versions)))
  expect_true(all(versions$year >= 2023))
  expect_equal(versions$source, rep("OnData", nrow(versions)))
})

test_that("list_istat_boundary_versions respects since_year", {
  skip_on_cran()

  versions_2023 <- list_istat_boundary_versions(
    since_year = 2023,
    verbose = FALSE
  )
  versions_2024 <- list_istat_boundary_versions(
    since_year = 2024,
    verbose = FALSE
  )

  expect_true(nrow(versions_2024) <= nrow(versions_2023))
  expect_true(all(versions_2024$year >= 2024))
})

test_that("list_istat_boundary_versions uses cache", {
  skip_on_cran()

  # Clear cache
  cache_dir <- get_cache_dir()
  cache_file <- file.path(cache_dir, "boundary_versions_2023.rds")
  if (file.exists(cache_file)) {
    unlink(cache_file)
  }

  # First call (no cache)
  versions1 <- list_istat_boundary_versions(
    since_year = 2023,
    use_cache = TRUE,
    verbose = FALSE
  )

  # Second call (should use cache)
  versions2 <- list_istat_boundary_versions(
    since_year = 2023,
    use_cache = TRUE,
    verbose = FALSE
  )

  expect_equal(versions1, versions2)
  expect_true(file.exists(cache_file))
})

test_that("list_istat_boundary_versions validates inputs", {
  expect_error(list_istat_boundary_versions(since_year = "2023"), "is.numeric")
  expect_error(list_istat_boundary_versions(use_cache = "yes"), "is.logical")
  expect_error(list_istat_boundary_versions(verbose = "yes"), "is.logical")
})

# 3. Metadata Management Tests -----

test_that("boundaries metadata save and load works", {
  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  # Create sample metadata
  metadata <- data.table::data.table(
    date = "20250101",
    territorial_level = "comuni",
    source = "OnData",
    file_path = "/path/to/file.shp",
    download_timestamp = Sys.time(),
    file_size_mb = 12.5
  )

  # Save
  save_boundaries_metadata(metadata)

  # Load
  loaded <- load_boundaries_metadata()

  expect_s3_class(loaded, "data.table")
  expect_equal(nrow(loaded), 1)
  expect_equal(loaded$territorial_level, "comuni")
  expect_equal(loaded$file_size_mb, 12.5)
})

test_that("load_boundaries_metadata returns empty table if no file", {
  # Create a fresh temp directory for this test
  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  # Force reload of metadata from new temp location
  metadata <- situas:::load_boundaries_metadata()

  expect_s3_class(metadata, "data.table")
  expect_equal(nrow(metadata), 0)
  expect_true(all(
    c("date", "territorial_level", "source", "file_path") %in% names(metadata)
  ))
})

# 3b. Extraction Tests -----

# Builds a ZIP archive containing the given (possibly nested) relative paths,
# so extraction can be tested without hitting the network.
make_boundary_zip <- function(paths, dir) {
  for (p in paths) {
    full <- file.path(dir, p)
    dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
    writeLines("fake", full)
  }

  withr::local_dir(dir)
  utils::zip("archive.zip", files = paths, flags = "-q")

  file.path(dir, "archive.zip")
}

test_that("extract_boundary_shapefile handles the OnData archive layout", {
  skip_if(Sys.which(Sys.getenv("R_ZIPCMD", "zip")) == "", "zip binary missing")

  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  src_dir <- withr::local_tempdir()
  zip_file <- make_boundary_zip(
    c("comuni.shp", "comuni.shx", "comuni.dbf", "comuni.prj", "comuni.cpg"),
    src_dir
  )

  result <- extract_boundary_shapefile(
    zip_file,
    date = "2026-01-01",
    territorial_level = "comuni",
    source = "OnData",
    verbose = FALSE
  )

  expect_true(result$success)
  expect_true(file.exists(result$shapefile))
  expect_equal(basename(result$shapefile), "comuni.shp")

  # Sidecar files must be extracted alongside the .shp
  expect_true(file.exists(sub("\\.shp$", ".dbf", result$shapefile)))
  expect_true(file.exists(sub("\\.shp$", ".prj", result$shapefile)))
})

test_that("extract_boundary_shapefile handles the nested ISTAT layout", {
  skip_if(Sys.which(Sys.getenv("R_ZIPCMD", "zip")) == "", "zip binary missing")

  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  src_dir <- withr::local_tempdir()
  zip_file <- make_boundary_zip(
    c(
      "Com01012026_g/Com01012026_g_WGS84.shp",
      "Com01012026_g/Com01012026_g_WGS84.dbf",
      "ProvCM01012026_g/ProvCM01012026_g_WGS84.shp",
      "ProvCM01012026_g/ProvCM01012026_g_WGS84.dbf"
    ),
    src_dir
  )

  # The single ISTAT bundle must yield the right level for each request
  comuni <- extract_boundary_shapefile(
    zip_file,
    date = "2026-01-01",
    territorial_level = "comuni",
    source = "ISTAT",
    verbose = FALSE
  )
  expect_true(comuni$success)
  expect_equal(basename(comuni$shapefile), "Com01012026_g_WGS84.shp")

  province <- extract_boundary_shapefile(
    zip_file,
    date = "2026-01-01",
    territorial_level = "province",
    source = "ISTAT",
    verbose = FALSE
  )
  expect_true(province$success)
  expect_equal(basename(province$shapefile), "ProvCM01012026_g_WGS84.shp")
})

test_that("extract_boundary_shapefile reports a missing shapefile", {
  skip_if(Sys.which(Sys.getenv("R_ZIPCMD", "zip")) == "", "zip binary missing")

  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  src_dir <- withr::local_tempdir()
  zip_file <- make_boundary_zip(c("readme.txt"), src_dir)

  result <- extract_boundary_shapefile(
    zip_file,
    date = "2026-01-01",
    territorial_level = "comuni",
    source = "OnData",
    verbose = FALSE
  )

  expect_false(result$success)
  expect_match(result$error, "No shapefile")
})

# 3c. Remote Endpoint Tests -----
#
# These tests assert that the URLs the package builds actually resolve. The
# 404 regression that broke download_istat_boundaries() went unnoticed because
# earlier tests only inspected the URL string.

test_that("OnData download URLs resolve for every territorial level", {
  skip_on_cran()
  skip_if_offline()

  latest <- fetch_ondata_releases(verbose = FALSE)
  skip_if(is.null(latest), "OnData release index unreachable")

  date <- as.Date(latest[1], format = "%Y%m%d")

  for (level in c("comuni", "province", "regioni", "ripartizioni")) {
    url <- build_ondata_url(date, level)
    response <- httr::HEAD(url, httr::timeout(30))

    expect_equal(
      httr::status_code(response),
      200L,
      label = paste0("HEAD ", url)
    )
  }
})

test_that("ISTAT fallback URL resolves for the latest published year", {
  skip_on_cran()
  skip_if_offline()

  url <- build_istat_url("2026-01-01")
  response <- httr::HEAD(url, httr::timeout(30))

  expect_equal(httr::status_code(response), 200L, label = paste0("HEAD ", url))
})

test_that("fetch_ondata_releases returns published release identifiers", {
  skip_on_cran()
  skip_if_offline()

  releases <- fetch_ondata_releases(verbose = FALSE)
  skip_if(is.null(releases), "OnData release index unreachable")

  expect_type(releases, "character")
  expect_true(all(grepl("^[0-9]{8}$", releases)))

  # Descending order, and the series extends well before 2020
  expect_identical(releases, sort(releases, decreasing = TRUE))
  expect_true(any(as.integer(substr(releases, 1, 4)) < 2020))
})

# 4. download_istat_boundaries Tests -----

test_that("download_istat_boundaries validates inputs", {
  skip_on_cran()

  expect_error(
    download_istat_boundaries(territorial_levels = c("comuni", "invalid")),
    "Invalid territorial levels"
  )

  expect_error(
    download_istat_boundaries(force_refresh = "yes"),
    "is.logical"
  )

  expect_error(
    download_istat_boundaries(verbose = "yes"),
    "is.logical"
  )
})

test_that("download_istat_boundaries rejects an unknown source", {
  expect_error(
    download_istat_boundaries(source = "elsewhere"),
    "'arg' should be one of"
  )
})

test_that("download_istat_boundaries downloads from OnData", {
  skip_on_cran()
  skip_if_offline()

  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  releases <- fetch_ondata_releases(verbose = FALSE)
  skip_if(is.null(releases), "OnData release index unreachable")

  result <- download_istat_boundaries(
    date = as.Date(releases[1], format = "%Y%m%d"),
    territorial_levels = "ripartizioni",
    source = "ondata",
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(result$status, "success")
  expect_equal(result$source, "OnData")
  expect_true(file.exists(result$file_path))
})

test_that("download_istat_boundaries falls back to the ISTAT archive", {
  skip_on_cran()
  skip_if_offline()

  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  result <- download_istat_boundaries(
    date = "2026-01-01",
    territorial_levels = c("ripartizioni", "regioni"),
    source = "istat",
    verbose = FALSE
  )

  expect_equal(result$status, c("success", "success"))
  expect_equal(unique(result$source), "ISTAT")
  expect_true(all(file.exists(result$file_path)))

  # The nested ISTAT layout is preserved on extraction
  expect_match(result$file_path[1], "RipGeo01012026_g")
})

test_that("download_istat_boundaries defaults to the current year", {
  skip_on_cran()
  skip_if_offline()

  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  result <- download_istat_boundaries(
    territorial_levels = "ripartizioni",
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_true(all(
    c("territorial_level", "status", "source", "file_path", "error") %in%
      names(result)
  ))
  expect_equal(result$status, "success")
})

test_that("download_istat_boundaries reports available releases on 404", {
  skip_on_cran()
  skip_if_offline()

  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  # A date with no published release: OnData 404s and ISTAT cannot serve it
  expect_warning(
    result <- download_istat_boundaries(
      date = "2026-06-15",
      territorial_levels = "ripartizioni",
      verbose = TRUE
    ),
    "Failed to download"
  )

  expect_equal(result$status, "failed")
  expect_match(result$error, "No boundary release published for 20260615")
  expect_match(result$error, "list_istat_boundary_versions")
})

test_that("download_istat_boundaries uses cache correctly", {
  skip_on_cran()
  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  # Create fake cached file
  cache_dir <- get_boundaries_cache_dir()
  date_dir <- file.path(cache_dir, "20250101")
  dir.create(date_dir, recursive = TRUE)

  fake_shp <- file.path(date_dir, "RipGeo_gen_WGS84.shp")
  writeLines("fake shapefile", fake_shp)

  # Create metadata
  metadata <- data.table::data.table(
    date = "20250101",
    territorial_level = "ripartizioni",
    source = "OnData",
    file_path = fake_shp,
    download_timestamp = Sys.time(),
    file_size_mb = 0.001
  )
  save_boundaries_metadata(metadata)

  # Download should use cache
  result <- download_istat_boundaries(
    date = "2025-01-01",
    territorial_levels = "ripartizioni",
    force_refresh = FALSE,
    verbose = FALSE
  )

  expect_equal(result$status, "cached")
  expect_equal(result$file_path, fake_shp)
})

test_that("download_istat_boundaries force_refresh ignores cache", {
  skip_on_cran()
  skip_if_offline()

  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  # Create fake cached file
  cache_dir <- get_boundaries_cache_dir()
  date_dir <- file.path(cache_dir, "20250101")
  dir.create(date_dir, recursive = TRUE)

  fake_shp <- file.path(date_dir, "RipGeo_gen_WGS84.shp")
  writeLines("fake shapefile", fake_shp)

  # Create metadata
  metadata <- data.table::data.table(
    date = "20250101",
    territorial_level = "ripartizioni",
    source = "OnData",
    file_path = fake_shp,
    download_timestamp = Sys.time(),
    file_size_mb = 0.001
  )
  save_boundaries_metadata(metadata)

  # force_refresh should re-download
  result <- download_istat_boundaries(
    date = "2025-01-01",
    territorial_levels = "ripartizioni",
    force_refresh = TRUE,
    verbose = FALSE
  )

  # Status should be success (downloaded) or failed, but not cached
  expect_true(result$status %in% c("success", "failed"))
  expect_false(result$status == "cached")
})

# 5. check_boundary_updates Tests -----

test_that("check_boundary_updates returns comparison table", {
  skip_on_cran()

  comparison <- check_boundary_updates(verbose = FALSE)

  expect_s3_class(comparison, "data.table")
  expect_true(all(
    c(
      "territorial_level",
      "current_date",
      "latest_date",
      "update_available"
    ) %in%
      names(comparison)
  ))
  expect_equal(nrow(comparison), 4) # 4 territorial levels
  expect_true(all(
    comparison$territorial_level %in%
      c("comuni", "province", "regioni", "ripartizioni")
  ))
})

test_that("check_boundary_updates detects missing boundaries", {
  skip_on_cran()
  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  # No cached boundaries
  comparison <- check_boundary_updates(verbose = FALSE)

  expect_true(all(is.na(comparison$current_date)))
  expect_true(all(comparison$update_available))
})

test_that("check_boundary_updates detects when up to date", {
  skip_on_cran()
  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  # Get latest date
  versions <- list_istat_boundary_versions(verbose = FALSE)
  latest_date <- versions$date[1]

  # Create metadata with latest date
  metadata <- data.table::data.table(
    date = rep(latest_date, 4),
    territorial_level = c("comuni", "province", "regioni", "ripartizioni"),
    source = "OnData",
    file_path = paste0(
      "/path/to/",
      c("comuni", "province", "regioni", "ripartizioni"),
      ".shp"
    ),
    download_timestamp = Sys.time(),
    file_size_mb = c(12.5, 2.1, 1.0, 0.5)
  )
  save_boundaries_metadata(metadata)

  comparison <- check_boundary_updates(verbose = FALSE)

  expect_false(any(comparison$update_available))
  expect_equal(comparison$current_date, rep(latest_date, 4))
})

# 6. get_cached_boundaries_info Tests -----

test_that("get_cached_boundaries_info returns empty when no cache", {
  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  info <- get_cached_boundaries_info(verbose = FALSE)

  expect_s3_class(info, "data.table")
  expect_equal(nrow(info), 0)
})

test_that("get_cached_boundaries_info lists all cached files", {
  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  # Create sample metadata
  metadata <- data.table::data.table(
    date = c("20250101", "20240101"),
    territorial_level = c("comuni", "comuni"),
    source = c("OnData", "OnData"),
    file_path = c("/path/to/comuni_2025.shp", "/path/to/comuni_2024.shp"),
    download_timestamp = c(Sys.time(), Sys.time() - 86400),
    file_size_mb = c(12.5, 12.3)
  )
  save_boundaries_metadata(metadata)

  info <- get_cached_boundaries_info(verbose = FALSE)

  expect_equal(nrow(info), 2)
  expect_true("exists" %in% names(info))
  expect_true(all(!info$exists)) # Files don't actually exist
})

test_that("get_cached_boundaries_info filters by territorial level", {
  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  # Create sample metadata
  metadata <- data.table::data.table(
    date = c("20250101", "20250101"),
    territorial_level = c("comuni", "province"),
    source = c("OnData", "OnData"),
    file_path = c("/path/to/comuni.shp", "/path/to/province.shp"),
    download_timestamp = Sys.time(),
    file_size_mb = c(12.5, 2.1)
  )
  save_boundaries_metadata(metadata)

  info_all <- get_cached_boundaries_info(
    territorial_level = "all",
    verbose = FALSE
  )
  info_comuni <- get_cached_boundaries_info(
    territorial_level = "comuni",
    verbose = FALSE
  )

  expect_equal(nrow(info_all), 2)
  expect_equal(nrow(info_comuni), 1)
  expect_equal(info_comuni$territorial_level, "comuni")
})

# 7. clean_boundary_cache Tests -----

test_that("clean_boundary_cache dry run doesn't remove files", {
  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  # Create sample metadata
  metadata <- data.table::data.table(
    date = c("20250101", "20240101"),
    territorial_level = c("comuni", "comuni"),
    source = c("OnData", "OnData"),
    file_path = c("/path/to/comuni_2025.shp", "/path/to/comuni_2024.shp"),
    download_timestamp = c(Sys.time(), Sys.time() - 86400),
    file_size_mb = c(12.5, 12.3)
  )
  save_boundaries_metadata(metadata)

  # Dry run
  removed <- clean_boundary_cache(
    keep_latest_n = 1,
    dry_run = TRUE,
    verbose = FALSE
  )

  # Metadata should be unchanged
  metadata_after <- load_boundaries_metadata()
  expect_equal(nrow(metadata_after), 2)

  # Should return files that would be removed
  expect_length(removed, 1)
})

test_that("clean_boundary_cache keeps latest N versions", {
  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  # Create cache directory structure
  cache_dir <- get_boundaries_cache_dir()

  # Create fake files
  dir.create(file.path(cache_dir, "20250101"), recursive = TRUE)
  dir.create(file.path(cache_dir, "20240101"), recursive = TRUE)

  file1 <- file.path(cache_dir, "20250101", "Com_gen_WGS84.shp")
  file2 <- file.path(cache_dir, "20240101", "Com_gen_WGS84.shp")

  writeLines("fake 2025", file1)
  writeLines("fake 2024", file2)

  # Create metadata
  metadata <- data.table::data.table(
    date = c("20250101", "20240101"),
    territorial_level = c("comuni", "comuni"),
    source = c("OnData", "OnData"),
    file_path = c(file1, file2),
    download_timestamp = c(Sys.time(), Sys.time() - 86400),
    file_size_mb = c(0.001, 0.001)
  )
  save_boundaries_metadata(metadata)

  # Clean - keep only latest 1
  removed <- clean_boundary_cache(
    keep_latest_n = 1,
    dry_run = FALSE,
    verbose = FALSE
  )

  # Should remove 1 file
  expect_length(removed, 1)
  expect_match(removed[1], "2024")

  # Metadata should have only 1 entry now
  metadata_after <- load_boundaries_metadata()
  expect_equal(nrow(metadata_after), 1)
  expect_equal(metadata_after$date, "20250101")
})

test_that("clean_boundary_cache filters by territorial level", {
  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  # Create metadata for different levels
  metadata <- data.table::data.table(
    date = c("20250101", "20240101", "20250101"),
    territorial_level = c("comuni", "comuni", "province"),
    source = c("OnData", "OnData", "OnData"),
    file_path = c(
      "/path/to/comuni_2025.shp",
      "/path/to/comuni_2024.shp",
      "/path/to/province_2025.shp"
    ),
    download_timestamp = c(Sys.time(), Sys.time() - 86400, Sys.time()),
    file_size_mb = c(12.5, 12.3, 2.1)
  )
  save_boundaries_metadata(metadata)

  # Clean only comuni
  removed <- clean_boundary_cache(
    keep_latest_n = 1,
    territorial_level = "comuni",
    dry_run = TRUE,
    verbose = FALSE
  )

  # Should only identify comuni files for removal
  expect_length(removed, 1)
  expect_match(removed[1], "comuni_2024")
})

test_that("clean_boundary_cache removes old files by date", {
  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  # Create metadata with old files
  old_time <- Sys.time() - (400 * 24 * 60 * 60) # 400 days ago

  metadata <- data.table::data.table(
    date = c("20250101", "20230101"),
    territorial_level = c("comuni", "comuni"),
    source = c("OnData", "OnData"),
    file_path = c("/path/to/comuni_2025.shp", "/path/to/comuni_2023.shp"),
    download_timestamp = c(Sys.time(), old_time),
    file_size_mb = c(12.5, 12.0)
  )
  save_boundaries_metadata(metadata)

  # Clean files older than 365 days
  removed <- clean_boundary_cache(
    older_than_days = 365,
    keep_latest_n = NULL,
    dry_run = TRUE,
    verbose = FALSE
  )

  expect_length(removed, 1)
  expect_match(removed[1], "2023")
})

test_that("clean_boundary_cache validates inputs", {
  expect_error(
    clean_boundary_cache(keep_latest_n = -1),
    "keep_latest_n must be"
  )
  expect_error(
    clean_boundary_cache(older_than_days = -5),
    "older_than_days must be"
  )
  expect_error(clean_boundary_cache(dry_run = "yes"), "dry_run must be logical")
  expect_error(clean_boundary_cache(verbose = "yes"), "verbose must be logical")
})

# 8. Integration Tests -----

test_that("full workflow: download, check, info, clean", {
  skip_on_cran()
  skip_if_offline()

  temp_dir <- withr::local_tempdir()
  withr::local_envvar(c("R_USER_DATA_DIR" = temp_dir))

  # Download ripartizioni (smallest file)
  download_result <- download_istat_boundaries(
    territorial_levels = "ripartizioni",
    verbose = FALSE
  )

  # Should either succeed or be cached
  expect_true(download_result$status %in% c("success", "cached", "failed"))

  if (download_result$status == "success") {
    # Check updates
    updates <- check_boundary_updates(verbose = FALSE)
    rip_update <- updates[territorial_level == "ripartizioni"]
    expect_false(rip_update$update_available)

    # Get info
    info <- get_cached_boundaries_info(verbose = FALSE)
    expect_true(nrow(info) > 0)

    # Clean (dry run)
    removed <- clean_boundary_cache(dry_run = TRUE, verbose = FALSE)
    expect_true(is.character(removed))
  }
})
