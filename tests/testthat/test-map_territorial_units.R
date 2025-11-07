# Tests for map_territorial_units.R

# 1. Setup test data -----
make_test_comuni_sf <- function() {
  coords_list <- list(
    rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)),
    rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)),
    rbind(c(0, 1), c(1, 1), c(1, 2), c(0, 2), c(0, 1))
  )
  polys <- lapply(coords_list, function(coords) {
    sf::st_polygon(list(coords))
  })

  sf::st_sf(
    COMUNE = c("Roma", "Milano", "Torino"),
    PRO_COM = c("058091", "015146", "001272"),
    COD_REG = c("12", "03", "01"),
    DEN_REG = c("Lazio", "Lombardia", "Piemonte"),
    COD_UTS = c("058", "015", "001"),
    DEN_UTS = c("Roma", "Milano", "Torino"),
    geometry = sf::st_sfc(polys, crs = 4326)
  )
}

make_test_regioni_sf <- function() {
  coords_list <- list(
    rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)),
    rbind(c(2, 0), c(4, 0), c(4, 2), c(2, 2), c(2, 0))
  )
  polys <- lapply(coords_list, function(coords) {
    sf::st_polygon(list(coords))
  })

  sf::st_sf(
    DEN_REG = c("Lazio", "Lombardia"),
    COD_REG = c("12", "03"),
    geometry = sf::st_sfc(polys, crs = 4326)
  )
}

make_test_province_sf <- function() {
  coords_list <- list(
    rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)),
    rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))
  )
  polys <- lapply(coords_list, function(coords) {
    sf::st_polygon(list(coords))
  })

  sf::st_sf(
    DEN_UTS = c("Roma", "Milano"),
    COD_UTS = c("058", "015"),
    DEN_REG = c("Lazio", "Lombardia"),
    COD_REG = c("12", "03"),
    geometry = sf::st_sfc(polys, crs = 4326)
  )
}

make_test_ripartizioni_sf <- function() {
  coords_list <- list(
    rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))
  )
  polys <- lapply(coords_list, function(coords) {
    sf::st_polygon(list(coords))
  })

  sf::st_sf(
    DEN_RIP = c("Nord-ovest"),
    COD_RIP = c("1"),
    geometry = sf::st_sfc(polys, crs = 4326)
  )
}


# 2. Tests for detect_name_column -----
test_that("detect_name_column identifies COMUNE first", {
  comuni_sf <- make_test_comuni_sf()
  result <- situas:::detect_name_column(comuni_sf)
  expect_equal(result, "COMUNE")
})

test_that("detect_name_column identifies DEN_REG when no COMUNE", {
  regioni_sf <- make_test_regioni_sf()
  result <- situas:::detect_name_column(regioni_sf)
  expect_equal(result, "DEN_REG")
})

test_that("detect_name_column identifies DEN_UTS for province", {
  province_sf <- make_test_province_sf()
  # Remove higher priority columns to test DEN_UTS detection
  province_sf$COMUNE <- NULL
  province_sf$DEN_REG <- NULL
  result <- situas:::detect_name_column(province_sf)
  expect_equal(result, "DEN_UTS")
})

test_that("detect_name_column returns NULL when no suitable column", {
  sf_obj <- sf::st_sf(
    x = 1:3,
    y = 4:6,
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2)),
      crs = 4326
    )
  )
  result <- situas:::detect_name_column(sf_obj)
  expect_null(result)
})


# 3. Tests for detect_id_column -----
test_that("detect_id_column identifies PRO_COM first", {
  comuni_sf <- make_test_comuni_sf()
  result <- situas:::detect_id_column(comuni_sf)
  expect_equal(result, "PRO_COM")
})

test_that("detect_id_column identifies COD_REG when no PRO_COM", {
  regioni_sf <- make_test_regioni_sf()
  result <- situas:::detect_id_column(regioni_sf)
  expect_equal(result, "COD_REG")
})

test_that("detect_id_column identifies COD_UTS for province", {
  province_sf <- make_test_province_sf()
  # Remove higher priority columns to test COD_UTS detection
  province_sf$PRO_COM <- NULL
  province_sf$COD_REG <- NULL
  result <- situas:::detect_id_column(province_sf)
  expect_equal(result, "COD_UTS")
})


# 4. Tests for detect_territorial_type -----
test_that("detect_territorial_type identifies comuni", {
  comuni_sf <- make_test_comuni_sf()
  result <- situas:::detect_territorial_type(comuni_sf)
  expect_equal(result, "comuni")
})

test_that("detect_territorial_type identifies regioni", {
  regioni_sf <- make_test_regioni_sf()
  result <- situas:::detect_territorial_type(regioni_sf)
  expect_equal(result, "regioni")
})

test_that("detect_territorial_type identifies province", {
  province_sf <- make_test_province_sf()
  result <- situas:::detect_territorial_type(province_sf)
  expect_equal(result, "province")
})

test_that("detect_territorial_type identifies ripartizioni", {
  ripartizioni_sf <- make_test_ripartizioni_sf()
  result <- situas:::detect_territorial_type(ripartizioni_sf)
  expect_equal(result, "ripartizioni")
})


# 5. Tests for get_default_popup_fields -----
test_that("get_default_popup_fields returns correct fields for comuni", {
  comuni_sf <- make_test_comuni_sf()
  result <- situas:::get_default_popup_fields(comuni_sf, "COMUNE", "PRO_COM")
  expect_true(all(c("COMUNE", "DEN_REG", "DEN_UTS", "PRO_COM") %in% result))
})

test_that("get_default_popup_fields returns correct fields for regioni", {
  regioni_sf <- make_test_regioni_sf()
  result <- situas:::get_default_popup_fields(regioni_sf, "DEN_REG", "COD_REG")
  expect_true(all(c("DEN_REG", "COD_REG") %in% result))
})

test_that("get_default_popup_fields handles NULL id_col", {
  regioni_sf <- make_test_regioni_sf()
  result <- situas:::get_default_popup_fields(regioni_sf, "DEN_REG", NULL)
  expect_true("DEN_REG" %in% result)
  expect_false("COD_REG" %in% result)
})


# 6. Tests for map_territorial_units -----
test_that("map_territorial_units creates leaflet map for comuni", {
  comuni_sf <- make_test_comuni_sf()
  map <- map_territorial_units(comuni_sf)
  expect_s3_class(map, "leaflet")
  expect_s3_class(map, "htmlwidget")
})

test_that("map_territorial_units works with color_by", {
  comuni_sf <- make_test_comuni_sf()
  map <- map_territorial_units(comuni_sf, color_by = "COD_REG")
  expect_s3_class(map, "leaflet")
})

test_that("map_territorial_units works for regioni", {
  regioni_sf <- make_test_regioni_sf()
  map <- map_territorial_units(regioni_sf)
  expect_s3_class(map, "leaflet")
})

test_that("map_territorial_units works for province", {
  province_sf <- make_test_province_sf()
  map <- map_territorial_units(province_sf)
  expect_s3_class(map, "leaflet")
})

test_that("map_territorial_units validates input sf object", {
  expect_error(
    map_territorial_units(data.frame(x = 1:3)),
    "territorial_sf must be an sf object"
  )
})

test_that("map_territorial_units validates color_by column exists", {
  comuni_sf <- make_test_comuni_sf()
  expect_error(
    map_territorial_units(comuni_sf, color_by = "nonexistent"),
    "color_by field 'nonexistent' not found"
  )
})

test_that("map_territorial_units allows manual name_col specification", {
  comuni_sf <- make_test_comuni_sf()
  map <- map_territorial_units(comuni_sf, name_col = "COMUNE")
  expect_s3_class(map, "leaflet")
})

test_that("map_territorial_units errors when name_col not found", {
  comuni_sf <- make_test_comuni_sf()
  expect_error(
    map_territorial_units(comuni_sf, name_col = "nonexistent"),
    "Specified name_col 'nonexistent' not found"
  )
})

test_that("map_territorial_units verbose mode prints messages", {
  comuni_sf <- make_test_comuni_sf()
  expect_message(
    map_territorial_units(comuni_sf, verbose = TRUE),
    "Detected territorial type"
  )
})

test_that("map_territorial_units works with custom popup_fields", {
  comuni_sf <- make_test_comuni_sf()
  map <- map_territorial_units(
    comuni_sf,
    popup_fields = c("COMUNE", "DEN_REG")
  )
  expect_s3_class(map, "leaflet")
})


# 7. Tests for map_territorial_choropleth -----
test_that("map_territorial_choropleth creates choropleth for comuni", {
  comuni_sf <- make_test_comuni_sf()
  data <- data.frame(
    PRO_COM = c("058091", "015146", "001272"),
    population = c(2800000, 1400000, 900000)
  )

  map <- map_territorial_choropleth(
    comuni_sf,
    data = data,
    value_col = "population"
  )
  expect_s3_class(map, "leaflet")
})

test_that("map_territorial_choropleth works for regioni", {
  regioni_sf <- make_test_regioni_sf()
  data <- data.frame(
    COD_REG = c("12", "03"),
    gdp = c(50000, 60000)
  )

  map <- map_territorial_choropleth(
    regioni_sf,
    data = data,
    value_col = "gdp"
  )
  expect_s3_class(map, "leaflet")
})

test_that("map_territorial_choropleth validates sf object", {
  data <- data.frame(
    PRO_COM = c("058091", "015146"),
    population = c(2800000, 1400000)
  )

  expect_error(
    map_territorial_choropleth(
      data.frame(x = 1:3),
      data = data,
      value_col = "population"
    ),
    "territorial_sf must be an sf object"
  )
})

test_that("map_territorial_choropleth validates data is data.frame", {
  comuni_sf <- make_test_comuni_sf()

  expect_error(
    map_territorial_choropleth(
      comuni_sf,
      data = list(a = 1, b = 2),
      value_col = "population"
    ),
    "data must be a data.frame"
  )
})

test_that("map_territorial_choropleth validates value_col exists in data", {
  comuni_sf <- make_test_comuni_sf()
  data <- data.frame(
    PRO_COM = c("058091", "015146"),
    population = c(2800000, 1400000)
  )

  expect_error(
    map_territorial_choropleth(
      comuni_sf,
      data = data,
      value_col = "nonexistent"
    ),
    "value_col 'nonexistent' not found in data"
  )
})

test_that("map_territorial_choropleth validates join_by exists in both", {
  comuni_sf <- make_test_comuni_sf()
  data <- data.frame(
    wrong_col = c("058091", "015146"),
    population = c(2800000, 1400000)
  )

  expect_error(
    map_territorial_choropleth(
      comuni_sf,
      data = data,
      join_by = "PRO_COM",
      value_col = "population"
    ),
    "join_by column 'PRO_COM' not found in data"
  )
})

test_that("map_territorial_choropleth works with manual join_by", {
  province_sf <- make_test_province_sf()
  data <- data.frame(
    COD_UTS = c("058", "015"),
    unemployment = c(10.5, 8.3)
  )

  map <- map_territorial_choropleth(
    province_sf,
    data = data,
    join_by = "COD_UTS",
    value_col = "unemployment"
  )
  expect_s3_class(map, "leaflet")
})

test_that("map_territorial_choropleth handles custom legend_title", {
  comuni_sf <- make_test_comuni_sf()
  data <- data.frame(
    PRO_COM = c("058091", "015146", "001272"),
    population = c(2800000, 1400000, 900000)
  )

  map <- map_territorial_choropleth(
    comuni_sf,
    data = data,
    value_col = "population",
    legend_title = "Population (2024)"
  )
  expect_s3_class(map, "leaflet")
})

test_that("map_territorial_choropleth handles custom palette", {
  comuni_sf <- make_test_comuni_sf()
  data <- data.frame(
    PRO_COM = c("058091", "015146", "001272"),
    population = c(2800000, 1400000, 900000)
  )

  map <- map_territorial_choropleth(
    comuni_sf,
    data = data,
    value_col = "population",
    palette = "YlGnBu"
  )
  expect_s3_class(map, "leaflet")
})

test_that("map_territorial_choropleth auto-detects join_by", {
  comuni_sf <- make_test_comuni_sf()
  data <- data.frame(
    PRO_COM = c("058091", "015146", "001272"),
    population = c(2800000, 1400000, 900000)
  )

  # join_by = NULL should auto-detect PRO_COM
  map <- map_territorial_choropleth(
    comuni_sf,
    data = data,
    join_by = NULL,
    value_col = "population"
  )
  expect_s3_class(map, "leaflet")
})
