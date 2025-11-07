#' Create Interactive Leaflet Map of Italian Territorial Units
#'
#' Creates an interactive web map for any Italian territorial unit type
#' (comuni, province, regioni, ripartizioni) using Leaflet. This function
#' automatically detects the type of territorial data and applies appropriate
#' defaults, or allows manual specification of display parameters.
#'
#' @param territorial_sf An sf object with territorial unit boundaries and attributes.
#'   Supports comuni, province, regioni, or ripartizioni data.
#' @param name_col Character string. Column name to use for labels. If NULL (default),
#'   the function auto-detects based on territorial type (COMUNE, DEN_REG, DEN_UTS, DEN_RIP).
#' @param id_col Character string. Column name containing IDs. If NULL (default),
#'   auto-detects based on territorial type (PRO_COM, COD_REG, COD_UTS, COD_RIP).
#' @param color_by Character string. Attribute name to use for coloring polygons.
#'   Set to NULL for uniform color. If NULL, no coloring is applied.
#' @param palette Character string. Color palette name from RColorBrewer.
#'   Defaults to "Set3". See \code{RColorBrewer::display.brewer.all()} for options.
#' @param popup_fields Character vector. Fields to display in popup when clicking
#'   a territorial unit. If NULL (default), appropriate fields are auto-selected
#'   based on territorial type.
#' @param tile_provider Character string. Base map tile provider. Options:
#'   "OpenStreetMap" (default), "CartoDB.Positron", "Esri.WorldImagery".
#' @param add_legend Logical. Add a legend for colored regions. Default TRUE.
#'   Only applicable when color_by is not NULL.
#' @param verbose Logical. If TRUE, prints messages about auto-detection decisions.
#'   Default FALSE.
#'
#' @return A leaflet map object that can be displayed interactively or embedded
#'   in Shiny applications.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Map comuni (municipalities)
#' comuni <- readRDS("data/comuni_map.rds")
#' map_territorial_units(comuni, color_by = "COD_REG")
#'
#' # Map regioni (regions)
#' regioni <- sf::st_read("regioni.geojson")
#' map_territorial_units(regioni, color_by = "COD_REG")
#'
#' # Map province (provinces) with custom popup
#' province <- sf::st_read("province.geojson")
#' map_territorial_units(
#'   province,
#'   popup_fields = c("DEN_UTS", "DEN_REG"),
#'   verbose = TRUE
#' )
#'
#' # Map ripartizioni (macro-regions) with uniform color
#' ripartizioni <- sf::st_read("ripartizioni.geojson")
#' map_territorial_units(ripartizioni, color_by = NULL)
#'
#' # Manual specification of columns
#' map_territorial_units(
#'   territorial_sf,
#'   name_col = "custom_name",
#'   id_col = "custom_id",
#'   color_by = "custom_category"
#' )
#' }
map_territorial_units <- function(territorial_sf,
                                   name_col = NULL,
                                   id_col = NULL,
                                   color_by = NULL,
                                   palette = "Set3",
                                   popup_fields = NULL,
                                   tile_provider = "OpenStreetMap",
                                   add_legend = TRUE,
                                   verbose = FALSE) {
  # 1. Input validation -----
  stopifnot(
    "territorial_sf must be an sf object" = inherits(territorial_sf, "sf"),
    "palette must be a character string" =
      is.character(palette) && length(palette) == 1,
    "tile_provider must be a character string" =
      is.character(tile_provider) && length(tile_provider) == 1,
    "add_legend must be logical" =
      is.logical(add_legend) && length(add_legend) == 1,
    "verbose must be logical" =
      is.logical(verbose) && length(verbose) == 1
  )

  # Validate geometry type (POLYGON or MULTIPOLYGON only)
  geom_types <- unique(as.character(sf::st_geometry_type(territorial_sf)))
  valid_geom_types <- c("POLYGON", "MULTIPOLYGON")

  if (!any(geom_types %in% valid_geom_types)) {
    stop(
      "territorial_sf must contain POLYGON or MULTIPOLYGON geometries.\n",
      "Found: ", paste(geom_types, collapse = ", "),
      call. = FALSE
    )
  }

  # 2. Auto-detect territorial type -----
  terr_type <- detect_territorial_type(territorial_sf)
  if (verbose) {
    message("Detected territorial type: ", terr_type)
  }

  # 3. Auto-detect or validate name column -----
  if (is.null(name_col)) {
    name_col <- detect_name_column(territorial_sf)
    if (is.null(name_col)) {
      stop(
        "Could not auto-detect name column. Please specify name_col.\n",
        "Available columns: ", paste(names(territorial_sf), collapse = ", "),
        call. = FALSE
      )
    }
    if (verbose) {
      message("Auto-detected name column: ", name_col)
    }
  } else {
    if (!name_col %in% names(territorial_sf)) {
      stop(
        "Specified name_col '", name_col, "' not found in data.\n",
        "Available columns: ", paste(names(territorial_sf), collapse = ", "),
        call. = FALSE
      )
    }
  }

  # 4. Auto-detect or validate id column -----
  if (is.null(id_col)) {
    id_col <- detect_id_column(territorial_sf)
    if (verbose && !is.null(id_col)) {
      message("Auto-detected ID column: ", id_col)
    }
  } else {
    if (!id_col %in% names(territorial_sf)) {
      stop(
        "Specified id_col '", id_col, "' not found in data.\n",
        "Available columns: ", paste(names(territorial_sf), collapse = ", "),
        call. = FALSE
      )
    }
  }

  # 5. Validate color_by if specified -----
  if (!is.null(color_by)) {
    stopifnot("color_by must be a character string" = is.character(color_by))
    if (!color_by %in% names(territorial_sf)) {
      stop(
        "color_by field '", color_by, "' not found in data.\n",
        "Available fields: ", paste(names(territorial_sf), collapse = ", "),
        call. = FALSE
      )
    }
  }

  # 6. Auto-detect or validate popup fields -----
  if (is.null(popup_fields)) {
    popup_fields <- get_default_popup_fields(territorial_sf, name_col, id_col)
    if (verbose && length(popup_fields) > 0) {
      message("Auto-selected popup fields: ", paste(popup_fields, collapse = ", "))
    }
  } else {
    stopifnot("popup_fields must be a character vector" = is.character(popup_fields))
  }

  # 7. Prepare popup content -----
  popup_text <- create_popup_text(territorial_sf, popup_fields)

  # 8. Initialize leaflet map -----
  map <- leaflet::leaflet(territorial_sf) %>%
    add_base_tiles(tile_provider)

  # 9. Add polygons with or without color -----
  if (!is.null(color_by)) {
    # Create color palette
    unique_values <- unique(territorial_sf[[color_by]])
    pal <- leaflet::colorFactor(
      palette = palette,
      domain = unique_values
    )

    map <- map %>%
      leaflet::addPolygons(
        fillColor = ~ pal(territorial_sf[[color_by]]),
        fillOpacity = 0.6,
        color = "#444444",
        weight = 1,
        opacity = 0.8,
        popup = popup_text,
        highlightOptions = leaflet::highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.8,
          bringToFront = TRUE
        ),
        label = ~ territorial_sf[[name_col]],
        labelOptions = leaflet::labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        )
      )

    # 10. Add legend -----
    if (add_legend) {
      legend_labels <- unique_values
      legend_title <- color_by

      # Try to map codes to human-readable names
      if (color_by == "COD_REG" && "DEN_REG" %in% names(territorial_sf)) {
        region_mapping <- unique(territorial_sf[, c("COD_REG", "DEN_REG"), drop = TRUE])
        names_vec <- region_mapping$DEN_REG
        names(names_vec) <- as.character(region_mapping$COD_REG)
        legend_labels <- names_vec[as.character(unique_values)]
        legend_title <- "Regione"
      } else if (color_by == "COD_UTS" && "DEN_UTS" %in% names(territorial_sf)) {
        province_mapping <- unique(territorial_sf[, c("COD_UTS", "DEN_UTS"), drop = TRUE])
        names_vec <- province_mapping$DEN_UTS
        names(names_vec) <- as.character(province_mapping$COD_UTS)
        legend_labels <- names_vec[as.character(unique_values)]
        legend_title <- "Provincia"
      } else if (color_by == "COD_RIP" && "DEN_RIP" %in% names(territorial_sf)) {
        rip_mapping <- unique(territorial_sf[, c("COD_RIP", "DEN_RIP"), drop = TRUE])
        names_vec <- rip_mapping$DEN_RIP
        names(names_vec) <- as.character(rip_mapping$COD_RIP)
        legend_labels <- names_vec[as.character(unique_values)]
        legend_title <- "Ripartizione"
      }

      map <- map %>%
        leaflet::addLegend(
          pal = pal,
          values = ~ territorial_sf[[color_by]],
          title = legend_title,
          labels = legend_labels,
          opacity = 0.7,
          position = "bottomright"
        )
    }
  } else {
    # Uniform color
    map <- map %>%
      leaflet::addPolygons(
        fillColor = "#3388ff",
        fillOpacity = 0.5,
        color = "#444444",
        weight = 1,
        opacity = 0.8,
        popup = popup_text,
        highlightOptions = leaflet::highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.8,
          bringToFront = TRUE
        ),
        label = ~ territorial_sf[[name_col]],
        labelOptions = leaflet::labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        )
      )
  }

  return(map)
}


#' Detect Name Column in Territorial SF Object
#'
#' Internal helper to identify the most likely column containing
#' territorial unit names.
#'
#' @param sf_obj An sf object with territorial data
#'
#' @return Character string with column name, or NULL if not found
#'
#' @keywords internal
#' @noRd
detect_name_column <- function(sf_obj) {
  # Priority order for name columns
  name_candidates <- c("COMUNE", "DEN_REG", "DEN_UTS", "DEN_RIP", "denominazione")

  for (col in name_candidates) {
    if (col %in% names(sf_obj)) {
      return(col)
    }
  }

  # Fallback: first character column that's not geometry
  char_cols <- names(sf_obj)[sapply(sf_obj, is.character)]
  geom_col <- attr(sf_obj, "sf_column")
  char_cols <- setdiff(char_cols, geom_col)

  if (length(char_cols) > 0) {
    return(char_cols[1])
  }

  return(NULL)
}


#' Detect ID Column in Territorial SF Object
#'
#' Internal helper to identify the most likely column containing
#' territorial unit identifiers.
#'
#' @param sf_obj An sf object with territorial data
#'
#' @return Character string with column name, or NULL if not found
#'
#' @keywords internal
#' @noRd
detect_id_column <- function(sf_obj) {
  # Priority order for ID columns
  id_candidates <- c("PRO_COM", "COD_REG", "COD_UTS", "COD_RIP")

  for (col in id_candidates) {
    if (col %in% names(sf_obj)) {
      return(col)
    }
  }

  # Fallback: look for columns with "cod" or "id" in name
  geom_col <- attr(sf_obj, "sf_column")
  potential_ids <- grep("^(cod|id|code)", names(sf_obj),
                        ignore.case = TRUE, value = TRUE)
  potential_ids <- setdiff(potential_ids, geom_col)

  if (length(potential_ids) > 0) {
    return(potential_ids[1])
  }

  return(NULL)
}


#' Detect Territorial Type from SF Object
#'
#' Internal helper to identify the type of territorial unit based on
#' the presence of key columns.
#'
#' @param sf_obj An sf object with territorial data
#'
#' @return Character string: "comuni", "province", "regioni", "ripartizioni", or "unknown"
#'
#' @keywords internal
#' @noRd
detect_territorial_type <- function(sf_obj) {
  cols <- names(sf_obj)

  # Comuni have COMUNE column
  if ("COMUNE" %in% cols) {
    return("comuni")
  }

  # Ripartizioni have DEN_RIP
  if ("DEN_RIP" %in% cols && !"DEN_REG" %in% cols) {
    return("ripartizioni")
  }

  # Province have DEN_UTS but not COMUNE
  if ("DEN_UTS" %in% cols && !"COMUNE" %in% cols) {
    return("province")
  }

  # Regioni have DEN_REG but not DEN_UTS
  if ("DEN_REG" %in% cols && !"DEN_UTS" %in% cols) {
    return("regioni")
  }

  return("unknown")
}


#' Get Default Popup Fields Based on Territorial Type
#'
#' Internal helper to select appropriate popup fields based on the
#' detected territorial type.
#'
#' @param sf_obj An sf object with territorial data
#' @param name_col Character string with name column
#' @param id_col Character string with ID column (can be NULL)
#'
#' @return Character vector of column names for popup
#'
#' @keywords internal
#' @noRd
get_default_popup_fields <- function(sf_obj, name_col, id_col) {
  terr_type <- detect_territorial_type(sf_obj)
  cols <- names(sf_obj)

  fields <- character(0)

  if (terr_type == "comuni") {
    # Comuni: show comune, region, province, and code
    fields <- c(name_col)
    if ("DEN_REG" %in% cols) fields <- c(fields, "DEN_REG")
    if ("DEN_UTS" %in% cols) fields <- c(fields, "DEN_UTS")
    if (!is.null(id_col)) fields <- c(fields, id_col)
  } else if (terr_type == "province") {
    # Province: show province, region, and code
    fields <- c(name_col)
    if ("DEN_REG" %in% cols) fields <- c(fields, "DEN_REG")
    if (!is.null(id_col)) fields <- c(fields, id_col)
  } else if (terr_type == "regioni") {
    # Regioni: show region name and code
    fields <- c(name_col)
    if (!is.null(id_col)) fields <- c(fields, id_col)
  } else if (terr_type == "ripartizioni") {
    # Ripartizioni: show name and code
    fields <- c(name_col)
    if (!is.null(id_col)) fields <- c(fields, id_col)
  } else {
    # Unknown: just show name
    fields <- c(name_col)
    if (!is.null(id_col)) fields <- c(fields, id_col)
  }

  # Keep only fields that exist
  fields <- intersect(fields, cols)

  return(fields)
}


#' Create Choropleth Map by Joining External Data
#'
#' Creates a choropleth (colored) map by joining external data to territorial
#' unit boundaries. Works with any Italian territorial unit type (comuni, province,
#' regioni, ripartizioni). Useful for visualizing statistics, census data, or any
#' other territorial-level indicators.
#'
#' @param territorial_sf An sf object with territorial unit boundaries
#' @param data A data.frame or data.table with values to visualize
#' @param join_by Character string. Column name to join on (must exist in both
#'   territorial_sf and data). If NULL (default), auto-detects based on territorial type
#'   (PRO_COM, COD_REG, COD_UTS, COD_RIP).
#' @param value_col Character string. Column name in data containing values to
#'   visualize (e.g., population, income, etc.)
#' @param name_col Character string. Column name to use for labels. If NULL (default),
#'   auto-detects based on territorial type.
#' @param palette Character string. Color palette for choropleth. Options:
#'   "YlOrRd" (default), "YlGnBu", "RdYlGn", "Spectral", or any RColorBrewer palette.
#' @param legend_title Character string. Title for the legend. Defaults to value_col.
#' @param popup_fields Character vector. Additional fields to show in popup. If NULL,
#'   auto-selected based on territorial type.
#'
#' @return A leaflet map object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Choropleth for comuni
#' comuni <- readRDS("data/comuni_map.rds")
#' pop_data <- data.frame(
#'   PRO_COM = sample(comuni$PRO_COM, 100),
#'   population = runif(100, 1000, 100000)
#' )
#' map_territorial_choropleth(
#'   comuni,
#'   data = pop_data,
#'   value_col = "population",
#'   legend_title = "Population"
#' )
#'
#' # Choropleth for regioni
#' regioni <- sf::st_read("regioni.geojson")
#' gdp_data <- data.frame(
#'   COD_REG = 1:20,
#'   gdp_per_capita = rnorm(20, 25000, 5000)
#' )
#' map_territorial_choropleth(
#'   regioni,
#'   data = gdp_data,
#'   value_col = "gdp_per_capita",
#'   legend_title = "GDP per capita (EUR)"
#' )
#'
#' # Choropleth for province with custom join
#' province <- sf::st_read("province.geojson")
#' employment_data <- data.frame(
#'   COD_UTS = unique(province$COD_UTS),
#'   unemployment_rate = runif(length(unique(province$COD_UTS)), 5, 15)
#' )
#' map_territorial_choropleth(
#'   province,
#'   data = employment_data,
#'   join_by = "COD_UTS",
#'   value_col = "unemployment_rate",
#'   legend_title = "Unemployment Rate (%)"
#' )
#' }
map_territorial_choropleth <- function(territorial_sf,
                                        data,
                                        join_by = NULL,
                                        value_col,
                                        name_col = NULL,
                                        palette = "YlOrRd",
                                        legend_title = value_col,
                                        popup_fields = NULL) {
  # 1. Input validation -----
  stopifnot(
    "territorial_sf must be an sf object" = inherits(territorial_sf, "sf"),
    "data must be a data.frame" = is.data.frame(data),
    "value_col must be a character string" =
      is.character(value_col) && length(value_col) == 1
  )

  # 2. Auto-detect or validate join_by -----
  if (is.null(join_by)) {
    join_by <- detect_id_column(territorial_sf)
    if (is.null(join_by)) {
      stop(
        "Could not auto-detect join column. Please specify join_by.\n",
        "Available columns in territorial_sf: ",
        paste(names(territorial_sf), collapse = ", "),
        call. = FALSE
      )
    }
  } else {
    stopifnot(
      "join_by must be a character string" =
        is.character(join_by) && length(join_by) == 1
    )
  }

  if (!join_by %in% names(territorial_sf)) {
    stop(
      "join_by column '", join_by, "' not found in territorial_sf.\n",
      "Available columns: ", paste(names(territorial_sf), collapse = ", "),
      call. = FALSE
    )
  }

  if (!join_by %in% names(data)) {
    stop(
      "join_by column '", join_by, "' not found in data.\n",
      "Available columns: ", paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }

  if (!value_col %in% names(data)) {
    stop(
      "value_col '", value_col, "' not found in data.\n",
      "Available columns: ", paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }

  # 3. Auto-detect or validate name_col -----
  if (is.null(name_col)) {
    name_col <- detect_name_column(territorial_sf)
    if (is.null(name_col)) {
      stop(
        "Could not auto-detect name column. Please specify name_col.\n",
        "Available columns: ", paste(names(territorial_sf), collapse = ", "),
        call. = FALSE
      )
    }
  }

  # 4. Auto-detect popup_fields if needed -----
  if (is.null(popup_fields)) {
    popup_fields <- get_default_popup_fields(territorial_sf, name_col, join_by)
    # Remove name_col from popup since it's shown separately
    popup_fields <- setdiff(popup_fields, name_col)
  }

  # 5. Join data -----
  territorial_joined <- merge(territorial_sf, data, by = join_by, all.x = TRUE)

  # 6. Create color palette -----
  pal <- leaflet::colorNumeric(
    palette = palette,
    domain = territorial_joined[[value_col]],
    na.color = "#CCCCCC"
  )

  # 7. Create popup HTML -----
  popup_html <- apply(territorial_joined, 1, function(row) {
    parts <- c(
      paste0("<b>", row[[name_col]], "</b>")
    )

    # Add value
    val <- row[[value_col]]
    if (!is.na(val) && !is.null(val)) {
      parts <- c(parts, paste0("<b>", legend_title, ":</b> ",
                               format(as.numeric(val), big.mark = ",")))
    } else {
      parts <- c(parts, paste0("<b>", legend_title, ":</b> N/A"))
    }

    # Add additional popup fields
    for (field in popup_fields) {
      if (field %in% names(row) && field != name_col) {
        field_val <- row[[field]]
        if (!is.na(field_val) && !is.null(field_val)) {
          parts <- c(parts, paste0("<b>", field, ":</b> ", field_val))
        }
      }
    }

    paste(parts, collapse = "<br/>")
  })

  # 8. Create map -----
  map <- leaflet::leaflet(territorial_joined) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      fillColor = ~ pal(territorial_joined[[value_col]]),
      fillOpacity = 0.7,
      color = "#444444",
      weight = 1,
      opacity = 0.8,
      popup = popup_html,
      highlightOptions = leaflet::highlightOptions(
        weight = 3,
        color = "#666",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = ~ paste0(
        territorial_joined[[name_col]], ": ",
        format(as.numeric(territorial_joined[[value_col]]), big.mark = ",")
      )
    ) %>%
    leaflet::addLegend(
      pal = pal,
      values = ~ territorial_joined[[value_col]],
      title = legend_title,
      opacity = 0.7,
      position = "bottomright"
    )

  return(map)
}
