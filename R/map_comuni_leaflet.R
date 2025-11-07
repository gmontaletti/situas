#' Create Interactive Leaflet Map of Italian Municipalities
#'
#' Creates an interactive web map of Italian municipalities using Leaflet.
#' This function demonstrates how to visualize the geographic data prepared
#' by \code{\link{prepare_comuni_maps}}.
#'
#' @param comuni_sf An sf object with municipality boundaries and attributes.
#'   Can be loaded from RDS file created by \code{\link{prepare_comuni_maps}}
#'   or read directly from GeoJSON.
#' @param color_by Character string. Attribute name to use for coloring polygons.
#'   Common options: "COD_REG" (by region), "COD_UTS" (by province).
#'   Set to NULL for uniform color. Default is "COD_REG".
#' @param palette Character string. Color palette name from RColorBrewer.
#'   Defaults to "Set3". See \code{RColorBrewer::display.brewer.all()} for options.
#' @param popup_fields Character vector. Fields to display in popup when clicking
#'   a municipality. Defaults to c("COMUNE", "DEN_REG", "DEN_UTS", "PRO_COM").
#' @param tile_provider Character string. Base map tile provider. Options:
#'   "OpenStreetMap" (default), "CartoDB.Positron", "Esri.WorldImagery".
#' @param add_legend Logical. Add a legend for colored regions. Default TRUE.
#'   Only applicable when color_by is not NULL.
#'
#' @return A leaflet map object that can be displayed interactively or embedded
#'   in Shiny applications.
#'
#' @note This function is now a wrapper around \code{\link{map_territorial_units}}.
#'   For more flexibility with different territorial units (provinces, regions, etc.),
#'   consider using \code{\link{map_territorial_units}} directly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Prepare maps
#' files <- prepare_comuni_maps()
#'
#' # Load the data
#' comuni <- readRDS(files$rds)
#'
#' # Basic map colored by region
#' map_comuni_leaflet(comuni)
#'
#' # Map colored by province
#' map_comuni_leaflet(comuni, color_by = "COD_UTS")
#'
#' # Uniform color, minimal popup
#' map_comuni_leaflet(
#'   comuni,
#'   color_by = NULL,
#'   popup_fields = "COMUNE"
#' )
#'
#' # Use satellite imagery
#' map_comuni_leaflet(
#'   comuni,
#'   tile_provider = "Esri.WorldImagery"
#' )
#'
#' # In a Shiny app
#' library(shiny)
#' library(leaflet)
#'
#' ui <- fluidPage(
#'   leafletOutput("map", height = "600px")
#' )
#'
#' server <- function(input, output) {
#'   comuni <- readRDS("data/comuni_map_20250114.rds")
#'   output$map <- renderLeaflet({
#'     map_comuni_leaflet(comuni)
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
map_comuni_leaflet <- function(comuni_sf,
                                color_by = "COD_REG",
                                palette = "Set3",
                                popup_fields = c("COMUNE", "DEN_REG", "DEN_UTS", "PRO_COM"),
                                tile_provider = "OpenStreetMap",
                                add_legend = TRUE) {
  # Call generalized function with comuni-specific defaults
  map_territorial_units(
    territorial_sf = comuni_sf,
    name_col = "COMUNE",
    id_col = "PRO_COM",
    color_by = color_by,
    palette = palette,
    popup_fields = popup_fields,
    tile_provider = tile_provider,
    add_legend = add_legend,
    verbose = FALSE
  )
}


#' Create Popup Text for Leaflet Map
#'
#' Internal helper to create HTML popup content from sf object attributes.
#'
#' @param sf_obj An sf object
#' @param fields Character vector of field names to include
#'
#' @return Character vector of HTML popup text
#'
#' @keywords internal
#' @noRd
create_popup_text <- function(sf_obj, fields) {
  # Filter to existing fields
  available_fields <- intersect(fields, names(sf_obj))

  if (length(available_fields) == 0) {
    return(rep("<b>No data</b>", nrow(sf_obj)))
  }

  # Create popup HTML for each feature
  popup_html <- apply(sf_obj[, available_fields, drop = FALSE], 1, function(row) {
    content <- sapply(names(row), function(field) {
      if (field == attr(sf_obj, "sf_column")) return(NULL)  # Skip geometry
      value <- row[[field]]
      if (is.na(value) || is.null(value)) return(NULL)
      paste0("<b>", field, ":</b> ", value)
    })

    content <- content[!sapply(content, is.null)]
    paste(content, collapse = "<br/>")
  })

  return(popup_html)
}


#' Add Base Tile Layer to Leaflet Map
#'
#' Internal helper to add appropriate base map tiles.
#'
#' @param map A leaflet map object
#' @param provider Character string specifying tile provider
#'
#' @return Updated leaflet map with base tiles
#'
#' @keywords internal
#' @noRd
add_base_tiles <- function(map, provider) {
  if (provider == "OpenStreetMap") {
    map <- map %>%
      leaflet::addTiles()
  } else {
    map <- map %>%
      leaflet::addProviderTiles(provider)
  }

  return(map)
}


#' Create Choropleth Map by Joining External Data
#'
#' Creates a choropleth (colored) map by joining external data to municipality
#' boundaries. Useful for visualizing statistics, census data, or any other
#' municipality-level indicators.
#'
#' @param comuni_sf An sf object with municipality boundaries
#' @param data A data.frame or data.table with values to visualize
#' @param join_by Character string. Column name to join on (must exist in both
#'   comuni_sf and data). Default is "PRO_COM".
#' @param value_col Character string. Column name in data containing values to
#'   visualize (e.g., population, income, etc.)
#' @param palette Character string. Color palette for choropleth. Options:
#'   "YlOrRd", "YlGnBu", "RdYlGn", "Spectral". Default is "YlOrRd".
#' @param legend_title Character string. Title for the legend.
#' @param popup_fields Character vector. Additional fields to show in popup.
#'
#' @return A leaflet map object
#'
#' @note This function is now a wrapper around \code{\link{map_territorial_choropleth}}.
#'   For more flexibility with different territorial units (provinces, regions, etc.),
#'   consider using \code{\link{map_territorial_choropleth}} directly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load municipality boundaries
#' files <- prepare_comuni_maps()
#' comuni <- readRDS(files$rds)
#'
#' # Create sample data (e.g., population by municipality)
#' pop_data <- data.frame(
#'   PRO_COM = sample(comuni$PRO_COM, 100),
#'   population = runif(100, 1000, 100000)
#' )
#'
#' # Create choropleth map
#' map_comuni_choropleth(
#'   comuni,
#'   data = pop_data,
#'   value_col = "population",
#'   legend_title = "Population"
#' )
#' }
map_comuni_choropleth <- function(comuni_sf,
                                   data,
                                   join_by = "PRO_COM",
                                   value_col,
                                   palette = "YlOrRd",
                                   legend_title = value_col,
                                   popup_fields = c("COMUNE", "DEN_REG")) {
  # Call generalized function with comuni-specific defaults
  map_territorial_choropleth(
    territorial_sf = comuni_sf,
    data = data,
    join_by = join_by,
    value_col = value_col,
    name_col = "COMUNE",
    palette = palette,
    legend_title = legend_title,
    popup_fields = popup_fields
  )
}
