# Examples for map_territorial_units.R
# These examples demonstrate the generalized mapping functions

library(situas)
library(sf)

# 1. Basic usage with comuni (municipalities) -----

# Load comuni data
comuni <- readRDS("data/comuni_map_20251014.rds")

# Simple map with auto-detection
map1 <- map_territorial_units(comuni)
map1

# Color by region
map2 <- map_territorial_units(comuni, color_by = "COD_REG")
map2

# Color by province
map3 <- map_territorial_units(comuni, color_by = "COD_UTS")
map3

# Uniform color with custom popup
map4 <- map_territorial_units(
  comuni,
  color_by = NULL,
  popup_fields = c("COMUNE", "DEN_REG")
)
map4

# Verbose mode to see auto-detection
map5 <- map_territorial_units(comuni, verbose = TRUE)
map5


# 2. Usage with regioni (regions) -----

# If you have region-level data (aggregated from comuni)
regioni <- comuni %>%
  group_by(COD_REG, DEN_REG) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Map regions
map_regioni <- map_territorial_units(regioni, color_by = "COD_REG")
map_regioni


# 3. Usage with province (provinces) -----

# If you have province-level data
province <- comuni %>%
  group_by(COD_UTS, DEN_UTS, COD_REG, DEN_REG) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Map provinces
map_province <- map_territorial_units(
  province,
  color_by = "COD_REG",  # Color by region
  popup_fields = c("DEN_UTS", "DEN_REG")
)
map_province


# 4. Choropleth maps with external data -----

# Example 1: Population by municipality
pop_data <- data.frame(
  PRO_COM = sample(comuni$PRO_COM, 500),
  population = runif(500, 1000, 500000)
)

map_pop <- map_territorial_choropleth(
  comuni,
  data = pop_data,
  value_col = "population",
  legend_title = "Population"
)
map_pop

# Example 2: Regional GDP
gdp_data <- data.frame(
  COD_REG = unique(comuni$COD_REG),
  gdp_per_capita = rnorm(length(unique(comuni$COD_REG)), 25000, 5000)
)

map_gdp <- map_territorial_choropleth(
  regioni,
  data = gdp_data,
  value_col = "gdp_per_capita",
  legend_title = "GDP per capita (EUR)",
  palette = "YlGnBu"
)
map_gdp

# Example 3: Provincial unemployment rate
unemployment_data <- data.frame(
  COD_UTS = unique(province$COD_UTS),
  unemployment_rate = runif(length(unique(province$COD_UTS)), 5, 15)
)

map_unemp <- map_territorial_choropleth(
  province,
  data = unemployment_data,
  join_by = "COD_UTS",
  value_col = "unemployment_rate",
  legend_title = "Unemployment Rate (%)",
  palette = "RdYlGn"
)
map_unemp


# 5. Advanced usage: Custom territorial data -----

# If you have custom territorial data with non-standard column names
custom_sf <- sf::st_read("custom_boundaries.geojson")

# Specify columns manually
map_custom <- map_territorial_units(
  custom_sf,
  name_col = "area_name",
  id_col = "area_code",
  color_by = "category",
  popup_fields = c("area_name", "category", "area_code")
)
map_custom


# 6. Different tile providers -----

# Use satellite imagery
map_satellite <- map_territorial_units(
  comuni,
  color_by = "COD_REG",
  tile_provider = "Esri.WorldImagery"
)
map_satellite

# Use minimal CartoDB basemap
map_minimal <- map_territorial_units(
  comuni,
  color_by = "COD_REG",
  tile_provider = "CartoDB.Positron"
)
map_minimal


# 7. Integration with Shiny -----

library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("Italian Territorial Units Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "color_by",
        "Color by:",
        choices = c("None" = "NULL", "Region" = "COD_REG", "Province" = "COD_UTS")
      ),
      selectInput(
        "tile_provider",
        "Base Map:",
        choices = c(
          "OpenStreetMap",
          "CartoDB.Positron",
          "Esri.WorldImagery"
        )
      )
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  comuni <- readRDS("data/comuni_map_20251014.rds")

  output$map <- renderLeaflet({
    color_var <- if (input$color_by == "NULL") NULL else input$color_by

    map_territorial_units(
      comuni,
      color_by = color_var,
      tile_provider = input$tile_provider
    )
  })
}

# Run the app
# shinyApp(ui, server)
