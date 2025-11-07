devtools::load_all()
library(data.table)
library(readxl)
library(janitor)
library(dplyr)
library(sf)


#  leggi la tavola dei CPI. ----

excel_sheets("../../progetti/datasets/Co_standards/Rev.005-ST-Classificazioni-Standard-1.xlsx")
cpi <- read_excel("../../progetti/datasets/Co_standards/Rev.005-ST-Classificazioni-Standard-1.xlsx"
                 , sheet = 4
                 , skip = 12
                 , col_types = c("text"
                                 , "skip"
                                 , "skip"
                                 , "text"
                                 , "skip"
                                 , "skip"
                                 , "text"
                                 , "guess"
                                 , "guess"
                                 , "skip"
                                 )
                 , .name_repair = "universal"
                 ) |> setDT()


cpi_den <- read_excel("../../progetti/datasets/Co_standards/Rev.005-ST-Classificazioni-Standard-1.xlsx"
                  , sheet = 3
                  , skip = 13
                  , .name_repair = "universal"
) |> setDT()

cpi_den <- cpi_den[, .(cpi = Codice.Univoco.CPI...NVARCHAR.11.., denominazione = Denominazione...NVARCHAR.255..)]

cpi[, PRO_COM_T := excel_numeric_to_date(as.numeric(Data.Fine.Validità...DATE.), date_system = "modern")]
cpi[, PRO_COM_T := gsub("'", "" ,Codice.ISTAT.Comune..numerico.)]
cpi[nchar(PRO_COM_T) < 6, PRO_COM_T := paste0("0", PRO_COM_T)]

str(cpi)


names(cpi)

# dettagli del report 61, comuni

situas::get_report_details(447)

metadati <- situas_reports_metadata

comuni <- get_situas_tables(447, force_refresh = F, date = as.Date("2025-01-01"))


# 0. Extract shapefiles once (recommended for better performance)
# source("data-raw/extract_shapefiles.R")

# tavole <- situas::list_available_reports()

# 1. Prepare map files
files <- prepare_comuni_maps(pfun = 61
                             , situas_data = comuni
                             , date = as.Date("2025-01-01")
                             , tolerance = 0.0001
                             )

# 2. Load the data
comuni_map <- readRDS(files$rds)
comuni_map <- merge(comuni_map, cpi[, .(PRO_COM_T, cpi = Codice.Univoco.CPI)])
comuni_map <- merge(comuni_map, cpi_den)
comuni_lom_map <- subset(comuni_map,  DEN_REG == "Lombardia")

saveRDS(comuni_lom_map, "data/comuni_lom_map.rds")


cpi_lom_map <- comuni_lom_map %>%
  group_by(cpi, denominazione) %>%
  summarise(
     # .by = geometry, # Specify that the summarization should happen by geometry
    geometry = st_union(geometry) # Perform the union on the geometry
)

saveRDS(cpi_lom_map, "data/cpi_lom_map.rds")

# 3. Create an interactive map
map <- map_territorial_units(cpi_lom_map
                             , name_col =  "denominazione"
                             , id_col = "cpi"
                             , color_by = "cpi" 
                             , add_legend = FALSE
                             , tile_provider = "CartoDB.Positron"
                             )
map  # Display in RStudio viewer or browser

sf_to_powerbi_topojson(
  sf_data = cpi_lom_map,
  file = "data/cpi_lom_map_not.json",
  id_col = "cpi",
  simplify = F
  
)

library(rmapshaper)

# To convert GeoJSON to TopoJSON with rmapshaper, use apply_mapshaper_commands
# First read the GeoJSON
geojson_obj <- geojsonio::geojson_read("data/cpi_lom_map_not.json", what = "sp")

# Apply mapshaper command to convert to TopoJSON with quantization
topojson_text <- rmapshaper::apply_mapshaper_commands(
  geojson_obj,
  command = "-o format=topojson quantization=1e5"
)

# Write to file
writeLines(topojson_text, "data/cpi_lom_map_fixed.json")



#  get a report -----


curl::curl_download("https://situas-servizi.istat.it/publish/reportspooljson?pfun=447&pdata=31/12/2021", destfile = "data/aree_funzionali.json")
are_funzionali <- jsonlite::fromJSON("data/aree_funzionali.json")$resultset
