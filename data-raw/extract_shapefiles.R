# Extract ISTAT Shapefiles from Zip Archive
#
# This script extracts the shapefiles from Limiti01012025_g.zip once,
# so they don't need to be extracted every time prepare_comuni_maps() is called.
#
# Run this script once after downloading the zip file:
# source("data-raw/extract_shapefiles.R")

# Check if zip file exists
zip_path <- "data-raw/Limiti01012025_g.zip"

if (!file.exists(zip_path)) {
  stop("Zip file not found: ", zip_path)
}

# The zip contains directories: Com01012025_g, ProvCM01012025_g, Reg01012025_g, RipGeo01012025_g
output_dir <- "data-raw"
marker_dir <- "data-raw/Com01012025_g"  # Check for municipalities directory

if (dir.exists(marker_dir)) {
  message("Shapefiles already extracted")
  message("Found: ", marker_dir)
  response <- readline(prompt = "Extract again and overwrite? (y/n): ")
  if (tolower(response) != "y") {
    message("Extraction cancelled")
    quit(save = "no")
  }
  # Remove existing directories
  to_remove <- c(
    "data-raw/Com01012025_g",
    "data-raw/ProvCM01012025_g",
    "data-raw/Reg01012025_g",
    "data-raw/RipGeo01012025_g"
  )
  for (dir in to_remove) {
    if (dir.exists(dir)) {
      unlink(dir, recursive = TRUE)
    }
  }
}

# Extract zip file
message("Extracting shapefiles from: ", zip_path)
message("Output directory: ", output_dir)

utils::unzip(
  zip_path,
  exdir = output_dir,
  junkpaths = FALSE
)

# Verify extraction
if (dir.exists(marker_dir)) {
  # List all extracted directories
  dirs <- c("Com01012025_g", "ProvCM01012025_g", "Reg01012025_g", "RipGeo01012025_g")
  message("\nSuccessfully extracted directories:")

  for (dir_name in dirs) {
    full_path <- file.path(output_dir, dir_name)
    if (dir.exists(full_path)) {
      files <- list.files(full_path)
      shp_files <- files[grepl("\\.shp$", files)]
      message("  - ", dir_name, "/")
      if (length(shp_files) > 0) {
        message("    Shapefile: ", shp_files[1])
      }
    }
  }

  message("\nExtraction complete!")
  message("The prepare_comuni_maps() function will now use these extracted files.")
  message("\nExtracted directories are excluded from package builds via .Rbuildignore")
} else {
  stop("Extraction failed - ", marker_dir, " not found")
}
