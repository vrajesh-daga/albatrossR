# Add EEZ (Exclusive Economic Zone) Column to Chick Dataset
# Source: World EEZ v12 (Flanders Marine Institute, October 2023)

# Methodology
# This script assigns an Exclusive Economic Zone (EEZ) label to each seabird GPS observation using a spatial point-in-polygon join.
# Observations that fall outside any national EEZ (i.e. 200 nautical miles past a coastline) are classified as "High Seas" (international waters).

# Steps:
#   1. The World EEZ v12 shapefile is loaded and geometries are validated to resolve any topological inconsistencies in the source data.
#   2. Bird GPS coordinates are converted to a spatial points object using the WGS84 coordinate reference system (EPSG:4326).
#   3. A spatial point-in-polygon join (st_within) is performed, testing each observation against all EEZ polygons simultaneously.
#   4. Points falling inside a polygon are assigned that EEZ's full descriptive name (GEONAME field). Points outside all polygons are assigned "High Seas".
#
# Input  : final_chick_data_pre_env_vars.csv
# Output : final_chick_data_eez.csv
# New column: eez — EEZ name (e.g. "United States Exclusive Economic Zone") or "High Seas" if in international waters

library(sf)
library(dplyr)

# Disable S2 spherical geometry engine, use GEOS planar engine instead to handle the EEZ shapefile's complex polygon boundaries correctly
sf_use_s2(FALSE)

# Define File paths
eez_folder  <- path.expand("~/Downloads/World_EEZ_v12_20231025")
eez_file    <- file.path(eez_folder, "eez_v12.shp")
input_path  <- path.expand("~/Downloads/final_chick_data_pre_env_vars.csv")
output_path <- path.expand("~/Downloads/final_chick_data_eez.csv")

cat("File paths:\n")
cat(paste0("  EEZ shapefile : ", eez_file, "\n"))
cat(paste0("  Input data    : ", input_path, "\n"))
cat(paste0("  Output        : ", output_path, "\n\n"))

if (!file.exists(eez_file))   stop("EEZ shapefile not found: ", eez_file)
if (!file.exists(input_path)) stop("Input CSV not found: ", input_path)

# Load EEZ shapefile and repair geometries
cat("Loading EEZ shapefile...\n")
eez <- st_read(eez_file, quiet = TRUE)
cat(paste0("Loaded ", nrow(eez), " EEZ polygons\n"))

cat("Validating geometries...\n")
eez <- st_make_valid(eez)
cat("Geometry validation complete\n\n")

# Read chick data and convert to spatial points
cat("Reading chick dataset...\n")
chick_data <- read.csv(input_path, stringsAsFactors = FALSE)
cat(paste0("Loaded ", nrow(chick_data), " observations\n\n"))

# Convert to sf spatial points object using WGS84 (EPSG:4326)
cat("Converting observations to spatial points...\n")
chick_sf <- st_as_sf(chick_data,
                     coords = c("longitude", "latitude"),
                     crs    = 4326,
                     remove = FALSE)   # retain original lat/lon columns

# Ensure CRS matches the EEZ shapefile
eez <- st_transform(eez, crs = st_crs(chick_sf))

# Spatial join — point in polygon
# st_within tests whether each point falls entirely within an EEZ polygon.
# left = TRUE retains all bird observations, including those outside any EEZ.
cat("Running spatial join (point-in-polygon)...\n")
cat("This matches each GPS point to the EEZ polygon it falls inside.\n\n")

start_time <- proc.time()

joined <- st_join(chick_sf, eez, join = st_within, left = TRUE)

elapsed <- round((proc.time() - start_time)["elapsed"], 1)
cat(paste0("Spatial join complete in ", elapsed, " seconds.\n\n"))

# Extract EEZ name and build output
chick_out      <- chick_data
chick_out$eez  <- as.character(st_drop_geometry(joined)[["GEONAME"]])

# Assign "High Seas" to points outside any national EEZ
chick_out$eez[is.na(chick_out$eez)] <- "High Seas"

# Summary
cat(rep("=", 60), "\n", sep = "")
cat("SUMMARY\n")
cat(rep("=", 60), "\n", sep = "")
cat(paste0("Total observations  : ", nrow(chick_out), "\n"))
cat(paste0("Inside an EEZ       : ", sum(chick_out$eez != "High Seas"), "\n"))
cat(paste0("High Seas (no EEZ)  : ", sum(chick_out$eez == "High Seas"), "\n"))

cat("\nTop EEZ zones by observation count:\n")
print(head(sort(table(chick_out$eez), decreasing = TRUE), 15))

# Save
cat(paste0("\nSaving to: ", output_path, "\n"))
write.csv(chick_out, output_path, row.names = FALSE)
cat("✓ Done! final_chick_data_eez.csv saved successfully.\n")
cat("\nColumn added:\n")
cat("  eez : EEZ name (GEONAME) or 'High Seas' if in international waters\n")
