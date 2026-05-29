# Add MPA (Marine Protected Area) Columns to Chick Dataset
# Source: WDPA/WDOECM May 2026 Public Marine Dataset (World Database on Protected Areas)

# METHODOLOGY
# This script assigns a Marine Protected Area (MPA) status to each seabird GPS observation using a spatial point-in-polygon join.

# The dataset is split across three shapefiles (_0, _1, _2) due to file size constraints; this script reads all three and combines them before joining.

# Steps:
#   1. The three WDPA polygon shapefiles are loaded, geometries validated,
#      and combined into a single spatial layer.
#   2. Bird GPS coordinates are converted to a spatial points object using
#      the WGS84 coordinate reference system (EPSG:4326).
#   3. A spatial point-in-polygon join (st_within) is performed, testing
#      each observation against all MPA polygons simultaneously.
#   4. Where a point falls inside multiple overlapping MPA polygons, the
#      largest MPA by area is retained (one row per observation).
#   5. Points falling inside a polygon are assigned that MPA's name and
#      IUCN protection category. Points outside all polygons are assigned
#      "No MPA" and "No MPA" respectively.
#
# Input  : final_chick_data_pre_env_vars.csv
# Output : final_chick_data_mpa.csv
#   New columns:
#     mpa        — MPA name (WDPA NAME field) or "No MPA"
#     mpa_status — IUCN protection category (e.g. Ia, II, VI) or "No MPA"

library(sf)
library(dplyr)

sf_use_s2(FALSE)

# File paths
wdpa_base   <- path.expand("~/Downloads")
input_path  <- path.expand("~/Downloads/final_chick_data_pre_env_vars.csv")
output_path <- path.expand("~/Downloads/final_chick_data_mpa.csv")

shp_files <- c(
  file.path(wdpa_base, "WDPA_WDOECM_May2026_Public_marine_shp_0",
            "WDPA_WDOECM_May2026_Public_marine_shp-polygons.shp"),
  file.path(wdpa_base, "WDPA_WDOECM_May2026_Public_marine_shp_1",
            "WDPA_WDOECM_May2026_Public_marine_shp-polygons.shp"),
  file.path(wdpa_base, "WDPA_WDOECM_May2026_Public_marine_shp_2",
            "WDPA_WDOECM_May2026_Public_marine_shp-polygons.shp")
)

cat("Checking shapefiles exist:\n")
for (f in shp_files) {
  exists <- file.exists(f)
  cat(paste0("  ", ifelse(exists, "✓", "✗"), " ", basename(dirname(f)), "\n"))
  if (!exists) stop("File not found: ", f)
}
cat("\n")

if (!file.exists(input_path)) stop("Input CSV not found: ", input_path)

# Combine all WDPA shapefiles
cat("Loading WDPA shapefiles (this may take a minute)...\n")

mpa_parts <- lapply(seq_along(shp_files), function(i) {
  cat(paste0("  Reading part ", i, " of 3...\n"))
  part <- st_read(shp_files[i], quiet = TRUE)
  cat(paste0("    Loaded ", nrow(part), " polygons\n"))
  part
})

cat("Combining all three parts...\n")
mpa <- do.call(rbind, mpa_parts)
cat(paste0("Total MPA polygons: ", nrow(mpa), "\n"))

cat("Validating geometries...\n")
mpa <- st_make_valid(mpa)
cat("Geometry validation complete\n\n")

# Read chick data and convert to spatial points
cat("Reading chick dataset...\n")
chick_data <- read.csv(input_path, stringsAsFactors = FALSE)
n <- nrow(chick_data)
cat(paste0("Loaded ", n, " observations\n\n"))

cat("Converting observations to spatial points...\n")
chick_sf <- st_as_sf(chick_data,
                     coords = c("longitude", "latitude"),
                     crs    = 4326,
                     remove = FALSE)

chick_sf$row_id <- 1:n
mpa <- st_transform(mpa, crs = st_crs(chick_sf))

# Spatial join (point in polygon)
cat("Running spatial join (point-in-polygon)...\n")
cat("Matching each GPS point to any MPA polygon it falls inside...\n\n")

start_time <- proc.time()
joined <- st_join(chick_sf, mpa, join = st_within, left = TRUE)
elapsed <- round((proc.time() - start_time)["elapsed"], 1)

cat(paste0("Spatial join complete in ", elapsed, " seconds.\n"))
cat(paste0("Rows before dedup: ", nrow(joined),
           " (", nrow(joined) - n, " points fell in overlapping MPAs)\n\n"))

# Deduplicate (one row per original observation) because some points fall inside multiple overlapping MPA polygons.
# This produces duplicate rows. This is resolved by keeping MPA with largest reported area (REP_AREA field) for each observation.
cat("Deduplicating (keeping largest MPA per point where overlaps exist)...\n")

joined_df <- st_drop_geometry(joined)

if ("REP_AREA" %in% names(joined_df)) {
  joined_df <- joined_df[order(joined_df$row_id,
                                -as.numeric(joined_df$REP_AREA),
                                na.last = TRUE), ]
} else {
  joined_df <- joined_df[order(joined_df$row_id), ]
}

joined_dedup <- joined_df[!duplicated(joined_df$row_id), ]
joined_dedup <- joined_dedup[order(joined_dedup$row_id), ]

cat(paste0("Rows after dedup: ", nrow(joined_dedup), " ✓\n\n"))

# Build output — mpa name + mpa_status (IUCN category)
chick_out <- chick_data

chick_out$mpa        <- as.character(joined_dedup[["NAME"]])
chick_out$mpa_status <- as.character(joined_dedup[["IUCN_CAT"]])

chick_out$mpa[is.na(chick_out$mpa)]               <- "No MPA"
chick_out$mpa_status[is.na(chick_out$mpa_status)] <- "No MPA"

# Summary
cat(rep("=", 60), "\n", sep = "")
cat("SUMMARY\n")
cat(rep("=", 60), "\n", sep = "")
cat(paste0("Total observations    : ", nrow(chick_out), "\n"))
cat(paste0("Inside an MPA         : ", sum(chick_out$mpa != "No MPA"), "\n"))
cat(paste0("Outside any MPA       : ", sum(chick_out$mpa == "No MPA"), "\n"))
cat(paste0("% inside MPA          : ",
           round(100 * sum(chick_out$mpa != "No MPA") / nrow(chick_out), 1), "%\n"))

cat("\nIUCN category breakdown:\n")
print(sort(table(chick_out$mpa_status), decreasing = TRUE))

cat("\nTop MPAs by observation count:\n")
print(head(sort(table(chick_out$mpa[chick_out$mpa != "No MPA"]), decreasing = TRUE), 15))

# Save
cat(paste0("\nSaving to: ", output_path, "\n"))
write.csv(chick_out, output_path, row.names = FALSE)
cat("✓ Done! final_chick_data_mpa.csv saved successfully.\n")
cat("\nColumns added:\n")
cat("  mpa        : MPA name or 'No MPA'\n")
cat("  mpa_status : IUCN protection category or 'No MPA'\n")
cat("               (Ia=Strict Reserve, II=National Park, VI=Sustainable Use, etc.)\n")
