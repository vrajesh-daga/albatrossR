# R Script to Add Bathymetry Data to Chick Dataset
# Source: GEBCO 2024 Sub-Ice Topo (GEBCO_2024_sub_ice_topo.nc)

library(ncdf4)

# File paths
gebco_file  <- path.expand("~/Downloads/GEBCO_2024_sub_ice_topo.nc")
input_path  <- path.expand("~/Downloads/final_chick_data_pre_env_vars.csv")
output_path <- path.expand("~/Downloads/final_chick_data_bathymetry.csv")

cat("File paths:\n")
cat(paste0("  GEBCO file : ", gebco_file, "\n"))
cat(paste0("  Input data : ", input_path, "\n"))
cat(paste0("  Output     : ", output_path, "\n\n"))

if (!file.exists(gebco_file)) stop("GEBCO file not found: ", gebco_file)
if (!file.exists(input_path)) stop("Input CSV not found: ", input_path)

# STEP 2: Read chick data
cat("Reading chick dataset...\n")
chick_data <- read.csv(input_path, stringsAsFactors = FALSE)
chick_data$bathymetry <- NA_real_
n <- nrow(chick_data)
cat(paste0("Loaded ", n, " observations\n\n"))

# STEP 3: Open GEBCO and read axis vectors only
cat("Opening GEBCO file...\n")
nc <- nc_open(gebco_file, readunlim = FALSE)

nc_lon <- ncvar_get(nc, "lon")  # 86400 values, -179.998 to 179.998
nc_lat <- ncvar_get(nc, "lat")  # 43200 values,  -89.998 to  89.998

cat(paste0("  Lon: ", nc_lon[1], " to ", nc_lon[length(nc_lon)],
           " (", length(nc_lon), " points)\n"))
cat(paste0("  Lat: ", nc_lat[1], " to ", nc_lat[length(nc_lat)],
           " (", length(nc_lat), " points)\n\n"))

# Pre-compute all grid indices (vectorized)
cat("Computing grid indices...\n")

lon_res <- (max(nc_lon) - min(nc_lon)) / (length(nc_lon) - 1)
lat_res <- (max(nc_lat) - min(nc_lat)) / (length(nc_lat) - 1)

# GEBCO is natively -180 to 180, same as your data — no conversion needed
lon_idx <- round((chick_data$longitude - min(nc_lon)) / lon_res) + 1
lat_idx <- round((chick_data$latitude  - min(nc_lat)) / lat_res) + 1

# Clamp to valid range (handles any edge cases at boundaries)
lon_idx <- pmax(1, pmin(length(nc_lon), lon_idx))
lat_idx <- pmax(1, pmin(length(nc_lat), lat_idx))

cat(paste0("  lon_idx range: [", min(lon_idx), ", ", max(lon_idx), "]\n"))
cat(paste0("  lat_idx range: [", min(lat_idx), ", ", max(lat_idx), "]\n\n"))

# Extract bathymetry one point at a time
# signedbyte = FALSE handles GEBCO's 16-bit short integer type
cat("Extracting bathymetry values...\n")
cat("(expect ~10-30 minutes for 14,000 reads from a 7.5 GB file)\n\n")

start_time <- proc.time()

for (i in 1:n) {

  chick_data$bathymetry[i] <- tryCatch({
    ncvar_get(nc, "elevation",
              start      = c(lon_idx[i], lat_idx[i]),
              count      = c(1, 1),
              signedbyte = FALSE)          # <-- critical fix
  }, error = function(e) NA_real_)

  # Progress every 500 rows
  if (i %% 500 == 0 || i == n) {
    elapsed <- round((proc.time() - start_time)["elapsed"], 1)
    pct     <- round(100 * i / n, 1)
    eta     <- if (i > 0) round((elapsed / i) * (n - i), 0) else "?"
    cat(paste0("  ", i, "/", n, " (", pct, "%) | ",
               elapsed, "s elapsed | ~", eta, "s remaining\r"))
    flush.console()
  }
}

nc_close(nc)
elapsed_total <- round((proc.time() - start_time)["elapsed"], 1)
cat(paste0("\n\nExtraction complete in ", elapsed_total, " seconds.\n"))

# STEP 6: Validation
cat("\n", rep("=", 60), "\n", sep = "")
cat("SUMMARY\n")
cat(rep("=", 60), "\n", sep = "")
cat(paste0("Total observations     : ", n, "\n"))
cat(paste0("Successfully extracted : ", sum(!is.na(chick_data$bathymetry)), "\n"))
cat(paste0("NA / failed            : ", sum(is.na(chick_data$bathymetry)), "\n"))

cat("\nBathymetry statistics (meters):\n")
print(summary(chick_data$bathymetry))

# Depth zone breakdown
bathy <- chick_data$bathymetry[!is.na(chick_data$bathymetry)]
cat("\nDepth zone breakdown:\n")
cat(paste0("  Above sea level (> 0m)        : ", sum(bathy >    0), " obs\n"))
cat(paste0("  Coastal/shelf   (0 to -200m)  : ", sum(bathy <= 0   & bathy > -200),  " obs\n"))
cat(paste0("  Slope           (-200 to -1000m): ", sum(bathy <= -200  & bathy > -1000), " obs\n"))
cat(paste0("  Deep sea        (-1000 to -3000m): ", sum(bathy <= -1000 & bathy > -3000), " obs\n"))
cat(paste0("  Abyssal         (< -3000m)    : ", sum(bathy <= -3000), " obs\n"))

# Flag above sea level
on_land <- chick_data[!is.na(chick_data$bathymetry) & chick_data$bathymetry > 0,
                      c("Tag", "datetime", "latitude", "longitude", "bathymetry")]
if (nrow(on_land) > 0) {
  cat(paste0("\nNote: ", nrow(on_land),
             " observation(s) above sea level (island/land/coastline):\n"))
  print(on_land)
}

# STEP 7: Save
cat(paste0("\nSaving to: ", output_path, "\n"))
write.csv(chick_data, output_path, row.names = FALSE)
cat("✓ Done! final_chick_data_bathymetry.csv saved successfully.\n")
cat("\nColumn added:\n")
cat("  bathymetry : Seafloor elevation in meters (GEBCO 2024, 15 arc-sec)\n")
cat("               Negative = below sea level | Positive = above sea level\n")
