# Add Wind Data (u10, v10)
# Source: ERA5 monthly wind data (NetCDF, one file per year)

# Methodology
# U and V wind components are extracted from ERA5 reanalysis monthly NetCDF files for each bird observation by matching to the nearest grid point in space and the corresponding year-month.

# Wind components:
#   wind_u : U-component of 10m wind (eastward, m/s)
#   wind_v : V-component of 10m wind (northward, m/s)

# Spatial matching: nearest-neighbor to ERA5 grid
# Temporal matching: year + month (ERA5 files are monthly means)

# Input  : final_chick_data_pre_env_vars.csv
# Output : final_chick_data_with_wind.csv
#   New columns: wind_u, wind_v

library(ncdf4)

# Define file paths
wind_dir    <- path.expand("~/Downloads/wind")
input_path  <- path.expand("~/Downloads/Final Outputs/final_chick_data_pre_env_vars.csv")
output_path <- path.expand("~/Downloads/final_chick_data_with_wind.csv")

cat("File paths:\n")
cat(paste0("  Wind directory : ", wind_dir, "\n"))
cat(paste0("  Input data     : ", input_path, "\n"))
cat(paste0("  Output         : ", output_path, "\n\n"))

if (!dir.exists(wind_dir))   stop("Wind directory not found: ", wind_dir)
if (!file.exists(input_path)) stop("Input CSV not found: ", input_path)

# Helper functions

# Find wind NetCDF file for a given year
wind_nc_path <- function(year, wind_dir) {
  path <- file.path(wind_dir, paste0("wind_", year, ".nc"))
  if (file.exists(path)) path else NULL
}

# Convert -180/180 longitude to 0/360 if needed to match ERA5 grid
lon_to_360 <- function(lon) {
  ifelse(lon < 0, lon + 360, lon)
}

# Find nearest index in a vector
nearest_idx <- function(vec, value) {
  which.min(abs(vec - value))
}

# Load chick data
cat("Reading chick dataset...\n")
chick_data          <- read.csv(input_path, stringsAsFactors = FALSE)
chick_data$datetime <- as.Date(chick_data$datetime)
chick_data$wind_u   <- NA_real_
chick_data$wind_v   <- NA_real_
n <- nrow(chick_data)
cat(paste0("Loaded ", n, " observations\n\n"))

# Extract wind values year by year
cat("Extracting wind values...\n")

years         <- sort(unique(format(chick_data$datetime, "%Y")))
missing_years <- c()

for (yr in years) {
  year     <- as.integer(yr)
  nc_path  <- wind_nc_path(year, wind_dir)

  if (is.null(nc_path)) {
    missing_years <- c(missing_years, year)
    cat(paste0("  ⚠ Missing file: wind_", year, ".nc — rows left as NA\n"))
    next
  }

  cat(paste0("  Processing year ", year, "...\n"))

  nc      <- nc_open(nc_path)

  # Read coordinate axes
  nc_lon  <- ncvar_get(nc, "longitude")
  nc_lat  <- ncvar_get(nc, "latitude")
  nc_time <- ncvar_get(nc, "valid_time")

  # Handle both "valid_time" and "time" dimension names
  time_var <- if ("valid_time" %in% names(nc$var) ||
                  "valid_time" %in% names(nc$dim)) "valid_time" else "time"
  nc_time  <- ncvar_get(nc, time_var)

  # ERA5 time is seconds since 1970-01-01
  nc_dates <- as.Date(as.POSIXct(nc_time, origin = "1970-01-01", tz = "UTC"))

  # Subset to this year
  year_mask <- format(chick_data$datetime, "%Y") == yr
  year_rows <- which(year_mask)

  for (i in year_rows) {
    obs_date <- chick_data$datetime[i]
    obs_lat  <- chick_data$latitude[i]
    obs_lon  <- chick_data$longitude[i]

    # Match to year-month (monthly means)
    obs_ym   <- format(obs_date, "%Y-%m")
    nc_ym    <- format(nc_dates, "%Y-%m")
    time_idx <- which(nc_ym == obs_ym)

    if (length(time_idx) == 0) next

    # Match ERA5 longitude convention
    obs_lon_360 <- lon_to_360(obs_lon)

    # Nearest spatial indices
    lon_idx <- nearest_idx(nc_lon, obs_lon_360)
    lat_idx <- nearest_idx(nc_lat, obs_lat)

    # Extract u10 and v10
    chick_data$wind_u[i] <- tryCatch(
      ncvar_get(nc, "u10",
                start = c(lon_idx, lat_idx, time_idx[1]),
                count = c(1, 1, 1)),
      error = function(e) NA_real_
    )

    chick_data$wind_v[i] <- tryCatch(
      ncvar_get(nc, "v10",
                start = c(lon_idx, lat_idx, time_idx[1]),
                count = c(1, 1, 1)),
      error = function(e) NA_real_
    )
  }

  nc_close(nc)
  cat(paste0("    Done — ",
             sum(!is.na(chick_data$wind_u[year_rows])),
             " / ", length(year_rows), " values extracted\n"))
}

# Summary
cat("\n", rep("=", 60), "\n", sep = "")
cat("SUMMARY\n")
cat(rep("=", 60), "\n", sep = "")
cat(paste0("Total observations     : ", n, "\n"))
cat(paste0("wind_u/v populated     : ", sum(!is.na(chick_data$wind_u)), "\n"))
cat(paste0("NA (missing files etc) : ", sum(is.na(chick_data$wind_u)), "\n"))

if (length(missing_years) > 0) {
  cat(paste0("\nMissing wind files for years: ",
             paste(missing_years, collapse = ", "), "\n"))
  cat(paste0("Place files named wind_YYYY.nc in: ", wind_dir, "\n"))
}

cat("\nwind_u statistics (m/s):\n")
print(summary(chick_data$wind_u))
cat("\nwind_v statistics (m/s):\n")
print(summary(chick_data$wind_v))

# Save
cat(paste0("\nSaving to: ", output_path, "\n"))
write.csv(chick_data, output_path, row.names = FALSE)
cat("✓ Done! final_chick_data_with_wind.csv saved successfully.\n")
cat("\nColumns added:\n")
cat("  wind_u : U-component of 10m wind (eastward, m/s) — ERA5 monthly\n")
cat("  wind_v : V-component of 10m wind (northward, m/s) — ERA5 monthly\n")
