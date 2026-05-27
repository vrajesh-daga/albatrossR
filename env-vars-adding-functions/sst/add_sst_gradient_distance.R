# Add Distance to SST Front (Gradient-Based)
# This script calculates SST gradient magnitude given each point and finds the distance to the nearest front (a region where there is a large change in temperature).

# Method:
# 1. For each observation, extract a spatial window of SST data around it
# 2. Calculate the SST gradient magnitude (rate of temperature change)
# 3. Identify pixels with strong gradients (fronts)
# 4. Calculate distance from the bird's location to nearest front

# Load required libraries
cat("Loading required libraries...\n")
library(ncdf4)
library(dplyr)

# Define file paths
sst_folder <- path.expand("~/Downloads/SST")
input_data_path <- path.expand("~/Downloads/final_chick_data_sst.csv")
output_path <- path.expand("~/Downloads/final_chick_data_sst_and_gradient.csv")

cat("File paths set:\n")
cat(paste0("  Input data: ", input_data_path, "\n"))
cat(paste0("  SST folder: ", sst_folder, "\n"))
cat(paste0("  Output: ", output_path, "\n\n"))

# Read the dataset with SST
cat("Reading dataset with SST...\n")
chick_data <- read.csv(input_data_path, stringsAsFactors = FALSE)
chick_data$date <- as.Date(chick_data$datetime)

# Initialize distance_to_front column
chick_data$distance_to_front <- NA

cat(paste0("Successfully loaded ", nrow(chick_data), " rows\n"))
cat(paste0("Years in dataset: ", paste(sort(unique(chick_data$Year)), collapse=", "), "\n\n"))

# Define helper functions

# Function to calculate distance between two lat/lon points (Haversine formula), returns distance in kilometers
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  lat1_rad <- lat1 * pi / 180
  lat2_rad <- lat2 * pi / 180
  lon1_rad <- lon1 * pi / 180
  lon2_rad <- lon2 * pi / 180
  
  dlat <- lat2_rad - lat1_rad
  dlon <- lon2_rad - lon1_rad
  
  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  # Earth's radius in km
  R <- 6371
  distance <- R * c
  
  return(distance)
}

# Function to find nearest index
find_nearest_idx <- function(vec, value) {
  which.min(abs(vec - value))
}

# Function to calculate SST gradient and find distance to front
calculate_distance_to_front <- function(sst_data, nc_lon, nc_lat, target_lon, target_lat, 
                                        date_idx, gradient_threshold = 0.05) {
  
  # Convert longitude to 0-360
  target_lon_360 <- ifelse(target_lon < 0, target_lon + 360, target_lon)
  
  # Find nearest indices for the bird's location
  center_lat_idx <- find_nearest_idx(nc_lat, target_lat)
  center_lon_idx <- find_nearest_idx(nc_lon, target_lon_360)
  
  # Define search window size (degrees) using ~5 degrees (~500km at mid-latitudes)
  window_deg <- 5
  
  # Find lat/lon indices within window
  lat_window <- which(abs(nc_lat - target_lat) <= window_deg)
  lon_window <- which(abs(nc_lon - target_lon_360) <= window_deg | 
                      abs(nc_lon - target_lon_360) >= (360 - window_deg))
  
  # Ensure we have enough data
  if (length(lat_window) < 3 || length(lon_window) < 3) {
    return(NA)
  }
  
  # Extract SST window
  sst_window <- sst_data[lon_window, lat_window, date_idx]
  lon_subset <- nc_lon[lon_window]
  lat_subset <- nc_lat[lat_window]
  
  # Calculate gradient magnitude for each pixel
  # Gradient = sqrt((dSST/dx)^2 + (dSST/dy)^2)
  gradient_magnitude <- matrix(NA, nrow = length(lon_window), ncol = length(lat_window))
  
  for (i in 2:(length(lon_window)-1)) {
    for (j in 2:(length(lat_window)-1)) {
      
      sst_center <- sst_window[i, j]
      
      # Check if center pixel has valid data
      if (is.na(sst_center)) next
      
      # Calculate gradients in x (longitude) and y (latitude) directions
      sst_left <- sst_window[i-1, j]
      sst_right <- sst_window[i+1, j]
      sst_bottom <- sst_window[i, j-1]
      sst_top <- sst_window[i, j+1]
      
      # Skip if any neighbor is NA
      if (any(is.na(c(sst_left, sst_right, sst_bottom, sst_top)))) next
      
      # Calculate spatial derivatives (°C per degree lat/lon)
      dSST_dx <- (sst_right - sst_left) / 2
      dSST_dy <- (sst_top - sst_bottom) / 2
      
      # Gradient magnitude (°C per degree)
      gradient_magnitude[i, j] <- sqrt(dSST_dx^2 + dSST_dy^2)
    }
  }
  
  # Find pixels with strong gradients (fronts)
  front_pixels <- which(gradient_magnitude >= gradient_threshold, arr.ind = TRUE)
  
  # If no fronts found, return NA
  if (nrow(front_pixels) == 0) {
    return(NA)
  }
  
  # Calculate distance from bird to each front pixel
  min_distance <- Inf
  
  for (k in 1:nrow(front_pixels)) {
    front_lon <- lon_subset[front_pixels[k, 1]]
    front_lat <- lat_subset[front_pixels[k, 2]]
    
    # Convert front_lon back to -180 to 180 for distance calc
    if (front_lon > 180) front_lon <- front_lon - 360
    
    # Calculate distance
    dist <- haversine_distance(target_lat, target_lon, front_lat, front_lon)
    
    if (dist < min_distance) {
      min_distance <- dist
    }
  }
  
  return(min_distance)
}

# Calculate distance to front for each observation
cat("Calculating distance to SST fronts...\n")
cat("This may take several minutes...\n\n")

# Gradient threshold for defining a "front" (°C per degree) --> 0.05 °C/degree is a moderate threshold
# Stronger fronts = higher values (e.g., 0.1)
# Weaker fronts = lower values (e.g., 0.03)
gradient_threshold <- 0.05

for (year in unique(chick_data$Year)) {
  cat(paste0("\nProcessing year ", year, "...\n"))
  
  nc_file <- file.path(sst_folder, paste0("sst.day.mean.", year, ".nc"))
  
  if (!file.exists(nc_file)) {
    cat(paste0("Warning: File not found: ", nc_file, "\n"))
    next
  }
  
  # Open NetCDF file
  nc <- nc_open(nc_file)
  
  # Read dimensions
  nc_lon <- ncvar_get(nc, "lon")
  nc_lat <- ncvar_get(nc, "lat")
  nc_time <- ncvar_get(nc, "time")
  
  # Convert time to dates
  time_units <- ncatt_get(nc, "time", "units")$value
  origin_date <- as.Date(sub(".*since ", "", time_units))
  nc_dates <- origin_date + nc_time
  
  # Read ALL SST data for the year
  cat("  Loading SST data...\n")
  sst_data <- ncvar_get(nc, "sst")
  
  # Get subset of chick data for this year
  year_data <- chick_data[chick_data$Year == year, ]
  
  cat(paste0("  Processing ", nrow(year_data), " observations...\n"))
  
  for (i in 1:nrow(year_data)) {
    # Skip if SST is already NA (can't calculate gradient without SST)
    if (is.na(year_data$SST[i])) {
      next
    }
    
    # Get target values
    target_date <- year_data$date[i]
    target_lat <- year_data$latitude[i]
    target_lon <- year_data$longitude[i]
    
    # Find date index
    date_idx <- which(nc_dates == target_date)
    
    if (length(date_idx) == 0) {
      next
    }
    
    # Calculate distance to front
    dist_to_front <- calculate_distance_to_front(
      sst_data, nc_lon, nc_lat, target_lon, target_lat, 
      date_idx, gradient_threshold
    )
    
    # Store in original dataframe
    original_idx <- which(chick_data$Year == year)[i]
    chick_data$distance_to_front[original_idx] <- dist_to_front
    
    # Progress indicator
    if (i %% 100 == 0) {
      cat(paste0("    Progress: ", i, "/", nrow(year_data), "\r"))
    }
  }
  
  nc_close(nc)
  cat(paste0("\n  Completed year ", year, "\n"))
}

# Summary
cat("\n", rep("=", 60), "\n", sep="")
cat("SUMMARY\n")
cat(rep("=", 60), "\n", sep="")
cat(paste0("Total rows: ", nrow(chick_data), "\n"))
cat(paste0("Missing distance_to_front values: ", sum(is.na(chick_data$distance_to_front)), "\n"))
cat(paste0("Successfully calculated: ", sum(!is.na(chick_data$distance_to_front)), "\n\n"))

cat("Distance to Front Statistics (km):\n")
print(summary(chick_data$distance_to_front))

cat("\nSST Statistics (°C):\n")
print(summary(chick_data$SST))

# Save
cat(paste0("\nSaving to ", output_path, "...\n"))
write.csv(chick_data, output_path, row.names = FALSE)
cat("✓ Done! Dataset saved successfully.\n\n")

cat("Interpretation:\n")
cat("- distance_to_front: Distance in kilometers to nearest SST front\n")
cat("- SST front: Region where temperature changes by >=", gradient_threshold, "°C per degree\n")
cat("- Lower values = bird is closer to a thermal front\n")
cat("- Higher values = bird is in more thermally uniform water\n")
cat("- NA values = no fronts detected within ~500km or missing SST data\n")
