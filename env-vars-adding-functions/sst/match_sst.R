# Add SST Data to Chick Dataset

library(ncdf4)
library(dplyr)

# Define paths
sst_folder <- "~/Downloads/SST"
chick_data_path <- "~/Downloads/final_chick_data_pre_env_vars.csv"
output_path <- "~/Downloads/final_chick_data_sst.csv"

# Read the chick dataset
cat("Reading chick dataset...\n")
chick_data <- read.csv(chick_data_path, stringsAsFactors = FALSE)
chick_data$date <- as.Date(chick_data$datetime)
chick_data$SST <- NA

# Function to find nearest index
find_nearest_idx <- function(vec, value) {
  which.min(abs(vec - value))
}

# Process each year
cat("Processing SST data by year...\n")

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
  
  cat(paste0("  Matching ", nrow(year_data), " observations...\n"))
  
  # Vectorized matching
  for (i in 1:nrow(year_data)) {
    # Get target values
    target_date <- year_data$date[i]
    lat <- year_data$latitude[i]
    lon <- year_data$longitude[i]
    
    # Convert longitude to 0-360
    lon_360 <- ifelse(lon < 0, lon + 360, lon)
    
    # Find indices
    date_idx <- which(nc_dates == target_date)
    
    if (length(date_idx) > 0) {
      lat_idx <- find_nearest_idx(nc_lat, lat)
      lon_idx <- find_nearest_idx(nc_lon, lon_360)
      
      # Extract SST
      sst_value <- sst_data[lon_idx, lat_idx, date_idx]
      
      # Store in original dataframe using the actual row index
      original_idx <- which(chick_data$Year == year)[i]
      chick_data$SST[original_idx] <- sst_value
    }
    
    # Progress
    if (i %% 500 == 0) {
      cat(paste0("  Progress: ", i, "/", nrow(year_data), "\r"))
    }
  }
  
  nc_close(nc)
  cat(paste0("\n  Completed year ", year, "\n"))
}

# Summary
cat("\n" , rep("=", 50), "\n", sep="")
cat("SUMMARY\n")
cat(rep("=", 50), "\n", sep="")
cat(paste0("Total rows: ", nrow(chick_data), "\n"))
cat(paste0("Missing SST values: ", sum(is.na(chick_data$SST)), "\n"))
cat(paste0("Successfully matched: ", sum(!is.na(chick_data$SST)), "\n"))
cat("\nSST Statistics:\n")
print(summary(chick_data$SST))

# Save
cat(paste0("\nSaving to ", output_path, "...\n"))
write.csv(chick_data, output_path, row.names = FALSE)
cat("✓ Done! Dataset saved successfully.\n")
