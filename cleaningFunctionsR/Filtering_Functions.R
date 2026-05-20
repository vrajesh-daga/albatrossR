library(dplyr)
library(lubridate)
library(tidyr)
library(sf)
library(rnaturalearth)

#' Bird Filtering Pipeline
#' @param data Input dataframe (expects Tag, datetime, type, correct_step_distance)
#' @param start_dates_vec A character vector of fledge dates
#' @param id_col The name of your ID column (unquoted)
#' @return A filtered dataframe excluding irrelevant dates, calculated velocities, equinoxes, extreme latitudes, and land-proximate points



process_bird_data <- function(data, start_dates_vec, id_col) {

  # 1. Map Start Dates to Tags
  tag_ids <- data %>% select({{ id_col }}) %>% distinct() %>% pull() %>% sort()

  date_mapping <- data.frame(
    temp_ids = tag_ids,
    Start_Date = mdy(start_dates_vec)
  )

  data %>%
    # 2. Extract Temporal Columns & Parse Dates
    mutate(
      date_parsed = parse_date_time(datetime, orders = c("ymd HMS", "ymd", "mdy HMS", "mdy")), #Accounts for different DDMMYY formats
      Month = month(date_parsed),
      Day   = day(date_parsed),
      Year  = year(date_parsed)
    ) %>%

    # 3. Filter based on the Fledge Date Mapping
    left_join(date_mapping, by = setNames("temp_ids", as_label(enquo(id_col)))) %>%
    filter(date_parsed >= Start_Date) %>%

    # 4. Calculate Velocities
    group_by({{ id_col }}) %>%
    arrange(date_parsed, .by_group = TRUE) %>%
    mutate(
      # Create continuous timestamp using 12h offset for noon
      timestamp = date_parsed + ifelse(type == "noon", hours(12), hours(0)),

      # Calculate hours between fixes
      time_diff = as.numeric(difftime(timestamp, lag(timestamp), units = "hours")),

      # LOGIC: If exactly ~12 hours, divide by 12.
      # If NA (first record) or any other gap (24h, etc), divide by 24.
      divisor = ifelse(!is.na(time_diff) & abs(time_diff - 12) < 0.1, 12, 24),

      velocities = correct_step_distance / divisor
    ) %>%
    filter(correct_step_distance <= 960, velocities <= 80) %>%

    ungroup() %>%
    # Optional: remove helper columns to keep it clean
    select(-timestamp, -time_diff, -divisor)
}


apply_geospatial_filters <- function(data) {

  # --- 3. EQUINOX FILTER ---
  # Filters ~Mar 10-30 and ~Sept 12-Oct 1
  data <- data %>%
    filter(!(
      (Month == 3 & Day >= 10 & Day <= 30) |
        (Month == 9 & Day >= 12) |
        (Month == 10 & Day <= 1)
    ))

  # --- 4. LATITUDE FILTER ---
  # Remove points north of 65N
  data <- data %>%
    filter(latitude <= 65)

  # --- 5. LAND MASS FILTER (>50km offshore) ---
  # A. Convert to spatial object (sf)
  # WGS84 (EPSG:4326) is the standard for lat/long
  gdf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

  # B. Get land data (Natural Earth)
  world <- ne_countries(scale = 110, returnclass = "sf") %>%
    filter(continent != "Antarctica")

  # C. Project both to Robinson (ESRI:54030) for metric buffering (meters)
  # Robinson is great for global datasets to minimize distance distortion
  robinson_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  gdf_proj <- st_transform(gdf, robinson_crs)
  land_proj <- st_transform(world, robinson_crs)

  # D. Validate and combine land masses
  land_combined <- land_proj %>%
    st_make_valid() %>%
    st_union()

  # E. Create 50km (50,000m) buffer
  land_buffer <- st_buffer(land_combined, 50000)

  # F. Filter points: Keep only those NOT within the land buffer
  # st_within returns a list of indices; we check if the list is empty for each point
  is_over_land <- st_within(gdf_proj, land_buffer, sparse = FALSE)

  # Convert back to regular dataframe and return
  final_df <- data[!as.vector(is_over_land), ]

  return(final_df)
}

#
# --- Pipeline Execution ---
# result <- df %>%
#   process_bird_data(start_dates) %>% # From previous turn
#   apply_geospatial_filters()

# write.csv(result, "Final_Filtered_Chick_Data.csv", row.names = FALSE)

df <- read.csv("mergedchickdata_o7_2013-2014_.csv")

startDates <- c('6/26/2014', '7/17/2013', '7/7/2013', '7/13/2013', '06/28/2009',
'7/01/2009','6/25/2009','7/10/2009', '7/10/2009', '7/12/2009', '7/17/2009', '7/7/2010', '7/9/2013', '07/11/2013') #Required

dfPipe <- df %>%
  process_bird_data(startDates, Tag) %>% #Tag or Bird ID column name
  apply_geospatial_filters()
write.csv(dfPipe, "all_chick_data_final_filter.csv", row.names = FALSE)

