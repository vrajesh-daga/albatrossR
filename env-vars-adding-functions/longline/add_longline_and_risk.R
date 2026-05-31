# Add longline effort and risk index

# METHODOLOGY
# Monthly longline fishing effort is assigned to each bird observation by matching to a 5°x5° spatial grid cell and year/month.

# Effort is defined as the total number of hooks set across all vessels and flag states within each 5°x5° cell for the corresponding year and month.

# Two complementary datasets cover the full Pacific:
#   1. WCPFC (Western & Central Pacific Fisheries Commission)
#      HHOOKS column is in hundred hooks — converted to hooks (x100)
#   2. IATTC (Inter-American Tropical Tuna Commission, Eastern Pacific)
#      Hooks column is in actual hooks
#      TunaBillfish and Shark files report hooks for same vessels;
#      max per cell/month is taken to avoid double-counting

# WCPFC is primary; IATTC fills Eastern Pacific cells.

# Note: Both datasets are sparse, rows only exist when fishing occurred. 
# Missing cell/month combinations represent zero effort, not missing data. The full grid of cells is reconstructed and all unmatched cells are assigned 0 hooks.

# Risk Index:
#   risk_index = longline_effort / max(longline_effort) per year-month
#   Normalized 0-1 within each year-month so that relative spatial exposure is comparable across time periods regardless of interannual variation in total effort.
#   0 = no fishing effort in that cell/month
#   1 = highest effort cell observed in that year-month

# Input  : final_chick_data_pre_env_vars.csv
# Output : final_chick_data_longline_risk.csv
#   New columns:
#     longline_effort : total hooks set in matching 5°x5° cell/year/month
#     risk_index      : normalized effort 0-1 within each year-month

library(dplyr)

# Define file paths
wcpfc_file  <- path.expand("~/Downloads/WCPFC_L_PUBLIC_BY_YY_MM.csv")
iattc_tuna  <- path.expand("~/Downloads/PublicLLTunaBillfishNum.csv")
iattc_shark <- path.expand("~/Downloads/PublicLLSharkNum.csv")
input_path  <- path.expand("~/Downloads/final_chick_data_pre_env_vars.csv")
output_path <- path.expand("~/Downloads/final_chick_data_longline_risk.csv")

for (f in c(wcpfc_file, iattc_tuna, iattc_shark, input_path)) {
  if (!file.exists(f)) stop("File not found: ", f)
}
cat("All input files found\n\n")

# Helper functions
snap_to_5deg <- function(x) floor(x / 5) * 5

parse_wcpfc_lat <- function(x) {
  x <- trimws(gsub('"', '', x))
  deg <- as.numeric(sub("[NS]", "", x))
  ifelse(grepl("S", x), -deg, deg)
}

parse_wcpfc_lon <- function(x) {
  x <- trimws(gsub('"', '', x))
  deg <- as.numeric(sub("[EW]", "", x))
  ifelse(grepl("W", x), -deg, deg)
}

# Load and clean WCPFC
cat("Loading WCPFC data...\n")
wcpfc_raw <- read.csv(wcpfc_file, stringsAsFactors = FALSE)

wcpfc <- wcpfc_raw %>%
  mutate(
    lat_sw = parse_wcpfc_lat(LAT5),
    lon_sw = parse_wcpfc_lon(LON5),
    year   = as.integer(gsub('"', '', YY)),
    month  = as.integer(gsub('"', '', MM)),
    hooks  = as.numeric(gsub('"', '', HHOOKS)) * 100
  ) %>%
  filter(year >= 2009, year <= 2018) %>%
  group_by(lat_sw, lon_sw, year, month) %>%
  summarise(wcpfc_hooks = sum(hooks, na.rm = TRUE), .groups = "drop")

cat(paste0("  WCPFC records (2009-2018): ", nrow(wcpfc), "\n\n"))

# STEP 4: Load and clean IATTC
cat("Loading IATTC data...\n")

prepare_iattc <- function(df) {
  df %>%
    mutate(
      lat_sw = snap_to_5deg(LatC5 - 2.5),
      lon_sw = snap_to_5deg(LonC5 - 2.5),
      year   = as.integer(Year),
      month  = as.integer(Month),
      hooks  = as.numeric(Hooks)
    ) %>%
    filter(year >= 2009, year <= 2018) %>%
    group_by(lat_sw, lon_sw, year, month) %>%
    summarise(hooks = sum(hooks, na.rm = TRUE), .groups = "drop")
}

iattc_t <- prepare_iattc(read.csv(iattc_tuna,  stringsAsFactors = FALSE))
iattc_s <- prepare_iattc(read.csv(iattc_shark, stringsAsFactors = FALSE))

iattc <- full_join(iattc_t, iattc_s,
                   by = c("lat_sw", "lon_sw", "year", "month"),
                   suffix = c("_t", "_s")) %>%
  mutate(iattc_hooks = pmax(hooks_t, hooks_s, na.rm = TRUE)) %>%
  select(lat_sw, lon_sw, year, month, iattc_hooks)

cat(paste0("  IATTC records (2009-2018): ", nrow(iattc), "\n\n"))

# Combine WCPFC and IATTC
cat("Combining WCPFC and IATTC...\n")

effort_grid <- full_join(wcpfc, iattc,
                         by = c("lat_sw", "lon_sw", "year", "month")) %>%
  mutate(
    total_hooks = case_when(
      !is.na(wcpfc_hooks) ~ wcpfc_hooks,
      !is.na(iattc_hooks) ~ iattc_hooks,
      TRUE                ~ 0
    )
  ) %>%
  select(lat_sw, lon_sw, year, month, total_hooks)

cat(paste0("  Combined effort grid cells: ", nrow(effort_grid), "\n\n"))

# Read chick data and snap to 5° grid
cat("Reading chick dataset...\n")
chick_data <- read.csv(input_path, stringsAsFactors = FALSE)
n <- nrow(chick_data)
cat(paste0("Loaded ", n, " observations\n\n"))

chick_data <- chick_data %>%
  mutate(
    lat_sw    = snap_to_5deg(latitude),
    lon_sw    = snap_to_5deg(longitude),
    year      = as.integer(Year),
    month_num = match(Month,
                  c("January","February","March","April","May","June",
                    "July","August","September","October","November","December"))
  )

# Join effort to bird observations
cat("Matching observations to effort grid...\n")

chick_out <- chick_data %>%
  left_join(effort_grid,
            by = c("lat_sw" = "lat_sw",
                   "lon_sw" = "lon_sw",
                   "year"   = "year",
                   "month_num" = "month")) %>%
  rename(longline_effort = total_hooks)

# Any remaining NAs after the join are cells truly outside both datasets
# (e.g. far Southern Ocean, Arctic) — set to 0 with a note
n_outside <- sum(is.na(chick_out$longline_effort))
chick_out$longline_effort[is.na(chick_out$longline_effort)] <- 0

cat(paste0("  Matched to effort grid     : ",
           sum(chick_out$longline_effort > 0), " obs with effort > 0\n"))
cat(paste0("  Zero effort (no fishing)   : ",
           sum(chick_out$longline_effort == 0), " obs\n"))
cat(paste0("    (of which truly outside  : ", n_outside, " obs)\n\n"))

# Calculate risk index
cat("Calculating risk index (normalized within each year-month)...\n")

chick_out <- chick_out %>%
  group_by(year, month_num) %>%
  mutate(
    max_effort_ym = max(longline_effort, na.rm = TRUE),
    risk_index    = ifelse(max_effort_ym == 0, 0,
                           longline_effort / max_effort_ym)
  ) %>%
  ungroup()

# Clean up helper columns
chick_final <- chick_out %>%
  select(-lat_sw, -lon_sw, -year, -month_num, -max_effort_ym)

# Summary
cat(rep("=", 60), "\n", sep = "")
cat("SUMMARY\n")
cat(rep("=", 60), "\n", sep = "")
cat(paste0("Total observations           : ", n, "\n"))
cat(paste0("Effort > 0 (fishing present) : ",
           sum(chick_final$longline_effort > 0), "\n"))
cat(paste0("Effort = 0 (no fishing)      : ",
           sum(chick_final$longline_effort == 0), "\n"))

cat("\nLongline effort statistics (hooks, non-zero only):\n")
print(summary(chick_final$longline_effort[chick_final$longline_effort > 0]))

cat("\nRisk index distribution:\n")
ri <- chick_final$risk_index
cat(paste0("  0 (no effort)         : ", sum(ri == 0), " obs\n"))
cat(paste0("  Low    (0 - 0.25)     : ", sum(ri > 0    & ri <= 0.25), " obs\n"))
cat(paste0("  Medium (0.25 - 0.50)  : ", sum(ri > 0.25 & ri <= 0.50), " obs\n"))
cat(paste0("  High   (0.50 - 0.75)  : ", sum(ri > 0.50 & ri <= 0.75), " obs\n"))
cat(paste0("  Very high (0.75 - 1)  : ", sum(ri > 0.75), " obs\n"))

# Save
cat(paste0("\nSaving to: ", output_path, "\n"))
write.csv(chick_final, output_path, row.names = FALSE)
cat("✓ Done! final_chick_data_longline_risk.csv saved successfully.\n")
cat("\nColumns added:\n")
cat("  longline_effort : Total hooks in matching 5°x5° cell/year/month\n")
cat("                    (WCPFC primary, IATTC for Eastern Pacific)\n")
cat("                    0 = no fishing reported that cell/month\n")
cat("  risk_index      : Normalized effort 0-1 within each year-month\n")
cat("                    0 = no effort, 1 = peak effort cell that period\n")
