# Add standardized longline fishing effort (no risk scores)

# Methodology

# Monthly longline fishing effort is assigned to each bird observation by matching to a 5°x5° spatial grid cell, year, and month.
# Effort = total hooks set across all vessels and flag states within each 5°x5° cell for the corresponding year and month (per WCPFC/SPC methodology).

# Two complementary datasets:
#   1. WCPFC (Western & Central Pacific Fisheries Commission)
#      File: WCPFC_L_PUBLIC_BY_YY_MM.csv
#      HHOOKS column = hundred hooks — converted to actual hooks (×100)
#      Coverage: Western and Central Pacific
#   2. IATTC (Inter-American Tropical Tuna Commission)
#      Files: PublicLLTunaBillfishNum.csv, PublicLLSharkNum.csv
#      Hooks column = actual hooks
#      Coverage: Eastern Pacific Ocean
#      TunaBillfish and Shark files report hooks for same vessels;
#      max per cell/month is taken to avoid double-counting

#   WCPFC is the primary source; IATTC fills Eastern Pacific cells where WCPFC has no data.

# NA vs Zero distinction:
#   0   = cell exists in dataset but no fishing effort reported that month
#   NA  = cell falls outside both WCPFC and IATTC geographic coverage;
#         true data availability unknown — do NOT interpret as zero effort

# Unmatched cells within known coverage areas are assigned 0.
# Cells outside all dataset coverage remain NA.

# Output columns:
#   longline_effort        : total hooks in matching 5°x5° cell/year/month
#                            (0 = no effort reported; NA = outside coverage)
#   longline_effort_scaled : longline_effort standardized to 0-1 scale
#                            across the full dataset (NAs excluded from scaling)
#                            Use this for the eventual risk index calculation:
#                            Risk = Bird Use (kernel density) × longline_effort_scaled
#
# NOTE: Final risk index is NOT computed here. It requires bird-use surfaces (kernel density estimates) which are generated separately. Risk index will be calculated at monthly, annual, age-class, and cohort scales once those surfaces are available.

# Input  : final_chick_data_pre_env_vars.csv
# Output : final_chick_data_longline_risk.csv

library(dplyr)

# Define File paths
wcpfc_file  <- path.expand("~/Downloads/WCPFC_L_PUBLIC_BY_YY_MM.csv")
iattc_tuna  <- path.expand("~/Downloads/PublicLLTunaBillfishNum.csv")
iattc_shark <- path.expand("~/Downloads/PublicLLSharkNum.csv")
input_path  <- path.expand("~/Downloads/Final Outputs/final_chick_data_pre_env_vars.csv")
output_path <- path.expand("~/Downloads/final_chick_data_longline_risk.csv")

for (f in c(wcpfc_file, iattc_tuna, iattc_shark, input_path)) {
  if (!file.exists(f)) stop("File not found: ", f)
}
cat("All input files found\n\n")

# Helper functions
snap_to_5deg <- function(x) floor(x / 5) * 5

parse_wcpfc_lat <- function(x) {
  x   <- trimws(gsub('"', '', x))
  deg <- as.numeric(sub("[NS]", "", x))
  ifelse(grepl("S", x), -deg, deg)
}

parse_wcpfc_lon <- function(x) {
  x   <- trimws(gsub('"', '', x))
  deg <- as.numeric(sub("[EW]", "", x))
  ifelse(grepl("W", x), -deg, deg)
}

# Load WCPFC
cat("Loading WCPFC data...\n")
wcpfc_raw <- read.csv(wcpfc_file, stringsAsFactors = FALSE)

wcpfc <- wcpfc_raw %>%
  mutate(
    lat_sw = parse_wcpfc_lat(LAT5),
    lon_sw = parse_wcpfc_lon(LON5),
    year   = as.integer(gsub('"', '', YY)),
    month  = as.integer(gsub('"', '', MM)),
    hooks  = as.numeric(gsub('"', '', HHOOKS)) * 100   # hundred hooks → hooks
  ) %>%
  filter(year >= 2009, year <= 2018) %>%
  group_by(lat_sw, lon_sw, year, month) %>%
  summarise(wcpfc_hooks = sum(hooks, na.rm = TRUE), .groups = "drop")

# Record WCPFC geographic coverage (lat/lon combinations that exist)
wcpfc_cells <- wcpfc %>% distinct(lat_sw, lon_sw)

cat(paste0("  WCPFC records (2009-2018): ", nrow(wcpfc), "\n"))
cat(paste0("  WCPFC unique cells: ", nrow(wcpfc_cells), "\n\n"))

# Load IATTC
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

# Take max of tuna and shark hooks per cell to avoid double-counting
# (both files report effort for the same vessels fishing in the same cells)
iattc <- full_join(iattc_t, iattc_s,
                   by = c("lat_sw", "lon_sw", "year", "month"),
                   suffix = c("_t", "_s")) %>%
  mutate(iattc_hooks = pmax(hooks_t, hooks_s, na.rm = TRUE)) %>%
  select(lat_sw, lon_sw, year, month, iattc_hooks)

# Record IATTC geographic coverage
iattc_cells <- iattc %>% distinct(lat_sw, lon_sw)

cat(paste0("  IATTC records (2009-2018): ", nrow(iattc), "\n"))
cat(paste0("  IATTC unique cells: ", nrow(iattc_cells), "\n\n"))

# Combine WCPFC and IATTC
cat("Combining WCPFC and IATTC...\n")

all_known_cells <- bind_rows(
  wcpfc_cells %>% mutate(source = "WCPFC"),
  iattc_cells %>% mutate(source = "IATTC")
) %>% distinct(lat_sw, lon_sw)

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

cat(paste0("  Combined effort grid records: ", nrow(effort_grid), "\n\n"))

# Load chick data and snap coordinates to 5° grid
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
                    "July","August","September","October",
                    "November","December"))
  )

# Join effort to bird data
cat("Matching observations to effort grid...\n")

chick_out <- chick_data %>%
  left_join(effort_grid,
            by = c("lat_sw"    = "lat_sw",
                   "lon_sw"    = "lon_sw",
                   "year"      = "year",
                   "month_num" = "month")) %>%
  rename(longline_effort = total_hooks)

chick_out <- chick_out %>%
  left_join(all_known_cells %>% mutate(in_coverage = TRUE),
            by = c("lat_sw", "lon_sw")) %>%
  mutate(
    longline_effort = case_when(
      !is.na(longline_effort)            ~ longline_effort,  # matched
      !is.na(in_coverage)                ~ 0,                # in coverage, no fishing
      TRUE                               ~ NA_real_          # outside coverage
    )
  ) %>%
  select(-in_coverage)

# Standardize effort to 0-1 scale
cat("Standardizing effort to 0-1 scale...\n")

effort_min <- min(chick_out$longline_effort, na.rm = TRUE)   # will be 0
effort_max <- max(chick_out$longline_effort, na.rm = TRUE)

chick_out <- chick_out %>%
  mutate(
    longline_effort_scaled = (longline_effort - effort_min) /
                             (effort_max - effort_min)
  )

cat(paste0("  Effort range: ", effort_min, " to ", comma(effort_max), " hooks\n"))
cat(paste0("  Scaled range: 0 to 1\n\n"))

# Clean up and build final output
chick_final <- chick_out %>%
  select(-lat_sw, -lon_sw, -year, -month_num)

# Summary
cat(rep("=", 60), "\n", sep = "")
cat("SUMMARY\n")
cat(rep("=", 60), "\n", sep = "")
cat(paste0("Total observations             : ", n, "\n"))
cat(paste0("Effort > 0 (fishing reported)  : ",
           sum(chick_final$longline_effort > 0,  na.rm = TRUE), "\n"))
cat(paste0("Effort = 0 (no fishing in cell): ",
           sum(chick_final$longline_effort == 0, na.rm = TRUE), "\n"))
cat(paste0("Effort = NA (outside coverage) : ",
           sum(is.na(chick_final$longline_effort)), "\n"))

cat("\nLongline effort statistics (hooks, non-zero non-NA):\n")
print(summary(chick_final$longline_effort[
  !is.na(chick_final$longline_effort) & chick_final$longline_effort > 0]))

cat("\nScaled effort distribution (non-NA):\n")
sc <- chick_final$longline_effort_scaled[!is.na(chick_final$longline_effort_scaled)]
cat(paste0("  = 0 (no effort)     : ", sum(sc == 0), " obs\n"))
cat(paste0("  0 < x ≤ 0.25 (low)  : ", sum(sc > 0    & sc <= 0.25), " obs\n"))
cat(paste0("  0.25 < x ≤ 0.5 (med): ", sum(sc > 0.25 & sc <= 0.50), " obs\n"))
cat(paste0("  0.5 < x ≤ 0.75 (hi) : ", sum(sc > 0.50 & sc <= 0.75), " obs\n"))
cat(paste0("  > 0.75 (very high)   : ", sum(sc > 0.75), " obs\n"))
cat(paste0("  NA (outside coverage): ",
           sum(is.na(chick_final$longline_effort_scaled)), " obs\n"))

cat("\nNote: NAs represent cells outside WCPFC and IATTC geographic coverage.\n")
cat("      Interpret as 'unknown' — NOT as zero effort.\n")
cat("      Consult WCPFC and IATTC metadata to confirm coverage boundaries.\n")

# Save
cat(paste0("\nSaving to: ", output_path, "\n"))
write.csv(chick_final, output_path, row.names = FALSE)
cat("✓ Done! final_chick_data_longline_risk.csv saved.\n\n")

cat("Columns added:\n")
cat("  longline_effort        : Total hooks in matching 5°x5° cell/year/month\n")
cat("                           0   = no fishing reported that cell/month\n")
cat("                           NA  = cell outside WCPFC + IATTC coverage\n")
cat("  longline_effort_scaled : Effort standardized to 0-1 across full dataset\n")
cat("                           For use in: Risk = Bird Use × longline_effort_scaled\n")
cat("\nNOTE: Final risk index (Bird Use × Effort) is NOT included here.\n")
cat("      Generate kernel density bird-use surfaces first, then combine.\n")
