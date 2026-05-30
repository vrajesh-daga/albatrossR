# add_chlorophyll_SSH.R
#
# Extracts monthly chlorophyll-a (NASA MODIS) and sea surface height anomaly
# (CMEMS) for each GPS point in the chick dataset, then saves the result as
# final_chick_wind_chlorophyll_SSH.csv
#
# Project folder layout expected:
#   add_chlorophyll_ssh/
#     scripts/
#       add_chlorophyll_SSH.R        <- this file
#     data/
#       final_chick_data_pre_env_vars.csv
#       chlorophyll/
#         AQUA_MODIS.YYYYMMDD_YYYYMMDD.L3m.MO.CHL.chlor_a.4km.nc  (one per month)
#       SSH/
#         YYYY/
#           dt_global_allsat_msla_h_yYYYY_mMM.nc                   (one per month)
#
# Environmental data covers 2009-2018. Points outside that window get NA.

library(ncdf4)
library(dplyr)
library(lubridate)

# ── 0. Paths — relative to add_chlorophyll_ssh/ project root ─────────────────
# In RStudio: Session > Set Working Directory > To Project Directory
# Or set manually: setwd("path/to/add_chlorophyll_ssh")

CHICK_FILE <- "data/final_chick_data_pre_env_vars.csv"
CHL_DIR    <- "data/chlorophyll"
SSH_DIR    <- "data/SSH"
OUT_FILE   <- "data/final_chick_chlorophyll_SSH.csv"

# ── 0a. Coverage limits — update if you add more .nc files later ─────────────

CHL_YEAR_MIN <- 2009L;  CHL_YEAR_MAX <- 2018L
SSH_YEAR_MIN <- 2009L;  SSH_YEAR_MAX <- 2018L

# Effective range is the intersection of both datasets
ENV_YEAR_MIN <- max(CHL_YEAR_MIN, SSH_YEAR_MIN)
ENV_YEAR_MAX <- min(CHL_YEAR_MAX, SSH_YEAR_MAX)

message(sprintf("Environmental data coverage: %d-%d", ENV_YEAR_MIN, ENV_YEAR_MAX))

# ── 1. Load chick data ────────────────────────────────────────────────────────

chick <- read.csv(CHICK_FILE, stringsAsFactors = FALSE) %>%
  mutate(datetime = as.Date(datetime))

message(sprintf("Loaded %d rows from %s", nrow(chick), CHICK_FILE))

# Warn about any points outside environmental data coverage (they get NA)
out_of_range <- chick %>%
  filter(year(datetime) < ENV_YEAR_MIN | year(datetime) > ENV_YEAR_MAX)

if (nrow(out_of_range) > 0) {
  warning(sprintf(
    "%d rows fall outside env data coverage (%d-%d) and will get NA values.",
    nrow(out_of_range), ENV_YEAR_MIN, ENV_YEAR_MAX
  ))
}

# ── 2. Helper: nearest-index lookup ──────────────────────────────────────────

nearest_idx <- function(vec, val) which.min(abs(vec - val))

# ── 3. Helper: build chlorophyll filename for a given year-month ──────────────
# Pattern: AQUA_MODIS.YYYYMMDD_YYYYMMDD.L3m.MO.CHL.chlor_a.4km.nc
# first date = first of month, second date = last day of month

chl_filename <- function(yr, mo) {
  start <- as.Date(sprintf("%04d-%02d-01", yr, mo))
  end   <- start %m+% months(1) - days(1)
  sprintf(
    "AQUA_MODIS.%s_%s.L3m.MO.CHL.chlor_a.4km.nc",
    format(start, "%Y%m%d"),
    format(end,   "%Y%m%d")
  )
}

# ── 4. Helper: build SSH filename for a given year-month ─────────────────────
# Pattern: SSH/YYYY/dt_global_allsat_msla_h_yYYYY_mMM.nc

ssh_path <- function(yr, mo) {
  file.path(SSH_DIR, sprintf("%04d", yr),
            sprintf("dt_global_allsat_msla_h_y%04d_m%02d.nc", yr, mo))
}

# ── 5. Extract values for one month ──────────────────────────────────────────

extract_month <- function(rows, yr, mo) {

  in_range <- yr >= ENV_YEAR_MIN & yr <= ENV_YEAR_MAX

  # ── 5a. Chlorophyll ─────────────────────────────────────────────────────────
  chl_path <- file.path(CHL_DIR, chl_filename(yr, mo))

  if (in_range && file.exists(chl_path)) {
    nc_chl   <- nc_open(chl_path)
    chl_lon  <- ncvar_get(nc_chl, "lon")        # length 8640
    chl_lat  <- ncvar_get(nc_chl, "lat")        # length 4320
    chl_mat  <- ncvar_get(nc_chl, "chlor_a")    # dim: [lon, lat]
    fill_chl <- ncatt_get(nc_chl, "chlor_a", "_FillValue")$value
    nc_close(nc_chl)

    chl_mat[chl_mat == fill_chl] <- NA

    rows$chlor_a <- mapply(function(lon, lat) {
      chl_mat[nearest_idx(chl_lon, lon), nearest_idx(chl_lat, lat)]
    }, rows$longitude, rows$latitude)

  } else {
    if (in_range) warning(sprintf("Chlorophyll file not found: %s", chl_path))
    rows$chlor_a <- NA_real_
  }

  # ── 5b. SSH (sea level anomaly) ─────────────────────────────────────────────
  sh_path <- ssh_path(yr, mo)

  if (in_range && file.exists(sh_path)) {
    nc_ssh  <- nc_open(sh_path)
    ssh_lon <- ncvar_get(nc_ssh, "longitude")   # length 2880, range -180:180
    ssh_lat <- ncvar_get(nc_ssh, "latitude")    # length 1440
    # ncdf4 automatically applies scale_factor on read — values already in metres
    sla_raw <- ncvar_get(nc_ssh, "sla")
    nc_close(nc_ssh)

    # ncdf4 drops the time dimension when size=1, so force to 2D matrix
    sla_mat <- matrix(sla_raw, nrow = length(ssh_lon), ncol = length(ssh_lat))

    rows$ssh_sla <- mapply(function(lon, lat) {
      sla_mat[nearest_idx(ssh_lon, lon), nearest_idx(ssh_lat, lat)]
    }, rows$longitude, rows$latitude)

  } else {
    if (in_range) warning(sprintf("SSH file not found: %s", sh_path))
    rows$ssh_sla <- NA_real_
  }

  rows
}

# ── 6. Main loop: group by year-month, extract, recombine ────────────────────

chick <- chick %>%
  mutate(
    .yr     = year(datetime),
    .mo     = month(datetime),
    chlor_a = NA_real_,
    ssh_sla = NA_real_
  )

year_months <- chick %>%
  distinct(.yr, .mo) %>%
  arrange(.yr, .mo)

message(sprintf("Processing %d year-months...", nrow(year_months)))

result_list <- vector("list", nrow(year_months))

for (k in seq_len(nrow(year_months))) {
  yr <- year_months$.yr[k]
  mo <- year_months$.mo[k]
  message(sprintf("  [%d/%d] %04d-%02d", k, nrow(year_months), yr, mo))

  idx              <- which(chick$.yr == yr & chick$.mo == mo)
  result_list[[k]] <- extract_month(chick[idx, ], yr, mo)
}

chick_out <- bind_rows(result_list) %>%
  select(-.yr, -.mo) %>%
  arrange(Tag, datetime)

# ── 7. Quick sanity check ────────────────────────────────────────────────────

n_chl_na <- sum(is.na(chick_out$chlor_a))
n_ssh_na <- sum(is.na(chick_out$ssh_sla))
message(sprintf(
  "chlor_a: %d / %d non-NA  |  ssh_sla: %d / %d non-NA",
  nrow(chick_out) - n_chl_na, nrow(chick_out),
  nrow(chick_out) - n_ssh_na, nrow(chick_out)
))

# ── 8. Save ──────────────────────────────────────────────────────────────────

write.csv(chick_out, OUT_FILE, row.names = FALSE)
message(sprintf("Done! Saved to %s  (%d rows)", OUT_FILE, nrow(chick_out)))
