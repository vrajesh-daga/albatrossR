# Haversine step distance (matches cleaningFunctionsPython/correct_step_size.py).
# Run from repo root: Rscript cleaningFunctionR/correct_step_size.R

colony_lat <- 21.5752667
colony_long <- -158.2733528

repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", args, value = TRUE)
  if (length(f)) {
    dirname(dirname(normalizePath(sub("^--file=", "", f))))
  } else {
    normalizePath(file.path(getwd()))
  }
}

correct_step_size <- function(df, lat, lon, identifier = "Tag") {
  if (!all(c(lat, lon, identifier) %in% names(df))) {
    stop("Data frame must contain columns: ", paste(c(lat, lon, identifier), collapse = ", "))
  }

  prev_lat <- ave(df[[lat]], df[[identifier]], FUN = function(x) c(NA, head(x, -1)))
  prev_long <- ave(df[[lon]], df[[identifier]], FUN = function(x) c(NA, head(x, -1)))

  prev_lat[is.na(prev_lat)] <- colony_lat
  prev_long[is.na(prev_long)] <- colony_long

  df$prev_lat <- prev_lat
  df$prev_long <- prev_long
  df$colony_lat <- colony_lat
  df$colony_long <- colony_long

  lat1 <- prev_lat * pi / 180
  long1 <- prev_long * pi / 180
  lat2 <- df[[lat]] * pi / 180
  long2 <- df[[lon]] * pi / 180

  dlat <- abs(lat1 - lat2)
  dlong <- abs(long1 - long2)

  R_earth <- 6371
  df$correct_step_distance <- (2 * R_earth) * asin(sqrt(
    (sin(dlat / 2))^2 +
      cos(lat1) * cos(lat2) * (sin(dlong / 2))^2
  ))

  df
}

launched_as_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  any(grepl("^--file=", args))
}

if (launched_as_script()) {
  root <- repo_root()
  in_path <- file.path(root, "original-datasets", "updated 2013-2014 chick coordinates - Sheet1.csv")
  df_insert <- utils::read.csv(in_path, stringsAsFactors = FALSE, check.names = FALSE)

  out <- correct_step_size(df_insert, lat = "latitude", lon = "longitude", identifier = "Tag")

  utils::write.csv(out, file.path(root, "chickdata.csv"), row.names = FALSE, na = "")
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(out, file.path(root, "chickdata.xlsx"))
  } else {
    message("Install 'writexl' for chickdata.xlsx output (CSV written).")
  }
}
