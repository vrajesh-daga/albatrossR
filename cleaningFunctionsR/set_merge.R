# Merge chick step datasets (matches cleaningFunctionsPython/set_merge.py).
# Run from repo root: Rscript cleaningFunctionR/set_merge.R
#
# Optional: install.packages("writexl") for Excel output.

CANONICAL_COLS <- c(
  "Tag",
  "Point number",
  "datetime",
  "type",
  "latitude",
  "longitude",
  "prev_lat",
  "prev_long",
  "colony_lat",
  "colony_long",
  "correct_step_distance"
)

repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", args, value = TRUE)
  if (length(f)) {
    dirname(dirname(normalizePath(sub("^--file=", "", f))))
  } else {
    normalizePath(file.path(getwd()))
  }
}

#' Parse ISO8601 datetime strings; supports fractional seconds (mixed precision).
parse_iso_to_date_str <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_character_, length(x))
  opts <- options(digits.secs = 6)
  on.exit(options(opts))
  for (i in seq_along(x)) {
    xi <- x[i]
    if (is.na(xi) || !nzchar(xi)) next
    pt <- NA
    for (fmt in c("%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%dT%H:%M:%SZ")) {
      tmp <- tryCatch(
        as.POSIXct(xi, tz = "UTC", format = fmt),
        warning = function(w) NA,
        error = function(e) NA
      )
      if (!is.na(tmp)) {
        pt <- tmp
        break
      }
    }
    if (!is.na(pt)) {
      out[i] <- format(as.Date(pt, tz = "UTC"), "%Y-%m-%d")
    }
  }
  out
}

#' Parse og7 date + midvalue (day-first, UK-style dates).
parse_og7_datetime_to_date_str <- function(date_part, time_part) {
  d <- trimws(as.character(date_part))
  t <- trimws(as.character(time_part))
  n <- length(d)
  out <- rep(NA_character_, n)
  opts <- options(digits.secs = 6)
  on.exit(options(opts))
  for (i in seq_len(n)) {
    if (isTRUE(is.na(d[i]) && is.na(t[i]))) next
    combined <- paste(d[i], t[i])
    pt <- NA
    for (fmt in c("%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%d-%m-%Y %H:%M", "%Y-%m-%d %H:%M")) {
      tmp <- tryCatch(
        as.POSIXct(combined, tz = "UTC", format = fmt),
        warning = function(w) NA,
        error = function(e) NA
      )
      if (length(tmp) == 1L && !is.na(tmp)) {
        pt <- tmp
        break
      }
    }
    if (length(pt) == 1L && !is.na(pt)) {
      out[i] <- format(as.Date(pt, tz = "UTC"), "%Y-%m-%d")
    }
  }
  out
}

standardize_left <- function(df) {
  miss <- setdiff(CANONICAL_COLS, names(df))
  if (length(miss)) stop("left frame missing columns: ", paste(sort(miss), collapse = ", "))
  out <- df[, CANONICAL_COLS, drop = FALSE]
  out$datetime <- parse_iso_to_date_str(out$datetime)
  out
}

standardize_og7 <- function(df) {
  rename_map <- c(
    "BirdID" = "Tag",
    "Data point #" = "Point number",
    "Fix type" = "type",
    "compensatedlat" = "latitude",
    "long" = "longitude"
  )
  miss <- setdiff(names(rename_map), names(df))
  if (length(miss)) stop("og7 frame missing columns: ", paste(sort(miss), collapse = ", "))
  if (!all(c("date", "midvalue") %in% names(df))) {
    stop("og7 frame needs date and midvalue to build datetime")
  }

  out <- df
  for (nm in names(rename_map)) {
    names(out)[names(out) == nm] <- rename_map[[nm]]
  }

  need <- setdiff(CANONICAL_COLS[CANONICAL_COLS != "datetime"], names(out))
  if (length(need)) stop("og7 frame missing column after rename: ", paste(need, collapse = ", "))

  dt_str <- parse_og7_datetime_to_date_str(out$date, out$midvalue)
  out <- out[, CANONICAL_COLS[CANONICAL_COLS != "datetime"], drop = FALSE]
  out$datetime <- dt_str
  out <- out[, CANONICAL_COLS, drop = FALSE]
  out
}

merge_chick_step_datasets <- function(left_path, right_path) {
  left_df <- utils::read.csv(left_path, stringsAsFactors = FALSE, check.names = FALSE)
  right_df <- utils::read.csv(right_path, stringsAsFactors = FALSE, check.names = FALSE)

  left_std <- standardize_left(left_df)
  right_std <- standardize_og7(right_df)

  combined <- rbind(left_std, right_std)

  combined$Tag <- suppressWarnings(as.integer(combined$Tag))
  combined$`Point number` <- suppressWarnings(as.integer(combined$`Point number`))

  lon <- suppressWarnings(as.numeric(combined$longitude))
  combined$calculation_long <- ((lon %% 360) + 360) %% 360

  combined
}

default_paths <- function(root) {
  left <- file.path(root, "original-datasets", "2013-2024correct_step_size.csv")
  right <- file.path(root, "original-datasets", "og7chicks_nofilter_correctstepdistance.csv")
  list(left = left, right = right)
}

launched_as_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  any(grepl("^--file=", args))
}

if (launched_as_script()) {
  root <- repo_root()
  p <- default_paths(root)
  combined <- merge_chick_step_datasets(p$left, p$right)

  out_csv <- file.path(root, "mergedchickdata(o7&2013-2014).csv")
  utils::write.csv(combined, out_csv, row.names = FALSE, na = "")

  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(combined, file.path(root, "mergedchickdata(o7&2013-2014).xlsx"))
  } else {
    message("Install 'writexl' for mergedchickdata Excel output (CSV written).")
  }
}
