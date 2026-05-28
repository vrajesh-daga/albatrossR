library(ggplot2)
library(dplyr)
library(scales)
library(readr)
library(maps)

# -------------------------------------------------------------------------
# 1. Distance from Colony vs. Age Class (Boxplot/Violin)
# -------------------------------------------------------------------------

#' Plot Distance from Natal Colony across Developmental Phases
#'
#' Generates a combined violin and boxplot visualizing the distribution of
#' albatross tracking distances from their natal colony, stratified by specific
#' life-stage cohorts (Year 1, Years 2-3, Years 4-5)[cite: 48, 49, 50, 54].
#'
#' @param data A data frame containing the consolidated albatross tracking data[cite: 13].
#'   Must include the columns: \code{years_post_fledge} (character or factor matching
#'   "year 1", "year 2-3", "year 4-5") and \code{correct_step_distance} (numeric values representing
#'   distance from colony in km).
#'
#' @return A ggplot2 object representing the violin/boxplot distribution visualization.
#' @export
plot_distance_by_age <- function(data) {
  # Clean column headers
  colnames(data) <- trimws(colnames(data))

  data_plot <- data %>%
    # Directly match data categories to the formal SOW presentation labels [cite: 48, 49, 50]
    mutate(age_class = factor(years_post_fledge,
                              levels = c("year 1", "year 2-3", "year 4-5"),
                              labels = c("Year 1\n(Dispersal)",
                                         "Years 2–3\n(Immature Roaming)",
                                         "Years 4–5\n(Pre-Breeding Prospecting)"))) %>%
    # Drop rows where the age class might be missing entirely
    filter(!is.na(age_class))

  ggplot(data_plot, aes(x = age_class, y = correct_step_distance, fill = age_class)) +
    geom_violin(alpha = 0.6, trim = FALSE) +
    # Updated to use the correct ggplot 4.0.0 argument for median lines
    geom_boxplot(width = 0.15, fill = "white", outlier.shape = NA, alpha = 0.8, median.linewidth = 1) +
    labs(
      title = "Albatross Distance from Natal Colony across Developmental Phases",
      x = "Age Class (Years Post-Fledging)",
      y = "Distance from Colony (km)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(face = "bold", size = 10)
    ) +
    scale_fill_brewer(palette = "YlOrRd")
}

# -------------------------------------------------------------------------
# 2a. Individual Trajectories
# -------------------------------------------------------------------------

#' Plot Spatial Distribution Density Map
#'
#' Creates a Pacific-centered spatial map displaying tracking coordinates as semi-transparent
#' points to illustrate space-use density hotspots, faceted by juvenile age classes[cite: 46, 59].
#' Automatically transforms standard -180 to 180 longitudes to a 0-360 Pacific projection baseline.
#'
#' @param data A data frame containing tracking locations. Must include columns:
#'   \code{prev_long} (numeric or character longitude), \code{prev_lat} (numeric or character latitude),
#'   and \code{years_post_fledge} (grouping factor for age-class faceting)[cite: 46].
#'
#' @return A ggplot2 object displaying the high-density space-use map over landmass boundaries.
#' @export
plot_trajectory_map <- function(data) {
  # 1. Clean column headers to strip out hidden spaces
  colnames(data) <- trimws(colnames(data))

  # 2. Ensure tracking coordinates are numeric and convert to a 0-360 scale
  # This shifts negative longitudes (like -160) into positive Pacific coordinates (like 200)
  data <- data %>%
    mutate(
      prev_long = as.numeric(as.character(prev_long)),
      prev_lat  = as.numeric(as.character(prev_lat)),
      prev_long_360 = ifelse(prev_long < 0, prev_long + 360, prev_long)
    )

  # 3. Pull the Pacific-centered world map (which naturally uses the 0-360 scale)
  world_map <- map_data("world2")

  # 4. Define precise bounding limits centered on the North Pacific basin
  # 110°E (Asia) to 240°E (which is 120°W, North America)
  lon_range <- c(110, 240)
  lat_range <- c(10, 65)

  ggplot() +
    # Draw background land masses using the Pacific baseline
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "gray92", color = "gray85", linewidth = 0.1) +

    # Draw transparent tracking points using our converted 360-degree column
    geom_point(data = data, aes(x = prev_long_360, y = prev_lat),
               alpha = 0.05, color = "darkblue", size = 0.4, na.rm = TRUE) +

    # Crop the viewport cleanly to the North Pacific area
    coord_quickmap(xlim = lon_range, ylim = lat_range) +

    # Facet into your developmental stages [cite: 46]
    facet_wrap(~years_post_fledge) +

    labs(
      title = "Albatross Spatial Distribution Density Map",
      subtitle = "North Pacific Basin View (0° to 360° Longitude Alignment)",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "aliceblue", color = NA),
      panel.grid.major = element_line(color = "white", linewidth = 0.2),
      strip.background = element_rect(fill = "gray95", color = "gray80"),
      strip.text = element_text(face = "bold")
    )
}

# -------------------------------------------------------------------------
# 2b. Latitude Shifts
# -------------------------------------------------------------------------

#' Plot Seasonal Latitudinal Shifts Across Age Classes
#'
#' Generates a monthly boxplot sequence displaying latitudinal movements across
#' chronological months, stratified by the bird's developmental age class to evaluate
#' seasonal migration schedules[cite: 44, 46, 57].
#'
#' @param data A data frame containing temporal tracking strings. Must include columns:
#'   \code{Month} (factor or ordered character abbreviation), \code{prev_lat} (numeric latitude),
#'   and \code{years_post_fledge} (developmental stage factor)[cite: 44, 46, 57].
#'
#' @return A ggplot2 object representing the latitudinal seasonality trend.
#' @export
plot_latitudinal_seasonality <- function(data) {
  # Clean column headers and ensure coordinates are numeric
  colnames(data) <- trimws(colnames(data))
  data$prev_lat  <- as.numeric(as.character(data$prev_lat))

  # Ensure Month is an ordered factor so it plots Jan-Dec chronologically [cite: 44]
  if(!is.factor(data$Month)) {
    data$Month <- factor(data$Month, levels = month.abb)
  }

  # Drop missing data rows for plotting cleanliness
  data_plot <- data %>% filter(!is.na(prev_lat) & !is.na(Month))

  ggplot(data_plot, aes(x = Month, y = prev_lat, fill = years_post_fledge)) +
    # Swapped 'fatten = 1' to 'median.linewidth = 1' to fix the version 4.0.0 warning
    geom_boxplot(outlier.alpha = 0.05, lwd = 0.4, median.linewidth = 1) +
    labs(
      title = "Seasonal Latitudinal Shifts Across Age Classes",
      subtitle = "Tracking seasonal migration timing and open-ocean distribution",
      x = "Month",
      y = "Latitude (Degrees North)",
      fill = "Age Class"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# -------------------------------------------------------------------------
# 3. Monthly Travel Distance / Net Displacement Trends
# -------------------------------------------------------------------------

#' Plot Monthly Movement/Displacement Trends
#'
#' Quantifies and plots the monthly arithmetic mean and standard error profiles of a specified
#' distance or displacement metric, pooled across birds and stratified by age class[cite: 45, 46, 55, 56].
#'
#' @param data A data frame containing metrics grouped by time frame[cite: 44]. Must include columns:
#'   \code{Month}, \code{years_post_fledge}, and the chosen metric variable[cite: 44, 46].
#' @param metric A string character defining the evaluation metric column to summarize.
#'   Defaults to \code{"correct_step_distance"}, but can evaluate alternative movement metrics
#'   like net displacement columns[cite: 55].
#'
#' @return A ggplot2 object showing monthly line trends with associated standard error whiskers.
#' @export
plot_monthly_movement_trends <- function(data, metric = "correct_step_distance") {
  # Clean up months if they are characters ("June", "July") rather than integers
  summary_df <- data %>%
    group_by(Month, years_post_fledge) %>%
    summarise(
      mean_val = mean(.data[[metric]], na.rm = TRUE),
      se_val = sd(.data[[metric]], na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )

  # Dynamic conversion to factor ordering so months don't sort alphabetically [cite: 44]
  if(is.character(summary_df$Month)) {
    summary_df <- summary_df %>%
      mutate(Month = factor(Month, levels = month.name))
  }

  ggplot(summary_df, aes(x = Month, y = mean_val, color = as.factor(years_post_fledge), group = years_post_fledge)) +
    geom_line(linewidth = 1.2) + # Updated size to linewidth to follow modern ggplot syntax
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = mean_val - se_val, ymax = mean_val + se_val), width = 0.2, alpha = 0.7) +
    labs(
      title = paste("Seasonal Profile of", gsub("_", " ", metric)),
      subtitle = "Pooled across individuals, stratified by age class",
      x = "Month",
      y = paste("Mean", gsub("_", " ", metric)),
      color = "Age Class"
    ) +
    theme_minimal() +
    scale_color_viridis_d(option = "plasma", end = 0.8)
}

# -------------------------------------------------------------------------
# 4. Cohort Comparison: Dispersal Extent by Fledging Year
# -------------------------------------------------------------------------

#' Plot Dispersal Distance Extent Across Fledging Cohorts
#'
#' Evaluates interannual variation by plotting the spatial footprint and distribution of
#' travel distances from the natal colony compared across different birth years (cohorts)[cite: 51, 99].
#'
#' @param data A data frame containing multi-year tracking metadata[cite: 51]. Must include columns:
#'   \code{cohort} (numeric or factor calendar birth years) and \code{correct_step_distance} (numeric dispersal footprint)[cite: 16, 99].
#'
#' @return A ggplot2 object displaying boxplot variations across separate tracking generations.
#' @export
plot_cohort_dispersal_comparison <- function(data) {
  data_plot <- data %>%
    mutate(cohort_factor = as.factor(cohort))

  ggplot(data_plot, aes(x = cohort_factor, y = correct_step_distance, fill = cohort_factor)) +
    geom_boxplot(outlier.alpha = 0.2, width = 0.6) +
    labs(
      title = "Dispersal Distance Extent Across Fledging Cohorts",
      subtitle = "Comparing spatial footprints by natal year group",
      x = "Cohort (Fledging Year)",
      y = "Distance from Colony (km)"
    ) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set2")
}

# -------------------------------------------------------------------------
# Execution / Processing Wrapper Example
# -------------------------------------------------------------------------

# Read and transform baseline CSV data [cite: 13]
csv <- read.csv("altered-datasets/final_chick_data_pre_env_vars.csv")
csv <- csv %>%
  mutate(
    Month = month.abb[match(Month, month.name)],
    Month = factor(Month, levels = month.abb)
  )

# Ready for programmatic calls:
# plot_monthly_movement_trends(csv)
# plot_trajectory_map(csv)
# plot_latitudinal_seasonality(csv)
# plot_distance_by_age(csv)
# plot_cohort_dispersal_comparison(csv)
