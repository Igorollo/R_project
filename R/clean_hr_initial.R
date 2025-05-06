# R/clean_hr_initial.R
# Function for initial cleaning and validation of RR interval data

# Required libraries (ensure they are installed)
# install.packages(c("lubridate", "dplyr", "readr"))
library(lubridate)
library(dplyr)
library(readr)

#' Initial Cleaning and Validation of RR Interval Data
#'
#' Performs initial data validation, timestamp parsing/sorting/deduplication,
#' handles NA RR values, and applies basic physiological range filtering.
#'
#' @param data A data frame containing time series data. Must contain
#'             columns specified by `time_col` and `rr_col`.
#' @param time_col Character string. Name of the timestamp column. Expects formats
#'                 parsable by `lubridate::parse_date_time` (e.g., "H:M:OSZ").
#' @param rr_col Character string. Name of the RR interval column (in ms).
#' @param min_rr Numeric. Minimum plausible RR interval (ms). Default: 300.
#' @param max_rr Numeric. Maximum plausible RR interval (ms). Default: 2000.
#' @param tz Character string. Timezone to use for parsing timestamps.
#'           Defaults to "UTC".
#'
#' @return A data frame sorted by timestamp, with columns:
#'         - `timestamp_posix`: POSIXct timestamps.
#'         - `rr_ms`: Numeric RR intervals (original values, NAs removed).
#'         - `original_time_str`: The original time string (for reference).
#'         Rows outside the [min_rr, max_rr] range or with invalid/duplicate
#'         timestamps are removed. An `initial_qc_passed` column (logical)
#'         is added, marking rows that pass this initial stage (useful if
#'         keeping all rows is desired later, though default removes failures).
#'         Throws errors on critical validation failures.
#'
#' @keywords internal
clean_hr_initial <- function(data, time_col = "Time", rr_col = "rr_ms",
                             min_rr = 200, max_rr = 2000, tz = "UTC") {

  # --- 1. Input Validation ---
  message("Step 1: Performing initial validation and cleaning...")
  if (!is.data.frame(data)) stop("Input 'data' must be a data frame.")
  if (nrow(data) == 0) {
      warning("Input 'data' is empty. Returning an empty data frame structure.")
      return(data.frame(timestamp_posix = as.POSIXct(character(0), tz = tz),
                        rr_ms = numeric(0),
                        original_time_str = character(0),
                        initial_qc_passed = logical(0)))
  }
  if (!time_col %in% names(data)) stop("Timestamp column '", time_col, "' not found.")
  if (!rr_col %in% names(data)) stop("RR interval column '", rr_col, "' not found.")
  if (!is.numeric(min_rr) || length(min_rr) != 1 || min_rr <= 0) stop("'min_rr' must be a single positive number.")
  if (!is.numeric(max_rr) || length(max_rr) != 1 || max_rr <= min_rr) stop("'max_rr' must be a single number greater than 'min_rr'.")

  # Store original time string before potential modification
  data$original_time_str <- as.character(data[[time_col]])

  # --- 2. Timestamp Processing ---
  message("--> Parsing and validating timestamps...")
  original_rows <- nrow(data)

  parsed_times <- tryCatch({
      lubridate::parse_date_time(data[[time_col]],
                                 orders = c("H:M:OS", "Ymd H:M:OS", "Y-m-d H:M:OS"),
                                 tz = tz, quiet = TRUE)
  }, error = function(e) {
      stop("Fatal error during timestamp parsing for column '", time_col, "'. Check format. Error: ", e$message)
  })

  na_times_indices <- which(is.na(parsed_times))
  if (length(na_times_indices) > 0) {
      warning(sprintf("Could not parse %d timestamp(s) in column '%s'. Example: '%s'. Removing these rows.",
                      length(na_times_indices), time_col, data[[time_col]][na_times_indices[1]]))
      data <- data[-na_times_indices, ]
      parsed_times <- parsed_times[-na_times_indices]
      if (nrow(data) == 0) stop("All timestamps failed to parse or were NA.")
  }
  data$timestamp_posix <- parsed_times

  # Sort by timestamp and handle duplicates
  data <- data %>% dplyr::arrange(timestamp_posix)
  duplicate_indices <- which(duplicated(data$timestamp_posix))
  if (length(duplicate_indices) > 0) {
      warning(sprintf("Found %d duplicate timestamp(s) after parsing. Keeping first occurrence.", length(duplicate_indices)))
      data <- data[!duplicated(data$timestamp_posix), ]
  }

  rows_after_time <- nrow(data)
  message(sprintf("--> Removed %d rows due to timestamp issues.", original_rows - rows_after_time))

  # --- 3. RR Interval Processing ---
  message("--> Validating and filtering RR intervals...")
  # Coerce RR to numeric if needed
  if (!is.numeric(data[[rr_col]])) {
      message("Attempting to coerce RR column '", rr_col, "' to numeric.")
      data[[rr_col]] <- suppressWarnings(as.numeric(as.character(data[[rr_col]])))
  }

  # Handle NA RR values
  na_rr_indices <- which(is.na(data[[rr_col]]))
  if (length(na_rr_indices) > 0) {
      message(sprintf("Found %d NA values in RR column '%s'. These rows will be marked as failing initial QC.", length(na_rr_indices), rr_col))
      # Mark them, don't remove yet, let artifact detection see them if needed, or remove later
      data$initial_qc_passed <- TRUE
      data$initial_qc_passed[na_rr_indices] <- FALSE # Mark NAs as failing
  } else {
      data$initial_qc_passed <- TRUE # Assume pass initially if no NAs
  }

  # Apply range filtering ONLY to rows that haven't failed yet
  range_filter <- data[[rr_col]] >= min_rr & data[[rr_col]] <= max_rr
  range_fail_indices <- which(!range_filter & data$initial_qc_passed) # Find new failures

  if (length(range_fail_indices) > 0) {
      message(sprintf("Found %d RR intervals outside plausible range [%.1f, %.1f] ms. Marking as failing initial QC.",
                      length(range_fail_indices), min_rr, max_rr))
      data$initial_qc_passed[range_fail_indices] <- FALSE
  }

  n_failed_initial_qc <- sum(!data$initial_qc_passed)
  message(sprintf("--> Marked %d rows in total as failing initial QC (NA or out of range).", n_failed_initial_qc))

  # Select and rename columns for output
  # Keep rows that failed initial QC for now, they might be interpolated later
  # The `initial_qc_passed` flag indicates their status.
  result_data <- data %>%
      dplyr::select(timestamp_posix,
                    rr_ms = all_of(rr_col),
                    original_time_str,
                    initial_qc_passed)

  message("Step 1: Initial cleaning finished. Rows passing initial QC: ", sum(result_data$initial_qc_passed), "/", nrow(result_data))
  return(result_data)
}
