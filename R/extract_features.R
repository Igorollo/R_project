# R/extract_features.R
# Functions for extracting Time-Domain and Frequency-Domain HRV Features (v2 - Robust Helpers)

# Required libraries (ensure installed: install.packages(c("dplyr", "zoo", "lubridate", "matrixStats", "signal")))
library(dplyr)
library(zoo)        # For rollapply
library(lubridate)  # For duration calculations
library(matrixStats)# For efficient row/column stats if needed (rollapply often sufficient)
library(signal)     # Potentially for filtering, though spec.pgram handles basics

# --- Helper Functions (Revised for Robustness) ---

#' Calculate RMSSD (Root Mean Square of Successive Differences)
#' Vectorized function for RMSSD calculation on a vector of NN intervals.
#' @param nn_intervals Numeric vector of NN intervals (ms).
#' @return RMSSD value (ms), or NA if calculation is not possible
#' @keywords internal
calculate_rmssd <- function(nn_intervals) {
  # Ensure input is numeric and has length
  if (!is.numeric(nn_intervals) || length(nn_intervals) == 0) return(NA_real_)
  # RMSSD needs at least 2 points *overall* to calculate diff
  if (length(nn_intervals) < 2) {
    return(NA_real_)
  }
  diffs <- diff(nn_intervals) # diff handles NAs by propagating them
  # Ensure diffs is not empty and remove NAs before squaring and meaning
  if(length(diffs) == 0) return(NA_real_) # Should not happen if length(nn_intervals) >= 2
  diffs_valid_sq <- diffs[!is.na(diffs)]^2
  # Need at least one valid difference squared
  if(length(diffs_valid_sq) == 0) return(NA_real_)
  # Use stats::mean explicitly
  return(sqrt(mean(diffs_valid_sq, na.rm = FALSE))) # na.rm=FALSE now safe
}

#' Calculate SDNN (Standard Deviation of NN intervals)
#' Vectorized function for SDNN calculation on a vector of NN intervals.
#' @param nn_intervals Numeric vector of NN intervals (ms).
#' @return SDNN value (ms), or NA if calculation is not possible.
#' @keywords internal
calculate_sdnn <- function(nn_intervals) {
  # Ensure input is numeric and has length
  if (!is.numeric(nn_intervals) || length(nn_intervals) == 0) return(NA_real_)
  # Remove NAs before checking length for sd calculation validity
  nn_intervals_valid <- nn_intervals[!is.na(nn_intervals)]
  if (length(nn_intervals_valid) < 2) { # sd requires at least 2 non-NA points
    return(NA_real_)
  }
  # Use stats::sd explicitly
  return(stats::sd(nn_intervals_valid, na.rm = FALSE)) # Use stats::sd explicitly, na.rm=FALSE now safe
}


#' Calculate Power Spectral Density and Band Power
#' Uses spec.pgram to estimate PSD and calculates power in specified bands.
#' @param hr_segment Numeric vector of evenly sampled instantaneous HR (bpm).
#' @param fs Numeric. Sampling frequency in Hz (must match the data).
#' @param lf_band Numeric vector. Low frequency band limits [min_hz, max_hz].
#' @param hf_band Numeric vector. High frequency band limits [min_hz, max_hz].
#' @param taper Logical. Apply cosine taper? Default TRUE.
#' @param detrend Logical. Detrend the segment? Default TRUE.
#' @return A list containing LF power, HF power, and LF/HF ratio, or NAs if calculation fails.
#' @keywords internal
calculate_psd_bands <- function(hr_segment, fs, lf_band = c(0.04, 0.15), hf_band = c(0.15, 0.4), taper = TRUE, detrend = TRUE) {

  n_points <- length(hr_segment)
  # Need a minimum number of points for reliable PSD estimation
  min_points_needed <- max(30 * fs, 64) # e.g., 30 seconds * fs, or 64 points

  # Check for sufficient non-NA points
  valid_points <- sum(!is.na(hr_segment))
  if (valid_points < min_points_needed) {
     # warning(paste("Insufficient valid data points (", valid_points, ") for reliable PSD estimation. Need ~", min_points_needed))
     return(list(lf_power = NA_real_, hf_power = NA_real_, lf_hf_ratio = NA_real_))
  }

  psd_result <- tryCatch({
    stats::spec.pgram(hr_segment,
                      spans = NULL,
                      kernel = NULL,
                      taper = if(taper) 0.1 else 0,
                      pad = 0,
                      fast = TRUE,
                      demean = detrend,
                      detrend = detrend,
                      plot = FALSE,
                      na.action = na.omit) # na.omit handles internal NAs
  }, error = function(e) {
    # warning("spec.pgram failed: ", e$message) # Reduce verbosity
    return(NULL)
  })

  if (is.null(psd_result) || length(psd_result$freq) == 0) {
    return(list(lf_power = NA_real_, hf_power = NA_real_, lf_hf_ratio = NA_real_))
  }

  # Extract frequencies and spectral density
  freqs <- psd_result$freq * fs # Convert normalized frequency (cycles/sample) to Hz
  spec <- psd_result$spec

  # Calculate power in bands by integrating density (approximated by summing over freq bins)
  delta_f <- freqs[2] - freqs[1] # Assumes approximately constant spacing

  # Calculate power by summing density * delta_f in each band
  lf_indices <- which(freqs >= lf_band[1] & freqs <= lf_band[2])
  hf_indices <- which(freqs >= hf_band[1] & freqs <= hf_band[2])

  lf_power <- sum(spec[lf_indices] * delta_f, na.rm = TRUE)
  hf_power <- sum(spec[hf_indices] * delta_f, na.rm = TRUE)

  # Avoid division by zero for ratio
  lf_hf_ratio <- if (hf_power > 1e-9) lf_power / hf_power else NA_real_

  return(list(lf_power = lf_power, hf_power = hf_power, lf_hf_ratio = lf_hf_ratio))
}


# --- Main Feature Extraction Functions ---

#' Extract Time-Domain HRV Features over Rolling Windows
#'
#' Calculates SDNN and RMSSD on NN intervals within rolling time windows.
#'
#' @param cleaned_rr_data A data frame from `preprocess_hr$cleaned_rr`, containing
#'                        `timestamp_posix`, `rr_ms`, and `is_artifact`.
#' @param window_seconds Numeric. The length of the rolling window in seconds.
#' @param step_seconds Numeric. The step size (in seconds) between consecutive
#'                     windows. A step smaller than `window_seconds` creates
#'                     overlapping windows. Default is `window_seconds / 2`.
#' @param min_nn_required Integer. The minimum number of valid NN intervals required
#'                        within a window to calculate metrics. Default: 10.
#'
#' @return A data frame (tibble) with columns:
#'         - `window_end_time`: POSIXct timestamp marking the end of each window.
#'         - `sdnn`: Standard deviation of NN intervals in the window (ms).
#'         - `rmssd`: Root mean square of successive differences in the window (ms).
#'         Returns NULL if input is invalid or has insufficient data.
#' @export
extract_hrv_timed <- function(cleaned_rr_data, window_seconds = 300, step_seconds = NULL, min_nn_required = 10) {

  message("Extracting Time-Domain HRV features...") # Reduce verbosity
  if (!is.data.frame(cleaned_rr_data) || !all(c("timestamp_posix", "rr_ms", "is_artifact") %in% names(cleaned_rr_data))) {
    stop("Input 'cleaned_rr_data' must be a data frame with 'timestamp_posix', 'rr_ms', 'is_artifact'.")
  }
  if (nrow(cleaned_rr_data) < 2) {
     warning("Input data has fewer than 2 rows. Cannot calculate time-domain features.")
     return(NULL)
  }
  if (is.null(step_seconds)) {
      step_seconds <- window_seconds / 2 # Default to 50% overlap
  }
  # Filter to get valid NN intervals and their timestamps
  # 1a. Subset rows
  tmp <- cleaned_rr_data[!cleaned_rr_data$is_artifact, ]
  # 1b. Subset & rename columns
  nn_data <- tmp[c("timestamp_posix", "rr_ms")]
  names(nn_data)[names(nn_data) == "rr_ms"] <- "nn_ms"
  if (nrow(nn_data) < min_nn_required) {
     warning("Fewer than 'min_nn_required' valid NN intervals found (", nrow(nn_data), "). Cannot calculate rolling features.")
     return(NULL)
  }

  # Define window parameters
  start_time <- min(nn_data$timestamp_posix)
  end_time <- max(nn_data$timestamp_posix)
  total_duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  if (total_duration < window_seconds) {
      warning("Total duration of valid NN intervals (", round(total_duration), "s) is less than window size (", window_seconds, "s). Cannot create windows.")
      return(NULL)
  }

  # Define window end points
  window_ends <- seq(from = start_time + seconds(window_seconds),
                     to = end_time,
                     by = seconds(step_seconds))

  if (length(window_ends) == 0) {
       warning("Could not define any analysis windows with the given parameters.")
       return(NULL)
  }

  results_list <- list()
  message("Processing ", length(window_ends), " time windows...") # Reduce verbosity
  for (i in seq_along(window_ends)) {
    win_end <- window_ends[i]
    win_start <- win_end - seconds(window_seconds)

    # Select NN intervals within the current window
    nn_in_window <- nn_data$nn_ms[
      nn_data$timestamp_posix >= win_start &
      nn_data$timestamp_posix <= win_end
    ]
    # Calculate metrics if enough valid intervals exist OVERALL
    # The helper functions now handle internal NA checks and length requirements
    if (length(nn_in_window) >= min_nn_required) {
        # Call the robust helper functions
        sdnn_val <- calculate_sdnn(nn_in_window)
        rmssd_val <- calculate_rmssd(nn_in_window)
    } else {
        sdnn_val <- NA_real_
        rmssd_val <- NA_real_
    }


    results_list[[i]] <- tibble::tibble(
      window_end_time = win_end,
      sdnn = sdnn_val,
      rmssd = rmssd_val
    )
  }

  # Combine results
  time_domain_features <- bind_rows(results_list)

  # message("Time-Domain feature extraction finished. Calculated features for ", nrow(time_domain_features), " windows.") # Reduce verbosity
  return(time_domain_features)
}


#' Extract Frequency-Domain HRV Features over Rolling Windows
#'
#' Calculates LF power, HF power, and LF/HF ratio from an evenly sampled HR signal
#' within rolling time windows using PSD estimation.
#'
#' @param interpolated_hr_data A data frame from `preprocess_hr$interpolated_hr`,
#'                             containing `timestamp_posix` and `hr_bpm`.
#'                             **Crucially, this data MUST be evenly sampled.**
#' @param fs Numeric. The sampling frequency (in Hz) of the `interpolated_hr_data`.
#'           This MUST match the `resample_rate` used in `preprocess_hr`.
#' @param window_seconds Numeric. The length of the rolling window in seconds.
#'                       Should be long enough for reliable low-frequency estimation
#'                       (e.g., >= 60 seconds, preferably >= 120s).
#' @param step_seconds Numeric. The step size (in seconds) between consecutive
#'                     windows. Default is `window_seconds / 2`.
#' @param lf_band Numeric vector. Low frequency band limits [min_hz, max_hz].
#'                Default: c(0.04, 0.15).
#' @param hf_band Numeric vector. High frequency band limits [min_hz, max_hz].
#'                Default: c(0.15, 0.4).
#'
#' @return A data frame (tibble) with columns:
#'         - `window_end_time`: POSIXct timestamp marking the end of each window.
#'         - `lf_power`: Absolute power in the LF band.
#'         - `hf_power`: Absolute power in the HF band.
#'         - `lf_hf_ratio`: The ratio of LF power to HF power.
#'         Returns NULL if input is invalid, not evenly sampled, or has insufficient data.
#' @export
extract_hrv_freq <- function(interpolated_hr_data, fs, window_seconds = 300, step_seconds = NULL,
                             lf_band = c(0.04, 0.15), hf_band = c(0.15, 0.4)) {

  # message("Extracting Frequency-Domain HRV features...") # Reduce verbosity
  if (!is.data.frame(interpolated_hr_data) || !all(c("timestamp_posix", "hr_bpm") %in% names(interpolated_hr_data))) {
    stop("Input 'interpolated_hr_data' must be a data frame with 'timestamp_posix', 'hr_bpm'.")
  }
   if (nrow(interpolated_hr_data) < 2) {
     # warning("Input interpolated HR data has fewer than 2 rows. Cannot calculate frequency-domain features.")
     return(NULL)
  }
  if (missing(fs) || !is.numeric(fs) || fs <= 0) {
      stop("Sampling frequency 'fs' (Hz) must be provided as a positive number.")
  }
   if (is.null(step_seconds)) {
      step_seconds <- window_seconds / 2 # Default to 50% overlap
  }

  # Check for even sampling (within a small tolerance)
  time_diffs <- diff(as.numeric(interpolated_hr_data$timestamp_posix))
  expected_diff <- 1 / fs
  # Increase tolerance slightly, as resampling might not be perfect
  if (nrow(interpolated_hr_data) > 1 && any(abs(time_diffs - expected_diff) > (expected_diff * 0.05))) { # Allow 5% tolerance
      warning("Input data time differences deviate significantly from expected interval (1/fs). Frequency analysis may be inaccurate. Ensure 'resample_rate' was set correctly in preprocess_hr().")
  }


  # Define window parameters in terms of samples
  window_samples <- round(window_seconds * fs)
  step_samples <- round(step_seconds * fs)
  n_total_samples <- nrow(interpolated_hr_data)

  if (n_total_samples < window_samples) {
      # warning("Total number of samples (", n_total_samples, ") is less than window size (", window_samples, " samples). Cannot create windows.")
      return(NULL)
  }

  # Define window start indices
  window_starts_idx <- seq(from = 1,
                           to = n_total_samples - window_samples + 1,
                           by = step_samples)

  if (length(window_starts_idx) == 0) {
       # warning("Could not define any analysis windows with the given parameters.")
       return(NULL)
  }

  results_list <- list()
  # message("Processing ", length(window_starts_idx), " frequency windows...") # Reduce verbosity

  for (i in seq_along(window_starts_idx)) {
    start_idx <- window_starts_idx[i]
    end_idx <- start_idx + window_samples - 1

    # Ensure end_idx does not exceed bounds (can happen with rounding)
    if(end_idx > n_total_samples) next # Skip incomplete window at the end

    # Get HR segment for the window
    hr_segment <- interpolated_hr_data$hr_bpm[start_idx:end_idx]

    # Get timestamp for the end of the window
    window_end_time <- interpolated_hr_data$timestamp_posix[end_idx]

    # Calculate PSD and band powers
    psd_results <- calculate_psd_bands(hr_segment, fs = fs, lf_band = lf_band, hf_band = hf_band)

    results_list[[i]] <- tibble::tibble(
      window_end_time = window_end_time,
      lf_power = psd_results$lf_power,
      hf_power = psd_results$hf_power,
      lf_hf_ratio = psd_results$lf_hf_ratio
    )
  }

  # Combine results
  freq_domain_features <- bind_rows(results_list)

  # message("Frequency-Domain feature extraction finished. Calculated features for ", nrow(freq_domain_features), " windows.") # Reduce verbosity
  return(freq_domain_features)
}


#' Extract Time and Frequency Domain HRV Features
#'
#' Wrapper function to run both time-domain and frequency-domain HRV feature
#' extraction using the output from `preprocess_hr`.
#'
#' @param preprocessed_results List. The output from the `preprocess_hr` function,
#'                             containing `$cleaned_rr` and `$interpolated_hr`.
#' @param fs Numeric. The sampling frequency (in Hz) of the `interpolated_hr_data`.
#'           Required for frequency-domain analysis. Must match `resample_rate`.
#' @param window_seconds Numeric. Window length in seconds for analysis. Used for
#'                       both time and frequency domains. Default: 300 (5 minutes).
#' @param step_seconds Numeric. Step size in seconds between windows. Default: 150 (2.5 minutes).
#' @param min_nn_required Integer. Minimum valid NN intervals for time-domain window. Default: 10.
#' @param lf_band Numeric vector. Low frequency band limits [min_hz, max_hz]. Default: c(0.04, 0.15).
#' @param hf_band Numeric vector. High frequency band limits [min_hz, max_hz]. Default: c(0.15, 0.4).
#' @param combine_results Logical. If TRUE, attempts to join the time and frequency
#'                        domain results into a single data frame based on the
#'                        `window_end_time`. Default: TRUE.
#'
#' @return If `combine_results` is TRUE, a single data frame (tibble) with
#'         `window_end_time` and all calculated time and frequency features.
#'         If `combine_results` is FALSE, a list containing two data frames:
#'         `$time_domain` and `$frequency_domain`.
#'         Returns NULL if preprocessing results are invalid or feature extraction fails.
#' @export
extract_hrv_features <- function(preprocessed_results, fs, window_seconds = 300, step_seconds = NULL,
                                 min_nn_required = 10, lf_band = c(0.04, 0.15), hf_band = c(0.15, 0.4),
                                 combine_results = TRUE) {

  message("===== Starting HRV Feature Extraction Pipeline =====")

  # Validate input
  if (!is.list(preprocessed_results) || !all(c("cleaned_rr", "interpolated_hr") %in% names(preprocessed_results))) {
    stop("'preprocessed_results' must be a list containing '$cleaned_rr' and '$interpolated_hr' data frames.")
  }
   if (missing(fs) || !is.numeric(fs) || fs <= 0) {
      stop("Sampling frequency 'fs' (Hz) must be provided for frequency-domain analysis.")
  }
   if (is.null(step_seconds)) {
      step_seconds <- window_seconds / 2 # Default overlap
      message("Using default step size: ", step_seconds, " seconds.")
  }


  # --- Extract Time-Domain Features ---
  td_features <- tryCatch({
      extract_hrv_timed(cleaned_rr_data = preprocessed_results$cleaned_rr,
                        window_seconds = window_seconds,
                        step_seconds = step_seconds,
                        min_nn_required = min_nn_required)
  }, error = function(e) {
      # Provide a more informative warning, including the original error message
      warning("Failed to extract time-domain features. Original error: ", e$message, call. = FALSE)
      return(NULL)
  })

  # --- Extract Frequency-Domain Features ---
  fd_features <- tryCatch({
       extract_hrv_freq(interpolated_hr_data = preprocessed_results$interpolated_hr,
                        fs = fs,
                        window_seconds = window_seconds,
                        step_seconds = step_seconds,
                        lf_band = lf_band,
                        hf_band = hf_band)
  }, error = function(e) {
      warning("Failed to extract frequency-domain features. Original error: ", e$message, call. = FALSE)
      return(NULL)
  })

  # --- Combine Results ---
  if (is.null(td_features) && is.null(fd_features)) {
      message("Both time and frequency domain feature extraction failed.")
      message("===== HRV Feature Extraction Pipeline Finished (Failed) =====")
      return(NULL)
  }
  td_failed <- is.null(td_features) || nrow(td_features) == 0
  fd_failed <- is.null(fd_features) || nrow(fd_features) == 0

  # create a string key on both tables
  td2 <- td_features %>%
    mutate(wet_round = round_date(window_end_time, "second"))
  fd2 <- fd_features %>%
    mutate(wet_round = round_date(window_end_time, "second"))
  if (combine_results) {
    # Case 1: both domains succeeded
    if (!td_failed && !fd_failed) {
      message("Combining time and frequency domain results...")
      combined <- td2 %>%
        inner_join(fd2, by = "wet_round") %>%
        rename(window_end_time = window_end_time.x) %>%
        select(-window_end_time.y, -wet_round) %>%
        arrange(window_end_time)
      if (!is.null(combined)) {
        message("Results combined successfully.")
        message("===== HRV Feature Extraction Pipeline Finished Successfully =====")
        return(combined)
      }
      warning("Returning features separately as join failed.", call. = FALSE)
      message("===== HRV Feature Extraction Pipeline Finished (Partial Success) =====")
      return(list(time_domain = td_features, frequency_domain = fd_features))
    }

    # Case 2: one or both domains failed
    message("===== HRV Feature Extraction Pipeline Finished (Partial Success) =====")
    if (td_failed && fd_failed) {
      message("Both domains failed.")
      return(NULL)
    }
    if (fd_failed) {
      warning("Frequency-domain features failed or produced no results, returning only time-domain features.", call. = FALSE)
      return(td_features)
    }
    # (here td_failed == TRUE)
    warning("Time-domain features failed or produced no results, returning only frequency-domain features.", call. = FALSE)
    return(fd_features)

  } else {
    # combine_results == FALSE
    message("Returning results as a list.")
    message("===== HRV Feature Extraction Pipeline Finished Successfully =====")
    return(list(time_domain = td_features, frequency_domain = fd_features))
  }
}


# --- Example Usage (within the script for testing, if run directly) ---
if (sys.nframe() == 0) {

    message("\n--- Running Feature Extraction Test (v2) ---")

    # 1. Ensure preprocessing functions are available and run preprocessing
    if (!exists("preprocess_hr")) {
        if(file.exists("R/preprocess_hr.R")) {
           source("R/preprocess_hr.R")
        } else {
            stop("Cannot find R/preprocess_hr.R. Please ensure it exists.")
        }
    }

    sample_file <- "hr_data/sample_hr_preprocess_test.txt" # Use the same sample file
    if (!file.exists(sample_file)) {
         message("Sample file '", sample_file, "' not found. Creating it.")
         # Recreate sample file content if needed
         if (!dir.exists("hr_data")) dir.create("hr_data")
         sample_content <- "Time,rr_ms\n01:22:59.835Z,1971.6\n01:23:00.319Z,481.4\n01:23:00.819Z,481.4\n01:23:01.303Z,50.0\n01:23:01.803Z,503.9\n01:23:01.900Z,2500.0\n01:23:02.537Z,531.2\n01:23:02.787Z,539.0\n01:23:03.271Z,NA\n01:23:03.800Z,550.0\n01:23:03.800Z,551.0\nBAD_TIME,555.0\n01:23:04.500Z,560.0\n01:23:05.000Z,565.0\n01:23:05.500Z,900.0\n01:23:06.000Z,570.0"
         writeLines(sample_content, sample_file)
    }

    # Run preprocessing WITH resampling enabled (e.g., 4Hz)
    sampling_freq <- 4.0
    preproc_results <- preprocess_hr(file_path = sample_file,
                                     resample_rate = sampling_freq, # MUST match fs below
                                     interpolation_method = "linear", # Use linear for testing robustness
                                     time_col = "Time", rr_col = "rr_ms")

    # 2. Extract features if preprocessing was successful
    if (!is.null(preproc_results)) {
        hrv_features <- extract_hrv_features(preproc_results,
                                             fs = sampling_freq, # MUST provide correct fs
                                             window_seconds = 60, # Use shorter window for demo
                                             step_seconds = 30,
                                             combine_results = TRUE)

        if (!is.null(hrv_features) && nrow(hrv_features) > 0) {
            message("\n--- Extracted HRV Features (Head) ---")
            print(head(hrv_features))

            # Optional: Plot one of the features over time
            if (requireNamespace("ggplot2", quietly = TRUE)) {
                library(ggplot2)
                p_rmssd <- ggplot(hrv_features, aes(x = window_end_time, y = rmssd)) +
                    geom_line(color = "darkorange", na.rm = TRUE) + # na.rm=TRUE for plotting gaps
                    geom_point(color = "darkorange", na.rm = TRUE) +
                    labs(title = "RMSSD over Time (60s windows)", y = "RMSSD (ms)") +
                    theme_minimal()
                print(p_rmssd)

                 p_lfhf <- ggplot(hrv_features, aes(x = window_end_time, y = lf_hf_ratio)) +
                    geom_line(color = "purple", na.rm = TRUE) +
                    geom_point(color = "purple", na.rm = TRUE) +
                    labs(title = "LF/HF Ratio over Time (60s windows)", y = "LF/HF Ratio") +
                    theme_minimal()
                print(p_lfhf)
            }

        } else {
            message("Feature extraction failed or produced no results.")
        }
    } else {
        message("Preprocessing failed, cannot extract features.")
    }

     message("\n--- Feature Extraction Test Finished ---")
}
