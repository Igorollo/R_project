# R/interpolate_hr.R
# Function to convert RR intervals to instantaneous HR and interpolate over artifacts.

# Required libraries
library(dplyr)
library(zoo) # For na.approx, na.spline

#' Convert RR to Instantaneous HR and Interpolate Artifacts
#'
#' Converts cleaned RR intervals (ms) to instantaneous heart rate (beats per minute),
#' then interpolates the HR values at time points marked as artifacts. Optionally
#' resamples the interpolated HR onto an evenly spaced time grid.
#'
#' @param data A data frame containing `timestamp_posix`, `rr_ms`, and `is_artifact`
#'             columns, typically the output from `detect_artifacts_malik`.
#' @param interpolation_method Character string. Method for interpolation.
#'        Options: "linear", "spline". Default: "linear".
#' @param resample_rate Numeric or NULL. If numeric, the target resampling rate in Hz
#'                      (e.g., 4 for 4 Hz). The interpolated HR signal will be
#'                      resampled onto an even time grid at this rate using the
#'                      specified `interpolation_method`. If NULL, interpolation only
#'                      happens at the original artifact time points, and the output
#'                      timestamps match the input. Default: NULL.
#'
#' @return A data frame with columns:
#'         - `timestamp_posix`: POSIXct timestamps. If resampling, these form an
#'                              even grid. Otherwise, they match the input timestamps.
#'         - `hr_bpm`: Numeric instantaneous heart rate (beats per minute),
#'                     interpolated over artifacts and potentially resampled.
#'
#' @keywords internal
interpolate_hr <- function(data, interpolation_method = "linear", resample_rate = NULL) {

    message("Step 3: Interpolating instantaneous heart rate...")
    if (!is.data.frame(data) || !all(c("timestamp_posix", "rr_ms", "is_artifact") %in% names(data))) {
        stop("Input 'data' must be a data frame with 'timestamp_posix', 'rr_ms', and 'is_artifact' columns.")
    }
    if (!interpolation_method %in% c("linear", "spline")) {
        stop("'interpolation_method' must be 'linear' or 'spline'.")
    }
    if (!is.null(resample_rate) && (!is.numeric(resample_rate) || length(resample_rate) != 1 || resample_rate <= 0)) {
        stop("'resample_rate' must be NULL or a single positive number (Hz).")
    }

    # --- 1. Calculate Instantaneous HR (only for non-artifact points) ---
    data <- data %>%
        dplyr::mutate(
            hr_bpm = ifelse(is_artifact, NA_real_, 60000 / rr_ms) # Calculate HR only for valid RR
        )

    n_artifacts <- sum(is.na(data$hr_bpm)) # Count points needing interpolation
    if (n_artifacts == 0) {
        message("--> No artifacts found needing interpolation.")
        if (is.null(resample_rate)) {
           # No interpolation needed, no resampling requested -> return HR at original times
           message("Step 3: Interpolation/Resampling finished (no action needed).")
           return(data %>% dplyr::select(timestamp_posix, hr_bpm))
        }
        # Proceed to resampling even if no artifacts
    } else {
        message(sprintf("--> Preparing to interpolate %d artifact points using '%s' method.", n_artifacts, interpolation_method))
    }

    # Ensure data is sorted by time for interpolation
    data <- data %>% dplyr::arrange(timestamp_posix)

    # --- 2. Interpolate HR over NA points (artifacts) ---
    # Convert timestamps to numeric for zoo functions if needed, but zoo handles POSIXct
    timestamps_num <- as.numeric(data$timestamp_posix)
    print(str(data))
print(head(data$timestamp_posix))
    if (interpolation_method == "linear") {
        # Use zoo::na.approx for linear interpolation
        # rule=2 extends the last known value if NAs are at the ends
        interpolated_hr <- zoo::na.approx(data$hr_bpm, x = data$timestamp_posix, na.rm = FALSE, rule = 2)
    } else if (interpolation_method == "spline") {
        # Use zoo::na.spline for cubic spline interpolation
        # Requires at least 4 non-NA points for default spline; check this.
        n_valid_hr <- sum(!is.na(data$hr_bpm))
        if (n_valid_hr < 4) {
             warning("Fewer than 4 valid HR points available. Falling back to linear interpolation instead of spline.")
             interpolated_hr <- zoo::na.approx(data$hr_bpm, x = data$timestamp_posix, na.rm = FALSE, rule = 2)
             interpolation_method <- "linear (fallback)" # Update message
        } else {
             interpolated_hr <- zoo::na.spline(data$hr_bpm, x = data$timestamp_posix, na.rm = FALSE)
             # na.spline might not handle leading/trailing NAs as robustly as na.approx rule=2
             # Fill remaining NAs (likely at ends) linearly if spline failed there
             if (any(is.na(interpolated_hr))) {
                 message("--> Filling remaining NAs (likely ends) after spline with linear method.")
                 interpolated_hr <- zoo::na.approx(interpolated_hr, x = data$timestamp_posix, na.rm = FALSE, rule = 2)
             }
        }
    }

    # Check if any NAs remain after interpolation attempt (shouldn't with rule=2)
    if (any(is.na(interpolated_hr))) {
       warning("NA values remain after interpolation. This might indicate issues like insufficient valid points at the start/end.")
       # Decide how to handle: remove rows, fill with mean/median, or leave as NA?
       # For now, leave them as NA and let downstream analysis handle.
       message("--> Remaining NAs: ", sum(is.na(interpolated_hr)))
    }

    data$hr_bpm_interpolated <- interpolated_hr
    message(sprintf("--> Interpolation using '%s' completed.", interpolation_method))


    # --- 3. Optional Resampling ---
    if (!is.null(resample_rate)) {
        message(sprintf("--> Resampling interpolated HR to %.2f Hz...", resample_rate))

        # Create the new regular time grid
        start_time <- min(data$timestamp_posix)
        end_time <- max(data$timestamp_posix)
        time_step_seconds <- 1 / resample_rate
        new_timestamps <- seq(from = start_time, to = end_time, by = time_step_seconds)

        # Interpolate onto the new grid using the *already interpolated* HR signal
        # We need an interpolation function that works on numeric time for the new grid
        if (interpolation_method == "linear" || grepl("linear", interpolation_method)) {
            resampled_hr <- stats::approx(x = as.numeric(data$timestamp_posix), y = data$hr_bpm_interpolated,
                                          xout = as.numeric(new_timestamps), method = "linear", rule = 2)$y
        } else { # Spline
             # Check again if enough points for spline resampling
             if (sum(!is.na(data$hr_bpm_interpolated)) < 4) {
                 warning("Fewer than 4 interpolated HR points. Using linear resampling instead of spline.")
                 resampled_hr <- stats::approx(x = as.numeric(data$timestamp_posix), y = data$hr_bpm_interpolated,
                                               xout = as.numeric(new_timestamps), method = "linear", rule = 2)$y
             } else {
                 resampled_hr <- stats::spline(x = as.numeric(data$timestamp_posix), y = data$hr_bpm_interpolated,
                                                xout = as.numeric(new_timestamps), method = "fmm")$y # Natural spline
             }
        }

        result_data <- data.frame(
            timestamp_posix = new_timestamps,
            hr_bpm = resampled_hr
        )
        message(sprintf("--> Resampling finished. Result has %d points.", nrow(result_data)))

    } else {
        # No resampling requested, return interpolated HR at original timestamps
        result_data <- data %>%
            dplyr::select(timestamp_posix, hr_bpm = hr_bpm_interpolated)
        message("--> No resampling performed.")
    }

    message("Step 3: Interpolation/Resampling finished.")
    # Remove rows with NA hr_bpm if any remained after interpolation/resampling
    # Defensive checks before returning
    if (!('timestamp_posix' %in% names(result_data))) {
        stop("result_data is missing 'timestamp_posix' column after resampling!")
    }
    if (!('hr_bpm' %in% names(result_data))) {
        stop("result_data is missing 'hr_bpm' column after resampling!")
    }
    if (!is.numeric(result_data$hr_bpm)) {
        stop("result_data$hr_bpm is not numeric after resampling!")
    }
    if (!inherits(result_data$timestamp_posix, 'POSIXct')) {
        stop("result_data$timestamp_posix is not POSIXct after resampling!")
    }
    final_result <- result_data[!is.na(result_data$hr_bpm), ]
    if(nrow(final_result) < nrow(result_data)){
        message("--> Removed ", nrow(result_data) - nrow(final_result), " rows with NA HR values after final processing.")
    }

    return(final_result)
}