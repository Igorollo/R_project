# R/detect_artifacts_malik.R
# Functions for detecting artifacts in RR interval series, including Malik criterion.

# Required libraries
library(dplyr)
library(zoo) # For rollmean (optional, if using local average)

#' Detect Artifacts in RR Interval Series using Malik Criterion
#'
#' Identifies potential artifacts in an RR interval series based on the Malik
#' criterion, which checks for excessive deviation from neighboring beats.
#' Assumes input data has passed initial QC (range checks, NA handling).
#'
#' @param data A data frame containing `timestamp_posix` and `rr_ms` columns,
#'             ideally output from `clean_hr_initial`. Must contain at least 3 rows
#'             of data that passed initial QC for the checks to work.
#' @param threshold Numeric. The tolerance threshold (ratio, 0 to 1). An RR interval
#'                  `RR(i)` is flagged if it differs from BOTH `RR(i-1)` and `RR(i+1)`
#'                  by more than `threshold * RR(i-1)` and `threshold * RR(i+1)`
#'                  respectively. Default: 0.2 (20%).
#' @param use_local_average Logical. If TRUE, additionally compares `RR(i)` to a
#'                          rolling mean of preceding beats instead of just `RR(i-1)`.
#'                          (Currently simplified to only use neighbors). Default: FALSE.
#' @param window_size Integer. If `use_local_average` is TRUE, the number of
#'                    preceding beats to include in the rolling mean. Default: 5.
#'
#' @return The input data frame with an added logical column `is_artifact`.
#'         `TRUE` indicates the RR interval at that row is considered an artifact.
#'         Rows that did not pass initial QC (if present in input) are also marked
#'         as artifacts.
#'
#' @keywords internal
detect_artifacts_malik <- function(data, threshold = 0.45, use_local_average = FALSE, window_size = 5) {

    message("Step 2: Detecting artifacts using Malik criterion...")
    if (!is.data.frame(data) || !all(c("timestamp_posix", "rr_ms", "initial_qc_passed") %in% names(data))) {
        stop("Input 'data' must be a data frame with 'timestamp_posix', 'rr_ms', and 'initial_qc_passed' columns.")
    }
    if (!is.numeric(threshold) || length(threshold) != 1 || threshold <= 0 || threshold >= 1) {
        stop("'threshold' must be a single number strictly between 0 and 1.")
    }

    # Work only on rows that passed initial QC for detection logic
    valid_indices <- which(data$initial_qc_passed)
    if (length(valid_indices) < 3) {
        warning("Fewer than 3 valid data points available after initial QC. Malik artifact detection requires neighbors and cannot be reliably performed. Marking all initial QC failures as artifacts.")
        data$is_artifact <- !data$initial_qc_passed
        return(data)
    }

    # Extract valid RR intervals
    rr_valid <- data$rr_ms[valid_indices]
    n_valid <- length(rr_valid)
    artifact_flags_valid <- rep(FALSE, n_valid) # Initialize artifact flags for valid points

    # --- Malik Criterion Logic ---
    # Compare RR(i) with RR(i-1) and RR(i+1)
    # Vectorized approach:
    # Calculate forward difference ratio: |RR(i) - RR(i+1)| / RR(i+1)
    # Calculate backward difference ratio: |RR(i) - RR(i-1)| / RR(i-1)

    # Need to handle endpoints carefully (first and last valid points cannot be checked this way)
    rr_prev <- c(NA, rr_valid[-n_valid]) # RR(i-1) for i=2..n
    rr_next <- c(rr_valid[-1], NA)       # RR(i+1) for i=1..n-1

    # Calculate absolute differences
    abs_diff_prev <- abs(rr_valid - rr_prev)
    abs_diff_next <- abs(rr_valid - rr_next)

    # Calculate thresholds based on neighbors
    # Avoid division by zero/small numbers, although unlikely for RR > min_rr
    threshold_prev <- ifelse(!is.na(rr_prev) & rr_prev > 1e-6, threshold * rr_prev, Inf)
    threshold_next <- ifelse(!is.na(rr_next) & rr_next > 1e-6, threshold * rr_next, Inf)

    # Flag as artifact if deviation from BOTH neighbors exceeds threshold
    # Ignore first and last valid points for this specific check (indices 2 to n_valid-1)
    malik_artifacts <- (abs_diff_prev > threshold_prev) & (abs_diff_next > threshold_next)

    # Apply flags (excluding first and last valid points initially)
    artifact_flags_valid[2:(n_valid - 1)] <- malik_artifacts[2:(n_valid - 1)]

    # --- Optional: Add other criteria here if needed ---
    # e.g., Simple successive difference check (from previous version)
    # rr_diff_ratio <- abs(rr_valid - rr_prev) / rr_prev
    # successive_artifacts <- rr_diff_ratio > some_other_threshold
    # artifact_flags_valid <- artifact_flags_valid | successive_artifacts # Combine rules

    # --- Combine Results ---
    # Initialize the final artifact column in the original dataframe
    data$is_artifact <- !data$initial_qc_passed # Start by marking initial failures as artifacts

    # Update the artifact status for rows that passed initial QC
    data$is_artifact[valid_indices] <- artifact_flags_valid

    n_artifacts_detected <- sum(artifact_flags_valid)
    n_total_artifacts <- sum(data$is_artifact)
    message(sprintf("--> Detected %d potential artifacts using Malik criterion (threshold=%.2f) among initially valid points.", n_artifacts_detected, threshold))
    message(sprintf("--> Total rows marked as artifacts (including initial QC failures): %d / %d.", n_total_artifacts, nrow(data)))

    message("Step 2: Artifact detection finished.")
    return(data)
}
