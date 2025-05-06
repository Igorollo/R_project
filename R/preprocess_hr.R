# R/preprocess_hr.R
# Main pipeline function for preprocessing HR (RR interval) data.
# Orchestrates cleaning, artifact detection, and interpolation.

# --- Source Helper Functions ---
# Ensure these files are in the R/ directory relative to the working directory
source("R/clean_hr_initial.R")
source("R/detect_artifacts_malik.R")
source("R/interpolate_hr.R")

# Required libraries (ensure installed)
# install.packages(c("readr", "dplyr", "lubridate", "zoo", "ggplot2"))
library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2) # For plotting in tests

#' Preprocess Heart Rate (RR Interval) Data
#'
#' Full pipeline to clean, detect artifacts, and interpolate RR interval data,
#' preparing it for time-domain (cleaned RR/NN) and frequency-domain (interpolated HR)
#' HRV analysis.
#'
#' @param data A data frame containing time series data, or a file path to a
#'             CSV/TXT file readable by `readr::read_csv`.
#' @param time_col Character string. Name of the timestamp column.
#' @param rr_col Character string. Name of the RR interval column (in ms).
#' @param min_rr Numeric. Minimum plausible RR interval (ms). Default: 300.
#' @param max_rr Numeric. Maximum plausible RR interval (ms). Default: 2000.
#' @param malik_threshold Numeric. Threshold for Malik artifact detection (ratio). Default: 0.2.
#' @param interpolation_method Character string. Method for HR interpolation
#'        ("linear" or "spline"). Default: "linear".
#' @param resample_rate Numeric or NULL. Target rate (Hz) for resampling interpolated HR.
#'                      If NULL, HR is interpolated only at artifact points. Default: 4.0.
#' @param tz Character string. Timezone for timestamp parsing. Default: "UTC".
#' @param file_path If `data` is NULL, specify the path to the data file here.
#'
#' @return A list containing two data frames:
#'         - `cleaned_rr`: Contains `timestamp_posix`, original `rr_ms`,
#'                         `initial_qc_passed` (logical), and `is_artifact` (logical).
#'                         Suitable for calculating time-domain HRV metrics from NN intervals
#'                         (by filtering where `is_artifact == FALSE`).
#'         - `interpolated_hr`: Contains `timestamp_posix` and `hr_bpm`. The HR is
#'                              interpolated over artifacts and potentially resampled
#'                              to `resample_rate`. Suitable for frequency-domain HRV.
#'         Returns NULL if input data loading or critical processing fails.
#' @export
#'
#' @examples
#' # --- Example using the sample file created in the test section ---
#' sample_file <- "/Users/igor/Downloads/R/hr_data/10.txt"
#' # Create the file if it doesn't exist (see test section below)
#' if (!dir.exists("hr_data")) dir.create("hr_data")
#' if (!file.exists(sample_file)) {
#'   writeLines("Time,rr_ms\n01:22:59.835Z,1971.6\n01:23:00.319Z,481.4\n01:23:00.819Z,481.4\n01:23:01.303Z,50.0\n01:23:01.803Z,503.9\n01:23:01.900Z,2500.0\n01:23:02.537Z,531.2\n01:23:02.787Z,539.0\n01:23:03.271Z,NA\n01:23:03.800Z,550.0\n01:23:03.800Z,551.0\nBAD_TIME,555.0\n01:23:04.500Z,560.0\n01:23:05.000Z,565.0\n01:23:05.500Z,900.0\n01:23:06.000Z,570.0", sample_file)
#' }
#
#' results <- preprocess_hr(file_path = sample_file,
#'                          time_col = "Time", rr_col = "rr_ms",
#'                          malik_threshold = 0.25, # Adjust threshold
#'                          resample_rate = 4.0)   # Request 4Hz resampling
#
#' if (!is.null(results)) {
#'    print("--- Cleaned RR Data (Head) ---")
#'    print(head(results$cleaned_rr))
#'    print(paste("Total artifacts marked:", sum(results$cleaned_rr$is_artifact)))
#
#'    print("--- Interpolated HR Data (Head) ---")
#'    print(head(results$interpolated_hr))
#' }
#'
preprocess_hr <- function(data = NULL,
                          file_path = NULL,
                          time_col = "Time",
                          rr_col = "rr_ms",
                          min_rr = 300,
                          max_rr = 2000,
                          malik_threshold = 0.2,
                          interpolation_method = "linear",
                          resample_rate = 4.0, # Default to 4Hz resampling
                          tz = "UTC") {

    message("===== Starting HR Preprocessing Pipeline =====")

    # --- 0. Load Data if file_path is provided ---
    if (is.null(data) && !is.null(file_path)) {
        message("Loading data from file: ", file_path)
        if (!file.exists(file_path)) {
            stop("File not found: ", file_path)
        }
        raw_data <- tryCatch({
            readr::read_csv(file_path, col_types = readr::cols(.default = "c"), # Read all as char first
                            show_col_types = FALSE) %>%
                # Attempt to convert rr_col specifically
                mutate(!!rlang::sym(rr_col) := suppressWarnings(as.numeric(!!rlang::sym(rr_col))))

        }, error = function(e) {
            stop("Error reading file '", file_path, "': ", e$message)
        })
         # Check if essential columns exist after loading
        if (!time_col %in% names(raw_data)) stop("Timestamp column '", time_col, "' not found in loaded file.")
        if (!rr_col %in% names(raw_data)) stop("RR interval column '", rr_col, "' not found in loaded file.")
        data <- raw_data
    } else if (is.null(data) && is.null(file_path)) {
        stop("Either 'data' (a data frame) or 'file_path' must be provided.")
    } else if (!is.data.frame(data)) {
        stop("'data' must be a data frame if provided directly.")
    }
     # Ensure columns exist in provided data frame
    if (!time_col %in% names(data)) stop("Timestamp column '", time_col, "' not found in provided data frame.")
    if (!rr_col %in% names(data)) stop("RR interval column '", rr_col, "' not found in provided data frame.")


    # --- 1. Initial Cleaning & Validation ---
    cleaned_initial_data <- tryCatch({
        clean_hr_initial(data, time_col = time_col, rr_col = rr_col,
                         min_rr = min_rr, max_rr = max_rr, tz = tz)
    }, error = function(e) {
        message("Error during initial cleaning step: ", e$message)
        return(NULL)
    })
    if (is.null(cleaned_initial_data) || nrow(cleaned_initial_data) == 0) {
        message("Preprocessing failed: No valid data after initial cleaning.")
        return(NULL)
    }

    # --- 2. Artifact Detection ---
    artifact_data <- tryCatch({
        detect_artifacts_malik(cleaned_initial_data, threshold = malik_threshold)
    }, error = function(e) {
        message("Error during artifact detection step: ", e$message)
        # Return the initial data with a warning? Or fail? Let's fail for now.
        return(NULL)
    })
     if (is.null(artifact_data)) {
        message("Preprocessing failed: Artifact detection step failed.")
        return(NULL)
    }


    # --- 3. Interpolation & Resampling ---
    interpolated_hr_data <- tryCatch({
        interpolate_hr(artifact_data,
                       interpolation_method = interpolation_method,
                       resample_rate = resample_rate)
    }, error = function(e) {
        message("Error during interpolation/resampling step: ", e$message)
        return(NULL)
    })
     if (is.null(interpolated_hr_data)) {
        print(interpolated_hr_data)
        message("Preprocessing failed: Interpolation/resampling step failed.")
        # We might still have the cleaned_rr data, maybe return partial results?
        # For now, return NULL if this step fails.
        return(NULL)
    }
    # --- 4. Prepare Output ---
    results <- list(
        cleaned_rr = artifact_data %>% # Select relevant columns for time-domain
                       dplyr::select(timestamp_posix, rr_ms, initial_qc_passed, is_artifact),
        interpolated_hr = interpolated_hr_data
    )

    message("===== HR Preprocessing Pipeline Finished Successfully =====")
    return(results)
}


# --- Example Usage & Testing ---

# Function to create a plot comparing raw, cleaned, and interpolated data
plot_preprocessing_results <- function(raw_data, cleaned_rr, interpolated_hr, time_col="Time", rr_col="rr_ms") {
    if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("tidyr", quietly = TRUE)) {
        message("Install ggplot2 and tidyr packages to generate comparison plots.")
        return(invisible(NULL))
    }
    library(ggplot2)
    library(tidyr)

    # Prepare raw data (handle potential parsing errors gracefully for plotting)
    raw_plot_data <- raw_data %>%
        mutate(timestamp_posix = suppressWarnings(lubridate::parse_date_time(!!rlang::sym(time_col),
                                                                             orders = c("H:M:OS", "Ymd H:M:OS", "Y-m-d H:M:OS"), # Removed literal Z
                                                                             tz = "UTC", quiet=TRUE)),
               rr_ms = suppressWarnings(as.numeric(!!rlang::sym(rr_col)))) %>%
        filter(!is.na(timestamp_posix), !is.na(rr_ms)) %>%
        select(timestamp_posix, rr_ms) %>%
        mutate(Source = "Raw RR")

    # Prepare cleaned RR data (mark artifacts visually)
    cleaned_plot_data <- cleaned_rr %>%
        mutate(Source = ifelse(is_artifact, "Artifact RR", "Cleaned RR")) %>%
        select(timestamp_posix, rr_ms, Source)

    # Prepare interpolated HR data (convert back to RR for comparison axis, if desired, or plot HR on secondary axis)
    # Let's plot HR on the same plot but potentially different scale/color
    interpolated_plot_data <- interpolated_hr %>%
        mutate(Source = "Interpolated HR") %>%
        select(timestamp_posix, hr_bpm, Source) # Keep HR in bpm

    # Combine RR data for plotting
    rr_combined <- bind_rows(raw_plot_data, cleaned_plot_data)

    # Create plot
    p <- ggplot() +
        # Plot Raw RR intervals as grey points/line
        geom_line(data = filter(rr_combined, Source == "Raw RR"), aes(x = timestamp_posix, y = rr_ms), color = "grey80", alpha = 0.6) +
        geom_point(data = filter(rr_combined, Source == "Raw RR"), aes(x = timestamp_posix, y = rr_ms), color = "grey80", size = 1, alpha = 0.6) +
        # Plot Cleaned RR intervals
        geom_point(data = filter(rr_combined, Source == "Cleaned RR"), aes(x = timestamp_posix, y = rr_ms), color = "blue", size = 1.5) +
        # Highlight Artifact RR intervals
        geom_point(data = filter(rr_combined, Source == "Artifact RR"), aes(x = timestamp_posix, y = rr_ms), color = "red", shape = 4, size = 2, stroke = 1) +
         # Plot Interpolated HR (needs secondary axis or transformation)
         # Option 1: Secondary Axis (can be complex to align)
         # Option 2: Plot on same axis - maybe less intuitive. Let's stick to RR plot for clarity here.
         # Add interpolated HR as a line on a separate plot or use facets.

        labs(title = "HR Preprocessing Comparison",
             x = "Timestamp",
             y = "RR Interval (ms)",
             color = "Data Type") +
        theme_minimal() +
        theme(legend.position = "bottom")

     print(p)

     # Separate plot for interpolated HR
     p_hr <- ggplot(interpolated_plot_data, aes(x = timestamp_posix, y = hr_bpm)) +
         geom_line(color = "darkgreen") +
         labs(title = "Interpolated & Resampled Instantaneous HR",
              x = "Timestamp",
              y = "Heart Rate (bpm)") +
         theme_minimal()
     print(p_hr)


    return(invisible(NULL))
}


# --- Script Execution Point (Example Test) ---
if (sys.nframe() == 0) { # Run only when script is executed directly

    message("\n--- Running Preprocessing Test ---")

    # 1. Define sample file path and create if needed
    sample_file_path <- "hr_data/3.txt"
    if (!dir.exists("hr_data")) {
        dir.create("hr_data")
        message("Created 'hr_data' directory.")
    }
    if (!file.exists(sample_file_path)) {
        sample_content <- "Time,rr_ms\n01:22:59.835Z,1971.6\n01:23:00.319Z,481.4\n01:23:00.819Z,481.4\n01:23:01.303Z,50.0\n01:23:01.803Z,503.9\n01:23:01.900Z,2500.0\n01:23:02.537Z,531.2\n01:23:02.787Z,539.0\n01:23:03.271Z,NA\n01:23:03.800Z,550.0\n01:23:03.800Z,551.0\nBAD_TIME,555.0\n01:23:04.500Z,560.0\n01:23:05.000Z,565.0\n01:23:05.500Z,900.0\n01:23:06.000Z,570.0"
        writeLines(sample_content, sample_file_path)
        message("Created dummy 'sample_hr_preprocess_test.txt' file.")
    }

    # 2. Run the preprocessing pipeline
    preprocessing_results <- preprocess_hr(file_path = sample_file_path,
                                         time_col = "Time", rr_col = "rr_ms",
                                         min_rr = 300, max_rr = 2000,
                                         malik_threshold = 0.3, # Example threshold
                                         interpolation_method = "linear",
                                         resample_rate = 4.0) # Resample to 4Hz
    if (resample_rate < 3) {
        message("Resample rate must be at least 3 Hz.")
        return(NULL)
    }

    # 3. Display results and plot comparison
    if (!is.null(preprocessing_results)) {
        message("\n--- Preprocessing Results Summary ---")

        message("\nCleaned RR Data (Head):")
        print(head(preprocessing_results$cleaned_rr))
        print(paste("Total artifacts marked:", sum(preprocessing_results$cleaned_rr$is_artifact)))
        print(paste("Total rows:", nrow(preprocessing_results$cleaned_rr)))


        message("\nInterpolated HR Data (Head):")
        print(head(preprocessing_results$interpolated_hr))
        print(paste("Total points:", nrow(preprocessing_results$interpolated_hr)))
        print(paste("Time difference between points (s):", round(as.numeric(diff(preprocessing_results$interpolated_hr$timestamp_posix[1:2])), 3)))


        # Load raw data again for plotting comparison
        raw_data_for_plot <- readr::read_csv(sample_file_path, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
        plot_preprocessing_results(raw_data = raw_data_for_plot,
                                   cleaned_rr = preprocessing_results$cleaned_rr,
                                   interpolated_hr = preprocessing_results$interpolated_hr,
                                   time_col = "Time", rr_col="rr_ms")

    } else {
        message("\nPreprocessing pipeline failed.")
    }

    message("\n--- Preprocessing Test Finished ---")
}
