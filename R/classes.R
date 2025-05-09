# R/classes.R
# Definition of the HRSeries S4 class for storing and analyzing HR data.

# Required libraries (ensure installed)
# install.packages(c("lubridate", "dplyr", "ggplot2", "patchwork"))
library(methods)   # Explicitly load methods for S4
library(lubridate)
library(dplyr)

# --- Class Definition ---

#' HRSeries S4 Class
#'
#' An S4 class to store and manage processed heart rate (HR) time series data,
#' including raw RR intervals, cleaned data, interpolated HR, extracted HRV features,
#' and associated metadata
#'
#' @slot metadata A list containing metadata about the data source and processing,
#'       including e.g., `file_source`, `subject_id` (optional), `processing_timestamp`,
#'       `preprocessing_params`, `feature_extraction_params`.
#' @slot cleaned_rr A data frame (tibble) containing the cleaned RR intervals,
#'       typically the `$cleaned_rr` output from `preprocess_hr`. Includes columns like
#'       `timestamp_posix`, `rr_ms`, `initial_qc_passed`, `is_artifact`.
#' @slot interpolated_hr A data frame (tibble) containing the interpolated and
#'       potentially resampled instantaneous heart rate, typically the
#'       `$interpolated_hr` output from `preprocess_hr`. Includes `timestamp_posix`, `hr_bpm`.
#' @slot hrv_features A data frame (tibble) containing the calculated time-domain
#'       and frequency-domain HRV features over windows, typically the output from
#'       `extract_hrv_features`. Includes `window_end_time` and feature columns
#'       (e.g., `sdnn`, `rmssd`, `lf_power`, `hf_power`, `lf_hf_ratio`).
#'
#' @name HRSeries-class
#' @rdname HRSeries-class
#' @exportClass HRSeries
setClass("HRSeries",
         slots = c(
           metadata = "list",
           cleaned_rr = "data.frame",      # Using data.frame for broader compatibility, tibbles work
           interpolated_hr = "data.frame",
           hrv_features = "data.frame"
         ),
         # Optional: Define default values or prototype
         prototype = list(
           metadata = list(
                file_source = NA_character_,
                subject_id = NA_character_,
                processing_timestamp = Sys.time(),
                preprocessing_params = list(),
                feature_extraction_params = list()
                ),
           cleaned_rr = data.frame(),
           interpolated_hr = data.frame(),
           hrv_features = data.frame()
         )
)

# --- Constructor Helper Function (Optional but Recommended) ---

#' Create an HRSeries Object
#'
#' A user-friendly function to create instances of the HRSeries class.
#'
#' @param preprocessed_data List. The output from `preprocess_hr`.
#' @param features_data Data frame. The output from `extract_hrv_features`.
#' @param file_source Character. Path or identifier of the original data file.
#' @param subject_id Character (Optional). Identifier for the subject.
#' @param preprocessing_params List (Optional). Parameters used in `preprocess_hr`.
#' @param feature_extraction_params List (Optional). Parameters used in `extract_hrv_features`.
#'
#' @return An object of class HRSeries.
#' @export
#'
#' @examples
#' # --- Assuming previous steps were run ---
#' # results <- preprocess_hr(file_path = "hr_data/3.txt", resample_rate = 4.0)
#' # my_fs <- 4.0
#' # hrv_data <- extract_hrv_features(preprocessed_results = results, fs = my_fs)
#' #
#' # --- Create the HRSeries object ---
#' # if (!is.null(results) && !is.null(hrv_data)) {
#' #   hr_object <- create_hrseries(
#' #     preprocessed_data = results,
#' #     features_data = hrv_data,
#' #     file_source = "hr_data/3.txt",
#' #     subject_id = "Subject_001",
#' #     preprocessing_params = list(resample_rate = 4.0, malik_threshold = 0.2), # Example params
#' #     feature_extraction_params = list(fs = my_fs, window_seconds = 300) # Example params
#' #   )
#' #   print(hr_object)
#' #   summary(hr_object)
#' # }
create_hrseries <- function(preprocessed_data, features_data,
                            file_source = NA_character_, subject_id = NA_character_,
                            preprocessing_params = list(), feature_extraction_params = list()) {

  # Basic validation
  if (!is.list(preprocessed_data) || !all(c("cleaned_rr", "interpolated_hr") %in% names(preprocessed_data))) {
    stop("'preprocessed_data' must be a list containing '$cleaned_rr' and '$interpolated_hr'.")
  }
   if (!is.data.frame(features_data)) {
       if(is.null(features_data)){
           features_data <- data.frame()
           warning("No HRV features data provided, storing empty data frame.")
       } else {
           stop("'features_data' must be a data frame or NULL.")
       }
   }

  # Create metadata list
  # THIS IS THE CRUCIAL PART: Ensure the arguments are used
  meta <- list(
    file_source = file_source,
    subject_id = subject_id,
    processing_timestamp = Sys.time(),
    preprocessing_params = preprocessing_params,         # Uses the function argument
    feature_extraction_params = feature_extraction_params, # Uses the function argument
    original_rr_count = nrow(preprocessed_data$cleaned_rr),
    valid_nn_count = sum(!preprocessed_data$cleaned_rr$is_artifact),
    interpolated_hr_points = nrow(preprocessed_data$interpolated_hr),
    feature_windows = nrow(features_data)
  )

  # Create and return the new HRSeries object using 'new()'
  new("HRSeries",
      metadata = meta,
      cleaned_rr = as.data.frame(preprocessed_data$cleaned_rr),
      interpolated_hr = as.data.frame(preprocessed_data$interpolated_hr),
      hrv_features = as.data.frame(features_data)
  )
}


# --- S4 Methods ---

#' Show Method for HRSeries Class
#'
#' Defines how an HRSeries object is printed to the console.
#'
#' @param object An object of class HRSeries.
#' @name show
#' @aliases show,HRSeries-method
#' @rdname HRSeries-class
#' @importMethodsFrom methods show
setMethod("show", "HRSeries", function(object) {
  cat("**** HRSeries Object ****\n")
  cat("---------------------------\n")
  cat("Metadata:\n")
  cat("  File Source:      ", object@metadata$file_source %||% "N/A", "\n")
  cat("  Subject ID:       ", object@metadata$subject_id %||% "N/A", "\n")
  cat("  Processed on:     ", format(object@metadata$processing_timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("---------------------------\n")
  cat("Data Dimensions:\n")
  cat("  Cleaned RR:       ", nrow(object@cleaned_rr), " rows, ", ncol(object@cleaned_rr), " cols (", object@metadata$valid_nn_count %||% "?", " valid NN)\n")
  cat("  Interpolated HR:  ", nrow(object@interpolated_hr), " rows, ", ncol(object@interpolated_hr), " cols\n")
  cat("  HRV Features:     ", nrow(object@hrv_features), " rows (windows), ", ncol(object@hrv_features), " cols\n")
  cat("---------------------------\n")
  cat("Use summary(object) for more details and plot(object) for visualizations.\n")
  cat("**** End HRSeries Object ****\n")
})

# Helper for printing NULLs gracefully in show/summary
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && length(a) > 0) a else b


#' Summary Method for HRSeries Class
#'
#' Provides a detailed summary of the HRSeries object.
#'
#' @param object An object of class HRSeries.
#' @param ... Additional arguments (not used).
#' @name summary
#' @aliases summary,HRSeries-method
#' @rdname HRSeries-class
setMethod("summary", "HRSeries", function(object, ...) {
  cat("**** Summary: HRSeries Object ****\n")
  cat("===================================\n")
  cat("** Metadata **\n")
  cat("  File Source:      ", object@metadata$file_source %||% "N/A", "\n")
  cat("  Subject ID:       ", object@metadata$subject_id %||% "N/A", "\n")
  cat("  Processed on:     ", format(object@metadata$processing_timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("  Preprocessing Params: \n")
  if(length(object@metadata$preprocessing_params) > 0) {
      for(pname in names(object@metadata$preprocessing_params)) {
          cat("    - ", pname, ": ", object@metadata$preprocessing_params[[pname]], "\n")
      }
  } else { cat("    (None provided)\n") }
  cat("  Feature Extraction Params: \n")
   if(length(object@metadata$feature_extraction_params) > 0) {
      for(pname in names(object@metadata$feature_extraction_params)) {
          cat("    - ", pname, ": ", object@metadata$feature_extraction_params[[pname]], "\n")
      }
  } else { cat("    (None provided)\n") }

  cat("\n** Data Overview **\n")
  cat("  Cleaned RR Intervals:\n")
  cat("    Total Records:    ", nrow(object@cleaned_rr), "\n")
  cat("    Valid NN Count:   ", object@metadata$valid_nn_count %||% "?", "\n")
  cat("    Artifact Count:   ", sum(object@cleaned_rr$is_artifact), "\n")
  if(object@metadata$valid_nn_count > 0) {
      nn_intervals <- object@cleaned_rr$rr_ms[!object@cleaned_rr$is_artifact]
      cat("    NN Interval Range (ms): [", round(min(nn_intervals, na.rm=TRUE), 1), ", ", round(max(nn_intervals, na.rm=TRUE), 1), "]\n")
      cat("    Mean NN Interval (ms):  ", round(mean(nn_intervals, na.rm=TRUE), 1), "\n")
  }

  cat("\n  Interpolated HR:\n")
  cat("    Data Points:      ", nrow(object@interpolated_hr), "\n")
  if(nrow(object@interpolated_hr) > 0) {
      fs <- object@metadata$feature_extraction_params$fs %||% NA
      ts_range <- range(object@interpolated_hr$timestamp_posix, na.rm=TRUE)
      duration_min <- difftime(ts_range[2], ts_range[1], units="mins")
      cat("    Sampling Rate (Hz): ", fs, "\n")
      cat("    Time Range:         ", format(ts_range[1], "%H:%M:%S"), " to ", format(ts_range[2], "%H:%M:%S"), "\n")
      cat("    Duration (min):     ", round(as.numeric(duration_min), 1), "\n")
      cat("    HR Range (bpm):     [", round(min(object@interpolated_hr$hr_bpm, na.rm=TRUE), 1), ", ", round(max(object@interpolated_hr$hr_bpm, na.rm=TRUE), 1), "]\n")
      cat("    Mean HR (bpm):      ", round(mean(object@interpolated_hr$hr_bpm, na.rm=TRUE), 1), "\n")
  }

  cat("\n  HRV Features:\n")
  cat("    Windows Analyzed: ", nrow(object@hrv_features), "\n")
  if(nrow(object@hrv_features) > 0) {
      cat("    Available Features: ", paste(names(object@hrv_features)[-1], collapse=", "), "\n") # Exclude time column
      cat("    Summary of Key Features:\n")
      feature_summary <- object@hrv_features %>%
          select(where(is.numeric)) %>% # Select only numeric feature columns
          summary()
      print(feature_summary)
  } else {
      cat("    (No features calculated or stored)\n")
  }

  cat("===================================\n")
  cat("**** End Summary ****\n")
})


#' Plot Method for HRSeries Class
#'
#' Generates various plots to visualize the data stored in an HRSeries object.
#'
#' @param x An object of class HRSeries.
#' @param y Ignored.
#' @param type Character vector. Specifies which plot(s) to generate. Options include:
#'        - `"cleaned_rr"`: Plots cleaned RR intervals, highlighting artifacts.
#'        - `"interpolated_hr"`: Plots the interpolated instantaneous HR signal.
#'        - `"hrv_features"`: Plots key HRV features (e.g., RMSSD, LF/HF ratio) over time.
#'        - `"all"`: Generates all available plots (default).
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns a list of ggplot objects if multiple plots are generated,
#'         or a single ggplot object if only one type is requested.
#' @name plot
#' @aliases plot,HRSeries,ANY-method
#' @rdname HRSeries-class
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal scale_color_manual ggtitle theme element_text
#' @importFrom dplyr filter mutate case_when select where
#' @importFrom patchwork wrap_plots plot_layout # For combining plots if type="all"
setMethod("plot", signature(x = "HRSeries", y = "missing"),
    function(x, y, type = "all", ...) {

        # Check if ggplot2 is available
        if (!requireNamespace("ggplot2", quietly = TRUE)) {
            stop("Package 'ggplot2' is required for plotting. Please install it.", call. = FALSE)
        }
        library(ggplot2) # Load ggplot2 for easier access to functions

        plots_list <- list()
        plot_types <- tolower(type)

        # --- Plot 1: Cleaned RR Intervals ---
        if ("all" %in% plot_types || "cleaned_rr" %in% plot_types) {
            if (nrow(x@cleaned_rr) > 0) {
                df_plot <- x@cleaned_rr %>%
                    mutate(Status = case_when(
                        is_artifact ~ "Artifact",
                        !initial_qc_passed ~ "QC Failed (Range/NA)", # Should also be artifact=TRUE
                        TRUE ~ "Valid NN"
                    ))

                p_rr <- ggplot(df_plot, aes(x = timestamp_posix, y = rr_ms, color = Status)) +
                    geom_point(alpha = 0.7, size = 1.5) +
                    scale_color_manual(values = c("Valid NN" = "blue", "Artifact" = "red", "QC Failed (Range/NA)" = "orange")) +
                    labs(title = "Cleaned RR Intervals",
                         subtitle = paste("Source:", basename(x@metadata$file_source %||% "N/A")),
                         x = "Timestamp", y = "RR Interval (ms)", color = "Status") +
                    theme_minimal() +
                    theme(legend.position = "bottom", plot.subtitle = element_text(size=8))
                plots_list[["cleaned_rr"]] <- p_rr
            } else {
                warning("No cleaned RR data available to plot.", call. = FALSE)
            }
        }

        # --- Plot 2: Interpolated HR ---
        if ("all" %in% plot_types || "interpolated_hr" %in% plot_types) {
             if (nrow(x@interpolated_hr) > 0) {
                 p_hr <- ggplot(x@interpolated_hr, aes(x = timestamp_posix, y = hr_bpm)) +
                    geom_line(color = "darkgreen", alpha = 0.8) +
                    labs(title = "Interpolated Instantaneous Heart Rate",
                         subtitle = paste("Sampling Rate:", x@metadata$feature_extraction_params$fs %||% "?", "Hz"),
                         x = "Timestamp", y = "Heart Rate (bpm)") +
                    theme_minimal() +
                     theme(plot.subtitle = element_text(size=8))
                 plots_list[["interpolated_hr"]] <- p_hr
             } else {
                 warning("No interpolated HR data available to plot.", call. = FALSE)
             }
        }

        # --- Plot 3: HRV Features ---
        if ("all" %in% plot_types || "hrv_features" %in% plot_types) {
             if (nrow(x@hrv_features) > 1) { # Need at least 2 points to plot lines
                 # Plot RMSSD if available
                 if ("rmssd" %in% names(x@hrv_features)) {
                     p_rmssd <- ggplot(x@hrv_features, aes(x = window_end_time, y = rmssd)) +
                         geom_line(color = "darkorange", na.rm = TRUE) +
                         geom_point(color = "darkorange", size=1, na.rm = TRUE) +
                         labs(title = "RMSSD over Time",
                              x = "Window End Time", y = "RMSSD (ms)") +
                         theme_minimal()
                     plots_list[["rmssd_ts"]] <- p_rmssd
                 }
                 # Plot LF/HF Ratio if available
                 if ("lf_hf_ratio" %in% names(x@hrv_features)) {
                     p_lfhf <- ggplot(x@hrv_features, aes(x = window_end_time, y = lf_hf_ratio)) +
                         geom_line(color = "purple", na.rm = TRUE) +
                         geom_point(color = "purple", size=1, na.rm = TRUE) +
                         labs(title = "LF/HF Ratio over Time",
                              x = "Window End Time", y = "LF/HF Ratio") +
                         theme_minimal()
                     plots_list[["lfhf_ts"]] <- p_lfhf
                 }
                  if(is.null(plots_list[["rmssd_ts"]]) && is.null(plots_list[["lfhf_ts"]])) {
                      warning("No standard HRV features (RMSSD, LF/HF) found in hrv_features slot to plot.", call. = FALSE)
                  }

             } else {
                 warning("Not enough HRV feature windows available to plot trends.", call. = FALSE)
             }
        }

        # --- Output ---
        if (length(plots_list) == 0) {
            message("No plots generated based on the requested type and available data.")
            return(invisible(NULL))
        } else if (length(plots_list) == 1) {
            # If only one plot was generated, print it directly
            print(plots_list[[1]])
            return(invisible(plots_list[[1]]))
        } else {
            # If multiple plots, arrange them using patchwork (if available)
            if (requireNamespace("patchwork", quietly = TRUE)) {
                 library(patchwork)
                 # Combine plots (adjust layout as needed)
                 combined_plot <- wrap_plots(plots_list, ncol = 1) + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
                 print(combined_plot)
                 return(invisible(plots_list)) # Return the list of plots
            } else {
                 warning("Package 'patchwork' not found. Cannot arrange multiple plots. Returning list of plots.", call. = FALSE)
                 # Print plots individually if patchwork isn't available
                 # for(p in plots_list) print(p)
                 return(invisible(plots_list))
            }
        }
    }
)


# --- Example Usage (within the script for testing, if run directly) ---
if (sys.nframe() == 0) {

    message("\n--- Running HRSeries Class Test ---")

    # 1. Need to generate sample data first (requires previous scripts)
    if (!exists("preprocess_hr")) {
        if(file.exists("R/preprocess_hr.R")) source("R/preprocess_hr.R") else stop("Need R/preprocess_hr.R")
    }
    if (!exists("extract_hrv_features")) {
         if(file.exists("R/extract_features.R")) source("R/extract_features.R") else stop("Need R/extract_features.R")
    }

    sample_file <- "hr_data/sample_hr_class_test.txt" # Use a distinct name
    if (!dir.exists("hr_data")) dir.create("hr_data")
    if (!file.exists(sample_file)) {
        # Create slightly longer sample data for better feature extraction demo
        sample_content <- paste0(
          "Time,rr_ms\n",
          paste(format(Sys.time() + seq(0, by=0.7, length.out=500), "%H:%M:%OS3Z"),
                round(rnorm(500, mean=800, sd=50) + rep(c(0, 50, -30, 0), each=125)), # Add some variation
                sep=",", collapse="\n"),
           # Add some artifacts
           "\n", format(Sys.time() + 500*0.7 + 1, "%H:%M:%OS3Z"), ",2500", # Too high
           "\n", format(Sys.time() + 500*0.7 + 2, "%H:%M:%OS3Z"), ",200",  # Too low
           "\n", format(Sys.time() + 500*0.7 + 3, "%H:%M:%OS3Z"), ",850"
        )
        writeLines(sample_content, sample_file)
        message("Created dummy '", sample_file, "' file.")
    }

    # 2. Run preprocessing and feature extraction
    sampling_freq <- 4.0
    preproc_results <- preprocess_hr(file_path = sample_file, resample_rate = sampling_freq)
    hrv_features <- NULL
    if(!is.null(preproc_results)) {
        hrv_features <- extract_hrv_features(preproc_results, fs = sampling_freq, window_seconds = 60, step_seconds = 30)
    }

    # 3. Create HRSeries object
    if (!is.null(preproc_results) && !is.null(hrv_features)) {
        hr_object <- create_hrseries(
            preprocessed_data = preproc_results,
            features_data = hrv_features,
            file_source = sample_file,
            subject_id = "TestSubject01",
            preprocessing_params = list(resample_rate = sampling_freq, malik_threshold = 0.2),
            feature_extraction_params = list(fs = sampling_freq, window_seconds = 60, step_seconds = 30)
        )

        # 4. Test methods
        message("\n--- Testing 'show' method ---")
        print(hr_object) # Implicitly calls show()

        message("\n--- Testing 'summary' method ---")
        summary(hr_object)

        message("\n--- Testing 'plot' method (type='all') ---")
        # Ensure graphics device is available
         if(interactive() && Sys.getenv("RSTUDIO") == "1") { # Basic check for RStudio interactive session
             dev.new() # Try opening a new plot window in RStudio if needed
         }
        plot(hr_object, type = "all")

        message("\n--- Testing 'plot' method (type='interpolated_hr') ---")
        plot(hr_object, type = "interpolated_hr")

    } else {
        message("Preprocessing or feature extraction failed. Cannot create or test HRSeries object.")
    }

    message("\n--- HRSeries Class Test Finished ---")
}
