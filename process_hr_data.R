# --- Configuration ---
# Set your working directory if necessary (e.g., if you run this script from a different location)
# setwd("/path/to/your/project/root")

# Define the path to the directory containing your R scripts
r_script_dir <- "R/"

# Define the path to the directory containing your HR data files
hr_data_dir <- "hr_data/"

# Specify which data file to process
sample_file_name <- "3.txt" # You can change this to "5.txt", "10.txt", or "11.txt"
sample_file_path <- file.path(hr_data_dir, sample_file_name)

# Define column names based on your file structure
time_column <- "Time"
rr_column <- "rr_ms"

# Define desired sampling frequency for interpolated HR (e.g., 4 Hz for 250 ms intervals)
sampling_freq <- 4 # Hz

# --- 1. Install and Load Dependencies ---
message("--- Step 1: Installing and loading R package dependencies ---")
source(file.path(r_script_dir, "install_dependencies.R"))

# --- 2. Load Core HR Processing Functions and Classes ---
message("\n--- Step 2: Loading core HR processing functions and classes ---")
source(file.path(r_script_dir, "clean_hr_initial.R"))
source(file.path(r_script_dir, "detect_artifacts_malik.R"))
source(file.path(r_script_dir, "interpolate_hr.R"))
source(file.path(r_script_dir, "preprocess_hr.R"))
source(file.path(r_script_dir, "extract_features.R"))
source(file.path(r_script_dir, "classes.R")) # For the HRSeries S4 class and create_hrseries function
source(file.path(r_script_dir, "model.R")) # For stress classification functions (if used later)

message(sprintf("\nProcessing file: %s", sample_file_path))

# --- 3. Preprocess the HR Data ---
message("\n--- Step 3: Preprocessing HR data (cleaning, artifact detection, interpolation) ---")
preproc_results <- preprocess_hr(
    data = sample_file_path,
    time_col = time_column,
    rr_col = rr_column,
    min_rr = 300,            # Minimum plausible RR interval (ms)
    max_rr = 2000,           # Maximum plausible RR interval (ms)
    malik_threshold = 0.2,   # Malik criterion threshold for artifact detection
    interpolation_method = "spline", # Options: "linear", "spline"
    resample_rate = sampling_freq # Resample interpolated HR to the specified frequency
)

if (is.null(preproc_results)) {
    stop("Preprocessing failed. Please check the console output for error messages from `preprocess_hr`.")
} else {
    message("\nPreprocessing complete. Summary of results:")
    message(sprintf("Cleaned RR Data rows: %d", nrow(preproc_results$cleaned_rr)))
    message(sprintf("Interpolated HR Data points: %d", nrow(preproc_results$interpolated_hr)))
}

# --- 4. Extract HRV Features ---
message("\n--- Step 4: Extracting HRV features ---")
hrv_features <- extract_hrv_features(
    cleaned_rr_data = preproc_results$cleaned_rr,
    interpolated_hr_data = preproc_results$interpolated_hr,
    fs = sampling_freq,      # Sampling frequency of the interpolated HR
    window_seconds = 60,     # Window size for feature extraction (e.g., 60 seconds)
    step_seconds = 30        # Step size for sliding windows (e.g., 30 seconds overlap)
)

if (is.null(hrv_features) || nrow(hrv_features) == 0) {
    warning("HRV feature extraction failed or produced no features.")
} else {
    message("\nHRV Feature Extraction complete. Head of features data:")
    print(head(hrv_features))
}

# --- 5. Create an HRSeries Object ---
# This step requires the 'classes.R' file to be sourced, which defines the HRSeries S4 class
# and the `create_hrseries` constructor function.
if (exists("create_hrseries") && !is.null(preproc_results) && !is.null(hrv_features) && nrow(hrv_features) > 0) {
    message("\n--- Step 5: Creating HRSeries object ---")
    hr_object <- create_hrseries(
        preprocessed_data = preproc_results,
        features_data = hrv_features,
        file_source = sample_file_path,
        subject_id = "TestSubject01", # Assign a meaningful subject ID
        preprocessing_params = list(
            resample_rate = sampling_freq,
            malik_threshold = 0.2,
            interpolation_method = "spline"
        ),
        feature_extraction_params = list(
            fs = sampling_freq,
            window_seconds = 60,
            step_seconds = 30
        )
    )

    message("\nHRSeries object created. Summary:")
    summary(hr_object)

    # You can also use the 'plot' method defined in 'classes.R'
    # if (interactive() && Sys.getenv("RSTUDIO") == "1") {
    #     message("\n--- Plotting HRSeries data (e.g., type='all') ---")
    #     plot(hr_object, type = "all")
    # }

} else if (!exists("create_hrseries")) {
    warning("HRSeries object creation skipped: `classes.R` was not sourced or `create_hrseries` function not found.")
} else {
    warning("HRSeries object creation skipped: Preprocessing or feature extraction results are missing or empty.")
}

message("\n--- Script execution finished. ---")