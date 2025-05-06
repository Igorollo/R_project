# R/models.R
# Functions for loading and using the pre-trained Python stress classification model.

# Required R packages (ensure installed)
# install.packages(c("reticulate", "dplyr", "tibble"))
library(reticulate) # For Python integration
library(dplyr)      # For data manipulation
library(tibble)     # For creating tibbles

# --- Python Environment Configuration (Important!) ---
# Reticulate needs to know where your Python executable is,
# especially the one that has scikit-learn, joblib, and numpy installed.
#
# Option 1: Automatic (if your default Python has the packages)
# reticulate::py_discover_config() # Run this to see what Python reticulate finds
#
# Option 2: Specify Python path directly (replace with your actual path)
# reticulate::use_python("/usr/bin/python3", required = TRUE)
# or for an Anaconda/Miniconda environment:
# reticulate::use_condaenv("my_sklearn_env", required = TRUE)
#
# Option 3: Use a virtual environment
# reticulate::use_virtualenv("path/to/your/venv", required = TRUE)
#
# It's good practice to configure this once at the start of your session or project.
# For this script, we'll assume it's configured or try a basic discovery.
# You might need to uncomment and adjust one of the above lines.

# Attempt to load necessary Python modules to check environment
tryCatch({
  joblib <- reticulate::import("joblib", delay_load = TRUE)
  np <- reticulate::import("numpy", delay_load = TRUE)
  message("Successfully configured reticulate and can find joblib & numpy.")
}, error = function(e) {
  warning("Could not import 'joblib' or 'numpy' via reticulate. ",
          "Please ensure Python is correctly configured with scikit-learn, joblib, and numpy installed. ",
          "You may need to use reticulate::use_python() or reticulate::use_condaenv(). Error: ", e$message)
})


# --- Global variable to hold the loaded Python model ---
# This avoids reloading the model on every prediction call.
.GlobalEnv$.stress_model_py <- NULL


#' Load the Pre-trained Python Stress Model
#'
#' Loads the scikit-learn pipeline model from a .pkl file using reticulate.
#' The model is stored in a global variable `.GlobalEnv$.stress_model_py`
#' to avoid reloading it multiple times.
#'
#' @param model_path Character string. The file path to the `stress_rf.pkl` model.
#' @return Invisibly returns TRUE if successful, FALSE otherwise.
#' @export
#'
#' @examples
#' # model_loaded <- load_stress_model("model/stress_rf.pkl") # Assuming model is in model/
#' # if(model_loaded) {
#' #   print("Stress model loaded successfully.")
#' # }
load_stress_model <- function(model_path = "stress_rf.pkl") {
  if (!file.exists(model_path)) {
    warning("Model file not found at: ", model_path)
    .GlobalEnv$.stress_model_py <- NULL
    return(invisible(FALSE))
  }

  tryCatch({
    message("Loading Python stress model from: ", model_path)
    # Ensure joblib is accessible (it was imported with delay_load)
    joblib_main <- reticulate::import("joblib", convert = TRUE) # convert=TRUE for R interaction
    loaded_model <- joblib_main$load(model_path)

    # Store in the global environment (or a dedicated environment)
    .GlobalEnv$.stress_model_py <- loaded_model
    message("Python stress model loaded and stored globally as .stress_model_py")
    return(invisible(TRUE))
  }, error = function(e) {
    warning("Failed to load Python model from '", model_path, "'. Error: ", e$message)
    .GlobalEnv$.stress_model_py <- NULL
    return(invisible(FALSE))
  })
}

#' Classify Stress using the Loaded Python Model
#'
#' Takes HRV features, prepares them for the Python model, and returns predictions.
#'
#' @param hrv_features_df A data frame (tibble) containing HRV features.
#'        It must include columns: `sdnn`, `rmssd`, `lf_power`, `hf_power`,
#'        and `lf_hf_ratio`. These will be mapped to the Python model's
#'        expected names (SDNN, RMSSD, LF, HF, LF_HF).
#' @param model_path Character string. Path to the .pkl model file. If the model
#'        is not already loaded globally, this function will attempt to load it.
#'        Default: "model/stress_rf.pkl".
#'
#' @return The input `hrv_features_df` augmented with two new columns:
#'         - `stress_prediction`: Predicted class (0 for non-stress, 1 for stress).
#'         - `stress_probability`: Probability of belonging to the stress class (class 1).
#'         Returns NULL if the model is not loaded or prediction fails.
#'         Rows with NA in required features will have NA predictions.
#' @export
#'
#' @examples
#' # --- Assuming previous steps and model loading ---
#' # source("R/preprocess_hr.R")
#' # source("R/extract_features.R")
#' # sample_file <- "hr_data/sample_hr_class_test.txt" # Ensure this exists
#' # fs <- 4.0
#' # preproc_data <- preprocess_hr(file_path = sample_file, resample_rate = fs)
#' # features_df <- NULL
#' # if(!is.null(preproc_data)) {
#' #   features_df <- extract_hrv_features(preproc_data, fs = fs, window_seconds = 60)
#' # }
#' #
#' # if (!is.null(features_df)) {
#' #   # Ensure model is loaded (or load it here)
#' #   if(is.null(.GlobalEnv$.stress_model_py)) {
#' #      load_stress_model("model/stress_rf.pkl") # Adjust path
#' #   }
#' #
#' #   if(!is.null(.GlobalEnv$.stress_model_py)) {
#' #      classified_features <- classify_stress_windows(features_df,
#' #                                                   model_path = "model/stress_rf.pkl")
#' #      if(!is.null(classified_features)) {
#' #          print(head(classified_features))
#' #          print(table(classified_features$stress_prediction, useNA="ifany"))
#' #      }
#' #   } else {
#' #      message("Model could not be loaded. Skipping classification.")
#' #   }
#' # }
classify_stress_windows <- function(hrv_features_df, model_path = "model/stress_rf.pkl") {

  # Check if model is loaded, if not, try to load it
  if (is.null(.GlobalEnv$.stress_model_py)) {
    message("Python model not found in global environment. Attempting to load...")
    model_loaded_successfully <- load_stress_model(model_path)
    if (!model_loaded_successfully || is.null(.GlobalEnv$.stress_model_py)) {
      warning("Stress model could not be loaded. Cannot perform classification.")
      return(NULL)
    }
  }

  # Python model expects specific feature names (uppercase)
  # Our R features are lowercase.
  required_r_features <- c("sdnn", "rmssd", "lf_power", "hf_power", "lf_hf_ratio")
  python_model_features <- c("SDNN", "RMSSD", "LF", "HF", "LF_HF") # As per Python script

  # Check if all required R features are present in the input dataframe
  missing_features <- setdiff(required_r_features, names(hrv_features_df))
  if (length(missing_features) > 0) {
    warning("Input data frame is missing required HRV features: ", paste(missing_features, collapse = ", "))
    return(NULL)
  }

  # Select and rename features for the model
  # The Python model was trained on features named SDNN, RMSSD, LF, HF, LF_HF
  features_for_model_r <- hrv_features_df %>%
    select(all_of(required_r_features)) %>%
    # Rename to match Python model's expected input
    rename_with(~ python_model_features[match(., required_r_features)], .cols = all_of(required_r_features))

  # Identify rows with any NA in the selected features
  # The Python script's preprocessing phase (df.dropna()) means the model expects no NAs.
  na_rows_idx <- which(apply(features_for_model_r, 1, function(row) any(is.na(row))))
  valid_rows_idx <- setdiff(1:nrow(features_for_model_r), na_rows_idx)

  # Initialize prediction columns with NAs
  hrv_features_df$stress_prediction <- NA_integer_
  hrv_features_df$stress_probability <- NA_real_

  if (length(valid_rows_idx) == 0) {
      warning("No valid rows (all contain NAs in required features) to send for prediction.")
      return(hrv_features_df) # Return with NA predictions
  }

  # Prepare data for prediction (only valid rows)
  features_for_model_py_valid <- tryCatch({
    # Convert the R data frame of valid rows to a NumPy array for scikit-learn
    # np$array should handle the conversion
    np_main <- reticulate::import("numpy", convert = TRUE)
    np_main$array(features_for_model_r[valid_rows_idx, ], dtype = "float64")
  }, error = function(e) {
    warning("Failed to convert R features to NumPy array: ", e$message)
    return(NULL)
  })

  if (is.null(features_for_model_py_valid)) {
    return(NULL) # Conversion failed
  }

  # Perform prediction using the Python model
  predictions_py <- NULL
  probabilities_py <- NULL

  tryCatch({
    message("Sending ", nrow(features_for_model_py_valid), " valid windows to Python model for prediction...")
    # Call the predict method of the scikit-learn pipeline
    predictions_py <- .GlobalEnv$.stress_model_py$predict(features_for_model_py_valid)
    # Call the predict_proba method for probabilities (usually second column is for class 1)
    probabilities_py <- .GlobalEnv$.stress_model_py$predict_proba(features_for_model_py_valid)[, 2] # Prob of class 1 (stress)

    message("Predictions received from Python model.")
  }, error = function(e) {
    warning("Prediction using Python model failed: ", e$message)
    # Return the original df with NA predictions if Python call fails
    return(hrv_features_df)
  })

  # Add predictions and probabilities back to the original R data frame for valid rows
  if (!is.null(predictions_py)) {
    hrv_features_df$stress_prediction[valid_rows_idx] <- as.integer(predictions_py)
  }
  if (!is.null(probabilities_py)) {
    hrv_features_df$stress_probability[valid_rows_idx] <- as.numeric(probabilities_py)
  }

  return(hrv_features_df)
}

# --- Example Usage (within the script for testing, if run directly) ---
if (sys.nframe() == 0) {

    message("\n--- Running Model Integration Test ---")

    # 1. Ensure other necessary functions are loaded
    if (!exists("preprocess_hr")) {
        if(file.exists("R/preprocess_hr.R")) source("R/preprocess_hr.R") else stop("Need R/preprocess_hr.R")
    }
    if (!exists("extract_hrv_features")) {
         if(file.exists("R/extract_features.R")) source("R/extract_features.R") else stop("Need R/extract_features.R")
    }

    # 2. Create dummy HRV features data (similar to output of extract_hrv_features)
    # In a real scenario, this would come from extract_hrv_features
    dummy_features_df <- tibble(
        window_end_time = Sys.time() + 0:(9)*300,
        sdnn = rnorm(10, 50, 10),
        rmssd = rnorm(10, 30, 5),
        lf_power = rnorm(10, 1000, 200),
        hf_power = rnorm(10, 800, 150),
        lf_hf_ratio = lf_power / hf_power,
        # Add some NAs to test robustness
        extra_col = 1:10
    )
    dummy_features_df$sdnn[3] <- NA # Introduce an NA
    dummy_features_df$lf_power[6] <- NA

    message("Created dummy HRV features data:")
    print(head(dummy_features_df))

    # 3. Define model path (IMPORTANT: Adjust this path to your actual model location)
    # For this test to run, you MUST have the 'stress_rf.pkl' file.
    # Let's assume it's in a 'model' subdirectory of the current working directory.
    model_file_path <- "model/stress_rf.pkl"
    if (!dir.exists("model")) dir.create("model")
    if (!file.exists(model_file_path)) {
        warning("CRITICAL: '", model_file_path, "' not found. ",
                "This test requires the pre-trained Python model. ",
                "Please place 'stress_rf.pkl' in the 'model/' directory or update path.")
    } else {
        # 4. Load the model (first time)
        model_loaded <- load_stress_model(model_path = model_file_path)

        if (model_loaded) {
            # 5. Classify stress
            classified_data <- classify_stress_windows(dummy_features_df, model_path = model_file_path)

            if (!is.null(classified_data)) {
                message("\n--- Classified Features Data (Head) ---")
                print(head(select(classified_data, window_end_time, sdnn, stress_prediction, stress_probability)))
                message("\n--- Prediction Summary ---")
                print(table(classified_data$stress_prediction, useNA = "ifany"))
            } else {
                message("Stress classification failed.")
            }
        } else {
            message("Model loading failed. Skipping classification test.")
        }
    }
    message("\n--- Model Integration Test Finished ---")
}
