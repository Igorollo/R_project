# R/model.R
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
# reticulate::use_python("c:/users/paula/appdata/local/programs/python/python313/python.exe", required = TRUE) # Example Windows path
# or for an Anaconda/Miniconda environment:
# reticulate::use_condaenv("my_sklearn_env", required = TRUE)
#
# Option 3: Use a virtual environment
# reticulate::use_virtualenv("path/to/your/venv", required = TRUE)
#
# It's good practice to configure this once at the start of your session or project.
# For the Shiny app, this is typically done in server.R or global.R.
# The server.R file you provided already has:
# reticulate::use_python("c:/users/paula/appdata/local/programs/python/python313/python.exe", required = TRUE) # Example Windows path


# Global variable to store the Python model
.stress_model_py <- NULL

#' Load the pre-trained Python stress classification model.
#'
#' This function loads the scikit-learn RandomForestClassifier model
#' saved as a .pkl file using joblib. It stores the loaded model
#' in a global R environment variable `.stress_model_py` for reuse.
#'
#' @param model_path Character string. The file path to the .pkl model file.
#' @return Invisible NULL. Loads the model as a side effect.
#' @export
load_stress_model <- function(model_path) {
  message("DEBUG (model.R): Entered load_stress_model function.") # VERY early debug
  if (is.null(.stress_model_py)) {
    message("Loading Python stress model from: ", model_path)
    tryCatch({
      # Ensure joblib is imported
      joblib <- reticulate::import("joblib", delay_load = TRUE) # Ensure delay_load for joblib
      message("DEBUG (model.R): joblib imported.") # Debug after import
      .stress_model_py <<- joblib$load(model_path)
      message("Python stress model loaded and stored globally as .stress_model_py")
    }, error = function(e) {
      stop("Failed to load Python model: ", e$message)
    })
  } else {
    message("Python stress model already loaded.")
  }
  message("DEBUG (model.R): Exiting load_stress_model function.") # VERY late debug
  invisible(NULL)
}

#' Classify stress levels using the pre-trained Python model.
#'
#' This function takes a data frame of HRV features, converts it to a
#' NumPy array, and uses the loaded Python scikit-learn model to predict
#' stress levels (0 or 1) and their probabilities. It then combines
#' these predictions with the original HRV features.
#'
#' @param hrv_features_df A data frame (tibble) containing HRV features
#'                        (e.g., sdnn, rmssd, lf_power, hf_power, lf_hf_ratio).
#'                        This should be the output of `extract_hrv_features`.
#' @param model_path Character string. Path to the .pkl model file. Used to load
#'                   the model if it's not already loaded.
#' @return A data frame (tibble) with original HRV features,
#'         `stress_prediction` (0 or 1), and `stress_probability`.
#'         Returns NULL if classification fails.
#' @export
classify_stress_windows <- function(hrv_features_df, model_path) {
  message("DEBUG (model.R): Entered classify_stress_windows function.") # VERY early debug
  if (is.null(hrv_features_df) || nrow(hrv_features_df) == 0) {
    message("HRV features data is NULL or empty. Skipping classification.")
    return(NULL)
  }

  # Ensure the model is loaded
  load_stress_model(model_path)

  if (is.null(.stress_model_py)) {
    message("Error: Python stress model not loaded after load_stress_model call.")
    return(NULL)
  }

  message("Sending ", nrow(hrv_features_df), " valid windows to Python model for prediction...")

  # Select only the features expected by the Python model
  # Ensure these column names match what the Python model was trained on
  # Based on train_stress_rf.py: SDNN, RMSSD, LF power, HF power, LF/HF ratio
  required_features <- c("sdnn", "rmssd", "lf_power", "hf_power", "lf_hf_ratio")
  missing_features <- setdiff(required_features, names(hrv_features_df))
  if (length(missing_features) > 0) {
    stop("Missing required HRV features for classification: ", paste(missing_features, collapse = ", "))
  }

  # Convert R data frame to Python pandas DataFrame / NumPy array
  # Only select the required feature columns in the correct order
  X_pred <- hrv_features_df %>%
    dplyr::select(all_of(required_features)) %>%
    as.matrix() # Convert to matrix for numpy array conversion

  # Use numpy for numerical operations and array conversion
  numpy <- reticulate::import("numpy", delay_load = TRUE)
  message("DEBUG (model.R): Converting R matrix to NumPy array.") # Added debug message
  X_pred_np <- numpy$array(X_pred, dtype = numpy$float64) # Explicitly cast to float64

  message("Performing predictions using Python model...")
  # Make predictions
  predictions <- .stress_model_py$predict(X_pred_np)
  predictions_proba <- .stress_model_py$predict_proba(X_pred_np)

  message("Predictions received from Python model.")

  # Convert Python objects back to R vectors/matrices
  # reticulate::py_to_r can handle this, but sometimes explicit conversion helps
  # Ensure predictions_proba is an R matrix
  message("predictions_proba is a Python object. Attempting conversion to R matrix...")
  predictions_proba_r <- tryCatch({
    reticulate::py_to_r(predictions_proba)
  }, error = function(e) {
    warning("Failed to convert predictions_proba from Python to R: ", e$message)
    return(NULL)
  })

  if (is.null(predictions_proba_r)) {
    message("Conversion of predictions_proba to R failed. Returning NULL.")
    return(NULL)
  }

  message("Successfully converted predictions_proba to R. Dimensions: ", paste(dim(predictions_proba_r), collapse = " x "))


  # Check dimensions of predictions_proba_r
  if (ncol(predictions_proba_r) != 2) {
    warning("Expected 2 columns for probabilities (class 0, class 1), but got ", ncol(predictions_proba_r))
    return(NULL)
  }


  # Combine with original features
  message("DEBUG (model.R): Combining predictions with HRV features.") # Added debug message
  classified_df <- hrv_features_df %>%
    dplyr::mutate(
      stress_prediction = as.numeric(reticulate::py_to_r(predictions)), # Convert Python array to R vector
      # Assuming 2nd column is probability of class 1 (stress)
      stress_probability = predictions_proba_r[, 2]
    ) %>%
    tibble::as_tibble() # Ensure it's a tibble

  message("DEBUG (model.R): Stress classification finished. Returning classified data frame.")
  return(classified_df)
}

# --- Optional: Self-contained test/example usage for development ---
# (Commented out in server.R context, but useful for direct testing of model.R)
# if (FALSE) { # Set to TRUE to run this test block
#   # For testing, you might need dummy data
#   message("--- Running self-contained test for model.R ---")
#
#   # Dummy HRV features data frame (replace with actual structure if needed)
#   dummy_features_df <- tibble::tibble(
#     window_id = 1:5,
#     window_start_time = as.POSIXct(c("2025-01-01 00:00:00", "2025-01-01 00:00:30", "2025-01-01 00:01:00", "2025-01-01 00:01:30", "2025-01-01 00:02:00"), tz = "UTC"),
#     window_end_time = as.POSIXct(c("2025-01-01 00:01:00", "2025-01-01 00:01:30", "2025-01-01 00:02:00", "2025-01-01 00:02:30", "2025-01-01 00:03:00"), tz = "UTC"),
#     sdnn = c(50, 45, 30, 40, 55),
#     rmssd = c(45, 40, 28, 35, 50),
#     lf_power = c(1000, 800, 500, 700, 1200),
#     hf_power = c(700, 600, 300, 500, 900),
#     lf_hf_ratio = c(1.4, 1.3, 1.6, 1.4, 1.3)
#   )
#
#   # Define the path to the model file
#   # Adjust this path based on where you put stress_rf.pkl relative to model.R
#   # If model.R is in R/ and stress_rf.pkl is in the project root:
#   model_file_path <- "../stress_rf.pkl"
#   # If model.R and stress_rf.pkl are in the same directory (e.g., both in R/):
#   # model_file_path <- "stress_rf.pkl"
#
#
#   if (!file.exists(model_file_path)) {
#       warning("CRITICAL: Model file '", model_file_path, "' not found. ",
#               "This test requires the pre-trained Python model. ",
#               "Please place 'stress_rf.pkl' correctly or update 'model_file_path'.")
#   } else {
#       # 4. Test classification
#       # load_stress_model is called implicitly by classify_stress_windows if needed
#       message("\nAttempting to classify stress using dummy data and model: ", model_file_path)
#       classified_data <- classify_stress_windows(dummy_features_df, model_path = model_file_path)
#
#       if (!is.null(classified_data)) {
#           message("\n--- Classified Features Data (Head) from Test ---")
#           # Select relevant columns for display
#           print(head(select(classified_data, window_id, sdnn, rmssd, lf_power, hf_power, lf_hf_ratio, stress_prediction, stress_probability)))
#           message("\n--- Prediction Summary from Test ---")
#           print(table(classified_data$stress_prediction, useNA = "ifany"))
#
#           message("\n--- Structure of classified_data from Test ---")
#           str(classified_data)
#       } else {
#           message("\nStress classification returned NULL during testing, indicating a failure.")
#           message("Check warnings above for details on model loading or prediction.")
#       }
#   }
# }