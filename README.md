1. Project Overview & GoalProject Name: StressAwareCore Concept: Develop a comprehensive R-based toolkit (Shiny dashboard + backend processing pipeline) for analyzing wearable heart-rate (HR) time-series data to assess and predict acute stress episodes.Target Users: Clinicians and self-trackers.Primary Goal: Enable users to upload HR data, have it automatically cleaned, extract relevant stress-related physiological features (HRV), visualize the results, and classify stress levels.Key Differentiators:Transparency: Provides a clear pipeline for users to understand each analysis step.Modularity: Designed for easy integration of new algorithms or models.Real-World Application: Addresses the need for accessible stress analysis from wearable data.

2. Core Analytical WorkflowAgents should understand and contribute to the following workflow steps:Data Ingestion: User uploads time-series HR data (format TBD, assume timestamp and HR columns).Data Cleaning & QC: Implement clean_hr() to handle signal dropouts, artifacts, noise, and validate timestamps. Employ defensive programming.Feature Extraction: Calculate established HRV metrics (SDNN, RMSSD, LF/HF ratio) over user-defined rolling windows using vectorized methods (extract_features()).Stress Classification: Apply a pre-existing model (link provided) or implement custom logic to classify time segments as "low-stress" or "high-stress" (classify_stress()).Visualization & Reporting: Develop a Shiny dashboard for interactive exploration of raw/processed data, features, stress flags, and allow export of results (CSV/PDF).

3. Technical Stack & Required R TechniquesAgents must adhere to the specified R techniques:Language: RCore Packages: shiny, zoo (for rollapply), signal (for fft if needed), matrixStats, potentially RHRV (or implement core logic manually).Advanced Functions: Implement robust functions (especially clean_hr()) with:Rigorous input validation (data types, column existence, value ranges).Timestamp consistency checks.Clear, informative error messages and warnings.Graceful handling of edge cases (e.g., insufficient data for windows).Object-Oriented Programming (S4):Define and use the HRSeries S4 class to encapsulate data (raw, cleaned), features, and metadata.Implement plot() and summary() methods for the HRSeries class.Vectorization:Mandatory: Prioritize vectorized operations for performance in feature extraction (extract_features.R).Utilize functions like rollapply, diff, sqrt, mean, sd, fft, and matrix/vector operations.Avoid: Row-by-row loops or apply functions where vectorized alternatives exist.Shiny Application:Modular design (consider Shiny Modules if complexity increases).Reactive programming for efficient updates.Clear separation of UI (ui.R) and Server (server.R) logic.Interactive elements (inputs for parameters like window size, plots using ggplot2 or plotly).

4. Project Structure & DeliverablesAgents must place code in the correct files and contribute towards the final deliverables:Core R Scripts (in R/ directory):R/clean_hr.R: Data cleaning function (clean_hr).R/extract_features.R: HRV feature extraction functions (e.g., extract_hrv_features).R/models.R: Stress classification logic (e.g., classify_stress, potentially loading the external model).R/classes.R: Definition of the HRSeries S4 class and its methods.Rule: Each script should be self-contained in terms of its primary functions but can depend on others (e.g., extract_features might expect cleaned data from clean_hr). They will be sourced by the main Shiny app.Shiny Application (in app/ directory):app/ui.R: Defines the user interface.app/server.R: Contains server logic, reactivity, plotting.Rule: The Shiny app orchestrates the workflow, calling functions from the R/ scripts.Demonstration Materials:data/sample_hr.csv (or similar): A small, anonymized sample dataset.vignettes/StressAware_demo.Rmd: An R Markdown vignette demonstrating usage.

5. Agent Operational Rules & GuidelinesCode Generation:Generate well-commented, readable R code.Adhere strictly to the specified file structure (R/, app/, vignettes/).Implement defensive programming checks in all relevant functions.Prioritize vectorization for performance-critical sections (feature extraction).Use the HRSeries S4 class consistently for data handling after cleaning.Collaboration:Assume other agents might work on different files concurrently. Ensure functions have clear inputs and outputs.When modifying existing code, explain the changes clearly.Testing:Include basic examples or assertions within function documentation where appropriate.Test functions with edge cases (e.g., empty data, data with many NaNs).Dependencies:Clearly state any required R packages at the beginning of scripts or in a central place (like the Shiny app's global.R or DESCRIPTION file if creating a package structure).Model Integration:If using the external model, provide clear instructions or code snippets for downloading/loading it.If implementing a custom model, keep it simple initially and clearly document the logic.Shiny Development:Focus on reactivity and user experience.Ensure plots are informative and interactive controls are intuitive.Implement data upload and result download functionalities.

TO RUN:
> source("/Users/igor/Downloads/R/model.R", encoding = "UTF-8")
Successfully configured reticulate and can find joblib & numpy.
> source("/Users/igor/Downloads/R/R/preprocess_hr.R", encoding = "UTF-8")
> source("/Users/igor/Downloads/R/R/classes.R", encoding = "UTF-8")
> source("/Users/igor/Downloads/R/R/extract_features.R", encoding = "UTF-8")
> library(reticulate)
> py_discover_config()
> source("/Users/igor/Downloads/R/R/model.R", encoding = "UTF-8")
> fs <- 4.0
> win_sec <- 120
> step_sec <- 60
> file <- "hr_data/3.txt"
> preproc_data <- preprocess_hr(file_path = file, resample_rate = fs)
>    hrv_data <- extract_hrv_features(preproc_data, fs = fs, window_seconds = win_sec, step_seconds = step_sec)
===== Starting HRV Feature Extraction Pipeline =====
Extracting Time-Domain HRV features...
Processing 31 time windows...
Combining time and frequency domain results...
Results combined successfully.
===== HRV Feature Extraction Pipeline Finished Successfully =====
> classified_output <- classify_stress_windows(hrv_data, model_path = "/Users/igor/Downloads/R/stress_rf.pkl")
Python model not found in global environment. Attempting to load...
Loading Python stress model from: /Users/igor/Downloads/R/stress_rf.pkl
Python stress model loaded and stored globally as .stress_model_py
Sending 31 valid windows to Python model for prediction...
Predictions received from Python model.
> View(classified_output)
> hr_series_obj <- create_hrseries( preprocessed_data = preproc_data, features_data = hrv_data, file_source = file, # Add any other relevant metadata preprocessing_params = list(resample_rate = fs), feature_extraction_params = list(fs = fs, window_seconds = win_sec, step_seconds = step_sec) )
+ )



##### LINK TO THE CONVERSATION WITH AI #####
## https://g.co/gemini/share/b9854c3d226c ##

Methods to use with class: 
    print(hr_series_obj)   # Calls show()
    summary(hr_series_obj) # Calls summary()
    plot(hr_series_obj)    # Calls plot() with type="all"
    plot(hr_series_obj, type = "cleaned_rr") # Plot specific type
# "cleaned_rr": Shows RR intervals over time, colored by status (Valid NN, Artifact).
# "interpolated_hr": Shows the continuous HR signal over time.
# "hrv_features": Creates time series plots for key features like RMSSD and LF/HF ratio (if they exist in the hrv_features slot).
# "all": Generates all the above plots.

#### Link to the hr_data directory ####
## https://physionet.org/content/respiratory-heartrate-dataset/1.0.0/HRM_rawData/#files-panel ##