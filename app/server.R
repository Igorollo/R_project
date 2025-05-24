# app/server.R

# IMPORTANT: Configure reticulate to use your specific Python installation
library(reticulate)
reticulate::use_python("c:/users/paula/appdata/local/programs/python/python313/python.exe", required = TRUE)
# Note: Use forward slashes for paths in R, even on Windows.
# Also, ensure you point to the 'python.exe' itself.

library(shiny)
library(ggplot2)
library(dplyr)
library(patchwork) # For combining plots
library(rmarkdown) # For PDF report generation
library(hrbrthemes) # For nice ggplot themes

library(tinytex)
library(pandoc)
Sys.setenv(RSTUDIO_PANDOC = "C:/Users/paula/AppData/Local/r-pandoc/r-pandoc/3.7.0.1")

# Example snippet from app/global.R or app/server.R
source("../R/clean_hr_initial.R")
source("../R/detect_artifacts_malik.R")
source("../R/interpolate_hr.R")
source("../R/preprocess_hr.R")
source("../R/extract_features.R")
source("../R/model.R") # This loads the model functions
source("../R/classes.R")

options(shiny.maxRequestSize = 30*1024^2) # Increase max upload size to 30MB

shinyServer(function(input, output, session) { # 'session' is explicitly available here

  # Reactive value to store the HRSeries object
  hr_series_obj <- reactiveVal(NULL)

  # Reactive value for stress classification results
  classified_stress_data <- reactiveVal(NULL)

  # Observe file upload and trigger processing pipeline
  observeEvent(input$file1, {
    req(input$file1) # Ensure a file is uploaded

    # Show a progress message
    withProgress(message = 'Processing HR Data...', value = 0, {

      incProgress(0.1, detail = "Reading data...")
      file_path <- input$file1$datapath
      file_name <- input$file1$name

      # Load data using readr::read_csv
      raw_data <- tryCatch({
        readr::read_csv(file_path, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
      }, error = function(e) {
        shiny::showNotification(paste("Error reading file:", e$message), type = "error", duration = NULL, session = session)
        return(NULL)
      })
      req(raw_data) # Ensure data was read successfully

      # --- Step 1: Preprocessing ---
      incProgress(0.2, detail = "Cleaning and interpolating HR data...")
      preproc_results <- tryCatch({
        preprocess_hr(
          data = raw_data,
          time_col = "Time", # Assuming these are the column names in the uploaded file
          rr_col = "rr_ms",
          resample_rate = input$resample_rate,
          malik_threshold = input$malik_threshold
        )
      }, error = function(e) {
        shiny::showNotification(paste("Preprocessing Error:", e$message), type = "error", duration = NULL, session = session)
        hr_series_obj(NULL) # Clear previous results on error
        return(NULL)
      })
      req(preproc_results) # Ensure preprocessing was successful


      # --- Step 2: Feature Extraction ---
      incProgress(0.3, detail = "Extracting HRV features...")
      hrv_features <- tryCatch({
        # Ensure preproc_results is not NULL and contains expected elements
        if (is.null(preproc_results) ||
            is.null(preproc_results$interpolated_hr) ||
            nrow(preproc_results$interpolated_hr) == 0 ||
            is.null(preproc_results$cleaned_rr) ||
            nrow(preproc_results$cleaned_rr) == 0) {
          stop("Preprocessing results are incomplete or empty for feature extraction.")
        }
        extract_hrv_features(
          preprocessed_results = preproc_results,
          fs = input$resample_rate,
          window_seconds = input$window_seconds,
          step_seconds = input$step_seconds
        )
      }, error = function(e) {
        shiny::showNotification(paste("Feature Extraction Error:", e$message), type = "error", duration = NULL, session = session)
        hr_series_obj(NULL)
        return(NULL)
      })
      req(hrv_features) # Ensure feature extraction was successful


      # --- Step 3: Create HRSeries Object ---
      incProgress(0.1, detail = "Creating HRSeries object...")
      tryCatch({
        current_hr_series_obj <- create_hrseries(
          preprocessed_data = preproc_results,
          features_data = hrv_features,
          file_source = file_name,
          subject_id = "UploadedData", # Can be made dynamic if needed
          preprocessing_params = list(resample_rate = input$resample_rate,
                                      malik_threshold = input$malik_threshold),
          feature_extraction_params = list(fs = input$resample_rate,
                                           window_seconds = input$window_seconds,
                                           step_seconds = input$step_seconds)
        )
        hr_series_obj(current_hr_series_obj)
      }, error = function(e) {
        shiny::showNotification(paste("HRSeries Object Creation Error:", e$message), type = "error", duration = NULL, session = session)
        hr_series_obj(NULL)
        return(NULL)
      })


      # --- Step 4: Stress Classification ---
      incProgress(0.2, detail = "Classifying stress levels...")
      if (!is.null(hr_series_obj()) && !is.null(hr_series_obj()@hrv_features)) {
        tryCatch({
          # Corrected: Use a relative path from the app/ directory to the project root
          # This path assumes stress_rf.pkl is in the R_PROJECT root
          model_path_relative_to_app <- "../stress_rf.pkl"

          # Attempt to load model explicitly if not already loaded globally
          if (!exists(".stress_model_py", envir = .GlobalEnv)) {
            load_stress_model(model_path = model_path_relative_to_app) # Pass the relative path
          }

          classified_data <- classify_stress_windows(hr_series_obj()@hrv_features, model_path = model_path_relative_to_app)

          # --- DEBUGGING STATEMENTS ---
          message("DEBUG: classify_stress_windows returned. Type of classified_data: ", class(classified_data))
          message("DEBUG: Is classified_data NULL? ", is.null(classified_data))
          if (!is.null(classified_data) && is.data.frame(classified_data)) {
              message("DEBUG: Number of rows in classified_data: ", nrow(classified_data))
              message("DEBUG: Names of columns in classified_data: ", paste(names(classified_data), collapse = ", "))
              # Optional: Print head of data if it's a data frame
              # print(head(classified_data))
          }
          # --- END DEBUGGING STATEMENTS ---

          classified_stress_data(classified_data) # This line assigns the result to a reactive value
          message("DEBUG: classified_stress_data reactive value updated.")


          if (!is.null(classified_data)) {
            shiny::showNotification("Stress classification complete!", type = "success", session = session)
          } else {
            shiny::showNotification("Stress classification returned no results.", type = "warning", session = session)
          }

        }, error = function(e) {
          # Use a simplified message to rule out issues with the error string content itself
          shiny::showNotification("An error occurred during stress classification. Please check the R console for details.",
                           type = "error", duration = NULL, session = session)
          classified_stress_data(NULL)
        })
      } else {
        shiny::showNotification("HRV features not available for stress classification.", type = "warning", session = session)
        classified_stress_data(NULL)
      }

      incProgress(0.1, detail = "Rendering results...")
    }) # End withProgress
  }) # End observeEvent


  # --- Output: Data Summary ---
  output$data_summary_output <- renderPrint({
    req(hr_series_obj())
    summary(hr_series_obj())
  })

  # --- Output: HRV Features Summary ---
  output$hrv_summary_output <- renderPrint({
    req(hr_series_obj())
    if (!is.null(hr_series_obj()@hrv_features)) {
      cat("HRV Features Data (Head):\n")
      print(head(hr_series_obj()@hrv_features))
      cat("\nHRV Features Data (Summary):\n")
      summary(hr_series_obj()@hrv_features)
    } else {
      "No HRV features available."
    }
  })

  # --- Dynamic Plot Outputs (based on user selection) ---

  output$plot_ui_cleaned_rr <- renderUI({
    if ("cleaned_rr" %in% input$plot_types) {
      plotOutput("cleaned_rr_plot", height = "400px")
    }
  })

  output$plot_ui_interpolated_hr <- renderUI({
    if ("interpolated_hr" %in% input$plot_types) {
      plotOutput("interpolated_hr_plot", height = "400px")
    }
  })

  output$plot_ui_hrv_features <- renderUI({
    if ("hrv_features" %in% input$plot_types) {
      plotOutput("hrv_features_plot", height = "400px")
    }
  })


  output$cleaned_rr_plot <- renderPlot({
    req(hr_series_obj())
    if ("cleaned_rr" %in% input$plot_types) {
      plot(hr_series_obj(), type = "cleaned_rr")
    }
  })

  output$interpolated_hr_plot <- renderPlot({
    req(hr_series_obj())
    if ("interpolated_hr" %in% input$plot_types) {
      plot(hr_series_obj(), type = "interpolated_hr")
    }
  })

  output$hrv_features_plot <- renderPlot({
    req(hr_series_obj())
    if ("hrv_features" %in% input$plot_types) {
      plot(hr_series_obj(), type = "hrv_features")
    }
  })

  # --- Output: Stress Classification Table ---
  output$stress_classification_table <- renderTable({
    req(classified_stress_data())
    # Select relevant columns for display
    classified_stress_data() %>%
      select(window_start_time, window_end_time,
             stress_prediction, stress_probability,
             rmssd, sdnn, lf_hf_ratio) %>%
      mutate(stress_prediction = as.factor(stress_prediction)) %>% # Ensure factor for display
      head(20) # Displaying first 20 rows for brevity
  }, rownames = FALSE)

  # --- Output: Stress Over Time Plot ---
  output$stress_over_time_plot <- renderPlot({
    req(classified_stress_data())
    plot_data <- classified_stress_data() %>%
      mutate(
        stress_color = case_when(
          stress_prediction == 1 ~ "High Stress",
          stress_prediction == 0 ~ "Low Stress",
          TRUE ~ "Unknown" # Handle any other cases
        ),
        stress_color = factor(stress_color, levels = c("Low Stress", "High Stress", "Unknown"))
      )

    ggplot(plot_data, aes(x = window_end_time, y = stress_prediction, color = stress_color)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_line(aes(y = stress_probability), linetype = "dashed", color = "grey50") + # Show probability trend
      scale_color_manual(values = c("Low Stress" = "#1f78b4", "High Stress" = "#e31a1c", "Unknown" = "grey")) +
      labs(title = "Stress Level Over Time",
           x = "Time",
           y = "Stress Prediction (0=Low, 1=High)",
           color = "Stress Level") +
      scale_y_continuous(breaks = c(0, 1), labels = c("Low", "High")) +
      theme_ipsum_rc(grid="Y") + # Using hrbrthemes
      theme(legend.position = "bottom",
            panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"))
  })


  # --- Download Report (PDF) ---
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("StressAware_Report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "StressAware_demo.Rmd")
      file.copy("../vignettes/StressAware_demo.Rmd", tempReport, overwrite = TRUE)

      # Determine the project root dynamically
      # If runApp("app") is called from the project root, then getwd() would be "app"
      # so dirname(getwd()) would be the project root.
      project_root_path <- dirname(getwd())


      # Pass parameters to the Rmd document using a new name for the list
      report_params <- list(
        hr_series_obj_param = hr_series_obj(),
        classified_stress_data_param = classified_stress_data(),
        plot_types_param = input$plot_types, # Pass selected plot types
        project_root_param = project_root_path # Pass the project root
      )

      # Render the Rmd file into a PDF
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = report_params, # Use the new name here
                        envir = new.env(parent = globalenv()) # Isolate environment
      )
    }
  )

  # --- Download CSV ---
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("HRV_Features_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(hr_series_obj())
      if (!is.null(hr_series_obj()@hrv_features)) {
        write.csv(hr_series_obj()@hrv_features, file, row.names = FALSE)
      } else {
        stop("No HRV features data to download.")
      }
    }
  )

})