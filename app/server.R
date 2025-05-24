# app/server.R

# IMPORTANT: Configure reticulate to use your specific Python installation
library(reticulate)
# Make sure this path points to YOUR Python executable that has scikit-learn, joblib, and numpy
reticulate::use_python("c:/users/paula/appdata/local/programs/python/python313/python.exe", required = TRUE)


library(shiny)
library(ggplot2)
library(dplyr)
library(patchwork) 
library(rmarkdown) 
library(hrbrthemes) 

library(tinytex)
# library(pandoc) # pandoc package itself is not usually needed directly if rmarkdown is used

# This line should point to the pandoc installed by pandoc_install() or a system-wide one.
# Ensure this path is correct for your system if you uncomment it.
# If Pandoc is in your system PATH and rmarkdown::pandoc_available() is TRUE, this line might not be needed.
Sys.setenv(RSTUDIO_PANDOC = "C:/Users/paula/AppData/Local/r-pandoc/r-pandoc/3.7.0.1")


# Source R scripts - paths are relative to the app/ directory
source("../R/clean_hr_initial.R")
source("../R/detect_artifacts_malik.R")
source("../R/interpolate_hr.R")
source("../R/preprocess_hr.R")
source("../R/extract_features.R")
source("../R/model.R") 
source("../R/classes.R")

options(shiny.maxRequestSize = 30*1024^2) 

shinyServer(function(input, output, session) {

  hr_series_obj <- reactiveVal(NULL)
  classified_stress_data <- reactiveVal(NULL)

  observeEvent(input$file1, {
    req(input$file1)

    withProgress(message = 'Processing HR Data...', value = 0, {
      incProgress(0.1, detail = "Reading data...")
      file_path <- input$file1$datapath
      file_name <- input$file1$name

      raw_data <- tryCatch({
        readr::read_csv(file_path, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
      }, error = function(e) {
        shiny::showNotification(paste("Error reading file:", e$message), type = "error", duration = NULL)
        return(NULL)
      })
      req(raw_data)

      incProgress(0.2, detail = "Cleaning and interpolating HR data...")
      preproc_results <- tryCatch({
        preprocess_hr(
          data = raw_data,
          time_col = "Time",
          rr_col = "rr_ms",
          resample_rate = input$resample_rate,
          malik_threshold = input$malik_threshold
        )
      }, error = function(e) {
        shiny::showNotification(paste("Preprocessing Error:", e$message), type = "error", duration = NULL)
        hr_series_obj(NULL)
        return(NULL)
      })
      req(preproc_results)

      incProgress(0.3, detail = "Extracting HRV features...")
      hrv_features <- tryCatch({
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
        shiny::showNotification(paste("Feature Extraction Error:", e$message), type = "error", duration = NULL)
        hr_series_obj(NULL)
        return(NULL)
      })
      req(hrv_features)

      incProgress(0.1, detail = "Creating HRSeries object...")
      tryCatch({
        current_hr_series_obj <- create_hrseries(
          preprocessed_data = preproc_results,
          features_data = hrv_features,
          file_source = file_name,
          subject_id = "UploadedData",
          preprocessing_params = list(resample_rate = input$resample_rate,
                                      malik_threshold = input$malik_threshold),
          feature_extraction_params = list(fs = input$resample_rate,
                                           window_seconds = input$window_seconds,
                                           step_seconds = input$step_seconds)
        )
        hr_series_obj(current_hr_series_obj)
      }, error = function(e) {
        shiny::showNotification(paste("HRSeries Object Creation Error:", e$message), type = "error", duration = NULL)
        hr_series_obj(NULL)
        return(NULL)
      })

      incProgress(0.2, detail = "Classifying stress levels...")
      if (!is.null(hr_series_obj()) && !is.null(hr_series_obj()@hrv_features) && nrow(hr_series_obj()@hrv_features) > 0) {
        tryCatch({
          model_path_relative_to_app <- "../stress_rf.pkl"

          classified_data <- classify_stress_windows(
            hrv_features_df = hr_series_obj()@hrv_features,
            model_path = model_path_relative_to_app
          )

          message("DEBUG: classify_stress_windows returned. Type of classified_data: ", class(classified_data))
          message("DEBUG: Is classified_data NULL? ", is.null(classified_data))
          if (!is.null(classified_data) && is.data.frame(classified_data)) {
              message("DEBUG: Number of rows in classified_data: ", nrow(classified_data))
              message("DEBUG: Names of columns in classified_data: ", paste(names(classified_data), collapse = ", "))
          }

          classified_stress_data(classified_data)
          message("DEBUG: classified_stress_data reactive value updated.")

          if (!is.null(classified_data)) {
            shiny::showNotification("Stress classification complete!", type = "message", duration = 5) 
          } else {
            shiny::showNotification("Stress classification returned no results. The model might not have produced predictions.", type = "warning", duration = 5)
          }

        }, error = function(e) {
          notification_title <- "Classification Process Error (Caught in server.R)"
          full_error_message <- paste("Message:", conditionMessage(e)) 
          
          message("--- DETAILED ERROR CAUGHT IN CLASSIFICATION TRYCATCH (server.R) ---")
          message("Error Class: ", paste(class(e), collapse=", "))
          message("Error Call: ", deparse(conditionCall(e)))   
          message("Error Message: ", conditionMessage(e)) 
          message("Full Error Object:")
          print(e) 
          message("--- END DETAILED ERROR ---")

          shiny::showNotification(
            ui = paste(notification_title, "\n", full_error_message),
            type = "error",
            duration = NULL 
          )
          classified_stress_data(NULL)
        })
      } else {
        shiny::showNotification("HRV features not available or empty, skipping stress classification.", type = "warning", duration = 5) 
        classified_stress_data(NULL)
      }
      incProgress(0.1, detail = "Rendering results...")
    })
  })

  output$data_summary_output <- renderPrint({
    req(hr_series_obj())
    summary(hr_series_obj())
  })

  output$hrv_summary_output <- renderPrint({
    req(hr_series_obj())
    if (!is.null(hr_series_obj()@hrv_features) && nrow(hr_series_obj()@hrv_features) > 0) {
      cat("HRV Features Data (Head):\n")
      print(head(hr_series_obj()@hrv_features))
      cat("\nHRV Features Data (Summary):\n")
      summary(hr_series_obj()@hrv_features)
    } else {
      "No HRV features available."
    }
  })

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
    if ("hrv_features" %in% input$plot_types && !is.null(hr_series_obj()@hrv_features) && nrow(hr_series_obj()@hrv_features) > 0) {
      plot(hr_series_obj(), type = "hrv_features")
    } else {
      ggplot() + theme_void() + labs(title = "No HRV Features to plot.")
    }
  })

  output$stress_classification_table <- renderTable({
    req(classified_stress_data())
    if (is.null(classified_stress_data()) || nrow(classified_stress_data()) == 0) {
        return(data.frame(Message = "No stress classification data to display."))
    }
    
    display_data <- classified_stress_data()
    
    cols_to_select <- c("window_end_time", "stress_prediction", "stress_probability",
                        "rmssd", "sdnn", "lf_hf_ratio")
    
    available_cols <- cols_to_select[cols_to_select %in% names(display_data)]
    
    if (length(available_cols) < length(cols_to_select)) {
        warning(paste("One or more expected columns for stress table are missing. Available:", paste(available_cols, collapse=", ")))
    }
    
    display_data <- display_data %>%
      select(all_of(available_cols)) 
      
    if ("stress_prediction" %in% names(display_data)) {
        display_data <- display_data %>%
            mutate(stress_prediction = as.factor(stress_prediction))
    }

    head(display_data, 20)
  }, rownames = FALSE)


  output$stress_over_time_plot <- renderPlot({
    req(classified_stress_data())
    if (is.null(classified_stress_data()) || nrow(classified_stress_data()) == 0) {
        return(ggplot() + theme_void() + labs(title = "No stress classification data to plot."))
    }
    
    plot_data <- classified_stress_data() %>%
      mutate(
        stress_color = case_when(
          stress_prediction == 1 ~ "High Stress",
          stress_prediction == 0 ~ "Low Stress",
          TRUE ~ "Unknown"
        ),
        stress_color = factor(stress_color, levels = c("Low Stress", "High Stress", "Unknown"))
      )

    if (!"window_end_time" %in% names(plot_data) || !"stress_prediction" %in% names(plot_data)) {
        return(ggplot() + theme_void() + labs(title = "Required columns for stress plot missing."))
    }

    ggplot(plot_data, aes(x = window_end_time, y = stress_prediction, color = stress_color)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_line(aes(y = stress_probability), linetype = "dashed", color = "grey50", na.rm = TRUE) +
      scale_color_manual(values = c("Low Stress" = "#1f78b4", "High Stress" = "#e31a1c", "Unknown" = "grey")) +
      labs(title = "Stress Level Over Time",
           x = "Time",
           y = "Stress Prediction (0=Low, 1=High)",
           color = "Stress Level") +
      scale_y_continuous(breaks = c(0, 1), labels = c("Low", "High")) +
      theme_ipsum_rc(grid="Y") +
      theme(legend.position = "bottom",
            panel.grid.major.x = element_line(linetype = "dotted", color = "lightgrey"))
  })

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("StressAware_Report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      if (!rmarkdown::pandoc_available(version = "1.12.3")) {
        error_msg <- "Pandoc version 1.12.3 or higher is required to generate the PDF report and was not found. Please install or update Pandoc. (You can check availability in R console with rmarkdown::pandoc_available()). If you used pandoc_install(), ensure RSTUDIO_PANDOC environment variable is correctly set at the top of server.R if needed, or that Pandoc is in your system PATH."
        shiny::showNotification(error_msg, type = "error", duration = NULL)
        writeLines(error_msg, file) 
        return() 
      }

      tempReport <- file.path(tempdir(), "StressAware_demo.Rmd")
      file.copy("../vignettes/StressAware_demo.Rmd", tempReport, overwrite = TRUE)

      project_root_path <- dirname(getwd()) 

      # --- MODIFICATION FOR STRESS PLOT IN REPORT ---
      # Get the plot types selected by the user
      current_plot_types <- input$plot_types
      # If stress classification data is available, ensure "stress_classification" is added
      # to the plot_types parameter that will be passed to the Rmd.
      if (!is.null(classified_stress_data()) && nrow(classified_stress_data()) > 0) {
          current_plot_types <- unique(c(current_plot_types, "stress_classification"))
      }
      # --- END MODIFICATION ---

      report_params <- list(
        hr_series_obj_param = hr_series_obj(),
        classified_stress_data_param = classified_stress_data(),
        plot_types_param = current_plot_types, # Use the modified plot types
        project_root_param = project_root_path
      )

      tryCatch({
        rmarkdown::render(tempReport,
                          output_file = file,
                          params = report_params,
                          envir = new.env(parent = globalenv())
        )
      }, error = function(e) {
        error_msg_render <- paste("Failed to render PDF report:", e$message, "Please check R console for LaTeX or Pandoc errors.")
        shiny::showNotification(error_msg_render, type = "error", duration = NULL)
        writeLines(c("Failed to generate PDF report.", "Error message:", e$message, "Please check the R console for detailed Pandoc or LaTeX logs."), file)
      })
    }
  )

  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("HRV_Features_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(hr_series_obj())
      if (!is.null(hr_series_obj()@hrv_features) && nrow(hr_series_obj()@hrv_features) > 0) {
        write.csv(hr_series_obj()@hrv_features, file, row.names = FALSE)
      } else {
        write.csv(data.frame(Message = "No HRV features data to download."), file, row.names = FALSE)
        shiny::showNotification("No HRV features data available to download.", type = "warning", duration = 5)
      }
    }
  )
})