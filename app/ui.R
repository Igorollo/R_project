# app/ui.R

library(shiny)
library(ggplot2) # For plotting capabilities in UI
library(shinythemes) # For a nicer theme

shinyUI(fluidPage(
  theme = shinytheme("flatly"), # Using a Flatly theme for a modern look

  # Application title
  titlePanel("StressAware: HR Data Analysis for Stress Assessment"),

  # Sidebar layout with a sidebar for inputs and main panel for outputs
  sidebarLayout(
    sidebarPanel(
      width = 3, # Adjust sidebar width

      h3("1. Upload HR Data"),
      fileInput("file1", "Choose CSV/TXT File",
                multiple = FALSE,
                accept = c(".csv", ".txt")),

      hr(), # Horizontal line for separation

      h3("2. Preprocessing & Feature Extraction Parameters"),
      numericInput("resample_rate", "Resampling Rate (Hz):",
                   value = 4, min = 1, max = 10, step = 1),
      numericInput("malik_threshold", "Malik Threshold (0-1):",
                   value = 0.2, min = 0, max = 1, step = 0.05),
      numericInput("window_seconds", "HRV Window Size (seconds):",
                   value = 120, min = 30, max = 600, step = 30),
      numericInput("step_seconds", "HRV Step Size (seconds):",
                   value = 60, min = 10, max = 300, step = 10),

      hr(),

      h3("3. Visualization & Reporting"),
      checkboxGroupInput("plot_types", "Select Plots to Display:",
                         choices = c("Cleaned RR Intervals" = "cleaned_rr",
                                     "Interpolated HR" = "interpolated_hr",
                                     "HRV Features" = "hrv_features"),
                         selected = c("cleaned_rr", "interpolated_hr", "hrv_features")),
      downloadButton("downloadReport", "Download Analysis Report (PDF)"),
      downloadButton("downloadCSV", "Download HRV Features (CSV)")
    ),

    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Overview",
                 h4("Welcome to StressAware!"),
                 p("Upload your heart rate (RR interval) data to analyze stress levels."),
                 p("This application processes your data through a robust pipeline:"),
                 tags$ul(
                   tags$li("Data Cleaning & Quality Control: Handles artifacts, signal dropouts, and validates timestamps."),
                   tags$li("Feature Extraction: Calculates time-domain and frequency-domain Heart Rate Variability (HRV) metrics."),
                   tags$li("Stress Classification: Applies a pre-trained model to classify stress levels (Low/High)."),
                   tags$li("Visualization & Reporting: Provides interactive plots and downloadable reports.")
                 ),
                 br(),
                 h4("Instructions:"),
                 tags$ol(
                   tags$li("Use the 'Choose CSV/TXT File' button to upload your HR data (ensure it has a timestamp and RR interval column, e.g., 'Time' and 'rr_ms')."),
                   tags$li("Adjust preprocessing and feature extraction parameters as needed."),
                   tags$li("Select which plots you'd like to see."),
                   tags$li("View the results in the 'Data Summary', 'Plots', and 'Stress Classification' tabs."),
                   tags$li("Download your analysis report or raw HRV features using the buttons on the left.")
                 )
        ),
        tabPanel("Data Summary",
                 h4("Uploaded Data and Preprocessing Summary"),
                 verbatimTextOutput("data_summary_output"),
                 br(),
                 h4("HRV Features Summary"),
                 verbatimTextOutput("hrv_summary_output")
        ),
        tabPanel("Plots",
                 h4("Visualizations"),
                 fluidRow(
                   column(12,
                          uiOutput("plot_ui_cleaned_rr"), # Dynamic UI for cleaned RR plot
                          uiOutput("plot_ui_interpolated_hr"), # Dynamic UI for interpolated HR plot
                          uiOutput("plot_ui_hrv_features") # Dynamic UI for HRV features plot
                   )
                 )
        ),
        tabPanel("Stress Classification",
                 h4("Stress Classification Results"),
                 tableOutput("stress_classification_table"),
                 br(),
                 h4("Stress Level Over Time"),
                 plotOutput("stress_over_time_plot", height = "400px")
        )
      )
    )
  )
))