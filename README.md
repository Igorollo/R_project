# StressAware: HR Data Analysis for Stress Assessment

## 1. Project Overview

StressAware is an R-based toolkit for analyzing heart rate (HR) time-series data to assess and predict acute stress episodes. It combines a Shiny web application for interactive analysis with a backend R pipeline for data processing and stress classification using a Python model.

**Key Features:**
* Interactive Shiny App for data upload, parameter tuning, and visualization.
* Comprehensive HR data preprocessing (cleaning, artifact detection, interpolation).
* Time-domain and Frequency-domain HRV feature extraction.
* Stress classification using a pre-trained Python RandomForest model.
* Downloadable PDF reports and HRV feature CSV files.

## 2. Directory Structure
```
igorollo/r_project/
├── app/                  # Shiny application (ui.R, server.R)
├── hr_data/              # Sample HR data files (e.g., 3.txt, 5.txt)
├── R/                    # Core R processing scripts
├── vignettes/            # R Markdown templates for reports
├── README.md             # This file
├── process_hr_data.R     # Standalone R script for batch processing
├── train_stress_rf.py    # Python script to train the stress model
└── stress_rf.pkl         # Pre-trained Python stress model
```

## 3. Setup and Installation

### 3.1. R Environment

1.  **Install R and RStudio.**
2.  **R Package Dependencies:**
    Open R or RStudio and run the following command in the console to install all required packages:
    ```R
    install.packages(c("readr", "dplyr", "lubridate", "zoo", "ggplot2", "signal", "matrixStats", "patchwork", "reticulate", "tibble", "shiny", "shinythemes", "rmarkdown", "hrbrthemes", "tinytex"))
    ```
    Alternatively, you can source the provided script (ensure your working directory is the project root):
    ```R
    source("R/install_dependencies.R") # This script checks and installs missing packages.
    ```
3.  **TinyTeX for PDF Reports:**
    ```R
    tinytex::install_tinytex() # Run if PDF report generation fails.
    ```
4.  **Pandoc:** Ensure Pandoc is installed (usually comes with RStudio).

### 3.2. Python Environment

1.  **Install Python.**
2.  **Install Python Packages:**
    ```bash
    pip install numpy scikit-learn joblib
    ```
3.  **Configure `reticulate` (VERY IMPORTANT):**
    The `app/server.R` file contains specific paths for Python and Pandoc that **MUST be updated** to your local machine's settings:
    * `reticulate::use_python("c:/users/paula/appdata/local/programs/python/python313/python.exe", required = TRUE)`
    * `Sys.setenv(RSTUDIO_PANDOC = "C:/Users/paula/AppData/Local/r-pandoc/r-pandoc/3.7.0.1")`
    Replace these example paths with the correct paths for your Python executable (that has numpy, scikit-learn, joblib) and your Pandoc installation if needed.

### 3.3. Pre-trained Model

Ensure the `stress_rf.pkl` model file is present in the project root directory. It can be generated using `train_stress_rf.py`.

## 4. How to Run

### 4.1. Shiny Web Application

1.  Navigate to the project root directory in RStudio or your R console/terminal.
2.  Ensure Python and Pandoc paths in `app/server.R` are correctly set for your system.
3.  **Run from RStudio:** Open `app/ui.R` or `app/server.R` and click "Run App".
4.  **Run from R console/terminal:**
    ```R
    # setwd("/path/to/your/igorollo/r_project/") # Set if not already in project root
    library(shiny)
    runApp("app")
    ```
    Alternatively, from your system terminal (replace with the full path):
    ```bash
    R -e "shiny::runApp('/path/to/your/igorollo/r_project/app')"
    ```

### 4.2. Standalone R Script (`process_hr_data.R`)

This script processes a single HR file. Configure paths and parameters within the script before running.
```R
source("process_hr_data.R")
```

## 5. Python Model Training (train_stress_rf.py)

To retrain the stress classification model using the WESAD dataset:
```bash
python train_stress_rf.py --data_dir /path/to/WESAD_dataset
```

This will generate the stress_rf.pkl file.

## 6. Authors

Paula Banach, 440186

Igor Kołodziej, 440239

**NOTE:** Ensure all paths and specific configurations (especially Python environment and pandoc in app/server.R) are adjusted for your local setup.