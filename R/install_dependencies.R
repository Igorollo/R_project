    # R/install_dependencies.R
    # Script to check for and install required packages for the StressAware HR processing pipeline.
    # List of required packages
    required_packages <- c(
      "readr",     # For reading data files (read_csv)
      "dplyr",     # For data manipulation (pipes, filter, mutate, etc.)
      "lubridate", # For timestamp parsing and handling
      "zoo",       # For interpolation (na.approx, na.spline) and rolling functions
      "ggplot2",
      'signal',
      'matrixStats',
      "patchwork",
      "reticulate",
      "tibble", # For plotting results (used in testing/examples)
      "shiny" # For interactive web applications
      # Add other packages here if needed later (e.g., 'signal' for FFT, specific HRV packages)
    )
    message("Checking for required packages...")

    # Loop through the required packages
    packages_to_install <- c()
    for (pkg in required_packages) {
      # Check if the package is installed using requireNamespace (avoids loading the package)
      if (!requireNamespace(pkg, quietly = TRUE)) {
        message("Package '", pkg, "' not found.")
        packages_to_install <- c(packages_to_install, pkg)
      } else {
        message("Package '", pkg, "' is already installed.")
      }
    }

    # Install missing packages
    if (length(packages_to_install) > 0) {
      message("\nAttempting to install missing package(s): ", paste(packages_to_install, collapse = ", "))
      # You might need to choose a CRAN mirror the first time you install packages
      # install.packages will ask interactively if needed, or you can set it:
      # options(repos = c(CRAN = "https://cloud.r-project.org/"))
      install.packages(packages_to_install)

      # Optional: Verify installation after attempting
      message("\nVerifying installation...")
      final_missing <- c()
      for (pkg in packages_to_install) {
          if (!requireNamespace(pkg, quietly = TRUE)) {
              final_missing <- c(final_missing, pkg)
              warning("Failed to install or load package: '", pkg, "'. Please install it manually.")
          } else {
               message("Successfully installed and verified package: '", pkg, "'")
          }
      }
       if(length(final_missing) > 0){
           message("\nSome packages could not be installed automatically. Please review warnings.")
       } else {
           message("\nAll required packages are now installed.")
       }

    } else {
      message("\nAll required packages are already installed.")
    }

    message("\nDependency check complete.")

    # Clean up variables
    # rm(required_packages, packages_to_install, pkg, final_missing) # Optional cleanup
    