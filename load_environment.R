############################################################
# Description: Load required R packages for environmental 
#              health and air pollution analysis.
# Author: Chengxu Tong
# Date: 2025
# Notes: 
#   - This script provides a consistent environment setup.
#   - Simply source("load_environment.R") at the beginning 
#     of your analysis scripts.
############################################################

# Function to install missing packages
load_or_install <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

### Package List ----
required_packages <- c(
  # Data wrangling
  "dplyr", "tidyr", "stringr", "lubridate", "purrr",
  
  # File I/O
  "readxl",
  
  # Statistical & mathematical modeling
  "pracma", "drc", "gnm", "survival", "splines", "dlnm",
  
  # Spatial analysis
  "sp", "gstat", "leaflet",
  
  # Visualization
  "ggplot2", "plotly", "grid", "gridExtra", 
  "forestploter", "metaviz", "Cairo", "openair",
  
  # Meteorological conversion
  "weathermetrics"
)

### Install & Load ----
load_or_install(required_packages)