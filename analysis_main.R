############################################################
# Script Name: analysis_main.R
# Description: Entry point to run single/co-exposure analysis
# Author: Chengxu Tong
# Date: 2025
############################################################

# --- 0) Paths & basic config ---
root_dir   <- here::here()                       # or setwd(...) then use getwd()
R_dir      <- file.path(root_dir, "R")           # put all .R functions here
data_path  <- file.path(root_dir, "data", "Reanalyze_database.Rdata")   


lagdays <- 5
area    <- "London"
y_var   <- "totdeath"

# --- 1) Load environment (packages) ---
source(file.path(root_dir, "load_environment.R"))

# --- 2) Source all function files under R/ ---
r_files <- list.files(R_dir, pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_files, source, encoding = "UTF-8"))

# --- 3) Read the database ---
db <- readRDS(data_path)
# --- 4) Run analysis ---
res <- single_co_exposure_func_PM25(
  areaname = 'London',Y='totdeath',
  db
  
)


res $Single_plot
