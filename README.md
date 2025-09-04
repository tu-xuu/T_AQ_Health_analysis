# Temperature & PM2.5 Co-Exposure Health Effect Modeling

This repository contains a suite of R functions and scripts designed to quantify the short-term health impacts of extreme temperature and PM2.5 exposure using stratified quasi-Poisson models. The framework supports single and co-exposure analysis, lag effect estimation, and interaction effect computation.

## Load environment
source("load_environment.R")
## How to Run the Main Analysis
source("analysis_main.R")

This script performs the following steps:
	1.	Loads the pre-cleaned dataset (Reanalyze_database.Rdata)
	2.	Extracts exposure and baseline periods
	3.	Constructs lag-specific modeling datasets
	4.	Fits stratified quasi-Poisson models (via gnm::gnm)
	5.	Computes single and co-exposure effects
	6.	Outputs results for visualization (e.g., forest plots)
## Function Overview

This repository includes a series of modular R scripts for analyzing the short-term health effects of extreme temperature and PM2.5 exposure, including co-exposure and interaction modeling. The functions are organized under the `/R` directory.

| File | Description |
|------|-------------|
| `BaselineDataset_func.R` | Constructs the baseline dataset. Used to create reference (control) periods for quasi-Poisson modeling. |
| `compute_interaction_effects.R` | Calculates interaction metrics such as the Relative Excess Risk due to Interaction (RERI), based on results from single and co-exposure models. |
| `data_optimal_temperature.R` | Defines or retrieves the optimal temperature value (minimum mortality temperature) for the study population. |
| `forestdataframe_func.R` | Prepares model results into a tidy format for forest plot visualization. Includes lag-specific estimates, confidence intervals. |
| `Get_co_exposure_data_func.R` | Identifies dates where both PM2.5 and temperature exceed defined thresholds, and creates a dataset for modeling joint exposure effects. |
| `get_temperature_range.R` | Extracts temperature boundaries (MMT) for defining basline days based on the study population. |
| `Getexposuredataset_func.R` | Filters the original dataset to create exposure datasets for either PM2.5 or temperature, based on user-specified percentile cutoffs. |
| `Getmodeldataset_func.R` | Builds the final modeling dataset by combining baseline and lagged exposure dataset, with appropriate exposure coding and stratification. |
| `Model_func.R` | Fits stratified quasi-Poisson regression models across multiple lag days. Outputs include relative risk (RR), confidence intervals, p-values, and AF (attributable fraction). |
| `remove_lagdays_func.R` | Excludes overlapping lag days from the baseline dataset to ensure clear separation between control and exposure periods. |
| `single_co_exposure_func_PM25.R` | Main wrapper function for single and co-exposure analyses. Integrates data construction, modeling, and visualization into a single pipeline. |

## After running the analysis pipeline, the following outputs will be available:

- **Relative risk (RR) estimates** for each lag day

- **95% confidence intervals** (upper, lower)

- **Interaction metrics** (e.g., RERI)

- **Data frame outputs** for visualization (e.g., forest plots)

**Maintainer:** Chengxu Tong

**For questions or collaborations**, feel free to open an issue or contact the maintainer.
