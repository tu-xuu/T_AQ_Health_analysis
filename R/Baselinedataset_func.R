############################################################
# Function: Baselinedataset_func
# Description: Filter the original dataset based on temperature 
#              and PM2.5 thresholds to create a baseline dataset.
# Author: Chengxu Tong
# Date: 2025
# Arguments:
#   originaldatabase - data frame, must contain columns: 
#                      year, tasmax, population_weighted_pm2.5
#   MinTem           - numeric, lower bound for temperature (tasmax)
#   MaxTem           - numeric, upper bound for temperature (tasmax)
#   PM_threshold     - numeric, quantile threshold for PM2.5 (e.g., 0.25, 0.5)
#   WHO              - logical, if TRUE, use WHO PM2.5 threshold (5 µg/m³),
#                      otherwise use yearly quantiles.
# Returns:
#   A filtered data frame (baseline dataset).
# Example:
#   baseline_dataset(df, MinTem = 10, MaxTem = 25, PM_threshold = 0.5, WHO = FALSE)
############################################################

Baselinedataset_func <- function(originaldatabase, MinTem, MaxTem, PM_threshold, WHO) {
  
  # Input validation
  if (!all(c("year", "tasmax", "population_weighted_pm2.5") %in% names(originaldatabase))) {
    stop("`originaldatabase` must contain columns: year, tasmax, population_weighted_pm2.5")
  }
  if (!is.numeric(MinTem) || !is.numeric(MaxTem)) {
    stop("`MinTem` and `MaxTem` must be numeric.")
  }
  if (!is.numeric(PM_threshold) || PM_threshold <= 0 || PM_threshold >= 1) {
    stop("`PM_threshold` must be a numeric value between 0 and 1 (e.g., 0.25).")
  }
  
  if (isFALSE(WHO)) {
    # Calculate yearly PM2.5 quantile thresholds
    pm25_quantiles <- originaldatabase %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(
        pm25_threshold = quantile(population_weighted_pm2.5, PM_threshold, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Join threshold back to dataset
    originaldatabase <- originaldatabase %>%
      dplyr::left_join(pm25_quantiles, by = "year")
    
    # Filter by temperature and PM2.5
    baseline_data <- originaldatabase %>% 
      dplyr::filter(
        tasmax >= MinTem & tasmax <= MaxTem &
          population_weighted_pm2.5 < pm25_threshold
      ) %>%
      dplyr::select(-pm25_threshold)
    
  } else {
    # Use fixed WHO threshold = 5 µg/m³
    baseline_data <- originaldatabase %>% 
      dplyr::mutate(pm25_threshold = 5) %>%
      dplyr::filter(
        tasmax >= MinTem & tasmax <= MaxTem &
          population_weighted_pm2.5 < pm25_threshold
      ) %>%
      dplyr::select(-pm25_threshold)
  }
  
  return(baseline_data)
}