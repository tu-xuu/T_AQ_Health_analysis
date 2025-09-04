############################################################
# Function: Getexposuredataset_func
# Description: Identify exposure days based on temperature 
#              or PM2.5 percentiles for each year.
# Author: Chengxu Tong
# Date: 2025
# Arguments:
#   originaldatabase     - data frame; must contain columns:
#                          year, tasmax, population_weighted_pm2.5
#   exposuretype         - character; "Temperature" or "PM2.5"
#   exposure_percentiles - numeric; percentile threshold (e.g., 0.90)
# Returns:
#   A filtered data frame containing only exposure days.
# Example:
#   get_exposure_dataset(df, "Temperature", 0.90)
#   get_exposure_dataset(df, "PM2.5", 0.75)
############################################################

Getexposuredataset_func <- function(originaldatabase, exposuretype, exposure_percentiles) {
  
  # Validate inputs
  if (!"year" %in% names(originaldatabase)) {
    stop("`originaldatabase` must contain column: year")
  }
  if (!exposuretype %in% c("Temperature", "PM2.5")) {
    stop("`exposuretype` must be either 'Temperature' or 'PM2.5'")
  }
  if (!is.numeric(exposure_percentiles) || exposure_percentiles <= 0 || exposure_percentiles >= 1) {
    stop("`exposure_percentiles` must be a numeric value between 0 and 1 (e.g., 0.90)")
  }
  
  if (exposuretype == "Temperature") {
    # Calculate yearly high temperature percentiles
    PercentileLabel <- originaldatabase %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(
        High_tem = quantile(tasmax, exposure_percentiles, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Filter high temperature days
    Exposure_days <- originaldatabase %>%
      dplyr::left_join(PercentileLabel, by = "year") %>%
      dplyr::filter(tasmax > High_tem) %>%
      dplyr::select(-High_tem)
    
  } else if (exposuretype == "PM2.5") {
    # Calculate yearly high PM2.5 percentiles
    PercentileLabel <- originaldatabase %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(
        High_PM2.5 = quantile(population_weighted_pm2.5, exposure_percentiles, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Filter high PM2.5 days
    Exposure_days <- originaldatabase %>%
      dplyr::left_join(PercentileLabel, by = "year") %>%
      dplyr::filter(population_weighted_pm2.5 > High_PM2.5) %>%
      dplyr::select(-High_PM2.5)
  }
  
  return(Exposure_days)
}