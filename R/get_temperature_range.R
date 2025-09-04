############################################################
# Function: get_temperature_range
# Description: Retrieve the optimal temperature range for a 
#              given region from the dataset 
#              `optimal_temperature_ranges`.
# Author: Chengxu Tong
# Date: 2025
# Arguments:
#   region_name - character string, the name of the region 
#                 (must match the "Region" column exactly).
# Returns:
#   A data frame with two columns: Lower_bound and Upper_bound.
# Example:
#   get_temperature_range("London")
############################################################

get_temperature_range <- function(region_name) {
  
  # Validate input
  if (!is.character(region_name) || length(region_name) != 1) {
    stop("`region_name` must be a single character string.")
  }
  
  # Lookup temperature range
  range <- optimal_temperature_ranges %>%
    dplyr::filter(Region == region_name) %>%
    dplyr::select(Lower_bound, Upper_bound)
  
  # Check if region exists in dataset
  if (nrow(range) == 0) {
    stop(paste0("Region '", region_name, "' not found in dataset."))
  }
  
  return(range)
}