############################################################
# Function: Get_co_exposure_data_func
# Description: Identify days where both temperature and PM2.5 
#              exposures occur (co-exposure days).
# Author: Chengxu Tong
# Date: 2025
# Arguments:
#   ExposureTem - data frame; must contain column "date" 
#                 (temperature exposure days)
#   ExposurePM  - data frame; must contain column "date" 
#                 (PM2.5 exposure days)
# Returns:
#   A data frame of days where both exposures occur.
# Example:
#   get_co_exposure_data(ExposureTem, ExposurePM)
############################################################

Get_co_exposure_data_func <- function(ExposureTem, ExposurePM) {
  
  # Input validation
  if (!"date" %in% names(ExposureTem) || !"date" %in% names(ExposurePM)) {
    stop("Both `ExposureTem` and `ExposurePM` must contain a 'date' column.")
  }
  
  # Find common dates
  common_dates <- intersect(ExposureTem$date, ExposurePM$date)
  
  # Subset to co-exposure days
  common_days <- ExposureTem %>%
    dplyr::filter(date %in% as.Date(common_dates))
  
  return(common_days)
}