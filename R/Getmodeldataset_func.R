############################################################
# Function: Getmodeldataset_func
# Description: Construct a modeling dataset by combining 
#              baseline days (non-exposure) and lagged exposure 
#              days (exposure = 1).
# Author: Chengxu Tong
# Date: 2025
# Arguments:
#   exposuredata        - data frame; must contain column "date"
#   baselinedata_cleaned- data frame; baseline days without lagged dates
#   originaldatabase    - data frame; full dataset with "date"
#   lagdays             - integer; number of lag days to consider
# Returns:
#   A combined data frame with a new column "exposure" 
#   (0 = baseline, 1 = exposure).
# Example:
#   get_model_dataset(exposuredata, baselinedata_cleaned, 
#                     originaldatabase, lagdays = 3)
############################################################

Getmodeldataset_func<- function(exposuredata, baselinedata_cleaned, originaldatabase, lagdays) {
  
  # Input validation
  if (!"date" %in% names(exposuredata) || !"date" %in% names(originaldatabase)) {
    stop("`exposuredata` and `originaldatabase` must contain a 'date' column.")
  }
  if (!is.numeric(lagdays) || lagdays < 0) {
    stop("`lagdays` must be a non-negative integer.")
  }
  
  # Identify lagged dates after exposure
  next_day_dates <- as.Date(exposuredata$date) + lagdays
  
  # Subset original data for lagged exposure days
  Lagdataset <- originaldatabase %>%
    dplyr::filter(date %in% next_day_dates)
  
  # Combine baseline (exposure = 0) and lagged exposure (exposure = 1)
  Mydataset <- dplyr::bind_rows(
    baselinedata_cleaned %>% dplyr::mutate(exposure = 0),
    Lagdataset %>% dplyr::mutate(exposure = 1)
  )
  
  return(Mydataset)
}