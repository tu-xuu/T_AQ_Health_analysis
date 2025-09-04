############################################################
# Function: remove_lagdays_func
# Description: Remove lagged days (following exposure days) 
#              from the baseline dataset to avoid overlap.
# Author: Chengxu Tong
# Date: 2025
# Arguments:
#   exposuredata - data frame; must contain column "date"
#   baselinedata - data frame; must contain column "date"
#   lagdays      - integer; number of lag days to remove 
#                  (e.g., 3 means remove day+1, day+2, day+3)
# Returns:
#   A cleaned baseline dataset with lagged days removed.
# Example:
#   remove_lag_days(exposuredata, baselinedata, lagdays = 5)
############################################################

remove_lagdays_func <- function(exposuredata, baselinedata, lagdays) {
  
  # Input validation
  if (!"date" %in% names(exposuredata) || !"date" %in% names(baselinedata)) {
    stop("Both `exposuredata` and `baselinedata` must contain a 'date' column.")
  }
  if (!is.numeric(lagdays) || lagdays < 1) {
    stop("`lagdays` must be a positive integer.")
  }
  
  # Generate lagged dates following exposure days
  repeated_dates <- unlist(lapply(exposuredata$date, function(x) {
    seq(x + 1, by = "1 day", length.out = lagdays)
  }))
  
  # Remove lagged dates from baseline dataset
  baselinedata_cleaned <- baselinedata %>%
    dplyr::filter(!date %in% as.Date(repeated_dates))
  
  return(baselinedata_cleaned)
}