############################################################
# Description: Background dataset of optimal temperature 
#              ranges by UK region.
# Author: Chengxu Tong
# Date: 2025
# Reference: Climate-related mortality, England and Wales: 1988 to 2022, Office for National Statistics (ONS). 
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/articles/climaterelatedmortalityandhospitaladmissionsenglandandwales/1988to2022#:~:text=In%20England%2C%20the%20lowest%20mortality,the%20relatively%20low%20mortality%20risk.
############################################################

optimal_temperature_ranges <- data.frame(
  Region = c("East of England", "East Midlands", "London", "North East", 
             "North West", "South East", "South West", "West Midlands", 
             "Yorkshire and The Humber", "Wales"),
  Lower_bound = c(12, 7, 13, 9, 8, 10, 9, 8, 6, 8),
  Upper_bound = c(22, 21, 22, 19, 21, 22, 21, 21, 21, 20)
)