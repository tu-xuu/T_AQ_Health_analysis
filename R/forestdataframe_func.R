############################################################
# Function: forestdataframe_func
# Description: Construct a wide forest-plot-ready dataframe
#              from lagged estimates across percentiles.
# Author: Chengxu Tong
# Date: 2025
# Arguments:
#   MYdf    - data.frame; must contain columns:
#             variable, lag, percentile, estimate, upper, lower
#   lagdays - integer; maximum lag (inclusive). Rows will be
#             created for lag = 0..lagdays under each variable.
#   type    - character; "Single" or "Co"
#             ("Co" will label columns as '... PM2.5')
# Returns:
#   A data.frame with columns:
#     group, <Percentile label 1>, Ci1, <Percentile label 2>, Ci2,
#     <Percentile label 3>, Ci3, and est1/2/3 + low1/2/3 + upp1/2/3
# Notes:
#   - The function automatically detects the top 3 percentiles
#     present in MYdf (sorted descending numerically).
#   - It creates a header row per `variable` followed by lag rows
#     "   lag0", "   lag1", ...
# Example:
#   forest_df <- forestdataframe_func(results_df, lagdays = 3, type = "Single")
############################################################
forestdataframe_func=function(MYdf,lagdays,type){
  MYdf=MYdf
  testmethod=MYdf%>%dplyr::select(variable,lag,estimate,upper,lower)
  lagdays <- lagdays
  flag=lagdays+1
  # Unique values of the variable from MYdf dataframe
  unique_vals <- unique(testmethod$variable)
  
  # Initialize an empty vector to store the group values
  group_vals <- c()
  
  # Loop through each unique value and generate corresponding group values
  for (val in unique_vals) {
    # Add the unique value first
    group_vals <- c(group_vals, val)
    
    # Add suffixes based on lag days
    for (i in 0:lagdays) {
      group_vals <- c(group_vals, paste0("   lag", i))
    }
  }
  
  if (type=='Single') {  
    testdf <- data.frame(
    group = group_vals,
    `Percentile 97.5` = NA,
    `Ci1` = NA,
    `Percentile 95` = NA,
    `Ci2` = NA,
    `Percentile 90` = NA,
    `Ci3` = NA,
    est1 = NA,
    est2 = NA,
    est3 = NA,
    low1 = NA,
    low2 = NA,
    low3 = NA,
    upp1 = NA,
    upp2 = NA,
    upp3 = NA,
    check.names = FALSE
  )
    
  
    }else if (type=='Co') {
    testdf <- data.frame(
      group = group_vals,
      `Percentile 97.5 PM2.5` = NA,
      `Ci1` = NA,
      `Percentile 95 PM2.5` = NA,
      `Ci2` = NA,
      `Percentile 90 PM2.5` = NA,
      `Ci3` = NA,
      est1 = NA,
      est2 = NA,
      est3 = NA,
      low1 = NA,
      low2 = NA,
      low3 = NA,
      upp1 = NA,
      upp2 = NA,
      upp3 = NA,
      check.names = FALSE
    )
  }else{
    stop('Input data is wrong, check again')
  }
  # Create dataframe with group column


  
  # 假设 testdf 已经定义
  # 假设 testmethod 包含 estimate, upper 和 lower 这些列
  
  # 给 est1 的 2-(flag+1) 赋值为 testmethod 的 estimate 的 1-flag
  testdf$est1[2:(flag+1)] <- testmethod$estimate[1:flag]
  testdf$est2[2:(flag+1)] <- testmethod$estimate[(3*flag+1):(4*flag)]
  testdf$est3[2:(flag+1)] <- testmethod$estimate[(6*flag+1):(7*flag)]
  
  # 给 upp1 的 2-(flag+1) 赋值为 testmethod 的 upper 的 1-flag
  testdf$upp1[2:(flag+1)] <- testmethod$upper[1:flag]
  testdf$upp2[2:(flag+1)] <- testmethod$upper[(3*flag+1):(4*flag)]
  testdf$upp3[2:(flag+1)] <- testmethod$upper[(6*flag+1):(7*flag)]
  # 给 low1 的 2-(flag+1) 赋值为 testmethod 的 lower 的 1-flag
  testdf$low1[2:(flag+1)] <- testmethod$lower[1:flag]
  testdf$low2[2:(flag+1)] <- testmethod$lower[(3*flag+1):(4*flag)]
  testdf$low3[2:(flag+1)] <- testmethod$lower[(6*flag+1):(7*flag)]
  

  # 给 est1 的 (flag+3)-(2*flag+2) 赋值为 testmethod 的 estimate 的 (flag+1)-(2*flag)
  testdf$est1[(flag+3):(2*flag+2)] <- testmethod$estimate[(flag+1):(2*flag)]
  testdf$est2[(flag+3):(2*flag+2)] <- testmethod$estimate[(4*flag+1):(5*flag)]
  testdf$est3[(flag+3):(2*flag+2)] <- testmethod$estimate[(7*flag+1):(8*flag)]
  # 给 upp1 的 (flag+3)-(2*flag+2) 赋值为 testmethod 的 upper 的 (flag+1)-(2*flag)
  testdf$upp1[(flag+3):(2*flag+2)] <- testmethod$upper[(flag+1):(2*flag)]
  testdf$upp2[(flag+3):(2*flag+2)] <- testmethod$upper[(4*flag+1):(5*flag)]
  testdf$upp3[(flag+3):(2*flag+2)] <- testmethod$upper[(7*flag+1):(8*flag)]
  # 给 low1 的 (flag+3)-(2*flag+2) 赋值为 testmethod 的 lower 的 (flag+1)-(2*flag)
  testdf$low1[(flag+3):(2*flag+2)] <- testmethod$lower[(flag+1):(2*flag)]
  testdf$low2[(flag+3):(2*flag+2)] <- testmethod$lower[(4*flag+1):(5*flag)]
  testdf$low3[(flag+3):(2*flag+2)] <- testmethod$lower[(7*flag+1):(8*flag)]
  
  
  # 给 est1 的 (2*flag+4)-(3*flag+3) 赋值为 testmethod 的 estimate 的 (2*flag+1)-(3*flag)
  testdf$est1[(2*flag+4):(3*flag+3)] <- testmethod$estimate[(2*flag+1):(3*flag)]
  testdf$est2[(2*flag+4):(3*flag+3)] <- testmethod$estimate[(5*flag+1):(6*flag)]
  testdf$est3[(2*flag+4):(3*flag+3)] <- testmethod$estimate[(8*flag+1):(9*flag)]
  # 给 upp1 的 (2*flag+4)-(3*flag+3) 赋值为 testmethod 的 upper 的 (2*flag+1)-(3*flag)
  testdf$upp1[(2*flag+4):(3*flag+3)] <- testmethod$upper[(2*flag+1):(3*flag)]
  testdf$upp2[(2*flag+4):(3*flag+3)] <- testmethod$upper[(5*flag+1):(6*flag)]
  testdf$upp3[(2*flag+4):(3*flag+3)] <- testmethod$upper[(8*flag+1):(9*flag)]
  # 给 low1 的 (2*flag+4)-(3*flag+3) 赋值为 testmethod 的 lower 的 (2*flag+1)-(3*flag)
  testdf$low1[(2*flag+4):(3*flag+3)] <- testmethod$lower[(2*flag+1):(3*flag)]
  testdf$low2[(2*flag+4):(3*flag+3)] <- testmethod$lower[(5*flag+1):(6*flag)]
  testdf$low3[(2*flag+4):(3*flag+3)] <- testmethod$lower[(8*flag+1):(9*flag)]
  
  return(testdf)
}





