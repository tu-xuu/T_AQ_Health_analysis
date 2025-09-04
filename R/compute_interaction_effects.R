############################################################
# Function: compute_interaction_effects
# Description: Compute interaction effects (RERI) across
#              combinations of extreme Tmax and PM2.5 percentiles.
# Author: Chengxu Tong
# Date: 2025
# Requirements:
#   - Single_data must include rows for:
#       variable: "Extreme Tmax only" and "Extreme PM2.5 only"
#       columns:  percentile, lag, estimate, SE
#   - Co_data must include rows where:
#       variable equals "<percentile> Tmax" (e.g., "97.5 percentile Tmax")
#       percentile equals "<percentile> PM2.5" (e.g., "97.5 percentile PM2.5")
#       columns:  lag, estimate, SE
# Arguments:
#   Single_data - data.frame; single-exposure results by percentile and lag
#   Co_data     - data.frame; co-exposure results by paired percentiles and lag
# Returns:
#   A data.frame with columns:
#     lag, estimate_R10, estimate_R01, estimate_R11,
#     SE_R10, SE_R01, SE_R11, Temperature, PM2.5,
#     RERI, SE_RERI, CI_lower, CI_upper
# Notes:
#   - R10: Extreme Tmax only; R01: Extreme PM2.5 only; R11: Co-exposure
#   - RERI = RR11 - RR10 - RR01 + 1; SE via simple sum of variances
# Example:
#   out <- compute_interaction_effects(Single_data, Co_data)
############################################################

compute_interaction_effects <- function(Single_data, Co_data) {
  # ---- Input validation ----
  need_single <- c("variable", "percentile", "lag", "estimate", "SE")
  need_co     <- c("variable", "percentile", "lag", "estimate", "SE")
  if (!all(need_single %in% names(Single_data))) {
    stop("`Single_data` must contain columns: variable, percentile, lag, estimate, SE.")
  }
  if (!all(need_co %in% names(Co_data))) {
    stop("`Co_data` must contain columns: variable, percentile, lag, estimate, SE.")
  }
  
  percentiles_vars <- c("97.5 percentile", "95 percentile", "90 percentile")
  alpha <- 0.05
  z <- stats::qnorm(1 - alpha / 2)
  
  t_var_list  <- paste(percentiles_vars, "Tmax")
  pm_var_list <- paste(percentiles_vars, "PM2.5")
  
  first_non_na <- function(x) {
    y <- x[!is.na(x)]
    if (length(y) == 0) NA_real_ else y[1]
  }
  
  out_list <- list()
  
  for (t_var in t_var_list) {
    for (pm_var in pm_var_list) {
      t_pct  <- stringr::str_extract(t_var, ".*(?= Tmax)")
      pm_pct <- stringr::str_extract(pm_var, ".*(?= PM2\\.5)")
      
      mydata <- dplyr::bind_rows(
        Single_data %>% dplyr::filter(variable == "Extreme Tmax only",    percentile == t_pct),
        Single_data %>% dplyr::filter(variable == "Extreme PM2.5 only",   percentile == pm_pct),
        Co_data     %>% dplyr::filter(variable == t_var,                  percentile == pm_var)
      ) %>%
        dplyr::mutate(
          category = dplyr::case_when(
            variable == "Extreme Tmax only"  ~ "R10",
            variable == "Extreme PM2.5 only" ~ "R01",
            TRUE                             ~ "R11"
          )
        ) %>%
        tidyr::pivot_wider(
          names_from  = category,
          values_from = c(estimate, SE),
          names_sep   = "_"
        )
      
     
      needed_cols <- c("estimate_R10","estimate_R01","estimate_R11",
                       "SE_R10","SE_R01","SE_R11","lag")
      for (nm in needed_cols) {
        if (!nm %in% names(mydata)) {
          mydata[[nm]] <- NA_real_
        }
      }
      
      if (!is.character(mydata$lag)) {
        mydata$lag <- paste0("lag", mydata$lag)
      }
      
      mydata <- mydata %>%
        dplyr::group_by(lag) %>%
        dplyr::summarise(
          estimate_R10 = first_non_na(estimate_R10),
          estimate_R01 = first_non_na(estimate_R01),
          estimate_R11 = first_non_na(estimate_R11),
          SE_R10       = first_non_na(SE_R10),
          SE_R01       = first_non_na(SE_R01),
          SE_R11       = first_non_na(SE_R11),
          .groups      = "drop"
        ) %>%
        dplyr::arrange(lag) %>%
        dplyr::mutate(Temperature = t_var, `PM2.5` = pm_var)
      
      out_list[[paste(t_var, pm_var, sep = " | ")]] <- mydata
    }
  }
  
  Interaction_df <- dplyr::bind_rows(out_list) %>%
    dplyr::mutate(
      RERI    = estimate_R11 - estimate_R10 - estimate_R01 + 1,
      SE_RERI = sqrt(SE_R11^2 + SE_R10^2 + SE_R01^2),
      CI_lower = RERI - z * SE_RERI,
      CI_upper = RERI + z * SE_RERI
    )
  
  return(Interaction_df)
}