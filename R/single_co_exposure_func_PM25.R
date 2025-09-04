single_co_exposure_func_PM25 <- function(areaname, Y, inputdata) {
 
  req_funs <- c("get_temperature_range","Getexposuredataset_func","Get_co_exposure_data_func",
                "Baselinedataset_func","remove_lagdays_func","Model_func",
                "forestdataframe_func","compute_interaction_effects","forest","forest_theme")
  miss <- req_funs[!vapply(req_funs, function(f) exists(f, mode="function"), logical(1))]
  if (length(miss)) stop("Missing required functions: ", paste(miss, collapse=", "))
  
  
  if (!exists("lagdays", inherits = TRUE)) {
    stop("`lagdays` not found. Please define `lagdays` in the calling environment.")
  }
  lagdays_local <- get("lagdays", inherits = TRUE)
  
  
  p_code <- function(p) {
    if (isTRUE(abs(p - 0.9)  < 1e-8))  return("90")
    if (isTRUE(abs(p - 0.95) < 1e-8))  return("95")
    if (isTRUE(abs(p - 0.975) < 1e-8)) return("975")
    stop("Unsupported percentile: ", p)
  }
  
 
  tr <- get_temperature_range(areaname)
  lowest_temperature  <- tr$Lower_bound
  highest_temperature <- tr$Upper_bound
  
  originaldatabase <- inputdata %>%
    dplyr::filter(area == areaname) %>%
    dplyr::mutate(
      year   = as.factor(format(date, "%Y")),
      dow    = as.factor(weekdays(date)),
      month  = lubridate::month(date),
      season = dplyr::case_when(
        month %in% c(6, 7, 8)   ~ "JJA",
        month %in% c(9, 10, 11) ~ "SON",
        month %in% c(12, 1, 2)  ~ "DJF",
        month %in% c(3, 4, 5)   ~ "MAM"
      ),
      Cir_Res_death = CauseDiseases.of.the.circulatory.system + CauseDiseases.of.the.respiratory.system
    ) %>%
    dplyr::arrange(date) %>%
    dplyr::select(
      date, totdeath, population_weighted_pm2.5, tasmax, year,
      Sex1, Sex2,
      Age65.74, Age75.84, Age85.and.over, AgeUnder.65,
      CauseDiseases.of.the.circulatory.system, CauseDiseases.of.the.respiratory.system,
      CauseOther.causes, Cir_Res_death, RH, month, dow, holiday, season
    )
  
 
  types <- c("Temperature", "PM2.5")
  pcts  <- c(0.90, 0.95, 0.975)
  
  make_single_key <- function(type, p) {
    paste0(ifelse(type == "PM2.5", "PM", "Tem"), "_exposure_data_", p_code(p))
  }
  
  single_list <- purrr::map(types, function(tp) {
    purrr::set_names(
      purrr::map(pcts, ~ Getexposuredataset_func(
        originaldatabase = originaldatabase,
        exposuretype = tp,
        exposure_percentiles = .x
      )),
      nm = vapply(pcts, function(px) make_single_key(tp, px), character(1))
    )
  })
  single_list <- do.call(c, single_list)  
  
  
  pm_q <- function(p) originaldatabase %>% dplyr::group_by(year) %>%
    dplyr::summarise(pm25_threshold = stats::quantile(population_weighted_pm2.5, p, na.rm = TRUE), .groups = "drop")
  tem_q <- function(p) originaldatabase %>% dplyr::group_by(year) %>%
    dplyr::summarise(Tem_threshold = stats::quantile(tasmax, p, na.rm = TRUE), .groups = "drop")
  
  pm25_quantiles_25  <- pm_q(0.25)
  pm25_quantiles_90  <- pm_q(0.90)
  pm25_quantiles_95  <- pm_q(0.95)
  pm25_quantiles_975 <- pm_q(0.975)
  
  Tem_quantiles_90   <- tem_q(0.90)
  Tem_quantiles_95   <- tem_q(0.95)
  Tem_quantiles_975  <- tem_q(0.975)
  
 
  make_co_key <- function(pT, pP) {
    paste0("co_expuredata_", p_code(pT), "_tem_", p_code(pP), "_pm2.5")
  }
  grid_co <- tidyr::expand_grid(pT = pcts, pP = pcts)
  co_list <- purrr::pmap(
    list(grid_co$pT, grid_co$pP),
    function(pT, pP) {
      Get_co_exposure_data_func(
        ExposureTem = single_list[[make_single_key("Temperature", pT)]],
        ExposurePM  = single_list[[make_single_key("PM2.5",      pP)]]
      )
    }
  )
  names(co_list) <- purrr::pmap_chr(list(grid_co$pT, grid_co$pP), make_co_key)
  
 
  Baselinedata <- Baselinedataset_func(
    originaldatabase = originaldatabase,
    MinTem = lowest_temperature,
    MaxTem = highest_temperature,
    PM_threshold = 0.25,
    WHO = FALSE
  )
  
  all_exp_list <- c(single_list, co_list)
  clean_baseline_list <- purrr::imap(all_exp_list, ~ remove_lagdays_func(
    exposuredata = .x,
    baselinedata = Baselinedata,
    lagdays      = lagdays_local
  ))
  
  names(clean_baseline_list) <- paste0("Baselinedata_cleaned_", names(all_exp_list))
  
 
  drop_same <- function(p_label) {
    co_key <- paste0("co_expuredata_", p_label, "_tem_", p_label, "_pm2.5")
    tem_key <- paste0("Tem_exposure_data_", p_label)
    pm_key  <- paste0("PM_exposure_data_",  p_label)
    if (!all(c(co_key, tem_key, pm_key) %in% names(c(co_list, single_list)))) {
      stop("Missing exposure objects for label ", p_label)
    }
    dtr <- co_list[[co_key]]$date
    single_list[[tem_key]] <<- dplyr::filter(single_list[[tem_key]], !(date %in% dtr))
    single_list[[pm_key]]  <<- dplyr::filter(single_list[[pm_key]],  !(date %in% dtr))
  }
  purrr::walk(c("975", "95", "90"), drop_same)
  
 
  myY <- Y
  process_data <- function(exposuredata, Baselinedata, variable, percentile) {
    Model_func(
      Y = myY, lagdays = lagdays_local,
      exposuredata = exposuredata,
      Baselinedata = Baselinedata,
      originaldatabase = originaldatabase
    ) %>% dplyr::mutate(variable = variable, percentile = percentile)
  }
  
 
  params_single <- list(
    list(exposuredata = single_list[["Tem_exposure_data_975"]], Baselinedata = clean_baseline_list[["Baselinedata_cleaned_Tem_exposure_data_975"]], variable = "Extreme Tmax only", percentile = "97.5 percentile"),
    list(exposuredata = single_list[["Tem_exposure_data_95"]],  Baselinedata = clean_baseline_list[["Baselinedata_cleaned_Tem_exposure_data_95"]],  variable = "Extreme Tmax only", percentile = "95 percentile"),
    list(exposuredata = single_list[["Tem_exposure_data_90"]],  Baselinedata = clean_baseline_list[["Baselinedata_cleaned_Tem_exposure_data_90"]],  variable = "Extreme Tmax only", percentile = "90 percentile"),
    list(exposuredata = single_list[["PM_exposure_data_975"]],  Baselinedata = clean_baseline_list[["Baselinedata_cleaned_PM_exposure_data_975"]],  variable = "Extreme PM2.5 only", percentile = "97.5 percentile"),
    list(exposuredata = single_list[["PM_exposure_data_95"]],   Baselinedata = clean_baseline_list[["Baselinedata_cleaned_PM_exposure_data_95"]],   variable = "Extreme PM2.5 only", percentile = "95 percentile"),
    list(exposuredata = single_list[["PM_exposure_data_90"]],   Baselinedata = clean_baseline_list[["Baselinedata_cleaned_PM_exposure_data_90"]],   variable = "Extreme PM2.5 only", percentile = "90 percentile"),
    list(exposuredata = co_list[["co_expuredata_975_tem_975_pm2.5"]], Baselinedata = clean_baseline_list[["Baselinedata_cleaned_co_expuredata_975_tem_975_pm2.5"]], variable = "Extreme Tmax and PM2.5", percentile = "97.5 percentile"),
    list(exposuredata = co_list[["co_expuredata_95_tem_95_pm2.5"]],   Baselinedata = clean_baseline_list[["Baselinedata_cleaned_co_expuredata_95_tem_95_pm2.5"]],   variable = "Extreme Tmax and PM2.5", percentile = "95 percentile"),
    list(exposuredata = co_list[["co_expuredata_90_tem_90_pm2.5"]],   Baselinedata = clean_baseline_list[["Baselinedata_cleaned_co_expuredata_90_tem_90_pm2.5"]],   variable = "Extreme Tmax and PM2.5", percentile = "90 percentile")
  )
  
  finalresult_single <- purrr::map_dfr(params_single, ~ process_data(.x$exposuredata, .x$Baselinedata, .x$variable, .x$percentile))
  col_order <- c("variable","lag","estimate","coefficients","SE","p_value","upper","lower","AF","AF_upper","AF_lower","variance_first_param","percentile")
  finalresult_single <- finalresult_single[, col_order]
  finalresult_single$percentile <- factor(finalresult_single$percentile, levels = c("97.5 percentile","95 percentile","90 percentile"))
  finalresult_single <- finalresult_single %>% dplyr::arrange(percentile)
  
  
  params_co <- list(
    list(exposuredata = co_list[["co_expuredata_975_tem_975_pm2.5"]], Baselinedata = clean_baseline_list[["Baselinedata_cleaned_co_expuredata_975_tem_975_pm2.5"]], variable = "97.5 percentile Tmax", percentile = "97.5 percentile PM2.5"),
    list(exposuredata = co_list[["co_expuredata_975_tem_95_pm2.5"]],  Baselinedata = clean_baseline_list[["Baselinedata_cleaned_co_expuredata_975_tem_95_pm2.5"]],  variable = "97.5 percentile Tmax", percentile = "95 percentile PM2.5"),
    list(exposuredata = co_list[["co_expuredata_975_tem_90_pm2.5"]],  Baselinedata = clean_baseline_list[["Baselinedata_cleaned_co_expuredata_975_tem_90_pm2.5"]],  variable = "97.5 percentile Tmax", percentile = "90 percentile PM2.5"),
    list(exposuredata = co_list[["co_expuredata_95_tem_975_pm2.5"]],  Baselinedata = clean_baseline_list[["Baselinedata_cleaned_co_expuredata_95_tem_975_pm2.5"]],  variable = "95 percentile Tmax",  percentile = "97.5 percentile PM2.5"),
    list(exposuredata = co_list[["co_expuredata_95_tem_95_pm2.5"]],   Baselinedata = clean_baseline_list[["Baselinedata_cleaned_co_expuredata_95_tem_95_pm2.5"]],   variable = "95 percentile Tmax",  percentile = "95 percentile PM2.5"),
    list(exposuredata = co_list[["co_expuredata_95_tem_90_pm2.5"]],   Baselinedata = clean_baseline_list[["Baselinedata_cleaned_co_expuredata_95_tem_90_pm2.5"]],   variable = "95 percentile Tmax",  percentile = "90 percentile PM2.5"),
    list(exposuredata = co_list[["co_expuredata_90_tem_975_pm2.5"]],  Baselinedata = clean_baseline_list[["Baselinedata_cleaned_co_expuredata_90_tem_975_pm2.5"]],  variable = "90 percentile Tmax",  percentile = "97.5 percentile PM2.5"),
    list(exposuredata = co_list[["co_expuredata_90_tem_95_pm2.5"]],   Baselinedata = clean_baseline_list[["Baselinedata_cleaned_co_expuredata_90_tem_95_pm2.5"]],   variable = "90 percentile Tmax",  percentile = "95 percentile PM2.5"),
    list(exposuredata = co_list[["co_expuredata_90_tem_90_pm2.5"]],   Baselinedata = clean_baseline_list[["Baselinedata_cleaned_co_expuredata_90_tem_90_pm2.5"]],   variable = "90 percentile Tmax",  percentile = "90 percentile PM2.5")
  )
  
  finalresult_co <- purrr::map_dfr(params_co, ~ process_data(.x$exposuredata, .x$Baselinedata, .x$variable, .x$percentile))
  finalresult_co <- finalresult_co[, col_order]
  finalresult_co$percentile <- factor(finalresult_co$percentile, levels = c("97.5 percentile PM2.5","95 percentile PM2.5","90 percentile PM2.5"))
  finalresult_co <- finalresult_co %>% dplyr::arrange(percentile)
  
 
  ci_cols_add <- function(df) {
    df$Ci1 <- paste(sprintf("%.3f (%.3f, %.3f)", df$est1, df$low1, df$upp1)); df$Ci1[grepl("NA", df$Ci1)] <- ""
    df$Ci2 <- paste(sprintf("%.3f (%.3f, %.3f)", df$est2, df$low2, df$upp2)); df$Ci2[grepl("NA", df$Ci2)] <- ""
    df$Ci3 <- paste(sprintf("%.3f (%.3f, %.3f)", df$est3, df$low3, df$upp3)); df$Ci3[grepl("NA", df$Ci3)] <- ""
    df
  }
  rng_x <- function(df) {
    xmin <- min(df[, c("low1","low2","low3")], na.rm = TRUE) * 0.95
    xmax <- max(df[, c("upp1","upp2","upp3")], na.rm = TRUE) * 1.05
    c(xmin, xmax)
  }
  tm_theme <- forest_theme(
    base_size = 12,
    refline_lty = "dashed",
    ci_pch = 20,
    ci_col = c("#d6604d"),
    ci_lwd = 2,
    ci_lty = 1,
    vertline_lty = "dashed",
    vertline_col = "#bababa",
    core = list(padding = grid::unit(c(4,3), "mm"))
  )
  
  #
  testdf <- forestdataframe_func(finalresult_single, lagdays_local, type = "Single") %>%
    ci_cols_add()
  testdf$`Percentile 97.5` <- paste(rep(" ", 15), collapse = " ")
  testdf$`Percentile 95`   <- paste(rep(" ", 15), collapse = " ")
  testdf$`Percentile 90`   <- paste(rep(" ", 15), collapse = " ")
  testdf <- testdf %>% dplyr::rename(` ` = group, `  RR (95% CI)` = Ci1, `  RR (95% CI) ` = Ci2, `  RR (95% CI)  ` = Ci3)
  p_single <- forest(
    testdf[, c(1,2,3,4,5,6,7)],
    est   = list(testdf$est1, testdf$est2, testdf$est3),
    lower = list(testdf$low1, testdf$low2, testdf$low3),
    upper = list(testdf$upp1, testdf$upp2, testdf$upp3),
    ci_column = c(2,4,6),
    ref_line = 1,
    xlim = rng_x(testdf),
    ticks_at = c(1, 1.5),
    sizes = 0.6,
    xlab = "Relative Risk",
    theme = tm_theme
  )
  print("P1 is finished")
  
  
  testdf <- forestdataframe_func(finalresult_co, lagdays_local, type = "Co") %>%
    dplyr::mutate(group = dplyr::case_when(
      group == "97.5 percentile Tmax" ~ "Percentile 97.5 Tmax",
      group == "95 percentile Tmax"   ~ "Percentile 95 Tmax",
      group == "90 percentile Tmax"   ~ "Percentile 90 Tmax",
      TRUE ~ group
    )) %>%
    ci_cols_add()
  testdf$`Percentile 97.5 PM2.5` <- paste(rep(" ", 15), collapse = " ")
  testdf$`Percentile 95 PM2.5`   <- paste(rep(" ", 15), collapse = " ")
  testdf$`Percentile 90 PM2.5`   <- paste(rep(" ", 15), collapse = " ")
  testdf <- testdf %>% dplyr::rename(` ` = group, `  RR (95% CI)` = Ci1, `  RR (95% CI) ` = Ci2, `  RR (95% CI)  ` = Ci3)
  p_Co <- forest(
    testdf[, c(1,2,3,4,5,6,7)],
    est   = list(testdf$est1, testdf$est2, testdf$est3),
    lower = list(testdf$low1, testdf$low2, testdf$low3),
    upper = list(testdf$upp1, testdf$upp2, testdf$upp3),
    ci_column = c(2,4,6),
    ref_line = 1,
    xlim = rng_x(testdf),
    ticks_at = c(1, 1.5),
    sizes = 0.6,
    xlab = "Relative Risk",
    theme = tm_theme
  )
  print("P2 is finished")
  
 
  Interactive_result <- compute_interaction_effects(finalresult_single, finalresult_co)
  unique_temperatures <- unique(Interactive_result$Temperature)
  unique_PM2.5        <- unique(Interactive_result$`PM2.5` %||% Interactive_result$PM2.5)  # 兼容列名变体
  if (is.null(unique_PM2.5)) stop("`compute_interaction_effects` output missing PM2.5 column.")
  
  full_combinations <- tidyr::expand_grid(
    Temperature = unique_temperatures,
    `PM2.5`     = unique_PM2.5,
    lag         = paste0("lag", 0:5)
  )
  Interactive_result <- full_combinations %>%
    dplyr::left_join(Interactive_result, by = c("Temperature","PM2.5","lag")) %>%
    dplyr::arrange(dplyr::desc(Temperature))
  Interactive_result_output <- Interactive_result
  
  Interactive_result <- Interactive_result %>%
    dplyr::select(lag, Temperature, `PM2.5`, RERI, CI_lower, CI_upper) %>%
    dplyr::rename(variable = Temperature, estimate = RERI, upper = CI_upper, lower = CI_lower, percentile = `PM2.5`) %>%
    dplyr::arrange(dplyr::desc(percentile))
  
  Interactive_result <- forestdataframe_func(Interactive_result, lagdays_local, type = "Co") %>%
    dplyr::mutate(group = dplyr::case_when(
      group == "97.5 percentile Tmax" ~ "Percentile 97.5 Tmax",
      group == "95 percentile Tmax"   ~ "Percentile 95 Tmax",
      group == "90 percentile Tmax"   ~ "Percentile 90 Tmax",
      TRUE ~ group
    )) %>%
    dplyr::filter(!group %in% c("   lag4","   lag5"))
  
  Interactive_result$Ci1 <- paste(sprintf("%.3f (%.3f, %.3f)", Interactive_result$est1, Interactive_result$low1, Interactive_result$upp1)); Interactive_result$Ci1[grepl("NA", Interactive_result$Ci1)] <- ""
  Interactive_result$Ci2 <- paste(sprintf("%.3f (%.3f, %.3f)", Interactive_result$est2, Interactive_result$low2, Interactive_result$upp2)); Interactive_result$Ci2[grepl("NA", Interactive_result$Ci2)] <- ""
  Interactive_result$Ci3 <- paste(sprintf("%.3f (%.3f, %.3f)", Interactive_result$est3, Interactive_result$low3, Interactive_result$upp3)); Interactive_result$Ci3[grepl("NA", Interactive_result$Ci3)] <- ""
  Interactive_result$`Percentile 97.5 PM2.5` <- paste(rep(" ", 15), collapse = " ")
  Interactive_result$`Percentile 95 PM2.5`   <- paste(rep(" ", 15), collapse = " ")
  Interactive_result$`Percentile 90 PM2.5`   <- paste(rep(" ", 15), collapse = " ")
  Interactive_result <- Interactive_result %>% dplyr::rename(` ` = group, `  RERI (95% CI)` = Ci1, `  RERI (95% CI) ` = Ci2, `  RERI (95% CI)  ` = Ci3)
  
  xr <- {
    xmin <- min(Interactive_result[, c("low1","low2","low3")], na.rm = TRUE) * 0.95
    xmax <- max(Interactive_result[, c("upp1","upp2","upp3")], na.rm = TRUE) * 1.05
    xmin <- ifelse(xmin < 0, xmin - 0.1, xmin)
    xmax <- ifelse(xmax < 0, xmax - 0.1, xmax)
    c(xmin, xmax)
  }
  Inter_plot <- forest(
    Interactive_result[, c(1,2,3,4,5,6,7)],
    est   = list(Interactive_result$est1, Interactive_result$est2, Interactive_result$est3),
    lower = list(Interactive_result$low1, Interactive_result$low2, Interactive_result$low3),
    upper = list(Interactive_result$upp1, Interactive_result$upp2, Interactive_result$upp3),
    ci_column = c(2,4,6),
    ref_line = 0,
    xlim = xr,
    ticks_at = c(0, 0.5),
    sizes = 0.6,
    xlab = "RERI",
    theme = tm_theme
  )
  
 
  res <- list(
    area = areaname,
    Y = Y,
    Single_plot = p_single,
    Co_plot = p_Co,
    Single_data = finalresult_single,
    Co_data = finalresult_co,
    Baselinedata = Baselinedata,
    Baselinedata_cleaned_975_tem_975_PM = clean_baseline_list[["Baselinedata_cleaned_co_expuredata_975_tem_975_pm2.5"]],
    Co_exposure_days_P975 = co_list[["co_expuredata_975_tem_975_pm2.5"]],
    Co_exposure_days_P95  = co_list[["co_expuredata_95_tem_95_pm2.5"]],
    Co_exposure_days_temP975_PMP90 = co_list[["co_expuredata_975_tem_90_pm2.5"]],
    Co_exposure_days_temP975_PMP95 = co_list[["co_expuredata_975_tem_95_pm2.5"]],
    PM25_P90  = pm25_quantiles_90,
    PM25_P95  = pm25_quantiles_95,
    PM25_P975 = pm25_quantiles_975,
    PM25_P25  = pm25_quantiles_25,
    Tem_P90   = Tem_quantiles_90,
    Tem_P95   = Tem_quantiles_95,
    Tem_P975  = Tem_quantiles_975,
    Interactive_result = Interactive_result_output,
    Inter_plot = Inter_plot
  )
  return(res)
}
