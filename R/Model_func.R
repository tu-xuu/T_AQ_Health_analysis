############################################################
# Function: Model_func
# Description: For each lag (0..lagdays), fit a stratified
#              quasi-Poisson model (via gnm) with exposure as
#              the predictor, extracting RR, CI, AF and variance.
# Author: Chengxu Tong
# Date: 2025
# Arguments:
#   y                    - character; outcome column name (e.g., "deaths")
#   lagdays              - integer; maximum lag (inclusive)
#   exposuredata         - data.frame; must contain "date"
#   baselinedata_cleaned - data.frame; baseline days (no lag overlap)
#   originaldatabase     - data.frame; full dataset; must contain:
#                          date, year, season, dow, RH, holiday, and y
# Returns:
#   A data.frame with one row per lag containing:
#   lag_index, lag_label, estimate (RR), lower, upper, coef, SE,
#   p_value, AF, AF_lower, AF_upper, var_exposure
# Example:
#   fit_lagged_quasipoisson(
#     y = "deaths",
#     lagdays = 3,
#     exposuredata = exp_days,
#     baselinedata_cleaned = base_clean,
#     originaldatabase = full_df
#   )
############################################################
Model_func <- function(Y, lagdays, exposuredata, Baselinedata, originaldatabase){
 
  if (!is.character(Y) || length(Y) < 1) {
    stop("`Y` must be a character vector with at least one outcome name.")
  }
  myY <- Y
  if (!myY[1] %in% names(originaldatabase)) {
    stop(sprintf("`originaldatabase` must contain the outcome column '%s'.", myY[1]))
  }
  req_cols <- c("date", "year", "season", "dow", "RH", "holiday")
  missing_main <- setdiff(req_cols, names(originaldatabase))
  if (length(missing_main) > 0) {
    stop(sprintf("`originaldatabase` is missing required columns: %s", paste(missing_main, collapse = ", ")))
  }
  if (!"date" %in% names(exposuredata)) {
    stop("`exposuredata` must contain a 'date' column.")
  }
  if (!"date" %in% names(Baselinedata)) {
    stop("`Baselinedata` must contain a 'date' column.")
  }
  if (!is.numeric(lagdays) || length(lagdays) != 1 || lagdays < 0) {
    stop("`lagdays` must be a single non-negative integer.")
  }
  if (!exists("Getmodeldataset_func", mode = "function")) {
    stop("`Getmodeldataset_func` not found. Please define it before calling `Model_func`.")
  }
  
 
  data_exp <- data.frame(
    estimate = rep(0, lagdays + 1),
    upper    = rep(0, lagdays + 1),
    lower    = rep(0, lagdays + 1)
  )
  
  for (i in 0:lagdays) {
  
    model_data <- Getmodeldataset_func(
      exposuredata = exposuredata,
      baselinedata = Baselinedata,
      originaldatabase = originaldatabase,
      lagdays = i
    )
    
   
    need2 <- c(myY[1], "year", "season", "dow", "RH", "holiday", "exposure")
    miss2 <- setdiff(need2, names(model_data))
    if (length(miss2) > 0) {
      stop(sprintf("`model_data` is missing required columns: %s", paste(miss2, collapse = ", ")))
    }
    
   
    model_data$stratum <- as.factor(paste(model_data$year, model_data$season, model_data$dow, sep = ":"))
    keep_cols <- c(myY, "stratum", "exposure", "RH", "holiday")
    model_data <- model_data[, keep_cols]
    
  
    fml <- stats::as.formula(paste0(myY[1], " ~ exposure+splines::ns(RH,3)+holiday"))
    
   
    model_cpoisson <- try(
      gnm::gnm(
        fml,
        data = model_data,
        family = stats::quasipoisson(),
        eliminate = factor(model_data$stratum)
      ),
      silent = TRUE
    )
    if (inherits(model_cpoisson, "try-error")) {
      stop(sprintf("`gnm` failed to fit at lag %d: %s", i, as.character(model_cpoisson)))
    }
    
    summary_output <- summary(model_cpoisson)
    vcov_matrix    <- try(stats::vcov(model_cpoisson), silent = TRUE)
    if (inherits(vcov_matrix, "try-error")) {
      stop(sprintf("`vcov` failed at lag %d.", i))
    }
    
   
    ci_raw <- try(suppressWarnings(stats::confint(model_cpoisson, parm = "exposure", level = 0.95)), silent = TRUE)
    
   
    coefs_named <- try(stats::coef(model_cpoisson), silent = TRUE)
    if (inherits(coefs_named, "try-error")) {
      stop(sprintf("`coef` failed at lag %d.", i))
    }
    if ("exposure" %in% names(coefs_named)) {
      beta <- as.numeric(coefs_named[["exposure"]])
    } else {
      
      beta <- as.numeric(coefs_named[1])
    }
    
    sm <- summary_output$coefficients
    
    se_col <- intersect(colnames(sm), c("Std. Error", "Std Error", "Std.Error"))
    p_col  <- intersect(colnames(sm), c("Pr(>|t|)", "Pr(>t)", "Pr(>|z|)", "Pr(>z)"))
    if (length(se_col) == 0) stop(sprintf("Cannot find Std. Error in summary at lag %d.", i))
    if (length(p_col)  == 0) stop(sprintf("Cannot find p-value column in summary at lag %d.", i))
    
    if ("exposure" %in% rownames(sm)) {
      SE_val <- as.numeric(sm["exposure", se_col[1]])
      p_val  <- as.numeric(sm["exposure", p_col[1]])
    } else {
     
      SE_val <- as.numeric(sm[1, se_col[1]])
      p_val  <- as.numeric(sm[1, p_col[1]])
    }
    
   
    if (!inherits(ci_raw, "try-error")) {
     
      ci_num <- as.numeric(ci_raw)
      if (length(ci_num) < 2) {
      
        z <- 1.96
        ci_num <- c(beta - z * SE_val, beta + z * SE_val)
      }
    } else {
      z <- 1.96
      ci_num <- c(beta - z * SE_val, beta + z * SE_val)
    }
    a <- exp(ci_num)  
    
  
    data_exp[i + 1, "estimate"]    <- exp(beta)
    data_exp[i + 1, "coefficients"] <- beta
    data_exp[i + 1, "SE"]          <- SE_val
    data_exp[i + 1, "p_value"]     <- p_val
    data_exp[i + 1, "upper"]       <- a[2]
    data_exp[i + 1, "lower"]       <- a[1]
    data_exp[i + 1, "AF"]          <- (exp(beta) - 1) / exp(beta)
   
    data_exp[i + 1, "AF_upper"]    <- (a[1] - 1) / a[1]
    data_exp[i + 1, "AF_lower"]    <- (a[2] - 1) / a[2]
    
   
    if ("exposure" %in% rownames(vcov_matrix) && "exposure" %in% colnames(vcov_matrix)) {
      data_exp[i + 1, "variance_first_param"] <- as.numeric(vcov_matrix["exposure", "exposure"])
    } else {
      data_exp[i + 1, "variance_first_param"] <- as.numeric(vcov_matrix[1, 1])
    }
  }
  
  data_exp$lag <- paste0("lag", 0:lagdays)
  return(data_exp)
}
