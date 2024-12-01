# Create the new test_cRIR function
test_cRIR <- function(est_eff, std_err, n_obs, n_covariates = 1, 
                      # sdx = NA, sdy = NA, 
                      R2 = NA, alpha = 0.05, tails = 2, to_return = "print") {
    # calculating critical r
    if (est_eff < 0) {
        critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 2) * -1
    } else {
        critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 2)
    }
    critical_r <- critical_t / sqrt((critical_t^2) + (n_obs - n_covariates - 2))
    
    ## calculate ryxGz or observed r
    ryxGz <- (est_eff / std_err) / sqrt(((n_obs - n_covariates - 2) + ((est_eff / std_err)^2)))
    
    # calculate cond_RIRpi_rxyz by default 
    if (abs(ryxGz) > abs(critical_r)) {
        ## this is equal to regular correlation based RIR 
        cond_RIRpi_rxyz <- (1 - critical_r/ryxGz) * 100
        cond_RIR_rxyz <- cond_RIRpi_rxyz * n_obs / 100
    } else if (abs(ryxGz) < abs(critical_r)) {
        cond_RIRpi_rxyz <- (1 - ryxGz/critical_r) * 100
        cond_RIR_rxyz <- cond_RIRpi_rxyz * n_obs / 100
    }
    
    # If sdx, sdy, and R2 are provided, calculate other conditional RIR measures
    cond_RIRpi_fixedY <- NA
    cond_RIRpi_null <- NA
    cond_RIR_fixedY <- NA
    cond_RIR_null <- NA
    
    if (!is.na(R2) & abs(ryxGz) > abs(critical_r)) {
        ryz <- rzy <- cal_ryz(ryxGz, R2)
        cond_RIRpi_fixedY <- (R2 - ryz^2 + ryz^2 * critical_r^2 - critical_r^2) / 
            (R2 - ryz^2 + ryz^2 * critical_r^2) * 100
        cond_RIR_fixedY <- cond_RIRpi_fixedY * n_obs / 100
        cond_RIRpi_null <- (1 - sqrt(critical_r^2 / 
                                         (R2 - ryz^2 + ryz^2 * critical_r^2))) * 100
        cond_RIR_null <- cond_RIRpi_null * n_obs / 100
    }
  
  # Return results based on user's preference
  if (to_return == "print") {
    cat(crayon::bold("Robustness of Inference to Replacement (RIR):\n"))
    cat("This application of RIR quantifies how much data we expect to have to replace when the properties of the replacement data are specified as conditional on other terms already in the model.")  
    cat(sprintf("If the replacement data points satisfy rxy|Z = 0 (the correlation between the focal predictor and the outcome conditional on the covariates), then RIR = %.3f.\n", cond_RIR_rxyz))
    if (!is.na(cond_RIR_fixedY)) cat(sprintf("If the replacement data points have a fixed or constant value, then RIR = %.3f.\n", cond_RIR_fixedY))
    if (!is.na(cond_RIR_null)) cat(sprintf("If the replacement data points follow a null distribution, then RIR = %.3f.\n", cond_RIR_null))
    if (!is.na(R2) & abs(ryxGz) < abs(critical_r)) cat(sprintf("When the inference is not significant, the RIR calculation is under development for scenarios where the replacement data points either have a fixed or constant value or follow a null distribution.")) 
    } else if (to_return == "raw_output") {
          return(output_list(obs_r = NA, act_r = NA,
                             # act_r only makes sense when nu!=0
                             critical_r = NA, r_final = NA,
                             # rxcv always be positive, rycv goes with itcv
                             rxcv = NA, rycv = NA,
                             rxcvGz = NA, rycvGz = NA,
                             itcvGz = NA, itcv = NA,
                             r2xz = NA, r2yz = NA,
                             delta_star = NA, delta_star_restricted = NA,
                             delta_exact = NA, delta_pctbias = NA,
                             cor_oster = NA, cor_exact = NA,
                             beta_threshold = NA,
                             beta_threshold_verify = NA,
                             perc_bias_to_change = NA,
                             RIR_primary = NA, RIR_supplemental = NA, RIR_perc = NA,
                             fragility_primary = NA, fragility_supplemental = NA,
                             starting_table = NA, final_table = NA,
                             user_SE = NA, analysis_SE = NA,
                             needtworows = NA, 
                             Fig_ITCV = NA,
                             Fig_RIR = NA,
                             cond_RIRpi_null = cond_RIRpi_null, 
                             cond_RIRpi_fixedY = cond_RIRpi_fixedY, 
                             cond_RIRpi_rxyz = cond_RIRpi_rxyz, 
                             cond_RIR_null = cond_RIR_null, 
                             cond_RIR_fixedY = cond_RIR_fixedY, 
                             cond_RIR_rxyz = cond_RIR_rxyz
                             ))
      }
}

## NOTE for print output
### general introduction to RIR 
### This application of RIR quantifies how much data to replace when the properties of the replacement data are specified as conditional on other terms already in the model.
## If the replacement data points have a fixed value, then RIR = %.3f.\n", cond_RIR_fixedY
## If the replacement data points follow a null distribution, then RIR = %.3f.\n", cond_RIR_null
## If the replacement data points satisfy rxy|Z = 0, then RIR = %.3f.\n", cond_RIR_rxyz
