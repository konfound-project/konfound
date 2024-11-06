# Create the new test_cRIR function
test_cRIR <- function(est_eff, std_err, n_obs, n_covariates = 1, sdx = NA, sdy = NA, R2 = NA, alpha = 0.05, tails = 2, to_return = "print") {
  # Calculate cond_RIRpi_null by default
  cond_RIRpi_null <- est_eff / (std_err * sqrt(n_obs)) * 100
  cond_RIR_null <- cond_RIRpi_null * n_obs / 100
  
  # If sdx, sdy, and R2 are provided, calculate other conditional RIR measures
  cond_RIRpi_fixedY <- NA
  cond_RIRpi_rxyz <- NA
  cond_RIR_fixedY <- NA
  cond_RIR_rxyz <- NA
  
  
  if (!is.na(sdx) && !is.na(sdy) && !is.na(R2)) {
    cond_RIRpi_fixedY <- (est_eff / sdx) * (1 - R2) * 100
    cond_RIRpi_rxyz <- (est_eff / (sdy * sqrt(1 - R2))) * 100
    cond_RIR_fixedY <- cond_RIRpi_fixedY * n_obs / 100
    cond_RIR_rxyz <- cond_RIRpi_rxyz * n_obs / 100
  }
  
  
  # Return results based on user's preference
  if (to_return == "print") {
    cat(crayon::bold("Robustness of Inference to Replacement (RIR):\n"))
    cat("This application of RIR quantifies how much data we expect to have to replace when the properties of the replacement data are specified as conditional on other terms already in the model.")  
    cat(sprintf("If the replacement data points follow a null distribution, then RIR = %.3f.\n", cond_RIR_null))
    if (!is.na(cond_RIR_fixedY)) cat(sprintf("If the replacement data points have a fixed value, then RIR = %.3f.\n", cond_RIR_fixedY))
    if (!is.na(cond_RIR_rxyz)) cat(sprintf("If the replacement data points satisfy rxy|Z = 0, then RIR = %.3f.\n", cond_RIR_rxyz))
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
