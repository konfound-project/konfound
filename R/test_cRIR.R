# Create the new test_cRIR function
test_cRIR <- function(est_eff, std_err, n_obs, n_covariates = 1, sdx = NA, sdy = NA, R2 = NA, alpha = 0.05, tails = 2, to_return = "print") {
  # Calculate cond_RIRpi_null by default
  cond_RIRpi_null <- est_eff / (std_err * sqrt(n_obs))
  
  # If sdx, sdy, and R2 are provided, calculate other conditional RIR measures
  cond_RIRpi_fixedY <- NA
  cond_RIRpi_rxyz <- NA
  
  if (!is.na(sdx) && !is.na(sdy) && !is.na(R2)) {
    cond_RIRpi_fixedY <- (est_eff / sdx) * (1 - R2)
    cond_RIRpi_rxyz <- (est_eff / (sdy * sqrt(1 - R2)))
  }
  
  # Return results based on user's preference
  if (to_return == "print") {
    cat("Conditional RIR (Null):", cond_RIRpi_null, "\n")
    if (!is.na(cond_RIRpi_fixedY)) cat("Conditional RIR (Fixed Y):", cond_RIRpi_fixedY, "\n")
    if (!is.na(cond_RIRpi_rxyz)) cat("Conditional RIR (rxyz):", cond_RIRpi_rxyz, "\n")
  } else if (to_return == "raw_output") {
    return(list(cond_RIRpi_null = cond_RIRpi_null, cond_RIRpi_fixedY = cond_RIRpi_fixedY, cond_RIRpi_rxyz = cond_RIRpi_rxyz))
  }
}

## NOTE for print output
### general introduction to RIR 
### This application of RIR quantifies how much data to replace when the properties of the replacement data are specified as conditional on other terms already in the model.
## If the replacement data points have a fixed value, then RIR = %.3f.\n", cond_RIR_fixedY
## If the replacement data points follow a null distribution, then RIR = %.3f.\n", cond_RIR_null
## If the replacement data points satisfy rxy|Z = 0, then RIR = %.3f.\n", cond_RIR_rxyz

## NOTE for raw output
## the three RIR with documentation 