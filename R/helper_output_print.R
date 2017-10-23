# Function to output printed text

output_print <- function(beta_diff, beta_threshhold, bias = NULL, sustain = NULL, recase, obs_r, critical_r, r_con) {
    if (abs(beta_diff) > abs(beta_threshhold)) {
        cat("To invalidate the inference,", round(bias, 3), "% of the estimate would have to be due to bias.\n")
        cat("To invalidate the inference,", round(recase, 3), "observations would have to be replaced with cases for which there is no effect.\n") }
    else if (abs(beta_diff) < abs(beta_threshhold)) {
        cat("To sustain the inference, ", round(sustain, 3), "% of the estimate would have to be due to bias.\n")
        cat("To sustain the inference, ", round(recase, 3), " of the cases with 0 effect would have to be replaced with cases at the threshold of inference.\n") }
    else if (beta_diff == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.\n") }
    
    cat("\n")
    
    if (abs(obs_r) > abs(critical_r)) {
        cat("An omitted variable would have to be correlated at", r_con, "with the outcome and at", r_con, 
            "with the predictor of interest (conditioning on observed covariates) to invalidate an inference.\n")
        cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be", round(sqrt(r_con), 3), "to invalidate an inference.\n")  }
    else if (abs(obs_r) < abs(critical_r)) {
        cat("An omitted variable would have to be correlated at", r_con, "with the outcome and at", r_con, 
            "with the predictor of interest (conditioning on observed covariates) to sustain an inference.\n")
        cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be", round(sqrt(r_con), 3), "to sustain an inference.\n") }
    else if (r_con == itcv) {
        warning("The correlation is exactly equal to the threshold.\n") }
}
