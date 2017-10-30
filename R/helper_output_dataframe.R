# Function to output the data frame

output_df <- function(beta_diff, beta_threshhold, unstd_beta, bias = NULL, sustain = NULL, recase, obs_r, critical_r, r_con, non_linear) {
    if (abs(beta_diff) > abs(beta_threshhold)) {
        df  <- dplyr::data_frame(inference = "to_invalidate",
                                 percent_bias = round(bias, 3),
                                 replace_null_cases = round(recase, 3),
                                 unstd_beta = unstd_beta,
                                 beta_threshhold = beta_threshhold,
                                 omitted_variable_corr = NA)
    }
    else if (abs(beta_diff) < abs(beta_threshhold)) {
        df <- dplyr::data_frame(inference = "to_sustain",
                                percent_bias = round(sustain, 3),
                                replace_null_cases = round(recase, 3),
                                unstd_beta = unstd_beta,
                                beta_threshhold = beta_threshhold,
                                omitted_variable_corr = NA)
    }
    else if (beta_diff == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.") }
    
    if (abs(obs_r) > abs(critical_r)) {
        df$omitted_variable_corr <- r_con
    }
    else if (abs(obs_r) < abs(critical_r)) {
        df$omitted_variable_corr <- r_con
    }
    else if (beta_diff == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.\n") }
    
    df
}