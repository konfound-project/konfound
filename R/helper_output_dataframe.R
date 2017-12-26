# Function to output the data frame

output_df <- function(est_eff, beta_threshhold, unstd_beta, bias = NULL, sustain = NULL, recase, obs_r, critical_r, r_con, itcv, non_linear) {
    if (abs(est_eff) > abs(beta_threshhold)) {
        df  <- dplyr::data_frame(action = "to_invalidate",
                                 inference = "reject_null",
                                 percent_bias_to_change_inference = round(bias, 3),
                                 replace_null_cases = round(recase, 3),
                                 unstd_beta = unstd_beta,
                                 beta_threshhold = beta_threshhold,
                                 omitted_variable_corr = r_con,
                                 itcv = itcv)
    }
    else if (abs(est_eff) < abs(beta_threshhold)) {
        df <- dplyr::data_frame(action = "to_sustain",
                                inference = "fail_to_reject_null",
                                percent_bias_to_change_inference = round(sustain, 3),
                                replace_null_cases = round(recase, 3),
                                unstd_beta = unstd_beta,
                                beta_threshhold = beta_threshhold,
                                omitted_variable_corr = r_con,
                                itcv = itcv)
    }
    else if (est_eff == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.") }
    
    # if (abs(obs_r) > abs(critical_r)) {
    #     df$omitted_variable_corr <- r_con
    # }
    # else if (abs(obs_r) < abs(critical_r)) {
    #     df$omitted_variable_corr <- r_con
    # }
    else if (est_eff == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.\n") }
    
    df
}