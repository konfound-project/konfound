# Main function to test sensitivity to be wrapped with pkonfound(), konfound(), and mkonfound()

test_sensitivity <- function(unstd_beta,
                             std_err,
                             n_obs,
                             n_covariates,
                             alpha,
                             tails,
                             nu,
                             to_return,
                             component_correlations) {
    # calculating statistics used in every case
    if (unstd_beta < 0) {
        critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 1) * -1 }
    else {critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 1) }
    beta_threshhold <- critical_t * std_err
    # dealing with cases where hypotheses other than whether unstd_beta differs from 0
    if (nu != 0) {
        beta_diff <- abs(unstd_beta - nu) } else {
            beta_diff <- unstd_beta - 0 } # this is just to make what this is doing evident
    
    # for replacement of cases approach
    
    # calculating percentage of effect and number of observations to sustain or invalidate inference
    if (abs(beta_diff) > abs(beta_threshhold)) {
        bias <- 100 * (1 - (beta_threshhold / beta_diff))
        recase <- round(n_obs * (bias / 100)) }
    else if (abs(beta_diff) < abs(beta_threshhold)) {
        sustain <- 100 * (1 - (beta_diff / beta_threshhold))
        recase <- round(n_obs * (sustain / 100)) }
    else if (beta_diff == beta_threshhold) {
        stop("The coefficient is exactly equal to the threshold.") }
    
    # for correlation-based approach
    
    # transforming t into r
    obs_r <- (beta_diff / std_err) / sqrt(((n_obs - n_covariates - 3) + ((beta_diff / std_err) ^ 2)))
    # finding critical r
    critical_r <- critical_t / sqrt((critical_t ^ 2) + (n_obs - n_covariates - 3))
    # calculating threshold
    if ((abs(obs_r) > abs(critical_r)) & ((obs_r * critical_r) > 0)) {
        mp <- -1
    } else {
        mp <- 1
    }
    # calculating impact of the confounding variable
    itcv <- (obs_r - critical_r) / (1 + mp * abs(critical_r))
    # finding correlation of confound to invalidate / sustain inference
    r_con <- round(sqrt(abs(itcv)), 3)
    
    if (component_correlations == TRUE){
        rsq <- # has to come from some kind of model object
            varY <- # has to come from some kind of model object
            varX <- # has to come from some kind of model object
            sdX <- # has to come from some kind of model object
            
            rsqYZ = (((obs_r ^ 2) - Rsq) / ((obs_r ^ 2) - 1))
        
        rsqXZ = max(0, 1 - ((VarY * (1 - RSQ))) / (VarX * (n_obs - n_covariates - 2) * (sdx * 2)))
        
        r_ycv = r_con * sqrt(1 - rsqYZ)
        r_xcv = r_con * sqrt(1 - rsqXZ)  
        # before conditioning on observed covariates
    }
    
    
    # needs obs_r, critical_r, and r_con
    
    # output dispatch
    if (to_return == "df") return(output_df(beta_diff, beta_threshhold, unstd_beta, bias, sustain, recase, obs_r, critical_r, r_con))
    else if (to_return == "plot") return(output_plot(beta_diff, beta_threshhold, unstd_beta, obs_r, critical_r, r_con))
    else if (to_return == "print") return(output_print(beta_diff, beta_threshhold, bias, sustain, recase, obs_r, critical_r, r_con))
    else stop("to_return must be set to df, print, or plot")
}
