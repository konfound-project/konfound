t_sensitivity <- function(unstd_beta,
                          std_err,
                          n_obs, 
                          n_covariates,
                          alpha,
                          tails,
                          nu,
                          to_return) {
    # calculating statistics used in every case
    if (unstd_beta < 0) {
        critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 1) * -1 } 
    else {critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 1) }
    
    # local `x'be_th = ``x'criticalt' * ``x'sd' +`nu'
    # local `x't_critr = ``x'be_th'/``x'sd'
    
    critical_r <- critical_t / sqrt((critical_t ^ 2) + (n_obs - n_covariates - 2))

    obs_r <- (unstd_beta / std_err) / sqrt((unstd_beta / std_err ^ 2) + n_obs - n_covariates - 2)
    
    print(critical_r)
    print(obs_r)
    
    if ((obs_r - critical_r) >= 0) {
        itcv = (obs_r - critical_r) / (1 - critical_r)
    } else { itcv = (obs_r - critical_r) / (1 - critical_r) }
    
    print(itcv)
    
local `x'impact =  string(``x'itcv',"%6.4f")
local `x'r_con = string(sqrt(abs(``x'itcv')),"%6.3f")
local `x'nr_con = -1 * ``x'r_con'
local `x'r_ycv = string(``x'r_con' * sqrt(1-(``x'RsqYZ')),"%6.3f")
local `x'r_xcv = string(``x'r_con' * sqrt(1-(``x'RsqXZ')),"%6.3f")
local `x'nr_xcv = -1 * ``x'r_xcv'
local `x'un_impact = string(``x'itcv'*sqrt(1-(``x'RsqYZ'))*sqrt(1-(``x'RsqXZ')),"%6.4f")
}

t_sensitivity(.4, .1, 100, 4, .05, 2)





test_sensitivity <- function(unstd_beta,
                             std_err,
                             n_obs, 
                             n_covariates,
                             alpha,
                             tails,
                             nu,
                             to_return) {
    # calculating statistics used in every case
    if (unstd_beta < 0) {
        critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 1) * -1 } 
    else {critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 1) }
    beta_threshhold <- critical_t * std_err
    # dealing with cases where hypotheses other than whether unstd_beta differs from 0
    if (nu != 0) {
        beta_diff <- abs(unstd_beta - nu) } else {
            beta_diff <- unstd_beta - 0 } # this is just to make what this is doing evident
    
    
    # calculating percentage of effect and number of observations to sustain or invalidate inference
    if (abs(beta_diff) > abs(beta_threshhold)) {
        bias <- 100 * (1 - (beta_threshhold / beta_diff))
        recase <- round(n_obs * (bias / 100)) }
    else if (abs(beta_diff) < abs(beta_threshhold)) {
        sustain <- 100 * (1 - (beta_diff / beta_threshhold))
        recase <- round(n_obs * (sustain / 100)) }
    else if (beta_diff == beta_threshhold) {
        stop("The coefficient is exactly equal to the threshold.") }
    # output dispatch
    if (to_return == "df") return(output_df(beta_diff, beta_threshhold, bias, sustain, recase))
    else if (to_return == "plot") return(output_plot(beta_diff, beta_threshhold, bias, sustain, recase)) 
    else if (to_return == "print") return(output_print(beta_diff, beta_threshhold, bias, sustain, recase))
    else stop("to_return must be set to df, print, or plot") }    