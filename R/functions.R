pkonfound <- function(unstd_beta,
                      standard_error,
                      n_obs, 
                      n_covariates = 3, 
                      alpha = .05, 
                      tails = 2) {

    critical_t <- qt(1 - (alpha / tails), n_obs - n_covariates)
    
    beta_threshhold <- critical_t * standard_error
    
    bias <- 100 * (1 - (beta_threshhold / unstd_beta))
    
    recase <- round(n_obs * (bias / 100))
    
    print(paste0("To invalidate the inference, ", round(bias, 2), " % of the estimate would have to be due to bias."))
    print(paste0("To invalidate the inference, ", round(recase, 3), " observations would have to be replaced with cases for which there is no effect."))

}

pkonfound(2, .4, 100, 10, tails = 2)