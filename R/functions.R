#' Perform sensitivity analysis for published studies
#' @description For published studies, this command calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient.
#' @param unstd_beta an unstandardized regression coefficient
#' @param standard_error the standard error of the estimate of the unstandardized regression coefficient
#' @param n_obs the number of observations in the sample
#' @param n_covariate the number of covariates in the regression model
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference
#' @examples
#' pkonfound(2, .4, 100, 3)
#' pkonfound(.4, 2, 100, 3)
#' @export

pkonfound <- function(unstd_beta,
                      standard_error,
                      n_obs, 
                      n_covariates = 3, 
                      alpha = .05, 
                      tails = 2) {

    critical_t <- qt(1 - (alpha / tails), n_obs - n_covariates)
    
    beta_threshhold <- critical_t * standard_error
    
    if (unstd_beta > beta_threshhold) {
        bias <- 100 * (1 - (beta_threshhold / unstd_beta))
        recase <- round(n_obs * (bias / 100))
        cat(paste0("To invalidate the inference, ", round(bias, 2), " % of the estimate would have to be due to bias.\n"))
        cat(paste0("To invalidate the inference, ", round(recase, 3), " observations would have to be replaced with cases for which there is no effect."))
    } else if (unstd_beta < beta_threshhold) {
        bias <- 100 * (1 - (unstd_beta / beta_threshhold))
        recase <- round(n_obs * (bias / 100))
        cat(paste0("To sustain the inference, ", round(bias, 2), " % of the estimate would have to be due to bias.\n"))
        cat(paste0("To sustain the inference, ", round(recase, 3), " of the cases with 0 effect would have to be replaced with cases at the threshold of inference."))
    } else if (unstd_beta == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.")
    }
    
}

### Questions for Ran

# Am I thinking about invalidate / sustain the right way?
# What does rep_0 do?
