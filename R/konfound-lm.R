# konfound-lm

konfound_lm <- function(model_object, tested_variable_string, alpha, tails, index, to_return) {
    tidy_output <- broom::tidy(model_object) # tidying output
    glance_output <- broom::glance(model_object)
    
    coef_df <- tidy_output[tidy_output$term == tested_variable_string, ]
    
    est_eff <- coef_df$estimate
    std_err <- coef_df$std.error
    n_obs <- glance_output$nobs
    n_covariates <- glance_output$df
    
    out <- test_sensitivity(
        est_eff = est_eff,
        std_err = std_err,
        n_obs = n_obs,
        n_covariates = n_covariates,
        alpha = alpha,
        tails = tails,
        index = index,
        nu = 0,
        to_return = to_return,
        model_object = model_object,
        tested_variable = tested_variable_string)
    
    return(out)
    
}
