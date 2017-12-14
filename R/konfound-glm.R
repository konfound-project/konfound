# konfound-glm

konfound_glm <- function(model_object, tested_variable_string, test_all, alpha, tails, to_return) {
    
    tidy_output <- broom::tidy(model_object) # tidying output
    glance_output <- broom::glance(model_object)
    
    if (test_all == FALSE) {
        coef_df <- tidy_output[tidy_output$term == tested_variable_string, ]
    } else {
        coef_df <- tidy_output[-1, ]
        coef_df$unstd_beta <- suppressWarnings(summary(margins::margins(model_object))$AME[names(summary(margins::margins(model_object))$AME) == tested_variable_string]) } # to remove intercept
    
    unstd_beta = round(coef_df$estimate, 3)
    unstd_beta <- suppressWarnings(summary(margins::margins(model_object))$AME[names(summary(margins::margins(model_object))$AME) == tested_variable_string])
    std_err = round(coef_df$std.error, 3)
    n_obs = glance_output$df.null
    n_covariates = glance_output$df.null - 2 # (for intercept and coefficient)
    
    print(unstd_beta)
    
    if (test_all == FALSE) {
        return(test_sensitivity(unstd_beta = unstd_beta,
                                std_err = std_err,
                                n_obs = n_obs,
                                n_covariates = n_covariates,
                                alpha = alpha,
                                tails = tails,
                                nu = 0,
                                to_return = to_return,
                                component_correlations = component_correlations,
                                non_linear = TRUE,
                                model_object = model_object,
                                tested_variable = tested_variable_string))
    } else {
        o <- mkonfound(data.frame(unstd_beta, std_err, n_obs, n_covariates))
        term_names <- dplyr::select(tidy_output, var_name = .data$term) # remove the first row for intercept
        term_names <- dplyr::filter(term_names, .data$var_name != "(Intercept)")
        o <- dplyr::bind_cols(term_names, o)
    }
}