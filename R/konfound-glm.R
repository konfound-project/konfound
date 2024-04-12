# konfound-glm

#' Konfound Analysis for Generalized Linear Models
#'
#' This function performs konfound analysis on a generalized linear model
#' object. It uses 'broom' to tidy model outputs and calculates the sensitivity
#' of inferences. It supports analysis for a single variable
#'  or multiple variables.
#'
#' @param model_object The model object produced by glm.
#' @param tested_variable_string The name of the variable being tested.
#' @param alpha Significance level for hypothesis testing.
#' @param tails Number of tails for the test (1 or 2).
#' @param index Type of sensitivity analysis ('RIR' by default).
#' @param to_return The type of output to return.
#' @return The results of the konfound analysis for the specified variable(s).
#' @importFrom broom tidy glance
#' @importFrom dplyr select filter bind_cols
#' @importFrom stats glm
#' @importFrom margins margins
konfound_glm <- function(model_object, 
                         tested_variable_string, 
                         alpha, tails, 
                         index = "RIR", 
                         to_return) {
    tidy_output <- broom::tidy(model_object) # tidying output
    glance_output <- broom::glance(model_object)
    
    coef_df <- tidy_output[tidy_output$term == tested_variable_string, ]
    est_eff <- coef_df$estimate
    warning("Due to an issue with the margins and predictions package, these are the raw coefficients, not the average marginal effects; we will address this in future patches")
    # est_eff <- suppressWarnings(
    #   summary(margins::margins(model_object))$AME[names(summary(
    #     margins::margins(model_object))$AME) == tested_variable_string])
    std_err <- coef_df$std.error
    n_obs <- glance_output$nobs
    n_covariates <- glance_output$df.null - glance_output$df.residual
    
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
