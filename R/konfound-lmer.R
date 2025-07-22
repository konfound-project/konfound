# # # konfound-lmer

#' Extract Degrees of Freedom for Fixed Effects in a Linear Mixed-Effects Model
#'
#' @param model_object The mixed-effects model object produced by lme4::lmer.
#' @return A vector containing degrees of freedom for the fixed effects in the model.
#' @importFrom lme4 fixef
#' @importFrom pbkrtest Lb_ddf
#' @importFrom pbkrtest vcovAdj
#' @importFrom purrr map_dbl
get_kr_df <- function(model_object) {
    L <- diag(rep(1, length(lme4::fixef(model_object))))
    L <- as.data.frame(L)
    vcov_adj <- pbkrtest::vcovAdj(model_object)
    vcov_0 <- vcov(model_object)
    out <- suppressWarnings(purrr::map_dbl(
      L,
      pbkrtest::Lb_ddf,
      V0 = vcov_0, Vadj = vcov_adj
    ))
    names(out) <- names(lme4::fixef(model_object))
    out
}

#' Konfound Analysis for Linear Mixed-Effects Models
#'
#' This function performs konfound analysis on a linear mixed-effects model
#' object produced by lme4::lmer. It calculates the sensitivity of inferences
#' for fixed effects in the model. It supports analysis for a single variable or multiple variables.
#'
#' @param model_object The mixed-effects model object produced by lme4::lmer.
#' @param tested_variable_string The name of the fixed effect being tested.
#' @param test_all Boolean indicating whether to test all fixed effects or not.
#' @param alpha Significance level for hypothesis testing.
#' @param tails Number of tails for the test (1 or 2).
#' @param index Type of sensitivity analysis ('RIR' by default).
#' @param to_return The type of output to return.
#' @return The results of the konfound analysis for the specified fixed effect(s).
#' @importFrom broom.mixed tidy
#' @importFrom dplyr filter bind_cols
konfound_lmer <- function(model_object, 
                          tested_variable_string, 
                          test_all, 
                          alpha, 
                          tails, 
                          index, 
                          to_return) {
    tidy_output <- broom.mixed::tidy(model_object) # tidying output
    
    coef_df <- tidy_output[tidy_output$term == tested_variable_string, ]
    est_eff <- coef_df$estimate
    std_err <- coef_df$std.error
    df_kr <- get_kr_df(model_object)
    df_kr <- df_kr[names(df_kr) == tested_variable_string]
    
    return(test_sensitivity(
        est_eff = est_eff,
        std_err = std_err,
        n_obs = df_kr,
        n_covariates = 0,
        alpha = alpha,
        tails = tails,
        index = index,
        nu = 0,
        to_return = to_return,
        model_object = model_object,
        tested_variable = tested_variable_string))
    
}
