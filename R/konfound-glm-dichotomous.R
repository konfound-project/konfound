# konfound-glm

#' Konfound Analysis for Generalized Linear Models with Dichotomous Outcomes
#'
#' This function performs konfound analysis on a generalized linear model
#' object with a dichotomous outcome. It uses 'broom' to tidy model outputs
#' and calculates the sensitivity of inferences.
#'
#' @param model_object The model object produced by glm.
#' @param tested_variable_string The name of the variable being tested.
#' @param alpha Significance level for hypothesis testing.
#' @param tails Number of tails for the test (1 or 2).
#' @param to_return The type of output to return.
#' @param n_treat Number of treatment cases.
#' @param switch_trm Term to switch for sensitivity analysis.
#' @param replace Boolean indicating whether to replace cases or not.
#' @return The results of the konfound analysis.
#' @importFrom broom tidy glance
#' @importFrom stats glm
konfound_glm_dichotomous <- function(model_object, 
                                     tested_variable_string, 
                                     alpha, tails,
                                     to_return, 
                                     n_treat, 
                                     switch_trm, 
                                     replace) {
  tidy_output <- broom::tidy(model_object) # tidying output
  glance_output <- broom::glance(model_object)

  coef_df <- tidy_output[tidy_output$term == tested_variable_string, ]

  est_eff <- coef_df$estimate
  std_err <- coef_df$std.error
  n_obs <- glance_output$nobs
  n_covariates <- glance_output$df.null - glance_output$df.residual
  
    out <- test_sensitivity_ln(
      est_eff = est_eff,
      std_err = std_err,
      n_obs = n_obs,
      n_covariates = n_covariates,
      n_treat = n_treat,
      switch_trm = switch_trm,
      replace = replace,
      alpha = alpha,
      tails = tails,
      nu = 0,
      to_return = to_return,
      model_object = model_object,
      tested_variable = tested_variable_string
    )
    
    return(out)

}
