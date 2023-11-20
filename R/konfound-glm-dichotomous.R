# konfound-glm

konfound_glm_dichotomous <- function(model_object, 
                                     tested_variable_string, 
                                     test_all, 
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
