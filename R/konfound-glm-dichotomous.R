# konfound-glm

konfound_glm_dichotomous <- function(model_object, tested_variable_string, test_all, alpha, tails, to_return, n_trm, switch_trm) {
  tidy_output <- broom::tidy(model_object) # tidying output
  glance_output <- broom::glance(model_object)

  # if (test_all == FALSE) {
  coef_df <- tidy_output[tidy_output$term == tested_variable_string, ]
  # } else {
  #   coef_df <- tidy_output[-1, ]
  #   coef_df$est_eff <- suppressWarnings(summary(margins::margins(model_object))$AME[names(summary(margins::margins(model_object))$AME) == tested_variable_string])
  # } # to remove intercept

  est_eff <- round(coef_df$estimate, 3)
  est_eff <- suppressWarnings(summary(margins::margins(model_object))$AME[names(summary(margins::margins(model_object))$AME) == tested_variable_string])
  std_err <- round(coef_df$std.error, 3)
  n_obs <- glance_output$df.null
  n_covariates <- glance_output$df.null - 2 # (for intercept and coefficient)

  if (test_all == FALSE) {
    out <- test_sensitivity_ln(
      est_eff = est_eff,
      std_err = std_err,
      n_obs = n_obs,
      n_covariates = n_covariates,
      n_trm = n_trm,
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
  } else {
    message("Note that this output is calculated based on the correlation-based approach used in mkonfound()")
    stop("Multiple variables cannot presently be tested for models fit using glm(); this will be added in the future.")
    d <- data.frame(t = est_eff / std_err, df = (n_obs - n_covariates - 1))
    o <- mkonfound(d, .data$t, .data$df)
    term_names <- dplyr::select(tidy_output, var_name = .data$term) # remove the first row for intercept
    term_names <- dplyr::filter(term_names, .data$var_name != "(Intercept)")
    return(dplyr::bind_cols(term_names, o))
  }
}
