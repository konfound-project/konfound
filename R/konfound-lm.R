# konfound-lm

konfound_lm <- function(model_object, tested_variable_string, test_all, alpha, tails, index, to_return) {
  tidy_output <- broom::tidy(model_object) # tidying output
  glance_output <- broom::glance(model_object)

  if (test_all == FALSE) {
    coef_df <- tidy_output[tidy_output$term == tested_variable_string, ]
  } else {
    coef_df <- tidy_output[-1, ]
  } # to remove intercept

  est_eff <- coef_df$estimate
  std_err <- coef_df$std.error
  n_obs <- glance_output$nobs
  n_covariates <- glance_output$df

  if (test_all == FALSE) {
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
      tested_variable = tested_variable_string
    )
    return(out)
  } else {
    message("Note that this output is calculated based on the correlation-based approach used in mkonfound()")
    d <- data.frame(t = est_eff / std_err, df = (n_obs - n_covariates - 1))
    o <- mkonfound(d, .data$t, .data$df)
    term_names <- dplyr::select(tidy_output, var_name = .data$term) # remove the first row for intercept
    term_names <- dplyr::filter(term_names, .data$var_name != "(Intercept)")
    return(dplyr::bind_cols(term_names, o))
  }
}
