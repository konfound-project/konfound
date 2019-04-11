# # # konfound-lmer

get_kr_df <- function(model_object) {
  L <- diag(rep(1, length(lme4::fixef(model_object))))
  L <- as.data.frame(L)
  out <- suppressWarnings(purrr::map_dbl(L, pbkrtest::get_Lb_ddf, object = model_object))
  names(out) <- names(lme4::fixef(model_object))
  out
}

konfound_lmer <- function(model_object, tested_variable_string, test_all, alpha, tails, to_return) {
  tidy_output <- broom::tidy(model_object) # tidying output

  if (test_all == FALSE) {
    coef_df <- tidy_output[tidy_output$term == tested_variable_string, ]
    est_eff <- round(coef_df$estimate, 3)
    std_err <- round(coef_df$std.error, 3)
    df_kr <- get_kr_df(model_object)
    df_kr <- df_kr[names(df_kr) == tested_variable_string]
  } else {
    coef_df <- tidy_output[-1, ] # to remove intercept
    coef_df <- filter(coef_df, !is.na(coef_df$std.error))
    est_eff <- coef_df$estimate
    std_err <- coef_df$std.error
    df_kr <- get_kr_df(model_object)
    df_kr <- df_kr[-1] # to remove intercept
  }

  if (test_all == FALSE) {
    return(test_sensitivity(
      est_eff = est_eff,
      std_err = std_err,
      n_obs = df_kr,
      n_covariates = 0,
      alpha = alpha,
      tails = tails,
      nu = 0,
      to_return = to_return,
      model_object = model_object,
      tested_variable = tested_variable_string
    ))
  } else {
    d <- data.frame(t = est_eff / std_err, df = df_kr)
    o <- mkonfound(d, .data$t, .data$df)
    return(o)
  }
}