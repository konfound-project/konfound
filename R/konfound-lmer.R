# # # konfound-lmer

get_kr_df <- function(model_object) {
  L <- diag(rep(1, length(lme4::fixef(model_object))))
  L <- as.data.frame(L)
  out <- purrr::map_dbl(L, pbkrtest::get_Lb_ddf, object = model_object)
  names(out) <- names(lme4::fixef(model_object))
  out
}

konfound_lmer <- function(model_object, tested_variable_string, test_all, alpha, tails, to_return) {
  tidy_output <- broom::tidy(model_object) # tidying output

  if (test_all == FALSE) {
    coef_df <- tidy_output[tidy_output$term == tested_variable_string, ]
  } else {
    coef_df <- tidy_output[-1, ]
  } # to remove intercept

  est_eff <- round(coef_df$estimate, 3)
  std_err <- round(coef_df$std.error, 3)

  df_kr <- get_kr_df(model_object)
  df_kr <- df_kr[names(df_kr) == tested_variable_string]

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
    term_names <- dplyr::select(tidy_output, var_name = .data$term) # remove the first row for intercept
    term_names <- dplyr::filter(term_names, .data$var_name != "(Intercept)")
    return(dplyr::bind_cols(term_names, o))
  }
}

# # # konfound-lme
# # library(nlme)
# # fm1 <- lme(distance ~ age, data = Orthodont)
# #
# #     tidy_output <- nlme::fixef(model_object) # tidying output
# #     glance_output <- broom::glance(model_object)
# #
# #     if (test_all == FALSE) {
# #         tested_variable_enquo <- rlang::enquo(tested_variable) # dealing with non-standard evaluation (so unquoted names for tested_variable can be used)
# #         tested_variable_string <- rlang::quo_name(tested_variable_enquo)
# #         coef_df <- tidy_output[names(tidy_output) == tested_variable_string]
# #     } else {
# #         coef_df <- tidy_output[-1]
# #         # coef_df$est_eff <- suppressWarnings(summary(margins::margins(model_object))$AME[names(summary(margins::margins(model_object))$AME) == tested_variable_string]) } # to remove intercept
# #
# #     est_eff = round(coef_df, 3)
# #     # est_eff <- suppressWarnings(summary(margins::margins(model_object))$AME[names(summary(margins::margins(model_object))$AME) == tested_variable_string])
# #     std_err = round(coef_df$std.error, 3)
# #     n_obs = glance_output$df.null
# #     n_covariates = glance_output$df.null - 2 # (for intercept and coefficient)
# #
# #     if (test_all == FALSE) {
# #         return(test_sensitivity(est_eff = est_eff,
# #                                 std_err = std_err,
# #                                 n_obs = n_obs,
# #                                 n_covariates = n_covariates,
# #                                 alpha = alpha,
# #                                 tails = tails,
# #                                 nu = 0,
# #                                 to_return = to_return,
# #                                 component_correlations = component_correlations,
# #                                 non_linear = TRUE,
# #                                 model_object = model_object,
# #                                 tested_variable = tested_variable_string))
# #     } else {
# #         o <- mkonfound(data.frame(est_eff, std_err, n_obs, n_covariates))
# #         term_names <- dplyr::select(tidy_output, var_name = term) # remove the first row for intercept
# #         term_names <- dplyr::filter(term_names, var_name != "(Intercept)")
# #         o <- dplyr::bind_cols(term_names, o)
# #     }
