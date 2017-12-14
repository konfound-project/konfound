# # konfound-lme

# library(nlme)
# fm1 <- lme(distance ~ age, data = Orthodont)
# 
#     tidy_output <- nlme::fixef(model_object) # tidying output
#     glance_output <- broom::glance(model_object)
# 
#     if (test_all == FALSE) {
#         tested_variable_enquo <- rlang::enquo(tested_variable) # dealing with non-standard evaluation (so unquoted names for tested_variable can be used)
#         tested_variable_string <- rlang::quo_name(tested_variable_enquo)
#         coef_df <- tidy_output[names(tidy_output) == tested_variable_string]
#     } else {
#         coef_df <- tidy_output[-1]
#         # coef_df$unstd_beta <- suppressWarnings(summary(margins::margins(model_object))$AME[names(summary(margins::margins(model_object))$AME) == tested_variable_string]) } # to remove intercept
# 
#     unstd_beta = round(coef_df, 3)
#     # unstd_beta <- suppressWarnings(summary(margins::margins(model_object))$AME[names(summary(margins::margins(model_object))$AME) == tested_variable_string])
#     std_err = round(coef_df$std.error, 3)
#     n_obs = glance_output$df.null
#     n_covariates = glance_output$df.null - 2 # (for intercept and coefficient)
# 
#     if (test_all == FALSE) {
#         return(test_sensitivity(unstd_beta = unstd_beta,
#                                 std_err = std_err,
#                                 n_obs = n_obs,
#                                 n_covariates = n_covariates,
#                                 alpha = alpha,
#                                 tails = tails,
#                                 nu = 0,
#                                 to_return = to_return,
#                                 component_correlations = component_correlations,
#                                 non_linear = TRUE,
#                                 model_object = model_object,
#                                 tested_variable = tested_variable_string))
#     } else {
#         o <- mkonfound(data.frame(unstd_beta, std_err, n_obs, n_covariates))
#         term_names <- dplyr::select(tidy_output, var_name = term) # remove the first row for intercept
#         term_names <- dplyr::filter(term_names, var_name != "(Intercept)")
#         o <- dplyr::bind_cols(term_names, o)
#     }