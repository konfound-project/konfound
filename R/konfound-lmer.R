# # # konfound-lmer
# # 
# # library(lme4)
# # 
# # fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
# # 
# # broom::tidy(fm1)
# # str(fm1)
# 
# konfound_lmer <- function(model_object, tested_variable_string, test_all, alpha, tails, to_return, p_kr) {
#     tidy_output <- broom::tidy(model_object) # tidying output
#     # glance_output <- broom::glance(model_object)
#     
#     if (test_all == FALSE) {
#         coef_df <- tidy_output[tidy_output$term == tested_variable_string, ] 
#     } else {
#         coef_df <- tidy_output[-1, ] } # to remove intercept
#     
#     unstd_beta = round(coef_df$estimate, 3)
#     std_err = round(coef_df$std.error, 3)
#     
#     if (p_kr == TRUE) {
#         df.kr <- suppressMessages(pbkrtest::get_Lb_ddf(fm1, lme4::fixef(fm1)))
#     } else {
#         print(tested_variable_string)
#         x <- dplyr::count(d, !!rlang::enquo(tested_variable_string))
#         
#         the_names <- names(model_object@frame)
#         group_name <- the_names[length(the_names)]
#         
#         y <- dplyr::select(model_object@frame, !!rlang::quo(tested_variable_string), !!rlang::quo(group_name))
#         print(y)
#         
#         z <- dplyr::left_join(y, )
#         
#         d %>% 
#             count(distance, name) %>% 
#             left_join(select(d, name, review = raw_reviews)) %>% 
#             group_by(name) %>% 
#             summarize(name_var = var(distance))
#     }
#     # n_obs = glance_output$df + glance_output$df.residual
#     # n_covariates = glance_output$df - 2 # (for intercept and coefficient)
#     
#     if (test_all == FALSE) {
#         return(test_sensitivity(unstd_beta = unstd_beta,
#                                 std_err = std_err,
#                                 n_obs = df.kr,
#                                 n_covariates = 0,
#                                 alpha = alpha,
#                                 tails = tails,
#                                 nu = 0,
#                                 to_return = to_return,
#                                 component_correlations = component_correlations,
#                                 non_linear = FALSE,
#                                 model_object = model_object,
#                                 tested_variable = tested_variable_string))
#     } else { 
#         o <- mkonfound(data.frame(unstd_beta, std_err, n_obs, n_covariates))
#         term_names <- dplyr::select(tidy_output, var_name = .data$term) # remove the first row for intercept
#         term_names <- dplyr::filter(term_names, .data$var_name != "(Intercept)")
#         return(dplyr::bind_cols(term_names, o))
#     } 
#     
# }
# 
# # # konfound-lme
# 
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
# #         # coef_df$unstd_beta <- suppressWarnings(summary(margins::margins(model_object))$AME[names(summary(margins::margins(model_object))$AME) == tested_variable_string]) } # to remove intercept
# # 
# #     unstd_beta = round(coef_df, 3)
# #     # unstd_beta <- suppressWarnings(summary(margins::margins(model_object))$AME[names(summary(margins::margins(model_object))$AME) == tested_variable_string])
# #     std_err = round(coef_df$std.error, 3)
# #     n_obs = glance_output$df.null
# #     n_covariates = glance_output$df.null - 2 # (for intercept and coefficient)
# # 
# #     if (test_all == FALSE) {
# #         return(test_sensitivity(unstd_beta = unstd_beta,
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
# #         o <- mkonfound(data.frame(unstd_beta, std_err, n_obs, n_covariates))
# #         term_names <- dplyr::select(tidy_output, var_name = term) # remove the first row for intercept
# #         term_names <- dplyr::filter(term_names, var_name != "(Intercept)")
# #         o <- dplyr::bind_cols(term_names, o)
# #     }