#' Perform sensitivity analysis on fitted models
#' @description For fitted models, this command calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient. Currently works for: models created with lm() (linear models).
#' @param model_object output from a model (currently works for: lm)
#' @param tested_variable Variable associated with the unstandardized beta coefficient to be tested
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param to_return whether to return a data.frame (by specifying this argument to equal "df"), table ("table"), or a plot ("plot"); default is to print the output to the console
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @param test_all whether to carry out the sensitivity test for all of the coefficients (defaults to FALSE)
#' @param component_correlations whether to return the component correlations as part of the correlation-based approach
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference
#' @examples
#' m1 <- lm(mpg ~ wt + hp, data = mtcars)
#' konfound(m1, wt)
#' konfound(m1, wt, test_all = TRUE)
#' konfound(m1, wt, to_return = "table")
#' @export

konfound <- function(model_object, 
                     tested_variable,
                     alpha = .05, 
                     tails = 2,
                     to_return = "print",
                     test_all = FALSE, 
                     component_correlations = F) {
    
    if (class(model_object)[1] %in% c("merMod", "lme", "nlme")) {
        stop("We recommend carrying out sensitivity analysis for mixed-effects or multi-level models using pkonfound().",
             "If using lme4, consider use of the lmerTest::lmer() function, which outputs degrees of freedom.")
        # tidy_output <- broom::tidy(model_object) # tidying output
        # glance_output <- broom::glance(model_object)
    }
    
    if (!(class(model_object)[1] %in% c("lm", "glm", "merMod", "lme"))) {
        stop("konfound() is currently implemented for models estimated with lm() and glm()")
    }
    
    if (to_return == "table" & test_all == TRUE) stop("cannot return a table when test_all is set to TRUE")
    
    tidy_output <- broom::tidy(model_object) # tidying output
    glance_output <- broom::glance(model_object)
    
    # Dispatching based on class
    
    if (class(model_object)[1] == "lm") {
        
        if (test_all == FALSE) {
            tested_variable_enquo <- rlang::enquo(tested_variable) # dealing with non-standard evaluation (so unquoted names for tested_variable can be used)
            tested_variable_string <- rlang::quo_name(tested_variable_enquo)
            coef_df <- tidy_output[tidy_output$term == tested_variable_string, ] 
        } else {
            coef_df <- tidy_output[-1, ] } # to remove intercept
        
        unstd_beta = round(coef_df$estimate, 3)
        std_err = round(coef_df$std.error, 3)
        n_obs = glance_output$df + glance_output$df.residual
        n_covariates = glance_output$df - 2 # (for intercept and coefficient)
        
        if (test_all == FALSE) {
            return(test_sensitivity(unstd_beta = unstd_beta,
                                    std_err = std_err,
                                    n_obs = n_obs,
                                    n_covariates = n_covariates,
                                    alpha = alpha,
                                    tails = tails,
                                    nu = 0,
                                    to_return = to_return,
                                    component_correlations = component_correlations,
                                    non_linear = FALSE,
                                    model_object = model_object,
                                    tested_variable = tested_variable_string))
        } else { 
            o <- mkonfound(data.frame(unstd_beta, std_err, n_obs, n_covariates))
            term_names <- dplyr::select(tidy_output, var_name = term) # remove the first row for intercept
            term_names <- dplyr::filter(term_names, var_name != "(Intercept)")
            o <- dplyr::bind_cols(term_names, o)
        } 
    }
    
    if (class(model_object)[1] == "glm") {
        warning("For a non-linear mode, impact threshold should not be used.")
        
        if (test_all == FALSE) {
            tested_variable_enquo <- rlang::enquo(tested_variable) # dealing with non-standard evaluation (so unquoted names for tested_variable can be used)
            tested_variable_string <- rlang::quo_name(tested_variable_enquo)
            coef_df <- tidy_output[tidy_output$term == tested_variable_string, ] 
        } else {
            coef_df <- tidy_output[-1, ]
            coef_df$unstd_beta <- suppressWarnings(summary(margins::margins(model_object))$AME[names(summary(margins::margins(model_object))$AME) == tested_variable_string]) } # to remove intercept
        
        unstd_beta = round(coef_df$estimate, 3)
        unstd_beta <- suppressWarnings(summary(margins::margins(model_object))$AME[names(summary(margins::margins(model_object))$AME) == tested_variable_string])
        std_err = round(coef_df$std.error, 3)
        n_obs = glance_output$df.null
        n_covariates = glance_output$df.null - 2 # (for intercept and coefficient)
        
        if (test_all == FALSE) {
            return(test_sensitivity(unstd_beta = unstd_beta,
                                    std_err = std_err,
                                    n_obs = n_obs,
                                    n_covariates = n_covariates,
                                    alpha = alpha,
                                    tails = tails,
                                    nu = 0,
                                    to_return = to_return,
                                    component_correlations = component_correlations,
                                    non_linear = TRUE,
                                    model_object = model_object,
                                    tested_variable = tested_variable_string))
        } else { 
            o <- mkonfound(data.frame(unstd_beta, std_err, n_obs, n_covariates))
            term_names <- dplyr::select(tidy_output, var_name = term) # remove the first row for intercept
            term_names <- dplyr::filter(term_names, var_name != "(Intercept)")
            o <- dplyr::bind_cols(term_names, o)
        } 
    }
    # 
    # if (class(model_object) == "lme") {
    #     
    #     if (test_all == FALSE) {
    #         tested_variable_enquo <- rlang::enquo(tested_variable) # dealing with non-standard evaluation (so unquoted names for tested_variable can be used)
    #         tested_variable_string <- rlang::quo_name(tested_variable_enquo)
    #         coef_df <- tidy_output[tidy_output$term == tested_variable_string, ] 
    #     } else {
    #         coef_df <- tidy_output[-1, ]
    #         coef_df$unstd_beta <- suppressWarnings(summary(margins::margins(model_object))$AME[names(summary(margins::margins(model_object))$AME) == tested_variable_string]) } # to remove intercept
    #     
    #     unstd_beta = round(coef_df$estimate, 3)
    #     unstd_beta <- suppressWarnings(summary(margins::margins(model_object))$AME[names(summary(margins::margins(model_object))$AME) == tested_variable_string])
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
    # }
    
    return(o)
}