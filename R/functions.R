#' Perform sensitivity analysis for published studies
#' @description For published studies, this command calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient.
#' @param unstd_beta an unstandardized regression coefficient
#' @param standard_error the standard error of the estimate of the unstandardized regression coefficient
#' @param n_obs the number of observations in the sample
#' @param n_covariates the number of covariates in the regression model
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @param nu what hypothesis to be tested; defaults to testing whether unstd_beta is significantly different from 0
#' @param to_return whether to return a data.frame (by specifying this argument to euqal "df") or a plot ("plot"); default is to print the output to the console
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference
#' @examples
#' pkonfound(2, .4, 100, 3)
#' pkonfound(.4, 2, 100, 3)
#' @export

pkonfound <- function(unstd_beta,
                      standard_error,
                      n_obs, 
                      n_covariates = 1, 
                      alpha = .05, 
                      tails = 2, 
                      nu = 0,
                      to_return = "print") {
    test_sensitivity(unstd_beta = unstd_beta,
                     standard_error = standard_error,
                     n_obs = n_obs, 
                     n_covariates = n_covariates,
                     alpha = alpha, 
                     tails = tails,
                     nu = nu,
                     to_return = to_return) }

#' Perform sensitivity analysis on fitted models
#' @description For fitted models, this command calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient. Currently works for: models created with lm() (linear models).
#' @param model_object output from a model (currently works for: lm)
#' @param tested_variable Variable associated with the unstandardized beta coefficient to be tested
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference
#' @examples
#' m1 <- lm(mpg ~ wt + hp, data = mtcars)
#' konfound(m1, wt)
#' @export

konfound <- function(model_object, 
                     tested_variable, 
                     alpha = .05, 
                     tails = 2) {
    tested_variable_enquo <- rlang::enquo(tested_variable) # dealing with non-standard evaluation (so unquoted names for tested_variable can be used)
    tested_variable_string <- rlang::quo_name(tested_variable_enquo)
    tidy_output <- broom::tidy(model_object) # tidying output
    coef_df <- dplyr::filter(tidy_output, term == tested_variable_string)
    glance_output <- broom::glance(model_object)
    
    # Dispatching based on class
    if (class(model_object) == "lm") {
        unstd_beta = coef_df$estimate
        standard_error = coef_df$std.error
        n_obs = glance_output$df + glance_output$df.residual
        n_covariates = glance_output$df - 2 # (for intercept and coefficient)
        test_sensitivity(unstd_beta = unstd_beta,
                         standard_error = standard_error,
                         n_obs = n_obs,
                         n_covariates = n_covariates,
                         alpha = alpha,
                         tails = tails,
                         to_return = "print") } }
    # else if (class(model_object) == "glm") {
    #     
    #     
    # } 
    # else {
    #     stop("input must be an object of class lm or glm")} }

#' Perform meta-analyses including sensitivity analysis
#' @description For fitted models, this command carries out sensitivity analysis for a number of models, when their parameters stored in a data.frame. 
#' @param df a data.frame with columns for the 'unstd_beta', 'standard_error', 'n_obs', and 'n_covariates' arguments (see ?pkonfound for more information on these)
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference for each of the cases in the data.frame
#' @export

mkonfound <- function(df, alpha, tails) {
    args <- list(as.list(dplyr::pull(df, 1)), 
                 as.list(dplyr::pull(df, 2)), 
                 as.list(dplyr::pull(df, 3)), 
                 as.list(dplyr::pull(df, 4)))
    x <- purrr::pmap_dfr(args, pkonfound, to_return = "df")
    dplyr::bind_cols(df, x) } 
