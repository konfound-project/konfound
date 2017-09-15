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
    coef_df <- tidy_output[tidy_output$term == tested_variable_string, ]
    glance_output <- broom::glance(model_object)
    
    # Dispatching based on class
    if (class(model_object) == "lm") {
        unstd_beta = coef_df$estimate
        std_err = coef_df$std.error
        n_obs = glance_output$df + glance_output$df.residual
        n_covariates = glance_output$df - 2 # (for intercept and coefficient)
        test_sensitivity(unstd_beta = unstd_beta,
                         std_err = std_err,
                         n_obs = n_obs,
                         n_covariates = n_covariates,
                         alpha = alpha,
                         tails = tails,
                         nu = 0,
                         to_return = "print") } }
# else if (class(model_object) == "glm") {
#     
#     
# } 
# else {
#     stop("input must be an object of class lm or glm")} }