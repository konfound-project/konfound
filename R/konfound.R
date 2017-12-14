#' Perform sensitivity analysis on fitted models
#' @description For fitted models, this command calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient. Currently works for: models created with lm() (linear models).
#' @param model_object output from a model (currently works for: lm)
#' @param tested_variable_string Variable associated with the unstandardized beta coefficient to be tested
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param to_return whether to return a data.frame (by specifying this argument to equal "raw_output"), table ("table"), or a plot ("plot"); default is to print the output to the console
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @param test_all whether to carry out the sensitivity test for all of the coefficients (defaults to FALSE)
#' @param component_correlations whether to return the component correlations as part of the correlation-based approach
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference
#' @importFrom rlang .data
#' @examples
#' m1 <- lm(mpg ~ wt + hp, data = mtcars)
#' konfound(m1, wt)
#' konfound(m1, wt, test_all = TRUE)
#' konfound(m1, wt, to_return = "table")
#' 
#' if (requireNamespace("forcats")) {
#' d <- forcats::gss_cat
#'
#' d$married <- ifelse(d$marital == "Married", 1, 0)
#' 
#' m2 <- glm(married ~ age, data = d, family = binomial(link = "logit"))
#' konfound(m2, age)
#' }
#' 
#' #' if (requireNamespace("lmer")) {
#' m3 <- fm1 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
#' konfound(m3, Days)
#' }
#'
#' @export

konfound <- function(model_object, 
                     tested_variable,
                     alpha = .05, 
                     tails = 2,
                     to_return = "print",
                     test_all = FALSE, 
                     component_correlations = F) {
    
    # Stop messages
    if (!(class(model_object)[1] %in% c("lm", "glm", "lmerMod"))) {
        stop("konfound() is currently implemented for models estimated with lm(), glm(), and lme4::lmer(); consider using pkonfound() instead")
    }
    
    if (to_return == "table" & test_all == TRUE) stop("cannot return a table when test_all is set to TRUE")
    
    # Dealing with non-standard evaluation
    tested_variable_enquo <- rlang::enquo(tested_variable) # dealing with non-standard evaluation (so unquoted names for tested_variable can be used)
    tested_variable_string <- rlang::quo_name(tested_variable_enquo)
    
    # Dispatching based on class
    if (class(model_object)[1] == "lm") {
        
        output <- konfound_lm(model_object = model_object,
                              tested_variable_string = tested_variable_string,
                              test_all = test_all,
                              alpha = alpha,
                              tails = tails,
                              to_return = to_return)
        
    }
    # if (length(to_return) > 1) {
    #     to_return <-to_return[!(to_return == "print")]
    #     konfound_output <- purrr::map(to_return,
    #                                   ~ test_sensitivity(
    #                                       unstd_beta = unstd_beta,
    #                                       std_err = std_err,
    #                                       n_obs = n_obs, 
    #                                       n_covariates = n_covariates,
    #                                       alpha = alpha, 
    #                                       tails = tails,
    #                                       nu = nu,
    #                                       to_return = .,
    #                                       component_correlations = component_correlations))
    #     konfound_output <- create_konfound_class(konfound_output)
    #     names(konfound_output) <- to_return
    #     output_print(beta_diff, beta_threshold, bias, sustain, nu, recase, obs_r, critical_r, r_con, itcv, non_linear = FALSE)
    #     
    #     cat("\n")
    #     message(paste("Print output created by default. Created", length(konfound_output), "other forms of output. Use list indexing or run summary() on the output to see how to access."))
    #     
    #     invisible(konfound_output) 
    # }
    
    if (inherits(model_object, "glm")) {
        warning("For a non-linear model, impact threshold should not be used.")
        
        output <- konfound_glm(model_object = model_object,
                               tested_variable_string = tested_variable_string,
                               test_all = test_all,
                               alpha = alpha,
                               tails = tails,
                               to_return = to_return)
    }
    if (inherits(model_object, "lmerMod")) {
        
        output <- konfound_lmer(model_object = model_object,
                                tested_variable_string = tested_variable_string,
                                test_all = test_all,
                                alpha = alpha,
                                tails = tails,
                                to_return = to_return)
        
    }
    return(output)
}
