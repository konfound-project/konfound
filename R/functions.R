# need to abstract out what is similar across these two
# function to create dataframe output

create_dataframe <- function(unstd_beta, beta_threshhold, n_obs) {
    if (abs(unstd_beta) > abs(beta_threshhold)) {
        bias <- 100 * (1 - (beta_threshhold / unstd_beta))
        recase <- round(n_obs * (bias / 100))
        return(data.frame(case = "bias", pct_value = round(bias, 2), obs = round(recase, 3)))
    } else if (abs(unstd_beta) < abs(beta_threshhold)) {
        sustain <- 100 * (1 - (unstd_beta / beta_threshhold))
        recase <- round(n_obs * (sustain / 100))
        return(data.frame(case = "sustain", pct_value = round(sustain, 2), obs = round(recase, 3)))
    } else if (unstd_beta == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.")}
}

# function to create printed output

create_print <- function(unstd_beta, beta_threshhold, n_obs) {
    if (abs(unstd_beta) > abs(beta_threshhold)) {
        bias <- 100 * (1 - (beta_threshhold / unstd_beta))
        recase <- round(n_obs * (bias / 100))
        cat("To invalidate the inference,", round(bias, 2), "% of the estimate would have to be due to bias.\n")
        cat("To invalidate the inference,", round(recase, 3), "observations would have to be replaced with cases for which there is no effect.") # why is this adding a ">" at the end?
    } else if (abs(unstd_beta) < abs(beta_threshhold)) {
        sustain <- 100 * (1 - (unstd_beta / beta_threshhold))
        recase <- round(n_obs * (sustain / 100))
        cat("To sustain the inference, ", round(sustain, 2), "% of the estimate would have to be due to bias.\n")
        cat("To sustain the inference, ", round(recase, 3), " of the cases with 0 effect would have to be replaced with cases at the threshold of inference.")
    } else if (unstd_beta == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.")}
}

# function to create plot

create_plot <- function(unstd_beta, beta_threshhold, n_obs) {
    create_dataframe(unstd_beta = unstd_beta, beta_threshhold = beta_threshhold, n_obs = n_obs) %>%
        mutate(remainder = 100 - pct_value) %>% 
        gather(key, val, -case, -obs) %>% 
        ggplot(aes(x = case, y = val, fill = key)) +
        geom_col(position = position_fill(reverse = TRUE)) +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_fill_manual("", values = c("cyan4", "lightgray"), breaks = "pct_value") +
        xlab("")
}

# Main function to test sensitivity to be wrapped with pkonfound() and konfound()

test_sensitivity <- function(unstd_beta,
                             standard_error,
                             n_obs, 
                             n_covariates,
                             alpha, 
                             tails,
                             to_return) {
    if (unstd_beta < 0) {
        critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates) * -1
    } else {
        critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates)}
    
    beta_threshhold <- critical_t * standard_error
    
    if (to_return == "df") {
        create_dataframe(unstd_beta = unstd_beta, beta_threshhold = beta_threshhold, n_obs = n_obs)
    } else if (to_return == "plot") {
        create_plot(unstd_beta = unstd_beta, beta_threshhold = beta_threshhold, n_obs = n_obs) 
    } else if (to_return == "print") {
        create_print(unstd_beta = unstd_beta, beta_threshhold = beta_threshhold, n_obs = n_obs) 
    } else {
        create_print(unstd_beta = unstd_beta, beta_threshhold = beta_threshhold, n_obs = n_obs) 
    }
}

#' Perform sensitivity analysis for published studies
#' @description For published studies, this command calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient.
#' @param unstd_beta an unstandardized regression coefficient
#' @param standard_error the standard error of the estimate of the unstandardized regression coefficient
#' @param n_obs the number of observations in the sample
#' @param n_covariates the number of covariates in the regression model
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
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
                      to_return = "print") {
    
    test_sensitivity(unstd_beta = unstd_beta,
                     standard_error = standard_error,
                     n_obs = n_obs, 
                     n_covariates = n_covariates,
                     alpha = alpha, 
                     tails = tails,
                     to_return = to_return)

}

#' Perform sensitivity analysis on fitted models
#' @description For fitted models, this command calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient. Currently works for: models created with lm() (linear models).
#' @param model_object output from a model (currently works for: lm)
#' @param tested_variable Variable associated with the unstandardized beta coefficient to be tested
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @param to_return whether to return the results in a data.frame (defaults to FALSE; instead results are printed to the console)
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference
#' @examples
#' m1 <- lm(mpg ~ wt + hp, data = mtcars)
#' konfound(m1, wt)
#' @export

konfound <- function(model_object, 
                     tested_variable, 
                     alpha = .05, 
                     tails = 2){
    
    # Dealing with non-standard evaluation (so unquoted names for tested_variable can be used)
    tested_variable_enquo <- rlang::enquo(tested_variable)
    tested_variable_string <- rlang::quo_name(tested_variable_enquo)
    
    # Tidying output
    tidy_output <- broom::tidy(model_object)
    
    coef_df <- dplyr::filter(tidy_output, term == tested_variable_string)
    glance_output <- broom::glance(model_object)
    
    # Dispatching based on class
    
    # lm:
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
                         to_return = to_return)
        
    } else if (class(model_object) == "glm") {
        
        
        
    } else {
        stop("input must be an object of class lm or glm")
    }
    
}

#' Perform meta-analyses including sensitivity analysis
#' @description For fitted models, this command carries out sensitivity analysis for a number of models, when their parameters stored in a data.frame. 
#' @param df a data.frame with columns for the 'unstd_beta', 'standard_error', 'n_obs', and 'n_covariates' arguments (see ?pkonfound for more information on these)
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference for each of the cases in the data.frame
#' @export

mkonfound <- function(df, alpha, tails) {
    args <- list(as.list(dplyr::pull(df, 1)), as.list(dplyr::pull(df, 2)), as.list(dplyr::pull(df, 3)), as.list(dplyr::pull(df, 4)))
    suppressWarnings(purrr::pmap_dfr(args, pkonfound, to_return = TRUE)) # remove suppresswarnings by fixing types
}
