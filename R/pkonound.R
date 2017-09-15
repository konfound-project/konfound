#' Perform sensitivity analysis for published studies
#' @description For published studies, this command calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient.
#' @param unstd_beta an unstandardized regression coefficient
#' @param std_err the standard error of the estimate of the unstandardized regression coefficient
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
                      std_err,
                      n_obs, 
                      n_covariates = 1, 
                      alpha = .05, 
                      tails = 2, 
                      nu = 0,
                      to_return = "print") {
    test_sensitivity(unstd_beta = unstd_beta,
                     std_err = std_err,
                     n_obs = n_obs, 
                     n_covariates = n_covariates,
                     alpha = alpha, 
                     tails = tails,
                     nu = nu,
                     to_return = to_return) }