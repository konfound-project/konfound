#' Perform sensitivity analysis for published studies
#' @description For published studies, this command calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient.
#' @param est_eff the estimated effect (such as an unstandardized beta coefficient or a group mean difference)
#' @param std_err the standard error of the estimate of the unstandardized regression coefficient
#' @param n_obs the number of observations in the sample
#' @param n_covariates the number of covariates in the regression model
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @param nu what hypothesis to be tested; defaults to testing whether est_eff is significantly different from 0
#' @param logistic whether the model is a logistic regression with a binomial outcome distribution; defaults to FALSE
#' @param n_trm the number of cases associated with the treatment condition; applicable only when logistic = TRUE
#' @param switch_trm whether to switch the treatment and control cases; defaults to FALSE; applicable only when logistic - TRUE
#' @param a cell is the number of cases in the control group showing unsuccessful results
#' @param b cell is the number of cases in the control group showing successful results
#' @param c cell is the number of cases in the treatment group showing unsuccessful results
#' @param d cell is the number of cases in the treatment group showing successful results
#' @param two_by_two_table table that is a matrix or can be coerced to one (data.frame, tibble, tribble) from which the a, b, c, and d arguments can be extracted
#' @param test whether using Fisher's Exact Test or A chi-square test; defaults to Fisher's Exact Test
#' @param replace whether using entire sample or the control group to calculate the base rate; default is the entire sample
#' @param to_return whether to return a data.frame (by specifying this argument to equal "raw_output" for use in other analyses) or a plot ("plot"); default is to print ("print") the output to the console; can specify a vector of output to return
#' @importFrom stats fisher.test
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference
#' @examples
#' # using pkonfound for linear models
#' pkonfound(2, .4, 100, 3)
#' pkonfound(-2.2, .65, 200, 3)
#' pkonfound(.5, 3, 200, 3)
#' pkonfound(-0.2, 0.103, 20888, 3, n_trm = 17888, logistic = TRUE)
#'
#' pkonfound(2, .4, 100, 3, to_return = "thresh_plot")
#' pkonfound(2, .4, 100, 3, to_return = "corr_plot")
#' 
#' pkonfound_output <- pkonfound(2, .4, 200, 3,
#'   to_return = c("raw_output", "thresh_plot", "corr_plot")
#' )
#' summary(pkonfound_output)
#' pkonfound_output$raw_output
#' pkonfound_output$thresh_plot
#' pkonfound_output$corr_plot
#' 
#' # using pkonfound for a 2x2 table
#' pkonfound(a = 35, b = 17, c = 17, d = 38)
#' pkonfound(a = 35, b = 17, c = 17, d = 38, alpha = 0.01)
#' pkonfound(a = 35, b = 17, c = 17, d = 38, alpha = 0.01, switch_trm = FALSE)
#' pkonfound(a = 35, b = 17, c = 17, d = 38, test = "chisq")
#' 
# my_table <- tibble::tribble(
# ~unsuccess, ~success,
# 35,         17,
# 17,         38,
# )
# 
# pkonfound(two_by_two_table = my_table)
#'
#' @export

 pkonfound <- function(est_eff,
                      std_err,
                      n_obs,
                      n_covariates = 1,
                      alpha = .05,
                      tails = 2,
                      nu = 0,
                      n_trm = NULL,
                      switch_trm = TRUE,
                      logistic = FALSE,
                      a = NULL,
                      b = NULL,
                      c = NULL,
                      d = NULL,
                      two_by_two_table = NULL,
                      test = "fisher",
                      replace = "control",
                      to_return = "print") {
  if ("table" %in% to_return) stop("a table can only be output when using konfound")
  # if (nu != 0) warning("Output for the impact of the confounding variable (the ITCV) is not valid for a non-0 null hypothesis about an effect")
  if (logistic == TRUE) {
    out <- test_sensitivity_ln(
      est_eff = est_eff,
      std_err = std_err,
      n_obs = n_obs,
      n_covariates = n_covariates,
      alpha = alpha,
      tails = tails,
      nu = nu,
      to_return = to_return,
      n_trm = n_trm,
      switch_trm = switch_trm,
      replace = replace
    )
  } else if(!is.null(a)) {
    # error handling
    if (is.null(a) | is.null(b) | is.null(c) | is.null(d)) {
      stop("Please enter values for a, b, c, and d to use the 2 x 2 table functionality")
    }
    
    out <- tkonfound(a = a, 
                     b = b, 
                     c =c, 
                     d = d, 
                     alpha = alpha, 
                     switch_trm = switch_trm,
                     test = test, 
                     replace = replace,
                     to_return = to_return)
    
  } else if(!is.null(two_by_two_table)) {
    
    a <- dplyr::pull(two_by_two_table[1, 1])
    b <- dplyr::pull(two_by_two_table[1, 2])
    c <- dplyr::pull(two_by_two_table[2, 1])
    d <- dplyr::pull(two_by_two_table[2, 2])
    
    out <- tkonfound(a = a, 
                     b = b, 
                     c = c, 
                     d = d, 
                     alpha = alpha, 
                     switch_trm = switch_trm,
                     test = test, 
                     replace = replace,
                     to_return = to_return)
  
  } else {
  out <- test_sensitivity(
    est_eff = est_eff,
    std_err = std_err,
    n_obs = n_obs,
    n_covariates = n_covariates,
    alpha = alpha,
    tails = tails,
    nu = nu,
    to_return = to_return
  )
}

if (!is.null(out)) { # dealing with a strange print issue
  return(out)
}

if (to_return == "print") {
  message("For other forms of output, run ?pkonfound and inspect the to_return argument")
}

message("For models fit in R, consider use of konfound().")
}
