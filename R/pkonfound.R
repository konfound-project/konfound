#' Perform sensitivity analysis for published studies
#' @description For published studies, this command calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient.
#' @param est_eff the estimated effect (such as an unstandardized beta coefficient or a group mean difference)
#' @param std_err the standard error of the estimate of the unstandardized regression coefficient
#' @param n_obs the number of observations in the sample
#' @param n_covariates the number of covariates in the regression model
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @param index whether output is RIR or IT (impact threshold); defaults to "RIR"
#' @param nu what hypothesis to be tested; defaults to testing whether est_eff is significantly different from 0
#' @param model_type the type of model being estimated; defaults to "ols" for a linear regression model; the other option is "logistic"
#' @param n_treat the number of cases associated with the treatment condition; applicable only when model_type = "logistic"
#' @param switch_trm whether to switch the treatment and control cases; defaults to FALSE; applicable only when model_type = "logistic"
#' @param a cell is the number of cases in the control group showing unsuccessful results
#' @param b cell is the number of cases in the control group showing successful results
#' @param c cell is the number of cases in the treatment group showing unsuccessful results
#' @param d cell is the number of cases in the treatment group showing successful results
#' @param two_by_two_table table that is a matrix or can be coerced to one (data.frame, tibble, tribble) from which the a, b, c, and d arguments can be extracted
#' @param test whether using Fisher's Exact Test or A chi-square test; defaults to Fisher's Exact Test
#' @param replace whether using entire sample or the control group to calculate the base rate; default is the entire sample
#' @param sdx the standard deviation of X
#' @param sdy the standard deviation of Y
#' @param R2 the unadjusted,original R2 in the observed function
#' @param signsuppression whether the threshold has the same sign of the estimated effect; default is same direction (0); specify 1 to be opposite
#' @param eff_thr unstandardized coefficient threshold to change an inference
#' @param FR2max the largest R2, or R2max, in the final model with unobserved confounder 
#' @param FR2max_multiplier the multiplier of R2 to get R2max, default is set to 1.3
#' @param to_return whether to return a data.frame (by specifying this argument to equal "raw_output" for use in other analyses) or a plot ("plot"); default is to print ("print") the output to the console; can specify a vector of output to return
#' @importFrom stats fisher.test
#' @importFrom dplyr select
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference
#' @examples
#' # using pkonfound for linear models
#' pkonfound(2, .4, 100, 3)
#' pkonfound(-2.2, .65, 200, 3)
#' pkonfound(.5, 3, 200, 3)
#' pkonfound(-0.2, 0.103, 20888, 3, n_treat = 17888, model_type = "logistic")
#'
#' pkonfound(2, .4, 100, 3, to_return = "thresh_plot")
#' pkonfound(2, .4, 100, 3, to_return = "corr_plot")
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
#
#' # use pkonfound to calculate delta* and delta_exact 
#' pkonfound(est_eff = .4, std_err = .1, n_obs = 290, sdx = 2, sdy = 6, R2 = .7,
#'  eff_thr = 0, FR2max = .8, index = "COP", to_return = "raw_output")
#' # use pkonfound to calculate rxcv and rycv when preserving standard error
#' pkonfound(est_eff = .5, std_err = .056, n_obs = 6174, eff_thr = .1,
#' sdx = 0.22, sdy = 1, R2 = .3, index = "PSE", to_return = "raw_output")
#'
#' @export

 pkonfound <- function(est_eff,
                      std_err,
                      n_obs,
                      n_covariates = 1,
                      alpha = .05,
                      tails = 2,
                      index = "RIR",
                      nu = 0,
                      n_treat = NULL,
                      switch_trm = TRUE,
                      model_type = "ols",
                      a = NULL,
                      b = NULL,
                      c = NULL,
                      d = NULL,
                      two_by_two_table = NULL,
                      test = "fisher",
                      replace = "control",
                      sdx = NA,
                      sdy = NA,
                      R2 = NA,
                      signsuppression = 0,
                      ## by default is zero
                      ## alternative is one
                      eff_thr = NA,
                      FR2max = 0,
                      FR2max_multiplier = 1.3,
                      to_return = "print") {
  if ("table" %in% to_return) stop("a table can only be
                                   output when using konfound")
  
   if (index == "COP") {
     
     # if user does not specify eff_thr then set default as 0 
     if (is.na(eff_thr)) {eff_thr = 0}  
     
     out <- test_cop(
       est_eff = est_eff, # unstandardized
       std_err = std_err, # unstandardized
       n_obs = n_obs,
       n_covariates, # the number of z 
       sdx = sdx,
       sdy = sdy,
       R2 = R2, # NOT the adjusted R2, should be the original R2
       eff_thr = eff_thr, # this is the unstandardized version
       FR2max_multiplier = FR2max_multiplier,
       FR2max = FR2max, # NOT the adjusted R2, should be the original R2
       alpha = alpha, 
       tails = tails, 
       to_return = to_return)
  
   } else if (index == "PSE") {
     
     out <- test_pse(
       est_eff = est_eff,
       std_err = std_err,
       n_obs = n_obs,
       n_covariates = n_covariates, # the number of z
       sdx = sdx,
       sdy = sdy,
       R2 = R2,
       eff_thr = eff_thr,
       to_return = to_return
     )
     
   } else if (model_type == "logistic" & !is.null(n_treat)) {
    out <- test_sensitivity_ln(
      est_eff = est_eff,
      std_err = std_err,
      n_obs = n_obs,
      n_covariates = n_covariates,
      alpha = alpha,
      tails = tails,
      nu = nu,
      to_return = to_return,
      n_treat = n_treat,
      switch_trm = switch_trm,
      replace = replace
    )
  } else if(!is.null(a)) {
    # error handling
    if (is.null(a) | is.null(b) | is.null(c) | is.null(d)) {
      stop("Please enter values for a, b, c,
           and d to use the 2 x 2 table functionality")
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
  
  } else if (model_type == "ols") {
    
  out <- test_sensitivity(
    est_eff = est_eff,
    std_err = std_err,
    n_obs = n_obs,
    n_covariates = n_covariates,
    sdx = sdx,
    sdy = sdy,
    R2 = R2,
    alpha = alpha,
    tails = tails,
    index = index,
    nu = nu,
    signsuppression = signsuppression,
    eff_thr = eff_thr,
    to_return = to_return
  )
} 
      
if (!is.null(out)) { # dealing with a strange print issue
  return(out)
}

if (to_return == "print") {
  cat("\n")
  message("For other forms of output, run
          ?pkonfound and inspect the to_return argument")
}

message("For models fit in R, consider use of konfound().")
}
