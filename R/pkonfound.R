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
#' @param replace whether using entire sample or the control group to calculate the base rate; default is control
#' @param sdx the standard deviation of X
#' @param sdy the standard deviation of Y
#' @param R2 the unadjusted, original R2 in the observed function
#' @param far_bound whether the estimated effect is moved to the boundary closer (default 0) or further away (1);
#' @param eff_thr for RIR: unstandardized coefficient threshold to change an inference; for IT: correlation defining the threshold for inference 
#' @param FR2max the largest R2, or R2max, in the final model with unobserved confounder 
#' @param FR2max_multiplier the multiplier of R2 to get R2max, default is set to 1.3
#' @param to_return whether to return a data.frame (by specifying this argument to equal "raw_output" for use in other analyses) or a plot ("plot"); default is to print ("print") the output to the console; can specify a vector of output to return
#' @param upper_bound optional (replaces the est_eff); the upper bound of the confidence interval
#' @param lower_bound optional (replaces the est_eff); the lowerxw bound of the confidence interval
#' @importFrom stats fisher.test
#' @importFrom dplyr select
#' @return pkonfound prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to nullify the inference. If to_return = "raw_output," a list will be given with the following components:
#' \describe{
#'   \item{obs_r}{correlation between predictor of interest (X) and outcome (Y) in the sample data.}
#'   \item{act_r}{correlation between predictor of interest (X) and outcome (Y) from the sample regression based on the t-ratio accounting for non-zero null hypothesis.}
#'   \item{critical_r}{critical correlation value at which the inference would be nullified (e.g., associated with p=.05).}
#'   \item{r_final}{final correlation value given CV. Should be equal to critical_r.}
#'   \item{rxcv}{correlation between predictor of interest (X) and CV necessary to nullify the inference for smallest impact.}
#'   \item{rycv}{correlation between outcome (Y) and CV necessary to nullify the inference for smallest impact.}
#'   \item{rxcvGz}{correlation between predictor of interest and CV necessary to nullify the inference for smallest impact conditioning on all observed covariates (given z).}  
#'   \item{rycvGz}{correlation between outcome and CV necessary to nullify the inference for smallest impact conditioning on all observed covariates (given z).}
#'   \item{itcvGz}{ITCV conditioning on the observed covariates.}
#'   \item{itcv}{Unconditional ITCV.}
#'   \item{r2xz}{R2 using all observed covariates to explain the predictor of interest (X).}
#'   \item{r2yz}{R2 using all observed covariates to explain the outcome (Y).}
#'   \item{delta_star}{delta calculated using Oster's unrestricted estimator.}
#'   \item{delta_star_restricted}{delta calculated using Oster's restricted estimator.}
#'   \item{delta_exact}{correlation-based delta.}
#'   \item{delta_pctbias}{percent of bias when comparing delta_star with delta_exact.}
#'   \item{cor_oster}{correlation matrix implied by delta_star.}
#'   \item{cor_exact}{correlation matrix implied by delta_exact.}
#'   \item{beta_threshold}{threshold value for estimated effect.}
#'   \item{beta_threshold_verify}{estimated effect given RIR. Should be equal to beta_threshold.}
#'   \item{perc_bias_to_change}{percent bias to change the inference.}
#'   \item{RIR_primary}{Robustness of Inference to Replacement (RIR).}
#'   \item{RIR_supplemental}{RIR for an extra row or column that is needed to nullify the inference.}
#'   \item{RIR_perc}{RIR as \% of total sample (for linear regression) or as \% of data points in the cell where replacement takes place (for logistic and 2 by 2 table).}
#'   \item{fragility_primary}{Fragility. the number of switches (e.g., treatment success to treatment failure) to nullify the inference.}
#'   \item{fragility_supplemental}{Fragility for an extra row or column that is needed to nullify the inference.} 
#'   \item{starting_table}{Observed 2 by 2 table before replacement and switching. Implied table for logistic regression.}
#'   \item{final_table}{The 2 by 2 table after replacement and switching.}
#'   \item{user_SE}{user entered standard error. Only applicable for logistic regression.}
#'   \item{needtworows}{whether double row switches are needed.}
#'   \item{analysis_SE}{the standard error used to generate a plausible 2 by 2 table. Only applicable for logistic regression.}
#'   \item{Fig_ITCV}{figure for ITCV.} 
#'   \item{Fig_RIR}{figure for RIR.}
#'   \item{cond_RIRpi_null}{Conditional RIR as \% of total sample if the replacement data points follow a null distribution.}
#'   \item{cond_RIRpi_fixedY}{Conditional RIR as \% of total sample if the replacement data points have a fixed value.}
#'   \item{cond_RIRpi_rxyz}{Conditional RIR as \% of total sample if the replacement data points satisfy rxy|Z = 0.}
#'   \item{cond_RIR_null}{Conditional RIR if the replacement data points follow a null distribution.}
#'   \item{cond_RIR_fixedY}{Conditional RIR if the replacement data points have a fixed value.}
#'   \item{cond_RIR_rxyz}{Conditional RIR if the replacement data points satisfy rxy|Z = 0.}
#' }  
#' @examples
#' # using pkonfound for linear models
#' pkonfound(2, .4, 100, 3)
#' pkonfound(-2.2, .65, 200, 3)
#' pkonfound(.5, 3, 200, 3)
#' pkonfound(-0.2, 0.103, 20888, 3, n_treat = 17888, model_type = "logistic")
#' pkonfound(upper_bound = 3, lower_bound = 1, 100, 3) # using a confidence interval 
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
                      far_bound = 0,
                      ## by default is zero
                      ## alternative is one
                      eff_thr = NA,
                      FR2max = 0,
                      FR2max_multiplier = 1.3,
                      to_return = "print",
                      upper_bound = NULL,
                      lower_bound = NULL
                      ) {
  if ("table" %in% to_return) stop("a table can only be
                                   output when using konfound")
     
     
    if (!is.null(upper_bound) & !is.null(lower_bound)) {
      est_eff = (upper_bound + lower_bound) / 2
      std_err = (est_eff - lower_bound) / qt(alpha / tails, n_obs - n_covariates, lower.tail = FALSE)
    }
  
   if (index == "COP") {
     
     # if user does not specify eff_thr then set default as 0 
     if (is.na(eff_thr)) {eff_thr <- 0}  
     
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
     
   } else if (index == "cRIR") {
  out <- test_cRIR(
    est_eff = est_eff,
    std_err = std_err,
    n_obs = n_obs,
    n_covariates = n_covariates,
    # sdx = sdx,
    # sdy = sdy,
    R2 = R2,
    alpha = alpha,
    tails = tails,
    to_return = to_return
  )
}else if (model_type == "logistic" & !is.null(n_treat)) {
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
    far_bound = far_bound,
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
