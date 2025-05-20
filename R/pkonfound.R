#' Perform sensitivity analysis for published studies
#'
#' For published studies, this command calculates (1) how much bias there must be in an estimate to nullify/sustain an inference; (2) the impact of an omitted variable necessary to nullify/sustain an inference for a regression coefficient.
#' For a full description of the command’s usage and additional examples, please refer to our \href{https://konfound-it.org/page/guide/}{practical guide}.
#' 
#' @param est_eff the estimated effect (e.g., an unstandardized beta coefficient or a group mean difference).
#' @param std_err the standard error of the estimate of the unstandardized regression coefficient.
#' @param n_obs the number of observations in the sample.
#' @param n_covariates the number of covariates in the regression model.
#' @param alpha the probability of rejecting the null hypothesis (defaults to 0.05).
#' @param tails integer indicating if the test is one-tailed (1) or two-tailed (2; defaults to 2).
#' @param index specifies whether output is RIR or IT (impact threshold); defaults to \code{"RIR"}.
#' @param nu specifies the hypothesis to be tested; defaults to testing whether \code{est_eff} is significantly different from 0.
#' @param sdx the standard deviation of X (used for unconditional ITCV).
#' @param sdy the standard deviation of Y (used for unconditional ITCV).
#' @param R2 the unadjusted, original \eqn{R^2} in the observed function (used for unconditional ITCV).
#' @param far_bound indicates whether the estimated effect is moved to the boundary closer (0, default) or further away (1).
#' @param eff_thr for RIR: the unstandardized coefficient threshold to change an inference; for IT: the correlation defining the threshold for inference.
#' @param FR2max the largest \eqn{R^2} (or \eqn{R^2_{\max}}) in the final model with an unobserved confounder (used for COP).
#' @param FR2max_multiplier the multiplier applied to \eqn{R^2} to derive \eqn{R^2_{\max}}; defaults to 1.3 (used for COP).
#' @param upper_bound optional (replaces \code{est_eff}); the upper bound of the confidence interval.
#' @param lower_bound optional (replaces \code{est_eff}); the lower bound of the confidence interval.
#' @param n_treat the number of cases associated with the treatment condition (for logistic regression models).
#' @param replace specifies whether to use the entire sample (\code{"entire"}) or the control group (\code{"control"}) for calculating the base rate; default is \code{"control"}.
#' @param switch_trm indicates whether to switch the treatment and control cases; defaults to \code{FALSE}.
#' @param raw_treatment_success optional; the unadjusted count of successful outcomes in the treatment group for calculating the specific RIR benchmark.
#' @param model_type the type of model being estimated; defaults to \code{"ols"} for a linear regression model or \code{"logistic"} for a logistic regression model.
#' @param a the number of cases in the control group showing unsuccessful results (2x2 table model).
#' @param b the number of cases in the control group showing successful results (2x2 table model).
#' @param c the number of cases in the treatment group showing unsuccessful results (2x2 table model).
#' @param d the number of cases in the treatment group showing successful results (2x2 table model).
#' @param two_by_two_table a table (matrix, data.frame, tibble, etc.) from which \code{a}, \code{b}, \code{c}, and \code{d} can be extracted.
#' @param test specifies whether to use Fisher's Exact Test (\code{"fisher"}) or a chi-square test (\code{"chisq"}); defaults to \code{"fisher"}.
#' @param to_return specifies the output format: \code{"print"} (default) to display output, \code{"plot"} for a plot, or \code{"raw_output"} to return a data.frame for further analysis.

#' @details
#' The function accepts arguments depending on the type of model:
#'
#' \strong{Linear Models (index: RIR, ITCV, PSE, COP)}
#' \itemize{
#'   \item est_eff, std_err, n_obs, n_covariates, alpha, tails, index, nu
#'   \item sdx, sdy, R2, far_bound, eff_thr, FR2max, FR2max_multiplier
#'   \item upper_bound, lower_bound
#' }
#'
#' \strong{Logistic Regression Model}
#' \itemize{
#'   \item est_eff, std_err, n_obs, n_covariates, n_treat, alpha, tails, nu
#'   \item replace, switch_trm, raw_treatment_success, model_type
#' }
#'
#' \strong{2x2 Table Model (Non-linear)}
#' \itemize{
#'   \item a, b, c, d, two_by_two_table, test, replace, switch_trm
#' }
#'
#' @section Values:
#' pkonfound prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to nullify the inference. If \code{to_return = "raw_output"}, a list is returned with the following components:
#' \subsection{RIR & ITCV for linear model}{
#' \describe{
#'   \item{\code{obs_r}}{correlation between predictor of interest (X) and outcome (Y) in the sample data}
#'   \item{\code{act_r}}{correlation between predictor of interest (X) and outcome (Y) from the sample regression based on the t-ratio accounting for non-zero null hypothesis}
#'   \item{\code{critical_r}}{critical correlation value at which the inference would be nullified (e.g., associated with p=.05)}
#'   \item{\code{r_final}}{final correlation value given confounding variable (CV). Should be equal to \code{critical_r}}
#'   \item{\code{rxcv}}{unconditional \eqn{corr(X,CV)} necessary to nullify the inference for smallest impact}
#'   \item{\code{rycv}}{unconditional \eqn{corr(Y,CV)} necessary to nullify the inference for smallest impact}
#'   \item{\code{rxcvGz}}{\eqn{corr(X,CV|Z)} conditioning on all observed covariates}
#'   \item{\code{rycvGz}}{\eqn{corr(Y,CV|Z)} conditioning on all observed covariates}
#'   \item{\code{itcv}}{unconditional ITCV (\code{uncond_rxcv * uncond_rycv})}
#'   \item{\code{itcvGz}}{conditional ITCV given all observed covariates}
#'   \item{\code{r2xz}}{\eqn{R^2} using all observed covariates to explain the predictor of interest (X)}
#'   \item{\code{r2yz}}{\eqn{R^2} using all observed covariates to explain the predictor of interest (Y)}
#'   \item{\code{beta_threshold}}{threshold for for estimated effect}
#'   \item{\code{beta_threshold_verify}}{verified threshold matching \code{beta_threshold}}
#'   \item{\code{perc_bias_to_change}}{percent bias to change inference}
#'   \item{\code{RIR_primary}}{Robustness of Inference to Replacement (RIR)}
#'   \item{\code{RIR_supplemental}}{RIR for an extra row or column that is needed to nullify the inference}
#'   \item{\code{RIR_perc}}{RIR as \% of total sample (for linear regression) or as \% of data points in the cell where replacement takes place (for logistic and 2 by 2 table)}
#'   \item{\code{Fig_ITCV}}{ITCV plot object}
#'   \item{\code{Fig_RIR}}{RIR threshold plot object}
#' }
#' }
#'
#' \subsection{COP for linear model}{
#' \describe{
#'   \item{\code{delta*}}{delta calculated using Oster’s unrestricted estimator}
#'   \item{\code{delta*restricted}}{delta calculated using Oster’s restricted estimator}
#'   \item{\code{delta_exact}}{delta calculated using correlation-based approach}
#'   \item{\code{delta_pctbias}}{percent bias when comparing \code{delta*} to \code{delta_exact}}
#'   \item{\code{var(Y)}}{variance of the dependent variable (\eqn{\sigma_Y^2})}
#'   \item{\code{var(X)}}{variance of the independent variable (\eqn{\sigma_X^2})}
#'   \item{\code{var(CV)}}{variance of the confounding variable (\eqn{\sigma_{CV}^2})}
#'   \item{\code{cor_oster}}{correlation matrix implied by \code{delta*}}
#'   \item{\code{cor_exact}}{correlation matrix implied by \code{delta_exact}}
#'   \item{\code{eff_x_M3_oster}}{effect estimate for X under the Oster‑PSE variant}
#'   \item{\code{eff_x_M3}}{effect estimate for X under the PSE adjustment}
#'   \item{\code{Table}}{formatted results table}
#'   \item{\code{Figure}}{COP diagnostic plot}
#' }
#' }
#' 
#' \subsection{PSE for linear model}{
#' \describe{
#'   \item{\code{corr(X,CV|Z)}}{correlation between X and CV conditional on Z}
#'   \item{\code{corr(Y,CV|Z)}}{correlation between Y and CV conditional on Z}
#'   \item{\code{corr(X,CV)}}{correlation between X and CV}
#'   \item{\code{corr(Y,CV)}}{correlation between X and CV}
#'   \item{\code{covariance matrix}}{covariance matrix among Y, X, Z, and CV under the PSE adjustment}
#'   \item{\code{eff_M3}}{estimated unstandardized regression coefficient for X in M3 under the PSE adjustment}
#'   \item{\code{se_M3}}{standard error of that coefficient in M3 under the PSE adjustment}
#'   \item{\code{Table}}{matrix summarizing key statistics from three nested regression models (M1, M2, M3)}
#' }
#' }
#' 
#' \subsection{RIR for logistic model}{
#' \describe{
#'   \item{\code{RIR_primary}}{Robustness of Inference to Replacement (RIR)}
#'   \item{\code{RIR_supplemental}}{RIR for an extra row or column that is needed to nullify the inference}
#'   \item{\code{RIR_perc}}{RIR as \% of data points in the cell where replacement takes place}
#'   \item{\code{fragility_primary}}{Fragility; the number of switches (e.g., treatment success to treatment failure) to nullify the inference}
#'   \item{\code{fragility_supplemental}}{Fragility for an extra row or column that is needed to nullify the inference}
#'   \item{\code{starting_table}}{observed (implied) 2 by 2 table before replacement and switching}
#'   \item{\code{final_table}}{the 2 by 2 table after replacement and switching}
#'   \item{\code{user_SE}}{user-entered standard error}
#'   \item{\code{analysis_SE}}{the standard error used to generate a plausible 2 by 2 table}
#'   \item{\code{needtworows}}{indicator whether extra switches were needed}
#' }
#' }
#'
#' \subsection{RIR for 2×2 table model}{
#' \describe{
#'   \item{\code{RIR_primary}}{Robustness of Inference to Replacement (RIR)}
#'   \item{\code{RIR_supplemental}}{RIR for an extra row or column that is needed to nullify the inference}
#'   \item{\code{RIR_perc}}{RIR as \% of data points in the cell where replacement takes place}
#'   \item{\code{fragility_primary}}{Fragility; the number of switches (e.g., treatment success to treatment failure) to nullify the inference}
#'   \item{\code{fragility_supplemental}}{Fragility for an extra row or column that is needed to nullify the inference}
#'   \item{\code{starting_table}}{observed 2 by 2 table before replacement and switching}
#'   \item{\code{final_table}}{the 2 by 2 table after replacement and switching}
#'   \item{\code{needtworows}}{indicator whether extra switches were needed}
#' }
#' }
#'
#' @note 
#' For a thoughtful background on benchmark options for ITCV, see 
#' \href{https://academic.oup.com/jrsssb/article-abstract/82/1/39/7056023}{Cinelli & Hazlett (2020)}, 
#' \href{https://journals.sagepub.com/doi/pdf/10.1177/01492063241293126}{Lonati & Wulff (2024)}, 
#' and 
#' \href{https://journals.sagepub.com/doi/10.1177/0049124100029002001}{Frank (2000)}.
#' 
#' @examples
#' ## Linear models
#' pkonfound(2, .4, 100, 3)
#' pkonfound(-2.2, .65, 200, 3)
#' pkonfound(.5, 3, 200, 3)
#' pkonfound(-0.2, 0.103, 20888, 3, n_treat = 17888, model_type = "logistic")
#'
#' # using a confidence interval 
#' pkonfound(upper_bound = 3, lower_bound = 1, n_obs = 100, n_covariates = 3)
#'
#' pkonfound(2, .4, 100, 3, to_return = "thresh_plot")
#' pkonfound(2, .4, 100, 3, to_return = "corr_plot")
#'
#' ## Logistic regression model example
#' pkonfound(-0.2, 0.103, 20888, 3, n_treat = 17888, model_type = "logistic")
#' 
#' ## 2x2 table examples
#' pkonfound(a = 35, b = 17, c = 17, d = 38)
#' pkonfound(a = 35, b = 17, c = 17, d = 38, alpha = 0.01)
#' pkonfound(a = 35, b = 17, c = 17, d = 38, alpha = 0.01, switch_trm = FALSE)
#' pkonfound(a = 35, b = 17, c = 17, d = 38, test = "chisq")
#'
#' ## Advanced examples
#' # Calculating unconditional ITCV and benchmark correlation for ITCV
#' pkonfound(est_eff = .5, std_err = .056, n_obs = 6174, sdx = 0.22, sdy = 1, R2 = .3,
#'           index = "IT", to_return = "print")
#' # Calculating delta* and delta_exact 
#' pkonfound(est_eff = .4, std_err = .1, n_obs = 290, sdx = 2, sdy = 6, R2 = .7,
#'          eff_thr = 0, FR2max = .8, index = "COP", to_return = "raw_output")
#' # Calculating rxcv and rycv when preserving standard error
#' pkonfound(est_eff = .5, std_err = .056, n_obs = 6174, eff_thr = .1,
#'          sdx = 0.22, sdy = 1, R2 = .3, index = "PSE", to_return = "raw_output")
#' 
#' @export
#' 
#' @param est_eff the estimated effect (e.g., an unstandardized beta coefficient or a group mean difference).
#' @param std_err the standard error of the estimate of the unstandardized regression coefficient.
#' @param n_obs the number of observations in the sample.
#' @param n_covariates the number of covariates in the regression model.
#' @param alpha the probability of rejecting the null hypothesis (defaults to 0.05).
#' @param tails integer indicating if the test is one-tailed (1) or two-tailed (2; defaults to 2).
#' @param index specifies whether output is RIR or IT (impact threshold); defaults to \code{"RIR"}.
#' @param nu specifies the hypothesis to be tested; defaults to testing whether \code{est_eff} is significantly different from 0.
#' @param n_treat the number of cases associated with the treatment condition (for logistic regression models).
#' @param switch_trm indicates whether to switch the treatment and control cases; defaults to \code{FALSE}.
#' @param model_type the type of model; defaults to \code{"ols"}, but can be set to \code{"logistic"}.
#' @param a the number of cases in the control group showing unsuccessful results (2x2 table model).
#' @param b the number of cases in the control group showing successful results (2x2 table model).
#' @param c the number of cases in the treatment group showing unsuccessful results (2x2 table model).
#' @param d the number of cases in the treatment group showing successful results (2x2 table model).
#' @param two_by_two_table a table (matrix, data.frame, tibble, etc.) from which \code{a}, \code{b}, \code{c}, and \code{d} can be extracted.
#' @param test specifies whether to use Fisher's Exact Test (\code{"fisher"}) or a chi-square test (\code{"chisq"}); defaults to \code{"fisher"}.
#' @param replace specifies whether to use the entire sample (\code{"entire"}) or the control group (\code{"control"}) for calculating the base rate; default is \code{"control"}.
#' @param sdx the standard deviation of X (used for unconditional ITCV).
#' @param sdy the standard deviation of Y (used for unconditional ITCV).
#' @param R2 the unadjusted, original \eqn{R^2} in the observed function (used for unconditional ITCV).
#' @param far_bound indicates whether the estimated effect is moved to the boundary closer (0, default) or further away (1).
#' @param eff_thr for RIR: the unstandardized coefficient threshold to change an inference; for IT: the correlation defining the threshold for inference.
#' @param FR2max the largest \eqn{R^2} (or \eqn{R^2_{\max}}) in the final model with an unobserved confounder (used for COP).
#' @param FR2max_multiplier the multiplier applied to \eqn{R^2} to derive \eqn{R^2_{\max}}; defaults to 1.3 (used for COP).
#' @param to_return specifies the output format: \code{"print"} (default) to display output, \code{"plot"} for a plot, or \code{"raw_output"} to return a data.frame for further analysis.
#' @param upper_bound optional (replaces \code{est_eff}); the upper bound of the confidence interval.
#' @param lower_bound optional (replaces \code{est_eff}); the lower bound of the confidence interval.
#' @param raw_treatment_success optional; the unadjusted count of successful outcomes in the treatment group for calculating the specific RIR benchmark.
#' 
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
                      lower_bound = NULL,
                      raw_treatment_success = NULL
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
      replace = replace,
      raw_treatment_success = raw_treatment_success
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
