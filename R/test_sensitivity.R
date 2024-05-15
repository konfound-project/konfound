# helpers for the core sensitivity analysis function

create_konfound_class <- function(x) {
  structure(x, class = "konfound")
}

# Main function to test sensitivity to be wrapped with pkonfound(), konfound(), and mkonfound()

test_sensitivity <- function(est_eff,
                             std_err,
                             n_obs,
                             n_covariates,
                             sdx = NA,
                             sdy = NA,
                             R2 = NA,
                             alpha = 0.05,
                             tails = 2,
                             index,
                             nu = 0, # null hypothesis
                             signsuppression = 0,
                             ## signsuprresion  means towards the other side of nu
                             ## by default is zero
                             ## alternative is one
                             eff_thr = NA, # another non-zero and arbitrary threshold in terms of beta
                             to_return,
                             model_object,
                             tested_variable) {

  ## warning messages for potential confusion
  if (signsuppression == 1) warning("signsuppression is defined by a threshold of opposite sign of the estimated effect.")
 
  if (!is.na(eff_thr) & nu != 0) {
      nu <- 0
      warning("Cannot test statistical significance from nu and evaluate relative to a specific threshold. Using the specified threshold for calculations and ignoring nu.")
  }
    
    if (!is.na(eff_thr) & index == "RIR") {
        warning("Interpreting the metric of the threshold in the metric of the estimated effect because you specified RIR.")
    } 

    if (!is.na(eff_thr) & index == "IT") {
        warning("Interpreting the effect threshold as a correlation because you specified ITCV. Future work will allow for thresholds in raw metric.")
    } 

    
  ## error message if input is inappropriate
  if (!(std_err > 0)) {stop("Did not run! Standard error needs to be greater than zero.")}
  if (!(n_obs > n_covariates + 3)) {stop("Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It.")}
  if ((!is.na(sdx) | !is.na(sdy) | !is.na(R2)) & (!((!is.na(sdx) & !is.na(sdy) & !is.na(R2))))) {
    stop("Did not run! Info regarding sdx, sdy and R2 are all needed to generate unconditional ITCV.")
  }

  # calculate critical_t (based on nu, not considering eff_thr)
  if (est_eff < nu) {
     critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 2) * -1
  } else {
     critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 2)
  }

  # create CI centered nu (if user does not specify eff_thr)   
  if (is.na(eff_thr)) {
      UPbound <- nu + abs(critical_t * std_err)
      LWbound <- nu - abs(critical_t * std_err)
  }
  
  # determine beta_threshold
    ## if user does not specify eff_thr   
  if (is.na(eff_thr)) {
      # if signsuppression == 1, then the opposite
      if (est_eff < nu) {
          beta_threshold <- (signsuppression == 0) * LWbound +
              (signsuppression == 1) * UPbound
      }
      if (est_eff >= nu) {
          beta_threshold <- (signsuppression == 1) * LWbound +
              (signsuppression == 0) * UPbound
      }
  } 

    ## if user specifies eff_thr    
  if (!is.na(eff_thr)) {
      if (est_eff < 0) {
          beta_threshold <- (signsuppression == 0) * (-1) * abs(eff_thr) +
              (signsuppression == 1) * abs(eff_thr)
      }
      if (est_eff >= 0) {
          beta_threshold <- (signsuppression == 1) * (-1) * abs(eff_thr) +
              (signsuppression == 0) * abs(eff_thr)
      }
  }


  # I. for RIR
  # right now calculation in terms of effect size (not correlation)
  # later if switch to correlation could do A D for signsuppression as well
  ## using -1 and +1 in the replacement

  # calculating percentage of effect and number of observations to sustain or invalidate inference
  if (est_eff * beta_threshold > 0) {
      if (abs(est_eff) > abs(beta_threshold)) {
        perc_to_change <- bias <- 100 * (1 - (beta_threshold / est_eff))
        recase <- round(n_obs * (bias / 100))
      } else if (abs(est_eff) < abs(beta_threshold)) {
        perc_to_change <- sustain <- 100 * (1 - (est_eff / beta_threshold))
        recase <- round(n_obs * (sustain / 100))
      } else if (est_eff == beta_threshold) {
        stop("The coefficient is exactly equal to the threshold.")
      }
  }
  
  ## error message when eff_thr and beta_threshold are at two sides of zero
  if (est_eff * beta_threshold < 0 & index == "RIR") {
      stop(sprintf("The condition you specified implies a threshold of %.3f. Cannot calculate RIR because replacement values would need to be arbitrarily more extreme than the threshold (%.3f) to achieve the threshold value. Consider using ITCV.", 
                   beta_threshold, beta_threshold))
  }

  ## error message when eff_thr == 0 
  if (beta_threshold == 0 & index == "RIR") {
      stop("The condition you specified implies a threshold of 0. Therefore, 100% of the data points would have to be replaced with data points with an effect of 0 to reduce the estimate to 0. If you would like to use a threshold based on statistical significance for a null hypothesis of 0 then do not specify an eff_thr value but instead specify nu value.")
  }
  
  ## error message when est_eff == 0
  if (est_eff == 0 & index == "RIR") {
      stop("The estimated effect is 0. Cannot modify the effect by replacing it with cases for which the effect is also 0.")
  }
  
  ## verify results 
  if (abs(est_eff) > abs(beta_threshold)) {
      beta_threshold_verify = perc_to_change / 100 * 0 + (1 - perc_to_change / 100) * est_eff
  } 
  if (abs(est_eff) < abs(beta_threshold)) {
        beta_threshold_verify = est_eff / (1 - perc_to_change / 100)
  }
  ## compare beta_threshold_verify with beta_threshold

  ## later when we introduce non-zero replacement 
  ## signsuppression == 1 and user specifies nu (statistical significance)
  ## if (signsuppression == 1 & is.na(eff_thr)) {
  ##  if ((est_eff < LWbound | est_eff > UPbound) & index == "RIR")  {
  ##          stop(sprintf(
  ##          "Cannot calculate RIR because replacement values would need to be arbitrarily more extreme \nthan the threshold (%.3f) to achieve the threshold value. Consider using ITCV.", beta_threshold))
  ##      }
  ##        if (est_eff >= LWbound & est_eff < nu) {
  ##          ## B case
  ##          ## consider est_eff as a combination of pi*LB+(1-pi)*UB
  ##          perc_to_change <- bias <- 100 * (UPbound - est_eff)/(UPbound - LWbound)
  ##          recase <- round(n_obs * (bias / 100))
  ##      }
  ##    if (est_eff >= nu & est_eff <= UPbound) {
  ##          ## C case
  ##          ## consider est_eff as a combination of pi*UB+(1-pi)*LB
  ##          perc_to_change <- bias <- 100 * (est_eff - LWbound)/(UPbound - LWbound)
  ##          recase <- round(n_obs * (bias / 100))
  ##      }
  ## }
    
 
  # II. for correlation-based approach

  # transforming t into obs_r
  obs_r <- (est_eff / std_err) / sqrt(((n_obs - n_covariates - 2) + ((est_eff / std_err)^2)))

  # finding critical r
  if (is.na(eff_thr)) {
    critical_r <- critical_t / sqrt((critical_t^2) + (n_obs - n_covariates - 2))
    # critical_t follows the direction of relative position of est_eff and nu
    # so critical_r should follow the same
        if (signsuppression == 1) {
        critical_r <- critical_r * (-1)
    }
  } 

  if (!is.na(eff_thr)) {
      critical_r <- eff_thr
  } 
  
  ## later: use sdx and sdy to calculate critical_r based on eff_thr
  ## assuming eff_thr is in terms of effect size 

  # calculating actual t and r (to account for non-zero nu)
  act_t <- (est_eff - nu)/std_err
  act_r <- act_t / sqrt(act_t^2 + n_obs - n_covariates - 2)
  
  # determine mp
  if (is.na(eff_thr)) {
      # user specified suppression argument should NOT impact this
      ##### qqqq when mp == 1, actually mp may be affected need to try both ways
      if ((est_eff > LWbound) & (est_eff < UPbound)) {mp <- 1}
      if (est_eff < LWbound | est_eff > UPbound) {mp <- -1}
  }
  
  if (!is.na(eff_thr)) {
      # determine mp
      # user specified suppression argument should NOT impact this
      ##### qqqq when mp == 1, actually mp may be affected need to try both ways
      if (abs(act_r) < abs(eff_thr)) {mp <- 1}
      if (abs(act_r) > abs(eff_thr)) {mp <- -1}
  }    
  
  # determine signITCV
  if (is.na(eff_thr)){
      if (est_eff < beta_threshold) {signITCV <- -1}
      if (est_eff > beta_threshold) {signITCV <- 1}
      if (est_eff == beta_threshold) {signITCV <- 0}
  }
  
  if (!is.na(eff_thr)) {
      if (act_r < eff_thr) {signITCV <- -1}
      if (act_r > eff_thr) {signITCV <- 1}
      if (act_r == eff_thr) {signITCV <- 0}
  }
  
  # calculating impact of the confounding variable
  itcv <- signITCV * abs(act_r - critical_r) / (1 + mp * abs(critical_r))

  # finding correlation of confound to invalidate / sustain inference
  r_con <- sqrt(abs(itcv))
  
  # error message if r_con >= 1
  if (r_con >= 1 & index == "IT") {
      stop("To achieve the threshold the absolute value of the correlations associated with the omitted confounding variable would have to be greater than or equal to one.")
  } 

  ## calculate the unconditional ITCV if user inputs sdx, sdy and R2
  if (!is.na(sdx) & !is.na(sdy) & !is.na(R2) & (n_covariates > 0)) {
    ## pull in the auxiliary function for R2yz and R2xz
    r2yz <- cal_ryz(obs_r, R2)^2
    uncond_rycv <- r_con * sqrt(1 - r2yz)
    r2xz <- cal_rxz(sdx^2, sdy^2, R2, n_obs-n_covariates-2, std_err)^2
    uncond_rxcv <- r_con * sqrt(1 - r2xz)
  } else if (n_covariates == 0) {
      r2yz <- r2xz <- NA
      uncond_rycv <- r_con
      uncond_rxcv <- r_con
      } else {
          r2yz <- uncond_rycv <- r2xz <- uncond_rxcv <- NA
          }

  uncond_rycv <- uncond_rycv * signITCV
  rycvGz <- r_con * signITCV
  rxcvGz <- r_con
  itcvGz <- itcv # conditional ITCV

  # verify result
  # act_r <- act_t / sqrt(act_t^2 + n_obs - n_covariates - 2)
  ## calculate act_r using one less df or maybe -1 instead
  act_r_forVF <- act_t / sqrt(act_t^2 + n_obs - n_covariates - 2)
  r_final <- (act_r_forVF - r_con * rycvGz)/sqrt((1 - r_con^2) * (1 - rycvGz^2))
  ## compare r_final with critical_r

  # output dispatch

  if (length(to_return) > 1) {
    to_return <- to_return[!(to_return == "print")]

    konfound_output <- purrr::map(
      to_return,
      ~ test_sensitivity(
        est_eff = est_eff,
        std_err = std_err,
        n_obs = n_obs,
        n_covariates = n_covariates,
        alpha = alpha,
        tails = tails,
        nu = nu,
        to_return = .
      )
    )
    konfound_output <- create_konfound_class(konfound_output)
    names(konfound_output) <- to_return
    output_print(n_covariates, est_eff, beta_threshold, bias, 
                 sustain, nu, eff_thr, recase, obs_r, critical_r, 
                 r_con, itcv, alpha, index)

    cat("\n")
    message(paste("Print output created by default. Created", length(konfound_output), "other forms of output. Use list indexing or run summary() on the output to see how to access."))

    return(konfound_output)
  }

  else if (to_return == "raw_output") {

    return(output_list(obs_r,
                       act_r,
                       # act_r only makes sense when nu!=0
                       critical_r, r_final = r_final,
                       # rxcv always be positive, rycv goes with itcv
                       rxcv = uncond_rxcv, rycv = uncond_rycv,
                       rxcvGz = rxcvGz, rycvGz = rycvGz,
                       itcvGz = itcvGz, itcv = uncond_rxcv * uncond_rycv,
                       r2xz = r2xz, r2yz = r2yz,
                       delta_star = NA, delta_star_restricted = NA,
                       delta_exact = NA, delta_pctbias = NA,
                       cor_oster = NA, cor_exact = NA,
                       beta_threshold = beta_threshold,
                       beta_threshold_verify = beta_threshold_verify,
                       perc_bias_to_change = perc_to_change,
                       RIR_primary = recase, RIR_supplemental = NA, RIR_perc = perc_to_change,
                       fragility_primary = NA, fragility_supplemental = NA,
                       starting_table = NA, final_table = NA,
                       user_SE = NA, analysis_SE = NA,
                       Fig_ITCV =
                         plot_correlation(r_con = r_con, obs_r = obs_r, critical_r = critical_r),
                       Fig_RIR = plot_threshold(beta_threshold = beta_threshold, est_eff = est_eff)))
  } else if (to_return == "thresh_plot") { # this still makes sense for NLMs (just not quite as accurate)
    return(plot_threshold(beta_threshold = beta_threshold, est_eff = est_eff))
  } else if (to_return == "corr_plot") {
    return(plot_correlation(r_con = r_con, obs_r = obs_r, critical_r = critical_r))
  } else if (to_return == "print") {
    return(output_print(n_covariates, est_eff, beta_threshold, bias, sustain, nu, eff_thr, recase, obs_r, critical_r, r_con, itcv, alpha, index, signsuppression))
  } else if (to_return == "table") {
    return(output_table(model_object, tested_variable))
  } else {
    stop("to_return must be set to 'raw_output', 'print', 'table', 'thresh_plot', or 'corr_plot' or some combination thereof")
  }
}
