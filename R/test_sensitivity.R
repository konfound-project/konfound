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
                             far_bound = 0,
                             ## far_bound means towards the further side
                             ## by default is zero
                             ## alternative is one
                             eff_thr = NA, # another non-zero and arbitrary threshold in terms of beta
                             to_return,
                             model_object,
                             tested_variable) {

  ## warning messages for potential confusion
  if (far_bound == 1) warning("far_bound is defined by whether the estimated effect is moved to the boundary closer(0) or further away(1).")
 
  if (!is.na(eff_thr) & nu != 0) {
      nu <- 0
      warning("Cannot test statistical significance from nu and evaluate relative to a\n specific threshold. Using the specified threshold for calculations and\n ignoring nu.")
  }
    
    if (!is.na(eff_thr) & index == "RIR") {
        warning("Interpreting the metric of the threshold in the metric of the estimated\n effect because you specified RIR.")
    } 

    if (!is.na(eff_thr) & index == "IT") {
        warning("Interpreting the effect threshold as a correlation because you specified ITCV.\n Future work will allow for thresholds in raw metric.")
    } 

    
  ## error message if input is inappropriate
  if (!(std_err > 0)) {stop("Did not run! Standard error needs to be greater than zero.")}
  if (!(n_obs > n_covariates + 3)) {stop("Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It.")}
  if ((!is.na(sdx) | !is.na(sdy) | !is.na(R2)) & (!((!is.na(sdx) & !is.na(sdy) & !is.na(R2))))) {
    stop("Conditional ITCV does not require R2, sdx, or sdy. Rerun without any of three\n if all you seek is the conditional ITCV. If you also want the unconditional ITCV\n then include sdy and sdx as well as R2.")
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
      # if far_bound == 1, then the opposite
      if (est_eff < nu) {
          beta_threshold <- (far_bound == 0) * LWbound +
              (far_bound == 1) * UPbound
      }
      if (est_eff >= nu) {
          beta_threshold <- (far_bound == 1) * LWbound +
              (far_bound == 0) * UPbound
      }
  } 

    ## if user specifies eff_thr    
  if (!is.na(eff_thr)) {
      if (est_eff < 0) {
          beta_threshold <- (far_bound == 0) * (-1) * abs(eff_thr) +
              (far_bound == 1) * abs(eff_thr)
      }
      if (est_eff >= 0) {
          beta_threshold <- (far_bound == 1) * (-1) * abs(eff_thr) +
              (far_bound == 0) * abs(eff_thr)
      }
  }


  # I. for RIR
  # right now calculation in terms of effect size (not correlation)
  # later if switch to correlation could do A D for far_bound as well
  ## using -1 and +1 in the replacement

  # calculating percentage of effect and number of observations to sustain or invalidate inference
  if (est_eff * beta_threshold >= 0) {
      if (abs(est_eff) > abs(beta_threshold)) {
        perc_to_change <- bias <- 100 * (1 - (beta_threshold / est_eff))
        recase <- round(n_obs * (bias / 100))
      } else if (abs(est_eff) < abs(beta_threshold)) {
        perc_to_change <- sustain <- 100 * (1 - (est_eff / beta_threshold))
        recase <- round(n_obs * (sustain / 100))
      } 
  }
    
  if (exists("perc_to_change") == FALSE) {perc_to_change <- -999}
  if (exists("recase") == FALSE) {recase <- -999}
    
 if (est_eff == beta_threshold & index == "RIR") {
     stop("The estimated effect equals the threshold value. Therefore no omitted variable is needed to make them equal.")
 }
  
  ## error message when eff_thr and beta_threshold are at two sides of zero
  if (est_eff * beta_threshold < 0 & index == "RIR") {
      stop(sprintf("The condition you specified implies a threshold of %.3f. Cannot calculate\n RIR because replacement values would need to be arbitrarily more extreme\n than the threshold (%.3f) to achieve the threshold value. Consider using ITCV.", 
                   beta_threshold, beta_threshold))
  }

  ## error message when eff_thr == 0 
  if (beta_threshold == 0 & index == "RIR") {
      stop("The condition you specified implies a threshold of 0. Therefore, 100% of the\n data points would have to be replaced with data points with an effect of 0\n to reduce the estimate to 0. If you would like to use a threshold based on\n statistical significance for a null hypothesis of 0 then do not specify an\n eff_thr value but instead specify nu value.")
  }
  
  ## error message when est_eff == 0
  if (est_eff == 0 & index == "RIR") {
      stop("The estimated effect is 0. Cannot modify the effect by replacing it with cases\n for which the effect is also 0.")
  }
  
  ## verify results 
  if (est_eff * beta_threshold < 0 & index == "IT") {
      perc_to_change = 101 
      recase = n_obs + 1
      if (to_return == "raw_output") {
          warning("Ignore the following elements beta_threshold, beta_threshold_verify, perc_bias_to_change,\n RIR_primary and RIR_perc in the raw_output.")
      }
    }
    
  if (abs(est_eff) > abs(beta_threshold)) {
      beta_threshold_verify = perc_to_change / 100 * 0 + (1 - perc_to_change / 100) * est_eff
  } 
  if (abs(est_eff) < abs(beta_threshold)) {
        beta_threshold_verify = est_eff / (1 - perc_to_change / 100)
  }
  ## compare beta_threshold_verify with beta_threshold

  ## later when we introduce non-zero replacement 
  ## far_bound == 1 and user specifies nu (statistical significance)
  ## if (far_bound == 1 & is.na(eff_thr)) {
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
        if (far_bound == 1) {
        critical_r <- critical_r * (-1)
    }
  } 

  if (!is.na(eff_thr)) {
      critical_r <- eff_thr
  }
  
  if (abs(critical_r) > 1 & index == "IT") {
      stop("Effect threshold for ITCV is interpreted as a correlation. You entered a value\n that is greater than 1 in absolute value. Please convert your threshold to a\n correlation by multiplying by sdx/sdy. This will be addressed in future versions.")
  } 
  
  ## later: use sdx and sdy to calculate critical_r based on eff_thr
  ## assuming eff_thr is in terms of effect size 

  # calculating actual t and r (to account for non-zero nu)
  act_t <- (est_eff - nu)/std_err
  act_r <- act_t / sqrt(act_t^2 + n_obs - n_covariates - 2)
  
  # determine mp
  if (is.na(eff_thr)) {
      if ((est_eff > LWbound) & (est_eff < UPbound)) {mp <- 1} # this one does not matter if far_bound == 0 or 1 
      if (est_eff < LWbound | est_eff > UPbound) {mp <- -1}
  }
  
  if (!is.na(eff_thr)) {
      # determine mp
      # user specified suppression argument should NOT impact this
      ##### qqqq when mp == 1, actually mp may be affected need to try both ways
      if (abs(act_r) < abs(eff_thr)) {mp <- 1}
      if (abs(act_r) > abs(eff_thr)) {mp <- -1}
  } 
  
  # note this overwrites the two conditions above (for both !is.na(eff_thr) and is.na(eff_thr)) 
  if (far_bound == 1) {mp <- 1}
  
  if ((!is.na(eff_thr)) & (abs(act_r) == abs(eff_thr)) & (index == "IT")) {
      stop("The estimated effect equals the threshold value. Therefore no omitted variable\n is needed to make them equal.")
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
      stop("To achieve the threshold the absolute value of the correlations associated with\n the omitted confounding variable would have to be greater than or equal to one.")
  }
 
  ###### qqqq check all the output elements that should not be interpreted in this case 
  if (r_con >= 1 & to_return == "raw_output") {
      warning("ITCV would require correlations greater than 1, ignore the following elements in\n the raw output: critical_r, r_final, rxcv, rycv, rxcvGz, rycvGz, itcvGz and itcv.")
  }
 
  
  # warning message when r_con is larger than 0.999
  if (r_con >= 0.9995) {
      warning("The correlations associated with the omitted confounding variable neccessary\n to change the inference have an absolute value larger than or equal to 0.9995.\n Due to rounding, print output will show as 1. Check raw_ouput for the specific\n values. This is an unusually robust inference. Confirm your input values.")
  }

  ## calculate the unconditional ITCV if user inputs sdx, sdy and R2
  if (!is.na(sdx) & !is.na(sdy) & !is.na(R2) & (n_covariates > 0)) {
    ## pull in the auxiliary function for R2yz and R2xz
    tryCatch({
      r2yz <- cal_ryz(obs_r, R2)^2
      uncond_rycv <- r_con * sqrt(1 - r2yz)
      r2xz <- cal_rxz(sdx^2, sdy^2, R2, n_obs - n_covariates - 2, std_err)^2
      uncond_rxcv <- r_con * sqrt(1 - r2xz)
    }, error = function(e) {
      message(e$message)
      sdx <<- NA
      sdy <<- NA
      R2 <<- R2
      r2yz <<- uncond_rycv <<- r2xz <<- uncond_rxcv <<- NA
    })
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
    return(output_print(n_covariates, est_eff, beta_threshold, bias, sustain, nu, eff_thr, recase, obs_r, critical_r, r_con, itcv, alpha, index, far_bound, R2))
  } else if (to_return == "table") {
    return(output_table(model_object, tested_variable))
  } else {
    stop("to_return must be set to 'raw_output', 'print', 'table', 'thresh_plot', or 'corr_plot' or some combination thereof")
  }
}
