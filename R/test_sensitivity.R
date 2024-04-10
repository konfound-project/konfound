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

  ## error message if input is inappropriate

  if (!(std_err > 0)) {stop("Did not run! Standard error needs to be greater than zero.")}
  if (!(n_obs > n_covariates + 3)) {stop("Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It.")}
  if ((!is.na(sdx) | !is.na(sdy) | !is.na(R2)) & (!((!is.na(sdx) & !is.na(sdy) & !is.na(R2))))) {
    stop("Did not run! Info regarding sdx, sdy and R2 are all needed to generate unconditional ITCV.")
  }
  if ((sdx != NA) & (sdy != NA) & (R2 != NA) & (n_covariates == 0)){
    print("sdx and sdy and R2 are only used to calculate the unconditional ITCV when there are covariates included (number of covariates > 0).")  
  }
  
  # calculate critical_t
  if (est_eff < nu) {
     critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 2) * -1
  } else {
     critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 2)
  }

  # create CI centered nu
  UPbound <- nu + abs(critical_t * std_err)
  LWbound <- nu - abs(critical_t * std_err)

  # determine mp
  # qqq user specified suppression argument should NOT impact this
  ##### qqqqq when mp == 1, actually mp may be affected need to try both ways
  if ((est_eff > LWbound) & (est_eff < UPbound)) {mp <- 1}
  if (est_eff < LWbound | est_eff > UPbound) {mp <- -1}

  # determine beta_threshold
  # if signsuppression == 1, then the opposite
  if (est_eff < nu) {
      beta_threshold <- (signsuppression == 0) * LWbound +
          (signsuppression == 1) * UPbound
      }
  if (est_eff >= nu) {
      beta_threshold <- (signsuppression == 1) * LWbound +
          (signsuppression == 0) * UPbound
      }

  # determine signITCV
  if (est_eff < beta_threshold) {signITCV <- -1}
  if (est_eff > beta_threshold) {signITCV <- 1}
  if (est_eff == beta_threshold) {signITCV <- 0}

  # I. for RIR
  # right now calculation in terms of effect size (not correlation)
  # if swtich to correlation could do A D for signsuppression as well
  ## using -1 and +1 in the replacement

  # calculating percentage of effect and number of observations to sustain or invalidate inference
  if (signsuppression == 0) {
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

  if (signsuppression == 1) {
      if (est_eff < LWbound | est_eff > UPbound) {
          stop("Such scenarios do not make sense to consider RIR.")
      }
      if (est_eff >= LWbound & est_eff < nu) {
          ## B case
          ## consider est_eff as a combination of pi*LB+(1-pi)*UB
          perc_to_change <- bias <- 100 * (UPbound - est_eff)/(UPbound - LWbound)
          recase <- round(n_obs * (bias / 100))
      }
      if (est_eff >= nu & est_eff <= UPbound) {
          ## C case
          ## consider est_eff as a combination of pi*UB+(1-pi)*LB
          perc_to_change <- bias <- 100 * (est_eff - LWbound)/(UPbound - LWbound)
          recase <- round(n_obs * (bias / 100))
      }
  }

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
  } else if (is.na(sdx) & is.na(sdy)) {
      critical_r <- eff_thr
  } else {
      critical_r <- eff_thr * sdx / sdy
  }

  # calculating actual t and r (to account for non-zero nu)
  act_t <- (est_eff - nu)/std_err
  act_r <- act_t / sqrt(act_t^2 + n_obs - n_covariates - 2)

  # calculating impact of the confounding variable
  itcv <- signITCV * abs(act_r - critical_r) / (1 + mp * abs(critical_r))

  # finding correlation of confound to invalidate / sustain inference
  r_con <- sqrt(abs(itcv))

  ## calculate the unconditional ITCV if user inputs sdx, sdy and R2
  if (!is.na(sdx) & !is.na(sdy) & !is.na(R2) & (n_covariates > 0)) {
    ## pull in the auxiliary function for R2yz and R2xz
    r2yz <- cal_ryz(obs_r, R2)^2
    uncond_rycv <- r_con * sqrt(1 - r2yz)
    r2xz <- cal_rxz(sdx^2, sdy^2, R2, n_obs-n_covariates-2, std_err)^2
    uncond_rxcv <- r_con * sqrt(1 - r2xz)
  } else if (n_covariates == 0) {
      r2yz = r2xz = NA
      uncond_rycv <- r_con
      uncond_rxcv <- r_con
      } else {
          r2yz = uncond_rycv = r2xz = uncond_rxcv = NA
          }

  uncond_rycv = uncond_rycv * signITCV
  rycvGz = r_con * signITCV
  rxcvGz = r_con
  itcvGz = itcv # conditional ITCV

  # verify
  # act_r <- act_t / sqrt(act_t^2 + n_obs - n_covariates - 2)
  ## calculate act_r using one less df or maybe -1 instead
  act_r_forVF <- act_t / sqrt(act_t^2 + n_obs - n_covariates - 2)
  r_final = (act_r_forVF - r_con * rycvGz)/sqrt((1 - r_con^2) * (1 - rycvGz^2))

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
    output_print(est_eff, beta_threshold, bias, sustain, nu, eff_thr, recase, obs_r, critical_r, r_con, itcv, alpha, index)

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
    return(output_print(est_eff, beta_threshold, bias, sustain, nu, eff_thr, recase, obs_r, critical_r, r_con, itcv, alpha, index))
  } else if (to_return == "table") {
    return(output_table(model_object, tested_variable))
  } else {
    stop("to_return must be set to 'raw_output', 'print', 'table', 'thresh_plot', or 'corr_plot' or some combination thereof")
  }
}
