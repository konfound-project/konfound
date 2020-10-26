
# Main function to test sensitivity for non-linear models to be wrapped with pkonfound(), konfound(), and mkonfound()

test_sensitivity_ln <- function(est_eff,
                                std_err,
                                n_obs,
                                n_covariates,
                                n_trm,
                                switch_trm = T,
                                replace = "entire",
                                alpha,
                                tails,
                                nu,
                                to_return,
                                model_object,
                                tested_variable) {
  if (est_eff < 0) {
    thr_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 1) * -1
  } else {
    thr_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 1)
  }
  
  # stop message
  if (n_obs <= 0 || n_trm <= 0) {
    stop("Please enter positive integers for sample size and number of treatment group cases.")
  }
  if (n_obs <= n_trm) {
    stop("The total sample size should be larger than the number of treatment group cases.")
  }
  
  odds_ratio <- exp(est_eff)
  # n_trm is the number of observations in the treatment group (c+d)
  # n_cnt is the number of observations in the control group (a+b)
  n_cnt <- n_obs - n_trm
  # t_ob is the t value calculated based on observed estimate and standard error
  t_ob <- est_eff / std_err
  # invalidate_ob is true - the observed result is significant - we are invalidating the observed result
  invalidate_ob <- isinvalidate(thr_t, t_ob)
  # dcroddsratio_ob is true - our goal is to decrease the odds ratio
  dcroddsratio_ob <- isdcroddsratio(thr_t, t_ob)
  
  # to record the original treatment cases in case we need to adjust it
  user_ntrm <- n_trm
  # check if the implied table solution may contain imaginary numbers
  haveimaginary <- F
  changepi <- F
  # set the default value for whether we need and can adjust pi (ratio of treatment cases)
  # to remove the imaginary part
  keyimagin <- (4 + 4 * odds_ratio^2 + odds_ratio *
                  (-8 + 4 * n_obs * std_err^2 - n_obs * n_trm * std_err^4 + n_trm^2 * std_err^4))
  minimgain <- 4 + 4 * odds_ratio^2 + odds_ratio * (-8 + n_obs * std_err^2 * (4 - 0.25 * n_obs * std_err^2))
  keyx1 <- 4 + 4 * odds_ratio^2 + odds_ratio * (-8 + 4 * n_obs * std_err^2)
  if (keyimagin > 0) {
    haveimaginary <- T
    if (minimgain <= 0 && keyx1 > 0) {
      changepi <- T
      n_trm <- n_obs * get_pi(odds_ratio, std_err, n_obs, n_trm)
      n_cnt <- n_obs - n_trm
    } else {
      stop("Cannot generate a usable contingency table; Please consider using the Pearson's chi-squared approach (under development).")
    }
  }
  
  # a1, b1, c1, d1 are one solution for the 4 cells in the contingency table
  a1 <- get_a1_kfnl(odds_ratio, std_err, n_obs, n_trm)
  b1 <- n_cnt - a1
  c1 <- get_c1_kfnl(odds_ratio, std_err, n_obs, n_trm)
  d1 <- n_trm - c1
  
  # a2, b2, c2, d2 are the second solution for the 4 cells in the contingency table
  a2 <- get_a2_kfnl(odds_ratio, std_err, n_obs, n_trm)
  b2 <- n_cnt - a2
  c2 <- get_c2_kfnl(odds_ratio, std_err, n_obs, n_trm)
  d2 <- n_trm - c2
  
  # Differences between these two sets of solutions:
  ### a1 c1 are small while a2 c2 are large
  ### b1 d1 are large while b2 d2 are small
  ### the goal is to get fewest swithes to invalidate the inference
  ### remove the solution if one cell has fewerer than 5 cases or negative cells or nan cells
  check1 <- check2 <- TRUE
  if (!(n_cnt >= a1 && a1 >= 5 && n_cnt >= b1 && b1 >= 5 && n_trm >= c1 && c1 >= 5 && n_trm >= d1 && d1 >= 5)
      || is.nan(a1) || is.nan(b1) || is.nan(c1) || is.nan(d1)) {
    check1 <- FALSE
  }
  
  if (!(n_cnt >= a2 && a2 >= 5 && n_cnt >= b2 && b2 >= 5 && n_trm >= c2 && c2 >= 5 && n_trm >= d2 && d2 >= 5)
      || is.nan(a2) || is.nan(b2) || is.nan(c2) || is.nan(d2)) {
    check2 <- FALSE
  }
  
  if (check1) {
    table_bstart1 <- get_abcd_kfnl(a1, b1, c1, d1)
    solution1 <- getswitch(table_bstart1, thr_t, switch_trm, n_obs)
  }
  if (check2) {
    table_bstart2 <- get_abcd_kfnl(a2, b2, c2, d2)
    solution2 <- getswitch(table_bstart2, thr_t, switch_trm, n_obs)
  }
  if (!check1 && !check2) {
    stop("Cannot generate a usable contingency table!")
  }
  
  # get the number of switches for solutions that satisfy the requirements
  if (check1 && check2) {
    final_switch1 <- solution1$final_switch
    final_switch2 <- solution2$final_switch
    if (final_switch1 < final_switch2) {
      final_solution <- getswitch(table_bstart1, thr_t, switch_trm, n_obs)
    } else {
      final_solution <- getswitch(table_bstart2, thr_t, switch_trm, n_obs)
    }
  }
  
  if (check1 && !check2) {
    final_solution <- getswitch(table_bstart1, thr_t, switch_trm, n_obs)
  }
  
  if (!check1 && check2) {
    final_solution <- getswitch(table_bstart2, thr_t, switch_trm, n_obs)
  }
  
  final <- final_solution$final_switch
  a <- final_solution$table_start[1,1]
  b <- final_solution$table_start[1,2]
  c <- final_solution$table_start[2,1]
  d <- final_solution$table_start[2,2]
  
  if (switch_trm && dcroddsratio_ob) {
    transferway <- "treatment success to treatment failure,"
    RIR <- round(final/((a+c)/n_obs))*(replace=="entire") + round(final/(a/(a+b)))*(1-(replace=="entire"))
    RIRway <- "treatment success"
  }
  if (switch_trm && !dcroddsratio_ob) {
    transferway <- "treatment failure to treatment success,"
    RIR <- round(final/((b+d)/n_obs))*(replace=="entire") + round(final/(b/(a+b)))*(1-(replace=="entire"))
    RIRway <- "treatment failure"
  }
  if (!switch_trm && dcroddsratio_ob) {
    transferway <- "control failure to control success,"
    RIR <- round(final/((b+d)/n_obs))*(replace=="entire") + round(final/(b/(a+b)))*(1-(replace=="entire"))
    RIRway <- "control failure"
  }
  if (!switch_trm && !dcroddsratio_ob) {
    transferway <- "control success to control failure,"
    RIR <- round(final/((a+c)/n_obs))*(replace=="entire") + round(final/(a/(a+b)))*(1-(replace=="entire"))
    RIRway <- "control success"
  }
  
  if (final_solution$needtworows) {
    final_extra <- final_solution$final_extra
    if (switch_trm && dcroddsratio_ob) {
      transferway_extra <- "control failure to control success,"
      RIR_extra <- round(final_extra/((b+d)/n_obs))*(replace=="entire") + 
        round(final_extra/(b/(b+d)))*(1-(replace=="entire"))
      RIRway_extra <- "control failure"
    }
    if (switch_trm && !dcroddsratio_ob) {
      transferway_extra <- "control success to control failure,"
      RIR_extra <- round(final_extra/((a+c)/n_obs))*(replace=="entire") +
        round(final_extra/(a/(a+b)))*(1-(replace=="entire"))
      RIRway_extra <- "control success"
    }
    if (!switch_trm && dcroddsratio_ob) {
      transferway_extra <- "treatment success to treatment failure,"
      RIR_extra <- round(final_extra/((a+c)/n_obs))*(replace=="entire") +
        round(final_extra/(a/(a+b)))*(1-(replace=="entire"))
      RIRway_extra <- "treatment success"
    }
    if (!switch_trm && !dcroddsratio_ob) {
      transferway_extra <- "treatment failure to treatment success,"
      RIR_extra <- round(final_extra/((b+d)/n_obs))*(replace=="entire") +
        round(final_extra/(b/(b+d)))*(1-(replace=="entire"))
      RIRway_extra <- "treatment failure"
    }
  }
  
  if (invalidate_ob) {
    change <- "To invalidate the inference,"
  } else {
    if (est_eff >= 0) {
      change <- "To sustain an inference for a positive treatment effect,"
    } else {
      change <- "To sustain an inference for a negative treatment effect,"
    }
  }
  
  if (!final_solution$needtworows & final_solution$final_switch > 1) {
    conclusion1 <- paste(
      change, sprintf("you would need to replace %d", RIR), RIRway,
      sprintf("cases with null hypothesis cases (RIR = %d).", RIR),
      sprintf("This is equivalent to transferring %d", final_solution$final_switch), 
      c("cases from"), transferway, c("as shown, from the Implied Table to the Transfer Table.")
    )
  } else if (!final_solution$needtworows & final_solution$final_switch == 1) {
    conclusion1 <- paste(
      change, sprintf("you would need to replace %d", RIR), RIRway,
      sprintf("cases with null hypothesis cases (RIR = %d).", RIR),
      sprintf("This is equivalent to transferring %d", final_solution$final_switch), 
      c("case from"), transferway, c("as shown, from the Implied Table to the Transfer Table.")
    )
  } else {
    conclusion1 <- paste(
      change, c("only transferring cases from"), transferway,
      sprintf("is not enough. We also need to transfer %d cases from", final_solution$final_extra),
      transferway_extra, c("as shown, from the User-entered Table to the Transfer Table."),
      sprintf("This means we need to replace %d of", RIR), RIRway, 
      sprintf("with null hypothesis cases; and replace %d", RIR_extra), RIRway_extra, 
      c("with null hypothesis cases to change the inference.")
    )
  }
  
  conclusion2 <- sprintf(
    "For the Implied Table, we have an estimate of %.3f, with a standard error of %.3f and a t-ratio of %.3f.",
    final_solution$est_eff_start, final_solution$std_err_start, final_solution$t_start
  )
  conclusion3 <- sprintf(
    "For the Transfer Table, we have an estimate of %.3f, with a standard error of %.3f and a t-ratio of %.3f.",
    final_solution$est_eff_final, final_solution$std_err_final, final_solution$t_final
  )
  
  notice <- c("(Values have been rounded to the nearest integer. This may cause a little change to the estimated effect for the Implied Table.)")
  
  if (haveimaginary && changepi) {
    conclusion1 <- paste(sprintf(
      "In order to generate a usable implied contingency table, we change the number of treatment cases to %d (originally this number is %d).",
      final_solution$table_start[2, 1] + final_solution$table_start[2, 2], user_ntrm
    ), conclusion1)
  }
  
  if (final_solution$needtworows) {
    total_switch <- final_solution$final_switch+final_solution$final_extra
    total_RIR <- RIR + RIR_extra
  } else {
    total_switch <- final_solution$final_switch
    total_RIR <- RIR
  }
  
  
  result <- list(conclusion1,
                 Implied_Table = final_solution$table_start, notice, Transfer_Table = final_solution$table_final,
                 conclusion2, conclusion3, Implied_Estimate = final_solution$est_eff_start, Transfer_Estimate = final_solution$est_eff_final,
                 Implied_SE = final_solution$std_err_start, Transfer_SE = final_solution$std_err_final,
                 Implied_tratio = final_solution$t_start, Transfer_tratio = final_solution$t_final,
                 Taylor_predict = final_solution$taylor_pred, Percent_bias_predict = final_solution$perc_bias_pred,
                 total_RIR = total_RIR, total_switch = total_switch
  )
  
  
  # output dispatch
  if (to_return == "print") {
    message("Note that this output is from an approach for non-linear models that is developmental and unpublished")
    return(result)
  } else {
    message("Note that only printed output is presently available for non-linear models")
  }
  # else if (to_return == "raw_output") {
  #     return(output_df(est_eff, beta_threshold, est_eff, bias, sustain, recase, obs_r, critical_r, r_con, itcv))
  # } else if (to_return == "thresh_plot") { # this still makes sense for NLMs (just not quite as accurate)
  #     return(plot_threshold(beta_threshold = beta_threshold, est_eff = est_eff))
  # } else if (to_return == "table") {
  #     return(output_table(model_object, tested_variable))
  # } else if (to_return == "nl_table") {
  #     return(output_table(model_object, tested_variable))
  # } else {
  #     stop("to_return must be set to 'raw_output', 'print', 'table', 'thresh_plot', or 'corr_plot' or some combination thereof")
  # }
}
