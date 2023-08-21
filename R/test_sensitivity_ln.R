# Main function to test sensitivity for non-linear models to be wrapped with pkonfound(), konfound(), and mkonfound()

test_sensitivity_ln <- function(est_eff,
                                std_err,
                                n_obs,
                                n_covariates,
                                n_treat,
                                switch_trm = T,
                                replace = "control",
                                alpha,
                                tails,
                                nu,
                                to_return,
                                model_object,
                                tested_variable) {
  
  ## error message if input is inappropriate
  if (!(std_err > 0)) {stop("Did not run! Standard error needs to be greater than zero.")}
  if (!(n_obs > n_covariates + 3)) {stop("Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It.")}

  if (est_eff < 0) {
    thr_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 3) * -1
  } else {
    thr_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 3)
  }
  
  # stop message
  if (n_obs <= 0 || n_treat <= 0) {
    stop("Please enter positive integers for sample size and number of treatment group cases.")
  }
  if (n_obs <= n_treat) {
    stop("The total sample size should be larger than the number of treatment group cases.")
  }
  
  odds_ratio <- exp(est_eff)
  
  # updated approach to deal with imaginary
  minse <- sqrt((4 * n_obs + 
                   sqrt(16 * n_obs^2 + 4 * n_treat * (n_obs - n_treat) * 
                          ((4 + 4 * odds_ratio^2) / odds_ratio - 7.999999)))/
                  (2 * n_treat * (n_obs - n_treat)))
  # check if the implied table solution may contain imaginary numbers
  changeSE <- F
  if (std_err < minse) {
    haveimaginary <- T
    changeSE <- T
    user_std_err <- std_err
    std_err <- minse
  }
    
  # n_treat is the number of observations in the treatment group (c+d)
  # n_cnt is the number of observations in the control group (a+b)
  n_cnt <- n_obs - n_treat
  # t_ob is the t value calculated based on observed estimate and standard error
  t_ob <- est_eff / std_err
  # invalidate_ob is true - the observed result is significant - we are invalidating the observed result
  invalidate_ob <- isinvalidate(thr_t, t_ob)
  # dcroddsratio_ob is true - our goal is to decrease the odds ratio
  dcroddsratio_ob <- isdcroddsratio(thr_t, t_ob)
  
  # previous approach to deal with imaginary 
  # to record the original treatment cases in case we need to adjust it
  # user_ntrm <- n_treat
  # check if the implied table solution may contain imaginary numbers
  # haveimaginary <- F
  # changepi <- F
  # set the default value for whether we need and can adjust pi (ratio of treatment cases)
  # to remove the imaginary part
  # keyimagin <- (4 + 4 * odds_ratio^2 + odds_ratio *
  #                 (-8 + 4 * n_obs * std_err^2 - n_obs * n_treat * std_err^4 + n_treat^2 * std_err^4))
  # minimgain <- 4 + 4 * odds_ratio^2 + odds_ratio * (-8 + n_obs * std_err^2 * (4 - 0.25 * n_obs * std_err^2))
  # keyx1 <- 4 + 4 * odds_ratio^2 + odds_ratio * (-8 + 4 * n_obs * std_err^2)
  # if (keyimagin > 0) {
    # haveimaginary <- T
    # if (minimgain <= 0 && keyx1 > 0) {
      # changepi <- T
      # n_treat <- n_obs * get_pi(odds_ratio, std_err, n_obs, n_treat)
      # n_cnt <- n_obs - n_treat
  #  } else {
  #    stop("Cannot generate a usable contingency table; Please consider using the Pearson's chi-squared approach (under development).")
  #  }
  #}
  
  # a1, b1, c1, d1 are one solution for the 4 cells in the contingency table
  a1 <- get_a1_kfnl(odds_ratio, std_err, n_obs, n_treat)
  b1 <- n_cnt - a1
  c1 <- get_c1_kfnl(odds_ratio, std_err, n_obs, n_treat)
  d1 <- n_treat - c1
  
  # a2, b2, c2, d2 are the second solution for the 4 cells in the contingency table
  a2 <- get_a2_kfnl(odds_ratio, std_err, n_obs, n_treat)
  b2 <- n_cnt - a2
  c2 <- get_c2_kfnl(odds_ratio, std_err, n_obs, n_treat)
  d2 <- n_treat - c2
  
  # Differences between these two sets of solutions:
  ### a1 c1 are small while a2 c2 are large
  ### b1 d1 are large while b2 d2 are small
  ### the goal is to get fewest swithes to invalidate the inference
  ### remove the solution if one cell has fewerer than 5 cases or negative cells or nan cells
  check1 <- check2 <- TRUE
  if (!(n_cnt >= a1 && a1 >= 5 && n_cnt >= b1 && b1 >= 5 && n_treat >= c1 && c1 >= 5 && n_treat >= d1 && d1 >= 5)
      || is.nan(a1) || is.nan(b1) || is.nan(c1) || is.nan(d1)) {
    check1 <- FALSE
  }
  
  if (!(n_cnt >= a2 && a2 >= 5 && n_cnt >= b2 && b2 >= 5 && n_treat >= c2 && c2 >= 5 && n_treat >= d2 && d2 >= 5)
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
    RIR <- ceiling(final/((a+c)/n_obs))*(replace=="entire") + ceiling(final/(a/(a+b)))*(1-(replace=="entire"))
    RIRway <- "treatment success"
  }
  if (switch_trm && !dcroddsratio_ob) {
    transferway <- "treatment failure to treatment success,"
    RIR <- ceiling(final/((b+d)/n_obs))*(replace=="entire") + ceiling(final/(b/(a+b)))*(1-(replace=="entire"))
    RIRway <- "treatment failure"
  }
  if (!switch_trm && dcroddsratio_ob) {
    transferway <- "control failure to control success,"
    RIR <- ceiling(final/((b+d)/n_obs))*(replace=="entire") + ceiling(final/(b/(a+b)))*(1-(replace=="entire"))
    RIRway <- "control failure"
  }
  if (!switch_trm && !dcroddsratio_ob) {
    transferway <- "control success to control failure,"
    RIR <- ceiling(final/((a+c)/n_obs))*(replace=="entire") + ceiling(final/(a/(a+b)))*(1-(replace=="entire"))
    RIRway <- "control success"
  }
  
  if (final_solution$needtworows) {
    final_extra <- final_solution$final_extra
    if (switch_trm && dcroddsratio_ob) {
      transferway_extra <- "control failure to control success,"
      RIR_extra <- ceiling(final_extra/((b+d)/n_obs))*(replace=="entire") + 
        ceiling(final_extra/(b/(b+d)))*(1-(replace=="entire"))
      RIRway_extra <- "control failure"
    }
    if (switch_trm && !dcroddsratio_ob) {
      transferway_extra <- "control success to control failure,"
      RIR_extra <- ceiling(final_extra/((a+c)/n_obs))*(replace=="entire") +
        ceiling(final_extra/(a/(a+b)))*(1-(replace=="entire"))
      RIRway_extra <- "control success"
    }
    if (!switch_trm && dcroddsratio_ob) {
      transferway_extra <- "treatment success to treatment failure,"
      RIR_extra <- ceiling(final_extra/((a+c)/n_obs))*(replace=="entire") +
        ceiling(final_extra/(a/(a+b)))*(1-(replace=="entire"))
      RIRway_extra <- "treatment success"
    }
    if (!switch_trm && !dcroddsratio_ob) {
      transferway_extra <- "treatment failure to treatment success,"
      RIR_extra <- ceiling(final_extra/((b+d)/n_obs))*(replace=="entire") +
        ceiling(final_extra/(b/(b+d)))*(1-(replace=="entire"))
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
      change, sprintf("you would need to replace %d", RIR), RIRway, "cases")
      
    if (replace == "control") {
      conclusion1a <- sprintf("with cases for which the probability of failure in the control group applies (RIR = %d).", RIR)
    } else {
      conclusion1a <- sprintf("with cases for which the probability of failure in the entire sample applies (RIR = %d).", RIR)
    }

    conclusion1b <- paste(
      sprintf("This is equivalent to transferring %d", final_solution$final_switch), 
      c("cases from"), transferway)
    
    conclusion1c <- "as shown, from the Implied Table to the Transfer Table."
    
  } else if (!final_solution$needtworows & final_solution$final_switch == 1) {
    conclusion1 <- paste(
      change, sprintf("you would need to replace %d", RIR), RIRway, "cases")
    
    if (replace == "control") {
      conclusion1a <- sprintf("with cases for which the probability of failure in the control group applies (RIR = %d).", RIR)
    } else {
      conclusion1a <- sprintf("with cases for which the probability of failure in the entire sample applies (RIR = %d).", RIR)
    }
    
    conclusion1b <- paste(
      sprintf("This is equivalent to transferring %d", final_solution$final_switch), 
      c("case from"), transferway)
    
    conclusion1c <- "as shown, from the Implied Table to the Transfer Table."
    
  } else {
    conclusion1 <- paste(
      change, c("only transferring cases from"), transferway, "is not enough.")
      
    conclusion1b <- paste(sprintf("We also need to transfer %d cases from", final_solution$final_extra),
      transferway_extra, c("as shown, from the User-entered Table to the Transfer Table."))
    
    conclusion1c <- paste(sprintf("This means we need to replace %d of", RIR), RIRway, 
      sprintf("with null hypothesis cases; and replace %d", RIR_extra), RIRway_extra, 
      c("with null hypothesis cases to change the inference.")
    )
  }
  
  conclusion2 <- sprintf(
    "For the Implied Table, we have an estimate of %.3f, with a SE of %.3f and a t-ratio of %.3f.",
    final_solution$est_eff_start, final_solution$std_err_start, final_solution$t_start
  )
  
  conclusion3 <- sprintf(
    "For the Transfer Table, we have an estimate of %.3f, with a SE of %.3f and a t-ratio of %.3f.",
    final_solution$est_eff_final, final_solution$std_err_final, final_solution$t_final
  )
  
  notice <- "Note: Values have been rounded to the nearest integer."
  noticeb <- "This may cause a little change to the estimated effect for the Implied Table."
  
  if (changeSE) {
    notice_SE <- sprintf(
      "In order to generate a usable implied contingency table, we increased the standard error to %.3f (the original one is %.3f).",
      std_err, user_std_err)
  }
  
  if (final_solution$needtworows) {
    total_switch <- final_solution$final_switch+final_solution$final_extra
    total_RIR <- RIR + RIR_extra
  } else {
    total_switch <- final_solution$final_switch
    total_RIR <- RIR
  }
  
  # result <- list(conclusion1,
  #                Implied_Table = final_solution$table_start, notice, Transfer_Table = final_solution$table_final,
  #                conclusion2, conclusion3, 
  #                total_RIR = total_RIR, total_switch = total_switch
  # )
  
  # output dispatch
  if (to_return == "raw_output") {
    
    if (changeSE) {
      result <- list(conclusion1,
                     conclusion1b,
                     conclusion1c,
                     Implied_Table = final_solution$table_start, 
                     notice,
                     Transfer_Table = final_solution$table_final,
                     conclusion2, 
                     conclusion3,
                     RIR = RIR,
                     notice_SE)
    } else {
      result <- list(conclusion1,
                     conclusion1b,
                     conclusion1c,
                     Implied_Table = final_solution$table_start, 
                     notice,
                     Transfer_Table = final_solution$table_final,
                     conclusion2, 
                     conclusion3,
                     RIR = RIR)
    }
  
    return(result)
    
  } else  if (to_return == "print") {
    
    # cat(crayon::bold("Background Information:"))
    # cat("\n")
    # cat(info1)
    # cat("\n")
    # cat("\n")
    cat(crayon::bold("Conclusion:"))
    cat("\n")
    cat(crayon::underline("User-entered Table:"))
    cat("\n")
    print(final_solution$table_start)
    cat("\n")
    if (changeSE) {
      cat(notice_SE)
      cat("\n")
      }
    cat(notice)
    cat(noticeb)
    cat("\n")
    cat("\n")
    cat(conclusion1)
    cat("\n")
    cat(conclusion1a)
    cat("\n")
    cat(conclusion1b)
    cat("\n")
    cat(conclusion1c)
    cat("\n")
    cat("\n")
    cat(crayon::underline("Transfer Table:"))
    cat("\n")
    print(final_solution$table_final)
    cat("\n")
    cat(conclusion2)
    cat("\n")
    cat(conclusion3)
    cat("\n")
    cat("\n")
    cat(crayon::bold("RIR:"))
    cat("\n")
    cat("RIR =", RIR)
    cat("\n")
    
  }
}
