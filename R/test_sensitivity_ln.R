# Main function to test sensitivity for non-linear models to be wrapped with pkonfound(), konfound(), and mkonfound()

test_sensitivity_ln <- function(est_eff,
                                std_err,
                                n_obs,
                                n_covariates,
                                n_treat,
                                switch_trm = T,
                                replace = "entire",
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
    thr_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 2) * -1
  } else {
    thr_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 2)
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
  user_std_err <- std_err
  if (std_err < minse) {
    haveimaginary <- T
    changeSE <- T
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
    transferway <- "treatment success to treatment failure"
    RIR <- ceiling(final/((a+c)/n_obs))*(replace=="entire") + ceiling(final/(a/(a+b)))*(1-(replace=="entire"))
    RIRway <- "treatment success"
    RIR_pi <- RIR / d * 100
  }
  if (switch_trm && !dcroddsratio_ob) {
    transferway <- "treatment failure to treatment success"
    RIR <- ceiling(final/((b+d)/n_obs))*(replace=="entire") + ceiling(final/(b/(a+b)))*(1-(replace=="entire"))
    RIRway <- "treatment failure"
    RIR_pi <- RIR / c * 100

  }
  if (!switch_trm && dcroddsratio_ob) {
    transferway <- "control failure to control success"
    RIR <- ceiling(final/((b+d)/n_obs))*(replace=="entire") + ceiling(final/(b/(a+b)))*(1-(replace=="entire"))
    RIRway <- "control failure"
    RIR_pi <- RIR / a * 100
  }
  if (!switch_trm && !dcroddsratio_ob) {
    transferway <- "control success to control failure"
    RIR <- ceiling(final/((a+c)/n_obs))*(replace=="entire") + ceiling(final/(a/(a+b)))*(1-(replace=="entire"))
    RIRway <- "control success"
    RIR_pi <- RIR / b * 100
  }
  RIR_extra <- final_extra <- NA
  
  if (final_solution$needtworows) {
    # if need two rows, then do not report RIR_pi 
    ## because denominator is tricky 
    RIR_pi <- NA
    final_extra <- final_solution$final_extra
    if (switch_trm && dcroddsratio_ob) {
      transferway_extra <- "control failure to control success"
      RIR_extra <- ceiling(final_extra/((b+d)/n_obs))*(replace=="entire") + 
        ceiling(final_extra/(b/(b+d)))*(1-(replace=="entire"))
      RIRway_extra <- "control failure"
    }
    if (switch_trm && !dcroddsratio_ob) {
      transferway_extra <- "control success to control failure"
      RIR_extra <- ceiling(final_extra/((a+c)/n_obs))*(replace=="entire") +
        ceiling(final_extra/(a/(a+b)))*(1-(replace=="entire"))
      RIRway_extra <- "control success"
    }
    if (!switch_trm && dcroddsratio_ob) {
      transferway_extra <- "treatment success to treatment failure"
      RIR_extra <- ceiling(final_extra/((a+c)/n_obs))*(replace=="entire") +
        ceiling(final_extra/(a/(a+b)))*(1-(replace=="entire"))
      RIRway_extra <- "treatment success"
    }
    if (!switch_trm && !dcroddsratio_ob) {
      transferway_extra <- "treatment failure to treatment success"
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
      c("cases from"), transferway)
    
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
      "In order to generate a usable implied contingency table, we increased the standard error to %.3f (the reported standard error is %.3f).",
      std_err, user_std_err)
  }
  
  if (final_solution$needtworows) {
    total_switch <- final_solution$final_switch+final_solution$final_extra
    total_RIR <- RIR + RIR_extra
  } else {
    total_switch <- final_solution$final_switch
    total_RIR <- RIR
  }

  ### not sure whether it is right way to calculate p-value
  if (tails == 2) {
    p_start <- (2 * (1 - pt(abs(final_solution$t_start), n_obs - n_covariates - 2)))
    p_final <- (2 * (1 - pt(abs(final_solution$t_final), n_obs - n_covariates - 2)))
  } 
  # For a one-tailed test (assuming upper tail)
  else if (tails == 1) {
    p_start <- (1 - pt(final_solution$t_start, n_obs - n_covariates - 2))
    p_final <- (1 - pt(final_solution$t_final, n_obs - n_covariates - 2))
  }        

  ### chi-square p
  p_start_chi <- chisq.test(final_solution$table_start,correct = FALSE)$p.value
  p_final_chi <- chisq.test(final_solution$table_final,correct = FALSE)$p.value

  ### Fisher's p
  p_start_fisher <- suppressWarnings(fisher.test(final_solution$table_start)$p.value)
  p_final_fisher <- suppressWarnings(fisher.test(final_solution$table_final)$p.value)
  
  ### Add for some cases with RIR_pi exceeding 100%
  if (!is.na(RIR_pi) && RIR_pi > 100) {   
    transfer <- switch(RIRway,
                       "treatment success" = final_solution$table_start[2,2],
                       "treatment failure" = final_solution$table_start[2,1],
                       "control failure" = final_solution$table_start[1,1],
                       "control success" = final_solution$table_start[1,2],
                       NA)       
    if (!is.na(transfer)) {
        # Calculate the value for the success_failure_Rate
      success_failure_Rate <- 1 - total_switch / transfer}
    } else {
    transfer <- NA
    success_failure_Rate <- NA
    }
    
  ### Add for indicating calculation of RIR   
      RIRway_phrase <- switch(RIRway,
                       "treatment success" = "success in the treatment group",
                       "treatment failure" = "failure in the treatment group",
                       "control failure" = "failure in the treatment group",
                       "control success" = "success in the control group",
                       NA) 

  ### Add for indicating probability of failure in control/entire group  
  if (replace == "control") {
  prob_replace <- final_solution$table_start[1,1]/n_cnt*100
  } else {
  prob_replace <- final_solution$table_start[1,1]/n_obs*100
  }

  
  # result <- list(conclusion1,
  #                Implied_Table = final_solution$table_start, notice, Transfer_Table = final_solution$table_final,
  #                conclusion2, conclusion3, 
  #                total_RIR = total_RIR, total_switch = total_switch
  # )
  
  # output dispatch
  if (to_return == "raw_output") {
       return(list(obs_r = NA, act_r = NA, 
                  critical_r = NA, r_final = NA,
                  rxcv = NA, rycv = NA, 
                  rxcvGz = NA, rycvGz = NA, 
                  itcvGz = NA, itcv = NA, 
                  r2xz = NA, r2yz = NA, 
                  delta_star = NA, delta_star_restricted = NA, 
                  delta_exact = NA, delta_pctbias = NA, 
                  cor_oster = NA, cor_exact = NA, 
                  beta_threshold = NA,
                  perc_bias_to_change = NA, 
                  ## to see intermediate outputs
                  t_start = final_solution$t_start, t_final = final_solution$t_final,   
                  p_start = p_start, p_final = p_final,
                  p_chi_start = p_start_chi, p_chi_final = p_final_chi,
                  p_fisher_start = p_start_fisher, p_fisher_final = p_final_fisher,
                  RIRway_phrase = RIRway_phrase,
                  RIR_primary = RIR,
                  RIR_supplemental = RIR_extra, 
                  RIR_perc = RIR_pi,  # need to discuss the denominator
                  fragility_primary = final,
                  fragility_supplemental = final_extra,        
                  starting_table = final_solution$table_start,
                  final_table = final_solution$table_final,
                  user_SE = user_std_err,
                  analysis_SE = std_err, 
                  Fig_ITCV = NA,
                  Fig_RIR = NA))
    
  #  if (changeSE) {
  #   result <- list(conclusion1,
  #                   conclusion1b,
  #                   conclusion1c,
  #                   Implied_Table = final_solution$table_start, 
  #                   notice,
  #                   Transfer_Table = final_solution$table_final,
  #                   conclusion2, 
  #                   conclusion3,
  #                   RIR = RIR,
  #                   notice_SE,
  #                   total_switch,
                    ### check intermediate vars.
  #                  thr_t = thr_t, t_ob = t_ob,
  #                  invalidate_ob = invalidate_ob,
  #                  final_switch = final_solution$final_switch,
  #                  total_RIR = total_RIR,
  #                  total_switch = total_switch,
  #                  needtworows = final_solution$needtworows,
  #                  changeSE = changeSE,
  #                  est_eff = est_eff, std_err = std_err)
  #  } else {
  #    result <- list(conclusion1,
  #                   conclusion1b,
  #                   conclusion1c,
  #                   Implied_Table = final_solution$table_start, 
  #                   notice,
  #                   Transfer_Table = final_solution$table_final,
  #                   conclusion2, 
  #                   conclusion3,
  #                   RIR = RIR,
                    ### check intermediate vars.
  #                  thr_t = thr_t, t_ob = t_ob,
  #                  invalidate_ob = invalidate_ob,
  #                  final_switch = final_solution$final_switch,
  #                  total_RIR = total_RIR,
  #                  total_switch = total_switch,
  #                  needtworows = final_solution$needtworows,
  #                  changeSE = changeSE,
  #                  est_eff = est_eff, std_err = std_err)
  #  }
  
  #  return(result)
    
  } else  if (to_return == "print") {

    result <- list(conclusion1,conclusion1b, conclusion1c,
                   Implied_Table = final_solution$table_start, notice, Transfer_Table = final_solution$table_final,
                   conclusion2, conclusion3,
                   total_RIR = total_RIR, total_switch = total_switch)    
   
    # Extracting the results into variables for cleaner reference
    conclusion1 <- result$conclusion1
    conclusion1b <- result$conclusion1b
    conclusion1c <- result$conclusion1c
    Implied_Table <- result$Implied_Table
    Transfer_Table <- result$Transfer_Table
    conclusion2 <- result$conclusion2
    conclusion3 <- result$conclusion3
    notice <- result$notice
    RIR_value <- result$RIR
    
      
    if (changeSE) {

      ### start from changeSE = T
      
      cat(sprintf("RIR = %d\n\n", RIR))
      cat("The table you entered or is implied by your effect size:\n\n")
      print(Implied_Table)
      cat("\n")
      cat(paste(sprintf("The reported effect size = %.3f, SE = %.3f, p-value = %.3f.",
                        est_eff, user_std_err, p_start),
                sprintf("\nThe SE has been adjusted to %.3f to generate a real number in the", final_solution$std_err_start), 
                sprintf("\nimplied table. Numbers in the table cells have been rounded"),
                sprintf("\nto integers, which may slightly alter the estimated effect from"), 
                sprintf("\nthe value originally entered.\n\n")
      ))
            
      if (invalidate_ob) {
#111
        ### when invalidate = T

        change <- sprintf("To invalidate the inference that the effect is different from 0 \n(alpha = %.3f)", alpha)
        ### 
        if (!final_solution$needtworows & final_solution$final_switch > 1) {
          #conclusion1 <- 
          cat(paste(
            change, sprintf("one would need to replace %d (%.2f%%)", RIR, RIR_pi), RIRway, "\ncases "))
          
          if (replace == "control") {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of failure in the control \ngroup (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          } else {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of failure in the entire \nsample (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          }
          
          #conclusion1b <- 
          cat(paste0(
            sprintf(" This is equivalent to transferring \n%d", final_solution$final_switch), 
            c(" cases from "), transferway, 
            sprintf(" (Fragility = %d).", total_switch),
            c("\n\nNote that RIR = Fragility/[1-P("), RIRway_phrase, c(")]"))
             ) 
                    

          cat("\n")

          ### for RIR_perc larger than 100%
          if (RIR_pi > 100){
            cat(paste0(sprintf("\nNote the RIR exceeds 100%%. Generating the transfer of %d cases would", total_switch),
                c("\nrequire replacing more cases that are in the "), RIRway, c(" condition\n"))
               ) 
            }

          cat(sprintf("\nThe transfer of %d cases yields the following table:", total_switch))

          
        } else if (!final_solution$needtworows & final_solution$final_switch == 1) {
          #conclusion1 <- 222
          cat(paste(
            change, sprintf("one would need to replace %d (%.2f%%)", RIR, RIR_pi), RIRway, "\ncases"))
          
          if (replace == "control") {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of failure in the control \ngroup (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          } else {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of failure in the entire \nsample (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          }
          
          #conclusion1b <- 
          cat(paste0(
            sprintf(" This is equivalent to transferring \n%d", final_solution$final_switch), 
            c(" cases from "), transferway, 
            sprintf(" (Fragility = %d).", total_switch),
            c("\n\nNote that RIR = Fragility/[1-P("), RIRway_phrase, c(")]"))
             )  
          
          cat("\n")

          ### for RIR_perc larger than 100%
          if (RIR_pi > 100){
            cat(paste0(sprintf("\nNote the RIR exceeds 100%%. Generating the transfer of %d cases would", total_switch),
                c("\nrequire replacing more cases that are in the "), RIRway, c(" condition\n"))
               ) 
            }

          cat(sprintf("\nThe transfer of %d cases yields the following table:", total_switch))

        } else {

          ### when needtworows = T
          
          #conclusion1 <- 
        cat(paste0(
            sprintf("The inference cannot be invalidated merely by switching cases in"),
            sprintf("\nonly one treatment condition. Therefore, cases have been switched from"),
            c("\n"), transferway, c(" and from "),
            transferway_extra, c("."), c("\n"),
            sprintf("The final Fragility(= %d) and RIR(= %d)", final_solution$final_switch, RIR),
            c(" reflect both sets of changes. \nPlease compare the after transfer table with the implied table.")
            )
              )
        }
        ### changed due to consistent linebreak
      } else {

          ### when invalidate = F (sustain)333
        
        if (est_eff >= 0) {
          change <- sprintf("To reach the threshold that would sustain an inference that the \neffect is different from 0 (alpha = %.3f)", alpha)
        } else {
          change <- sprintf("To reach the threshold that would sustain an inference that the \neffect is different from 0 (alpha = %.3f)", alpha)
        }
        ###
        if (!final_solution$needtworows & final_solution$final_switch > 1) {
          #conclusion1 <- 
          cat(paste(
            change, sprintf("one would need to replace %d \n(%.2f%%)", RIR, RIR_pi), RIRway, "cases "))
          
          if (replace == "control") {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of \nfailure in the control group (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          } else {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of \nfailure in the entire sample (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          }
          
          #conclusion1b <- 
          cat(paste0(
            sprintf(" This is equivalent \nto transferring %d", final_solution$final_switch), 
            c(" cases from "), transferway, 
            sprintf("\n(Fragility = %d).", total_switch),
            c("\n\nNote that RIR = Fragility/[1-P("), RIRway_phrase, c(")]"))
             )  
          
     cat("\n")

          ### for RIR_perc larger than 100%
          if (RIR_pi > 100){
            cat(paste0(sprintf("\nNote the RIR exceeds 100%%. Generating the transfer of %d cases would", total_switch),
                c("\nrequire replacing more cases that are in the "), RIRway, c(" condition\n"))
               ) 
            }

          cat(sprintf("\nThe transfer of %d cases yields the following table:", total_switch))

          
        } else if (!final_solution$needtworows & final_solution$final_switch == 1) {
          #conclusion1 <- 444
          cat(paste(
            change, sprintf("one would need to replace %d \n(%.2f%%)", RIR, RIR_pi), RIRway, "\ncases"))
          
          if (replace == "control") {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of \nfailure in the control group (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          } else {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of \nfailure in the entire sample (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          }
          
          #conclusion1b <- 
          cat(paste0(
            sprintf(" This is equivalent to transferring \n%d", final_solution$final_switch), 
            c(" cases from "), transferway, 
            sprintf(" (Fragility = %d).", total_switch),
            c("\n\nNote that RIR = Fragility/[1-P("), RIRway_phrase, c(")]"))
             ) 

          cat("\n")

          ### for RIR_perc larger than 100%
          if (RIR_pi > 100){
            cat(paste0(sprintf("\nNote the RIR exceeds 100%%. Generating the transfer of %d cases would", total_switch),
                c("\nrequire replacing more cases that are in the "), RIRway, c(" condition\n"))
               ) 
            }

          cat(sprintf("\nThe transfer of %d cases yields the following table:", total_switch))
          
        } else {

          ### when needtworows = T

          #conclusion1 <- 
        cat(paste0(
            sprintf("The inference cannot be sustained merely by switching cases in"),
            sprintf("\nonly one treatment condition. Therefore, cases have been switched from"),
            c("\n"), transferway, c(" and from "),
            transferway_extra, c("."), c("\n"),
            sprintf("The final Fragility(= %d) and RIR(= %d)", final_solution$final_switch, RIR),
            c(" reflect both sets of changes. \nPlease compare the after transfer table with the implied table.")
            )
              )
        }
        ###
      }

      cat("\n\n")
      print(Transfer_Table)
      cat(sprintf("Effect size = %.3f, SE = %.3f, p-value = %.3f. \nIndicate that",
                  final_solution$est_eff_final, final_solution$std_err_final, p_final),
                 c("this is based on t = estimated effect/standard error")
         )
      
      
      
    } else {
      
      ### when changeSE = F
      
      cat(sprintf("RIR = %d\n\n", RIR))
      cat("The table you entered or is implied by your effect size:\n\n")
      print(Implied_Table)
      cat("\n")
      cat(paste(sprintf("The reported effect size = %.3f, and SE = %.3f, p-value = %.3f.",
                        est_eff, user_std_err, p_start),
                sprintf("\nValues have been rounded to the nearest integer. This may cause"), 
                sprintf("\na little change to the estimated effect for the table.\n\n"))
      )
      
      ### start here
      if (invalidate_ob) {

        ### invalidate = T555
        
        change <- sprintf("To invalidate the inference that the effect is different from 0 \n(alpha = %.3f)", alpha)
        ### 
        if (!final_solution$needtworows & final_solution$final_switch > 1) {
          #conclusion1 <- 
          cat(paste(
            change, sprintf("one would need to replace %d (%.2f%%)", RIR, RIR_pi), RIRway, "\ncases "))
          
          if (replace == "control") {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of failure in the control \ngroup (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          } else {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of failure in the entire \nsample (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          }
          
          #conclusion1b <- 
          cat(paste0(
            sprintf(" This is equivalent to transferring \n%d", final_solution$final_switch), 
            c(" cases from "), transferway, 
            sprintf(" (Fragility = %d).", total_switch),
            c("\n\nNote that RIR = Fragility/[1-P("), RIRway_phrase, c(")]"))
             ) 

          cat("\n")

          ### for RIR_perc larger than 100%
          if (RIR_pi > 100){
            cat(paste0(sprintf("\nNote the RIR exceeds 100%%. Generating the transfer of %d cases would", total_switch),
                c("\nrequire replacing more cases that are in the "), RIRway, c(" condition\n"))
               ) 
            }

          cat(sprintf("\nThe transfer of %d cases yields the following table:", total_switch))
          
        } else if (!final_solution$needtworows & final_solution$final_switch == 1) {
          #conclusion1 <- 666
          cat(paste(
            change, sprintf("one would need to replace %d (%.2f%%)", RIR, RIR_pi), RIRway, "\ncases"))
          
          if (replace == "control") {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of failure in the control \ngroup (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          } else {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of failure in the entire \nsample (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          }
          
          #conclusion1b <- 
          cat(paste0(
            sprintf(" This is equivalent to transferring \n%d", final_solution$final_switch), 
            c(" cases from "), transferway, 
            sprintf(" (Fragility = %d).", total_switch),
            c("\n\nNote that RIR = Fragility/[1-P("), RIRway_phrase, c(")]"))
             ) 

          cat("\n")

          ### for RIR_perc larger than 100%
          if (RIR_pi > 100){
            cat(paste0(sprintf("\nNote the RIR exceeds 100%%. Generating the transfer of %d cases would", total_switch),
                c("\nrequire replacing more cases that are in the "), RIRway, c(" condition\n"))
               ) 
            }

          cat(sprintf("\nThe transfer of %d cases yields the following table:", total_switch))
          
    
          
        } else {

            ### needtworows = T
          
          #conclusion1 <- 
            cat(paste0(
            sprintf("The inference cannot be invalidated merely by switching cases in"),
            sprintf("\nonly one treatment condition. Therefore, cases have been switched from"),
            c("\n"), transferway, c(" and from "),
            transferway_extra, c("."), c("\n"),
            sprintf("The final Fragility(= %d) and RIR(= %d)", final_solution$final_switch, RIR),
            c(" reflect both sets of changes. \nPlease compare the after transfer table with the implied table.")
            )
              )
                   }
        ### changed due to consistent linebreak
      } else {

        ### invalidate = F (sustain) 777
        
        if (est_eff >= 0) {
          change <- sprintf("To reach the threshold that would sustain an inference that the \neffect is different from 0 (alpha = %.3f)", alpha)
        } else {
          change <- sprintf("To reach the threshold that would sustain an inference that the \neffect is different from 0 (alpha = %.3f)", alpha)
        }
        ###
        if (!final_solution$needtworows & final_solution$final_switch > 1) {
          #conclusion1 <- 
          cat(paste(
            change, sprintf("one would need to replace %d \n(%.2f%%)", RIR, RIR_pi), RIRway, "cases "))
          
          if (replace == "control") {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of \nfailure in the control group (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          } else {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of \nfailure in the entire sample (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          }
          
          #conclusion1b <- 
          cat(paste0(
            sprintf(" This is equivalent \nto transferring %d", final_solution$final_switch), 
            c(" cases from "), transferway, 
            sprintf("\n(Fragility = %d).", total_switch),
            c("\n\nNote that RIR = Fragility/[1-P("), RIRway_phrase, c(")]"))
             )  

          cat("\n")

          ### for RIR_perc larger than 100%
          if (RIR_pi > 100){
            cat(paste0(sprintf("\nNote the RIR exceeds 100%%. Generating the transfer of %d cases would", total_switch),
                c("\nrequire replacing more cases that are in the "), RIRway, c(" condition\n"))
               ) 
            }

          cat(sprintf("\nThe transfer of %d cases yields the following table:", total_switch))
          
        } else if (!final_solution$needtworows & final_solution$final_switch == 1) {
          #conclusion1 <- 888
          cat(paste(
            change, sprintf("one would need to replace %d (%.2f%%)", RIR, RIR_pi), RIRway, "\ncases"))
          
          if (replace == "control") {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of \nfailure in the control group (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          } else {
            #conclusion1a <- 
            cat(sprintf("with cases for which the probability of \nfailure in the entire sample (%.2f%%) applies (RIR = %d).", prob_replace, RIR))
          }
          
          #conclusion1b <- 
          cat(paste0(
            sprintf(" This is equivalent \nto transferring %d", final_solution$final_switch), 
            c(" cases from "), transferway, 
            sprintf("\n(Fragility = %d).", total_switch),
            c("\n\nNote that RIR = Fragility/[1-P("), RIRway_phrase, c(")]"))
             ) 

          cat("\n")

          ### for RIR_perc larger than 100%
          if (RIR_pi > 100){
            cat(paste0(sprintf("\nNote the RIR exceeds 100%%. Generating the transfer of %d cases would", total_switch),
                c("\nrequire replacing more cases that are in the "), RIRway, c(" condition\n"))
               ) 
            }

          cat(sprintf("\nThe transfer of %d cases yields the following table:", total_switch))
          
        } else {

            ### needtworows = T
          
          #conclusion1 <- 
            cat(paste0(
            sprintf("The inference cannot be sustained merely by switching cases in"),
            sprintf("\nonly one treatment condition. Therefore, cases have been switched from"),
            c("\n"), transferway, c(" and from "),
            transferway_extra, c("."), c("\n"),
            sprintf("The final Fragility(= %d) and RIR(= %d)", final_solution$final_switch, RIR),
            c(" reflect both sets of changes. \nPlease compare the after transfer table with the implied table.")
            )
              )
        }
        ###
      }

      cat("\n\n")
      print(Transfer_Table)
      cat(sprintf("Effect size = %.3f, SE = %.3f, p-value = %.3f. \nIndicate that",
                  final_solution$est_eff_final, final_solution$std_err_final, p_final),
                 c("this is based on t = estimated effect/standard error")
         )
      
      
    }
  }
}
