# Main function to test sensitivity for non-linear models to be wrapped
# with pkonfound(), konfound(), and mkonfound()

#' @importFrom stats pt
test_sensitivity_ln <- function(est_eff,
                                std_err,
                                n_obs,
                                n_covariates,
                                n_treat,
                                switch_trm = TRUE,
                                replace = "entire",
                                alpha,
                                tails,
                                nu,
                                to_return,
                                model_object,
                                tested_variable) {
  
  ## error message if input is inappropriate
  if (!(std_err > 0)) {
  stop("Did not run! Standard error needs to be greater than zero.")}
  if (!(n_obs > n_covariates + 3)) {
  stop("Did not run! There are too few observations relative to
   the number of observations and covariates. Please specify a
    less complex model to use KonFound-It.")}

  if (est_eff < 0) {
    thr_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 2) * -1
  } else {
    thr_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 2)
  }

  # stop message
  if (n_obs <= 0 || n_treat <= 0) {
    stop("Please enter positive integers for sample size
     and number of treatment group cases.")
  }
  if (n_obs <= n_treat) {
    stop("The total sample size should be larger than
     the number of treatment group cases.")
  }

  odds_ratio <- exp(est_eff)

  # updated approach to deal with imaginary
  minse <- sqrt((4 * n_obs +
                   sqrt(16 * n_obs^2 + 4 * n_treat * (n_obs - n_treat) *
                          ((4 + 4 * odds_ratio^2) / odds_ratio - 7.999999)))/
                  (2 * n_treat * (n_obs - n_treat)))
  # check if the implied table solution may contain imaginary numbers
  changeSE <- FALSE
  user_std_err <- std_err
  if (std_err < minse) {
    haveimaginary <- TRUE
    changeSE <- TRUE
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
  # haveimaginary <- FALSE
  # changepi <- FALSE
  # set the default value for whether we need and can adjust pi (ratio of treatment cases)
  # to remove the imaginary part
  # keyimagin <- (4 + 4 * odds_ratio^2 + odds_ratio *
  #                 (-8 + 4 * n_obs * std_err^2 - n_obs * n_treat * std_err^4 + n_treat^2 * std_err^4))
  # minimgain <- 4 + 4 * odds_ratio^2 + odds_ratio * (-8 + n_obs * std_err^2 * (4 - 0.25 * n_obs * std_err^2))
  # keyx1 <- 4 + 4 * odds_ratio^2 + odds_ratio * (-8 + 4 * n_obs * std_err^2)
  # if (keyimagin > 0) {
    # haveimaginary <- TRUE
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
  check1 <- check_starting_table(n_cnt, n_treat, a1, b1, c1, d1)
  check2 <- check_starting_table(n_cnt, n_treat, a2, b2, c2, d2)

  if (check1) {
    table_bstart1 <- get_abcd_kfnl(a1, b1, c1, d1)
    solution1 <- getswitch(table_bstart1, thr_t, switch_trm, n_obs)
  }
  if (check2) {
    table_bstart2 <- get_abcd_kfnl(a2, b2, c2, d2)
    solution2 <- getswitch(table_bstart2, thr_t, switch_trm, n_obs)
  }
  if (!check1 && !check2) {
    stop("Cannot generate a usable contingency table! This may be due to small cell sizes (less than 5) implied by the quantities you entered, please verify your input values.")
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
  
  ## final is total switch
  ## final includes two row fragility if allnotenough == 1
  final <- final_solution$final_switch
  ## final_primary is the fragility in the starting row
  final_primary <- final_solution$final_switch - final_solution$final_extra
  
  if (final %% 1 == 0.5) {
      final <- floor(final)  # Round down if final is x.5
  }
  
  a <- final_solution$table_start[1,1]
  b <- final_solution$table_start[1,2]
  c <- final_solution$table_start[2,1]
  d <- final_solution$table_start[2,2]
  
  if (switch_trm && dcroddsratio_ob) {
    transferway <- "treatment success to treatment failure"
    transferway_start <- "treatment row"
    RIR <- ceiling(final_primary/((a+c)/n_obs))*(replace=="entire") +
           ceiling(final_primary/(a/(a+b)))*(1-(replace=="entire"))
    RIRway <- "treatment success"
    RIRway_start <- "treatment row"
    RIR_pi <- RIR / d * 100
    #p_destination <- round(a/n_cnt * 100, 3)
    
    p_destination <- round((a+c)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
        round(a/(a+b) * 100, 3) * (1 - (replace == "entire"))
    
    if (replace == "entire"){
      RIRway_phrase <- "success in the entire sample"
    } else if (replace == "control"){
      RIRway_phrase <- "success in the control group"
    }
  }
  if (switch_trm && !dcroddsratio_ob) {
    transferway <- "treatment failure to treatment success"
    transferway_start <- "treatment row"
    RIR <- ceiling(final_primary/((b+d)/n_obs))*(replace=="entire") +
           ceiling(final_primary/(b/(a+b)))*(1-(replace=="entire"))
    RIRway <- "treatment failure"
    RIRway_start <- "treatment row"
    RIR_pi <- RIR / c * 100
    #p_destination <- round(b/n_cnt * 100, 3)
    
    p_destination <- round((b+d)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
        round(b/(a+b) * 100, 3) * (1 - (replace == "entire"))
    
    ### added to show RIR calculation

    if (replace == "entire"){
      RIRway_phrase <- "failure in the entire sample"
    } else if (replace == "control"){
      RIRway_phrase <- "failure in the control group"
    }
  }
  if (!switch_trm && dcroddsratio_ob) {
    transferway <- "control failure to control success"
    transferway_start <- "control row"
    RIR <- ceiling(final_primary/((b+d)/n_obs))*(replace=="entire") + 
        ceiling(final_primary/(b/(a+b)))*(1-(replace=="entire"))
    RIRway <- "control failure"
    RIRway_start <- "control row"
    RIR_pi <- RIR / a * 100
    #p_destination <- round(b/n_cnt * 100, 3)
    
    p_destination <- round((b+d)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
        round(b/(a+b) * 100, 3) * (1 - (replace == "entire"))
    
    if (replace == "entire"){
      RIRway_phrase <- "failure in the entire sample"
    } else if (replace == "control"){
      RIRway_phrase <- "failure in the control group"
    }
  }
  if (!switch_trm && !dcroddsratio_ob) {
    transferway <- "control success to control failure"
    transferway_start <- "control row"
    RIR <- ceiling(final_primary/((a+c)/n_obs))*(replace=="entire") + 
        ceiling(final_primary/(a/(a+b)))*(1-(replace=="entire"))
    RIRway <- "control success"
    RIRway_start <- "control row"
    RIR_pi <- RIR / b * 100
    #p_destination <- round(a/n_cnt * 100, 3)
    
    p_destination <- round((a+c)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
        round(a/(a+b) * 100, 3) * (1 - (replace == "entire"))
    
    if (replace == "entire"){
      RIRway_phrase <- "success in the entire sample"
    } else if (replace == "control"){
      RIRway_phrase <- "success in the control group"
    }
  }
  
  RIRway_split <- strsplit(RIRway, " ")[[1]][1]
  RIR_extra <- final_extra <- NA

  if (final_solution$needtworows) {
    # if need two rows, then do not report RIR_pi
    ## because denominator is tricky
    RIR_pi <- NA
    final_extra <- final_solution$final_extra
    
    # apply similar rounding down like `final` for `final_extra` if needed
    if (final_extra %% 1 == 0.5) {
        final_extra <- floor(final_extra)
    }
    
    if (switch_trm && dcroddsratio_ob) {
      transferway_extra <- "control failure to control success"
      transferway_extra_start <- "control row"
      RIR_extra <- ceiling(final_extra/((b+d)/n_obs))*(replace=="entire") +
        ceiling(final_extra/(b/(b+a)))*(1-(replace=="entire"))
      RIRway_extra <- "control failure"
      RIRway_extra_start <- "control row"
      #p_destination_extra <- round(b/n_cnt * 100, 3)
      
      p_destination_extra <- round((b+d)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
          round(b/(a+b) * 100, 3) * (1 - (replace == "entire"))
      
      
    }
    if (switch_trm && !dcroddsratio_ob) {
      transferway_extra <- "control success to control failure"
      transferway_extra_start <- "control row"
      RIR_extra <- ceiling(final_extra/((a+c)/n_obs))*(replace=="entire") +
        ceiling(final_extra/(a/(a+b)))*(1-(replace=="entire"))
      RIRway_extra <- "control success"
      RIRway_extra_start <- "control row"
      #p_destination_extra <- round(a/n_cnt * 100, 3)
      
      p_destination_extra <- round((a+c)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
          round(a/(a+b) * 100, 3) * (1 - (replace == "entire"))
      
    }
    if (!switch_trm && dcroddsratio_ob) {
      transferway_extra <- "treatment success to treatment failure"
      transferway_extra_start <- "treatment row"
      RIR_extra <- ceiling(final_extra/((a+c)/n_obs))*(replace=="entire") +
        ceiling(final_extra/(a/(a+b)))*(1-(replace=="entire"))
      RIRway_extra <- "treatment success"
      RIRway_extra_start <- "treatment row"
      #p_destination_extra <- round(a/n_cnt * 100, 3)
      
      p_destination_extra <- round((a+c)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
          round(a/(a+b) * 100, 3) * (1 - (replace == "entire"))
      
    }
    if (!switch_trm && !dcroddsratio_ob) {
      transferway_extra <- "treatment failure to treatment success"
      transferway_extra_start <- "treatment row"
      RIR_extra <- ceiling(final_extra/((b+d)/n_obs))*(replace=="entire") +
        ceiling(final_extra/(b/(b+a)))*(1-(replace=="entire"))
      RIRway_extra <- "treatment failure"
      RIRway_extra_start <- "treatment row"
      #p_destination_extra <- round(b/n_cnt * 100, 3)
      
      p_destination_extra <- round((b+d)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
          round(b/(a+b) * 100, 3) * (1 - (replace == "entire"))
      
    }
  }

  if (final_solution$needtworows) {
    total_switch <- final_solution$final_switch
    total_RIR <- RIR + RIR_extra
  } else {
    total_switch <- final_solution$final_switch
    total_RIR <- RIR
  }

 ### Add to calculate p-value
 if (tails == 2) {
	 p_start <- 2 * stats::pt(abs(final_solution$t_start), n_obs - n_covariates - 2, lower.tail = FALSE)
	 p_final <- 2 * stats::pt(abs(final_solution$t_final), n_obs - n_covariates - 2, lower.tail = FALSE)
 } else if (tails == 1) {
	 p_start <- pt(abs(final_solution$t_start), n_obs - n_covariates - 2, lower.tail = FALSE)
	 p_final <- pt(abs(final_solution$t_final), n_obs - n_covariates - 2, lower.tail = FALSE)
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


  ### Add for indicating probability of failure in control/entire group
  if (replace == "control") {
  prob_replace <- final_solution$table_start[1,1]/n_cnt*100
  } else {
  prob_replace <- final_solution$table_start[1,1]/n_obs*100
  }

# Components from final_solution$table_start
control_failure_start <- final_solution$table_start[1,1]
control_success_start <- final_solution$table_start[1,2]
treatment_success_start <- final_solution$table_start[2,2]
treatment_failure_start <- final_solution$table_start[2,1]

# Calculating success percentages and totals for the start table
success_percent_control_start <- control_success_start / (control_failure_start + control_success_start) * 100
success_percent_treatment_start <- treatment_success_start / (treatment_failure_start + treatment_success_start) * 100
total_fail_start <- control_failure_start + treatment_failure_start
total_success_start <- control_success_start + treatment_success_start
total_percentage_start <- total_success_start / (total_fail_start + total_success_start) * 100
	
# Formatting success rates with "%" symbol
success_rate_control_start <- paste0(sprintf("%.2f", success_percent_control_start), "%")
success_rate_treatment_start <- paste0(sprintf("%.2f", success_percent_treatment_start), "%")
total_rate_start <- paste0(sprintf("%.2f", total_percentage_start), "%")

# Adjusting the 3x3 start table to include Success Rate with "%" and updated column name
table_start_3x3 <- data.frame(
    Fail = c(control_failure_start, treatment_failure_start, total_fail_start),
    Success = c(control_success_start, treatment_success_start, total_success_start),
    `Success_Rate` = c(success_rate_control_start, success_rate_treatment_start, total_rate_start),
    row.names = c("Control", "Treatment", "Total")
)

# Repeat the process for final_solution$table_final for the transferred table
control_failure_final <- final_solution$table_final[1,1]
control_success_final <- final_solution$table_final[1,2]
treatment_success_final <- final_solution$table_final[2,2]
treatment_failure_final <- final_solution$table_final[2,1]

# Check if a_final, b_final, c_final, or d_final is exactly 0.5, and if so, set them to 0
if (control_failure_final == 0.5) control_failure_final <- 0
if (control_success_final == 0.5) control_success_final <- 0
if (treatment_failure_final == 0.5) treatment_failure_final <- 0
if (treatment_success_final == 0.5) treatment_success_final <- 0

# Calculating success percentages and totals for the final table
success_percent_control_final <- control_success_final / (control_failure_final + control_success_final) * 100
success_percent_treatment_final <- treatment_success_final / (treatment_failure_final + treatment_success_final) * 100
total_fail_final <- control_failure_final + treatment_failure_final
total_success_final <- control_success_final + treatment_success_final
total_percentage_final <- total_success_final / (total_fail_final + total_success_final) * 100
	
# Formatting success rates for the final table
success_rate_control_final <- paste0(sprintf("%.2f", success_percent_control_final), "%")
success_rate_treatment_final <- paste0(sprintf("%.2f", success_percent_treatment_final), "%")
total_rate_final <- paste0(sprintf("%.2f", total_percentage_final), "%")

# Adjusting the 3x3 final table to include Success Rate with "%" and updated column name
table_final_3x3 <- data.frame(
    Fail = c(control_failure_final, treatment_failure_final, total_fail_final),
    Success = c(control_success_final, treatment_success_final, total_success_final),
    `Success_Rate` = c(success_rate_control_final, success_rate_treatment_final, total_rate_final),
    row.names = c("Control", "Treatment", "Total")
)

### Output language objects
# Conditional Fragility calculation component 
if (p_start < 0.05) {
    if (RIRway == "treatment success") {
        prob_indicator = "failure"  
    } else if (RIRway == "treatment failure") {
        prob_indicator = "success"  
    } else if (RIRway == "control success") {
        prob_indicator = "failure"  
    } else if (RIRway == "control failure") {
        prob_indicator = "success" 
    }
} else {  # p_start > 0.05
    if (RIRway == "treatment success") {
        prob_indicator = "failure"  
    } else if (RIRway == "treatment failure") {
        prob_indicator = "success" 
    } else if (RIRway == "control success") {
        prob_indicator = "failure" 
    } else if (RIRway == "control failure") {
        prob_indicator = "success" 
    }
}

if (final_solution$needtworows) {
    # Conditional Fragility calculation component 
    if (p_start < 0.05) {
        if (RIRway_extra == "treatment success") {
            prob_indicator_extra = "failure"  
        } else if (RIRway_extra == "treatment failure") {
            prob_indicator_extra = "success"  
        } else if (RIRway_extra == "control success") {
            prob_indicator_extra = "failure"  
        } else if (RIRway_extra == "control failure") {
            prob_indicator_extra = "success" 
        }
    } else {  # p_start > 0.05
        if (RIRway_extra == "treatment success") {
            prob_indicator_extra = "failure"  
        } else if (RIRway_extra == "treatment failure") {
            prob_indicator_extra = "success" 
        } else if (RIRway_extra == "control success") {
            prob_indicator_extra = "failure" 
        } else if (RIRway_extra == "control failure") {
            prob_indicator_extra = "success" 
        }
    }
}

# Summarizing statement for the start
conclusion_sum <- if (!final_solution$needtworows) {
    paste0("RIR = ", total_RIR, "\nFragility = ", total_switch, "\n\n")
} else if (final_solution$needtworows) {
    paste0("RIR = ", RIR, " + ", RIR_extra, " = ", total_RIR, "\n",
           "Total RIR = Primary RIR in ", RIRway_start, " + Supplemental RIR in ", RIRway_extra_start, "\n\n",
           "Fragility = ", final_primary, " + ", final_extra, " = ", total_switch, "\n",
           "Total Fragility = Primary Fragility in ", transferway_start, " + Supplemental Fragility in ", transferway_extra_start, "\n\n")
}

### Table output
table_header1 <- "The table implied by the parameter estimates and sample sizes you entered:\n"

# The summary of the estimates of implied table
if (changeSE) {
    estimates_summary1 <- paste(
        sprintf("The reported log odds = %.3f, SE = %.3f, and p-value = %.3f.", est_eff, user_std_err, p_start),
        sprintf("\nThe SE has been adjusted to %.3f to generate real numbers in the implied table", final_solution$std_err_start),
        sprintf("\nfor which the p-value would be %.3f. Numbers in the table cells have been rounded", p_start),
        sprintf("\nto integers, which may slightly alter the estimated effect from the value originally entered.\n\n")
    )
} else if (!changeSE){
    estimates_summary1 <- paste(
        sprintf("The reported log odds = %.3f, SE = %.3f, and p-value = %.3f.", est_eff, user_std_err, p_start),
        "\nValues in the table have been rounded to the nearest integer. This may cause",
        "\na small change to the estimated effect for the table.\n\n"
    )
}

# The summary of the estimates of transfer table
estimates_summary2 <- paste0(
    sprintf("The log odds (estimated effect) = %.3f, SE = %.3f, p-value = %.3f.", 
            final_solution$est_eff_final, final_solution$std_err_final, p_final),
    "\nThis is based on t = estimated effect/standard error"
)

if (invalidate_ob) {
    change <- sprintf("To invalidate the inference that the effect is different from 0 (alpha = %.3f),", alpha)
    change_t <- sprintf("to invalidate the inference, ")
} else if (!invalidate_ob){
    change <- sprintf("To sustain an inference that the effect is different from 0 (alpha = %.3f),", alpha)
    change_t <- sprintf("to sustain an inference, ")
}

if (!final_solution$needtworows) {
    conclusion1 <- paste0(
        change, 
        sprintf("\none would need to transfer %d data points from ", final),
        sprintf("%s (Fragility = %d).\n", transferway, total_switch),
        sprintf("This is equivalent to replacing %g (%.3f%%) %s data points with data points", total_RIR, RIR_pi, RIRway)
    )
}

conclusion2 <- if (replace == "control") {
    paste0(sprintf("\nfor which the probability of %s in the control group (%.3f%%) applies (RIR = %d).", prob_indicator, p_destination, total_RIR))
} else {
    paste0(sprintf("\nfor which the probability of %s in the entire sample (%.3f%%) applies (RIR = %d).", prob_indicator, p_destination, total_RIR))
}

conclusion3 <- paste0(
    "\n\nNote that RIR = Fragility/P(destination)\n"
)

conclusion4 <- sprintf("\nThe transfer of %d data points yields the following table:\n", total_switch)

### Special case if RIR percentage > 100
if (!final_solution$needtworows && RIR_pi > 100) {
    conclusion_large_rir <- paste0(
        sprintf("\nNote the RIR exceeds 100%%. Generating the transfer of %d data points would", total_switch),
        "\nrequire replacing more data points than are in the ", RIRway, " condition.\n")
} else {
    conclusion_large_rir <- ""  # Empty string if RIR_pi <= 100
}

if (final_solution$needtworows) {
    conclusion_twoway_1 <- paste0(
        "In terms of Fragility, ", change_t, "transferring ", final_primary, " data points from\n", 
        transferway, " is not enough to change the inference.\n",
        "One would also need to transfer ", final_extra, " data points from ", transferway_extra, "\n",
        "as shown, from the User-entered Table to the Transfer Table.\n\n"
    )
    
    conclusion_twoway_2 <- paste0(
        "In terms of RIR, generating the ", final_primary, " switches from ", transferway, "\n",
        "is equivalent to replacing ", RIR, " ", RIRway, " data points with data points for which\n", 
        "the probability of ", prob_indicator, " in the ", replace, " sample (", p_destination, "%) applies.\n\n",
        "In addition, generating the ", final_extra, " switches from ", transferway_extra, " is\n",
        "equivalent to replacing ", RIR_extra, " ", RIRway_extra, " data points with data points for which\n",
        "the probability of ", prob_indicator_extra, " in the ", replace, " sample (", p_destination_extra, "%) applies.\n\n"
    )
    
    conclusion_twoway_3 <- paste0(
        "Therefore, the total RIR is ", RIR + RIR_extra, ".\n\n",
        "RIR = Fragility/P(destination)\n"
    )
}

citation <- paste0(
    "See Frank et al. (2021) for a description of the methods.\n\n",
    "*Frank, K. A., *Lin, Q., *Maroulis, S., *Mueller, A. S., Xu, R., Rosenberg, J. M., ... & Zhang, L. (2021).\n",
    "Hypothetical case replacement can be used to quantify the robustness of trial results. ",
    crayon::italic("Journal of Clinical\nEpidemiology, 134"), ", 150-159.\n",
    "*authors are listed alphabetically.\n\n",
    "Accuracy of results increases with the number of decimals entered.\n"
)

  # output dispatch
  if (to_return == "raw_output") {
      return(output_list(obs_r = NA, act_r = NA,
                  critical_r = NA, r_final = NA,
                  rxcv = NA, rycv = NA,
                  rxcvGz = NA, rycvGz = NA,
                  itcvGz = NA, itcv = NA,
                  r2xz = NA, r2yz = NA,
                  delta_star = NA, delta_star_restricted = NA,
                  delta_exact = NA, delta_pctbias = NA,
                  cor_oster = NA, cor_exact = NA,
                  beta_threshold = NA,
                  beta_threshold_verify = NA,
                  perc_bias_to_change = NA,
                  RIR_primary = RIR,
                  RIR_supplemental = RIR_extra,
                  RIR_perc = RIR_pi,  # need to discuss the denominator
                  fragility_primary = final_primary,
                  fragility_supplemental = final_extra,
                  starting_table = final_solution$table_start,
                  final_table = final_solution$table_final,
                  user_SE = user_std_err,
                  analysis_SE = std_err,
                  needtworows = final_solution$needtworows,
                  Fig_ITCV = NA,
                  Fig_RIR = NA))

  } else  if (to_return == "print") {

    cat(crayon::bold("Robustness of Inference to Replacement (RIR):\n"))

    cat(conclusion_sum)
    cat(table_header1)
    cat(crayon::underline("User-entered Table:\n"))
    print(table_start_3x3)
    cat("\n")
    cat(estimates_summary1)
    
    if (!final_solution$needtworows & final_solution$final_switch > 1) {
        
        cat(conclusion1, conclusion2, conclusion3)
        cat(conclusion_large_rir)
        
    } else if (!final_solution$needtworows & final_solution$final_switch == 1) {
        
        cat(conclusion1, conclusion2, conclusion3)
        cat(conclusion_large_rir)
        
        
    } else {
        
        cat(conclusion_twoway_1)
        cat(conclusion_twoway_2)
        cat(conclusion_twoway_3)
        
    }
    
    cat(conclusion4) 
    cat(crayon::underline("Transfer Table:\n"))
    print(table_final_3x3)
    cat("\n")    
    cat(estimates_summary2)
    cat("\n")
    cat("\n")
    cat(citation)

  }
}
