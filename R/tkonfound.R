#' Perform Sensitivity Analysis on 2x2 Tables
#'
#' This function performs a sensitivity analysis on a 2x2 contingency table.
#' It calculates the number of cases that need to be replaced to invalidate
#' or sustain the statistical inference. The function also allows switching
#' between treatment success and failure or control success and failure
#' based on the provided parameters.
#'
#' @param a Number of unsuccessful cases in the control group.
#' @param b Number of successful cases in the control group.
#' @param c Number of unsuccessful cases in the treatment group.
#' @param d Number of successful cases in the treatment group.
#' @param alpha Significance level for the statistical test, default is 0.05.
#' @param switch_trm Boolean indicating whether to switch treatment row cells,
#'        default is TRUE.
#' @param test Type of statistical test to use, either "fisher"
#' (default) or "chisq".
#' @param replace Indicates whether to use the entire sample or the control
#' group for base rate calculation, default is "control".
#' @param to_return Type of output to return, either "raw_output" or "print".
#'
#' @importFrom crayon bold underline
#'
#' @return Returns detailed information about the sensitivity analysis,
#'         including the number of cases to be replaced (RIR), user-entered
#'         table, transfer table, and conclusions.
#'
#' @export
tkonfound <- function(a, b, c, d,
                      alpha = 0.05, 
                      switch_trm = TRUE,
                      test = "fisher", 
                      replace = "control", 
                      to_return = to_return){
  # stop message
  if (a < 0 || b < 0 || c < 0 || d < 0) {
    stop("Please enter non-negative integers for each cell.")
  }
 
  if (a != as.integer(a) || b != as.integer(b) || c != as.integer(c) || d != as.integer(d)) {
    stop("Please enter non-negative integers for each cell.")
  }
    
  # use fisher if any of the cell is smaller than 5
  expected_a <- (a + c) * (a + b) / (a + b + c + d)
  expected_b <- (a + b) * (b + d) / (a + b + c + d)
  expected_c <- (a + c) * (c + d) / (a + b + c + d)
  expected_d <- (c + d) * (b + d) / (a + b + c + d)
  
  if ((test != "fisher") & (expected_a < 5 || expected_b < 5 || expected_c < 5 || expected_d < 5)){
      warning("Because the expected value in at least one cell is less than 5 consider rerunning using Fisher's exact p-value.\n")
  }

  ## if any of a, b, c, d is exactly zero, then we add 0.5 to all of them
  ## note that this is only for computing odds ratio and log OR
  if (a == 0 || b == 0 || c == 0 || d == 0) {
      a_OR <- a + 0.5
      b_OR <- b + 0.5
      c_OR <- c + 0.5
      d_OR <- d + 0.5
  } else {
      a_OR <- a
      b_OR <- b
      c_OR <- c 
      d_OR <- d
  }  

  odds_ratio <- a_OR * d_OR / (b_OR * c_OR)
  n_cnt <- a+b
  n_trm <- c+d
  n_obs <- n_cnt + n_trm
  est <- log(odds_ratio)

  # this is the 2 by 2 table we start with
  table_ob <- matrix(c(a, b, c, d), byrow = TRUE, 2, 2)

  if (test == "fisher") {
    p_ob <- fisher_p(a, b, c, d)
    fisher_ob <- fisher_oddsratio(a, b, c, d)
  }
  if (test == "chisq") {
    p_ob <- chisq_p(a, b, c, d)
    chisq_ob <- chisq_value(a, b, c, d)
  }

  # get solution
  if (test == "chisq"){
    solution <- getswitch_chisq(a, b, c, d, odds_ratio, alpha, switch_trm)
    chisq_final <- solution$chisq_final
  }

  if (test == "fisher"){
    solution <- getswitch_fisher(a, b, c, d, odds_ratio, alpha, switch_trm)
    fisher_final <- solution$fisher_final
  }

  table_final <- solution$Transfer_Table
  table_start <- table_ob
  dcroddsratio_ob <- solution$dcroddsratio_ob
  allnotenough <- solution$needtworows
  final <- solution$final_switch
  p_final <- solution$p_final
  taylor_pred <- solution$taylor_pred
  perc_bias_pred <- solution$perc_bias_pred
  final_extra <- solution$final_extra
  final_primary <- final - final_extra
  
  # 'final' is rounded down if it is a non-integer like 3.5
  if (final_primary %% 1 == 0.5) {
      final_primary <- floor(final_primary)  # Round down if final is x.5
  }
  
  ### add column and row names to contingency tables
  rownames(table_start) <- rownames(table_final) <- c("Control", "Treatment")
  colnames(table_start) <- colnames(table_final) <- c("Fail", "Success")

a_start <- table_ob[1,1]
b_start <- table_ob[1,2]
c_start <- table_ob[2,1]
d_start <- table_ob[2,2]

success_rate_control_start <- b_start / (a_start + b_start) * 100
success_rate_treatment_start <- d_start / (c_start + d_start) * 100
total_fail_start <- a_start + c_start
total_success_start <- b_start + d_start
total_rate_start <- total_success_start / (total_fail_start + total_success_start) * 100
  
  # Adjust table_start to a data.frame for revised 3x3 format
  table_start_revised <- data.frame(
    Fail = c(a_start, c_start, total_fail_start),
    Success = c(b_start, d_start, total_success_start),
    `Success_Rate` = c(sprintf("%.2f%%", success_rate_control_start), 
                       sprintf("%.2f%%", success_rate_treatment_start), 
                       sprintf("%.2f%%", total_rate_start))
  )
  
  rownames(table_start_revised) <- c("Control", "Treatment", "Total")

a_final <- solution$Transfer_Table[1,1]
b_final <- solution$Transfer_Table[1,2]
c_final <- solution$Transfer_Table[2,1]
d_final <- solution$Transfer_Table[2,2]

# Check if a_final, b_final, c_final, or d_final is exactly 0.5, and if so, set them to 0
if (a_final == 0.5) a_final <- 0
if (b_final == 0.5) b_final <- 0
if (c_final == 0.5) c_final <- 0
if (d_final == 0.5) d_final <- 0

success_rate_control_final <- b_final / (a_final + b_final) * 100
success_rate_treatment_final <- d_final / (c_final + d_final) * 100
total_fail_final <- a_final + c_final
total_success_final <- b_final + d_final
total_rate_final <- total_success_final / (total_fail_final + total_success_final) * 100
  
  # Adjust table_final to a data.frame for revised 3x3 format
  table_final_revised <- data.frame(
    Fail = c(a_final, c_final, total_fail_final),
    Success = c(b_final, d_final, total_success_final),
    `Success_Rate` = c(sprintf("%.2f%%", success_rate_control_final), 
                       sprintf("%.2f%%", success_rate_treatment_final), 
                       sprintf("%.2f%%", total_rate_final))
  )
  
  rownames(table_final_revised) <- c("Control", "Treatment", "Total")
  
  if (switch_trm && dcroddsratio_ob) {
    transferway <- "treatment success to treatment failure"
    transferway_start <- "treatment row"
    if (replace == "entire") {
        RIR <- ceiling(final_primary/((a+c)/n_obs))
    } else {
        RIR <- ceiling(final_primary/(a/(a+b)))
    }
    RIRway <- "treatment success"
    RIRway_start <- "treatment row"
    RIR_pi <- RIR / d * 100
    p_destination_control <- a/(a+b)
    p_destination <- round((a+c)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
        round(a/(a+b) * 100, 3) * (1 - (replace == "entire"))
  }
  if (switch_trm && !dcroddsratio_ob) {
    transferway <- "treatment failure to treatment success"
    transferway_start <- "treatment row"
    if (replace == "entire") {
        RIR <- ceiling(final_primary/((b+d)/n_obs))
    } else {
        RIR <- ceiling(final_primary/(b/(a+b)))
    }
    RIRway <- "treatment failure"
    RIRway_start <- "treatment row"
    RIR_pi <- RIR / c * 100
    p_destination_control <- b/(a+b)
    p_destination <- round((b+d)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
        round(b/(a+b) * 100, 3) * (1 - (replace == "entire"))
  }
  if (!switch_trm && dcroddsratio_ob) {
    transferway <- "control failure to control success"
    transferway_start <- "control row"
    if (replace == "entire") {
        RIR <- ceiling(final_primary/((b+d)/n_obs))
    } else {
        RIR <- ceiling(final_primary/(b/(a+b)))
    }
    RIRway <- "control failure"
    RIRway_start <- "control row"
    RIR_pi <- RIR / a * 100
    p_destination_control <- b/(a+b)
    p_destination <- round((b+d)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
        round(b/(a+b) * 100, 3) * (1 - (replace == "entire"))
  }
  if (!switch_trm && !dcroddsratio_ob) {
    transferway <- "control success to control failure"
    transferway_start <- "control row"
    if (replace == "entire") {
        RIR <- ceiling(final_primary/((a+c)/n_obs))
    } else {
        RIR <- ceiling(final_primary/(a/(a+b)))
    }
    RIRway <- "control success"
    RIRway_start <- "control row"
    RIR_pi <- RIR / b * 100
    p_destination_control <- a/(a+b)
    p_destination <- round((a+c)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
        round(a/(a+b) * 100, 3) * (1 - (replace == "entire"))
  }
  
  RIRway_split <- strsplit(RIRway, " ")[[1]][1]
  RIR_extra <- NA
  p_destination_control_extra <- NA

  if (allnotenough) {
      # if need two rows, then do not report RIR_pi
      ## because denominator is tricky
      RIR_pi <- NA
      
      # apply similar rounding down like `final` for `final_extra` if needed
      if (final_extra %% 1 == 0.5) {
          final_extra <- floor(final_extra)
      }
      
    if (switch_trm && dcroddsratio_ob) {
      transferway_extra <- "control failure to control success"
      transferway_extra_start <- "control row"
      if (replace == "entire") {
          RIR_extra <- ceiling(final_extra/((b+d)/n_obs))
      } else {
          RIR_extra <- ceiling(final_extra/(b/(b+a)))
      }
      RIRway_extra <- "control failure"
      RIRway_extra_start <- "control row"
      p_destination_control_extra <- b/(a+b)
      p_destination_extra <- round((b+d)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
          round(b/(a+b) * 100, 3) * (1 - (replace == "entire"))
    }
    if (switch_trm && !dcroddsratio_ob) {
      transferway_extra <- "control success to control failure"
      transferway_extra_start <- "control row"
      if (replace == "entire") {
          RIR_extra <- ceiling(final_extra/((a+c)/n_obs))
      } else {
          RIR_extra <- ceiling(final_extra/(a/(a+b)))
      }
      RIRway_extra <- "control success"
      RIRway_extra_start <- "control row"
      p_destination_control_extra <- a/(a+b)
      p_destination_extra <- round((a+c)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
          round(a/(a+b) * 100, 3) * (1 - (replace == "entire"))
    }
    if (!switch_trm && dcroddsratio_ob) {
      transferway_extra <- "treatment success to treatment failure"
      transferway_extra_start <- "treatment row"
      if (replace == "entire") {
          RIR_extra <- ceiling(final_extra/((a+c)/n_obs))
      } else (
          RIR_extra <- ceiling(final_extra/(a/(a+b)))
      )
      RIRway_extra <- "treatment success"
      RIRway_extra_start <- "treatment row"
      p_destination_control_extra <- a/(a+b)
      p_destination_extra <- round((a+c)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
          round(a/(a+b) * 100, 3) * (1 - (replace == "entire"))
    }
    if (!switch_trm && !dcroddsratio_ob) {
      transferway_extra <- "treatment failure to treatment success"
      transferway_extra_start <- "treatment row"
      if (replace == "entire") {
          RIR_extra <- ceiling(final_extra/((b+d)/n_obs))
      } else {
          RIR_extra <- ceiling(final_extra/(b/(b+a)))
      }
      RIRway_extra <- "treatment failure"
      RIRway_extra_start <- "treatment row"
      p_destination_control_extra <- b/(a+b)
      p_destination_extra <- round((b+d)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
          round(b/(a+b) * 100, 3) * (1 - (replace == "entire"))
    }
  }

  
  ### Output language objects
  # Conditional Fragility calculation component 
  if (p_ob < 0.05) {
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
  
  if (allnotenough) {
      # Conditional Fragility calculation component 
      if (p_ob < 0.05) {
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
  
  
  if (p_ob < alpha) {
    change <- paste0("To invalidate the inference that the effect is different from 0 (alpha = ", alpha, "),\n")
  } else {
    change <- paste0("To sustain an inference that the effect is different from 0 (alpha = ", alpha, "),\n")
  }

  if (!allnotenough & final > 1) {
    conclusion1 <- paste0(
      change, 
      sprintf("one would need to transfer %g data points from ", final), transferway, " as shown,\n",
      "from the User-entered Table to the Transfer Table (Fragility = ", final, ").\n",
      sprintf("This is equivalent to replacing %g (%.3f%%) ", RIR, RIR_pi), RIRway, " data points with data points")

    if (replace == "control") {
      conclusion1b <- paste0(
        sprintf("for which the probability of %s in the control group (%g%%) applies (RIR = %g). ", prob_indicator, p_destination, RIR))
    } else {
      conclusion1b <- paste0(
        sprintf("for which the probability of %s in the entire group (%g%%) applies (RIR = %g). ", prob_indicator, p_destination, RIR))
    }

    conclusion1c <- paste0(
      "\nRIR = Fragility/P(destination)"
      )
  }

  if (!allnotenough & final == 1) {
      conclusion1 <- paste0(
          change, 
          sprintf("one would need to transfer %g data points from ", final), transferway, " as shown,\n",
          "from the User-entered Table to the Transfer Table (Fragility = ", final, ").\n",
          sprintf("This is equivalent to replacing %g (%.3f%%) ", RIR, RIR_pi), RIRway, " data points with data points")
      
      if (replace == "control") {
          conclusion1b <- paste0(
              sprintf("for which the probability of %s in the control group (%g%%) applies (RIR = %g). ", prob_indicator, p_destination, RIR))
      } else {
          conclusion1b <- paste0(
              sprintf("for which the probability of %s in the entire group (%g%%) applies (RIR = %g). ", prob_indicator, p_destination, RIR))
      }
      
      conclusion1c <- paste0(
          "\nRIR = Fragility/P(destination)"
      )
  }

  ### Special case if RIR percentage > 100
  if (!allnotenough && RIR_pi > 100) {
      total_Fragility <- final_primary + final_extra
      conclusion_large_rir <- paste0(
          sprintf("\nNote the RIR exceeds 100%%. Generating the transfer of %d data points would", total_Fragility),
          "\nrequire replacing more data points than are in the ", RIRway, " condition.")
  } else {
      conclusion_large_rir <- ""  # Empty string if RIR_pi <= 100
  }
  
  if (allnotenough) {
      change_t <- paste0(tolower(substr(change, 1, 1)), substr(change, 2, nchar(change)))
      
      conclusion1 <- paste0(
          "In terms of Fragility, ", change_t, "only transferring ", final_primary, " data points from ", 
          transferway, " is not enough to change the\n",
          "inference. One would also need to transfer ", final_extra, " data points from ", transferway_extra
      )
      conclusion1b <- paste0(
          "as shown, from the User-entered Table to the Transfer Table.\n"
      )
      conclusion1c <- paste0(
          "In terms of RIR, generating the ", final_primary, " switches from ", transferway, " is equivalent to\n",
          "replacing ", RIR, " ", RIRway, " data points with data points for which the probability of ", prob_indicator, " in the\n", 
          replace, " sample (", p_destination, "%) applies.\n\n",
          "In addition, generating the ", final_extra, " switches from ", transferway_extra, " is equivalent to\n",
          "replacing ", RIR_extra, " ", RIRway_extra, " data points with data points for which the probability of ", prob_indicator_extra, "\n",
          "in the ", replace, " sample (", p_destination_extra, "%) applies.\n"
      )
      conclusion1d <- paste0(
          "Therefore, the total RIR is ", RIR + RIR_extra, ".\n\n",
          "RIR = Fragility/P(destination)"
      )
      
  }

  citation <- paste0(
      "See Frank et al. (2021) for a description of the methods.\n\n",
      "*Frank, K. A., *Lin, Q., *Maroulis, S., *Mueller, A. S., Xu, R., Rosenberg, J. M., ... & Zhang, L. (2021).\n",
      "Hypothetical case replacement can be used to quantify the robustness of trial results. Journal of Clinical\n",
      "Epidemiology, 134, 150-159.\n",
      "*authors are listed alphabetically." 
  )
  
  if (test == "chisq"){
    conclusion2 <- sprintf(
      "For the User-entered Table, the Pearson's chi square is %.3f, with p-value of %.3f:", chisq_ob, p_ob)
    conclusion3 <- sprintf(
      "For the Transfer Table, the Pearson's chi square is %.3f, with p-value of %.3f:", chisq_final, p_final)
  }

  if (test == "fisher"){
    conclusion2 <- sprintf(
      "For the User-entered Table, the estimated odds ratio is %.3f, with p-value of %.3f:", fisher_ob, p_ob)
    conclusion3 <- sprintf(
      "For the Transfer Table, the estimated odds ratio is %.3f, with p-value of %.3f:", fisher_final, p_final)
  }

  info1 <- "This function calculates the number of data points that would have to be replaced with"
  info2 <- "zero effect data points (RIR) to invalidate the inference made about the association"
  info3 <- "between the rows and columns in a 2x2 table."
  info4 <- "One can also interpret this as switches (Fragility) from one cell to another, such as from the"
  info5 <- "treatment success cell to the treatment failure cell."

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
                     RIR_perc = RIR_pi,
                     fragility_primary = final_primary,
                     fragility_supplemental = final_extra,
                     starting_table = table_start_revised,
                     final_table = table_final_revised,
                     user_SE = NA,
                     analysis_SE = NA,
                     needtworows = allnotenough,
                     Fig_ITCV = NA,
                     Fig_RIR = NA))
    result <- list(info1,
                   info2,
                   conclusion1,
                   conclusion1b,
                   conclusion1c,
                   User_enter_value = table_start,
                   Transfer_Table = table_final,
                   conclusion2,
                   conclusion3,
                   RIR = RIR)

    return(result)
    
  } else  if (to_return == "print") {
    cat(crayon::bold("Robustness of Inference to Replacement (RIR):"))
    cat("\n")
    if(!allnotenough){
        cat("RIR =", RIR)
        cat("\n")
        cat("Fragility =", final)
        cat("\n")
    } else if (allnotenough){
        # Total RIR = primary RIR + supplemental RIR
        total_RIR <- RIR + RIR_extra
        cat("RIR = ", RIR, " + ", RIR_extra, " = ", total_RIR, "\n", sep = "")
        cat("Total RIR = Primary RIR in ", RIRway_start, " + Supplemental RIR in ", RIRway_extra_start, "\n\n", sep = "")
        
        # Total Fragility = primary Fragility + supplemental Fragility
        total_Fragility <- final_primary + final_extra
        cat("Fragility = ", final_primary, " + ", final_extra, " = ", total_Fragility, "\n", sep = "")
        cat("Total Fragility = Primary Fragility in ", transferway_start, " + Supplemental Fragility in ", transferway_extra_start, "\n", sep = "")
    }
   
    cat("\n")
    cat(info1)
    cat("\n")
    cat(info2)
    cat("\n")
    cat(info3)
    cat("\n")
    cat(info4)
    cat("\n")
    cat(info5)
    cat("\n")
    cat("\n")
    cat(conclusion1)
    cat("\n")
    cat(conclusion1b)
    cat("\n")
    cat(conclusion1c)
    cat(conclusion_large_rir)
    if (allnotenough){
        cat("\n")
        cat(conclusion1d)
    }
    cat("\n")
    if (p_destination_control == 0 | 
        (!is.na(p_destination_control_extra) & p_destination_control_extra == 0)) {
        cat("\n")
        cat("The RIR is infinite because the probability used to represent the target cell is zero:")
        cat("\nRIR=Fragility/p(replacement source). Consider rerunning specifying that replacements")
        cat("\nshould be based on the probability of success/failure in the overall sample rather than")
        cat("\na specific cell (replace = 'entire').\n")
    }
    cat("\n")
    cat(conclusion2)
    cat("\n")
    cat(crayon::underline("User-entered Table:"))
    cat("\n")
    print(table_start_revised)
    cat("\n")
    cat(conclusion3)
    cat("\n")
    cat(crayon::underline("Transfer Table:"))
    cat("\n")
    print(table_final_revised)
    cat("\n")
    cat(citation)
    cat("\n")
    
  }
  
}
