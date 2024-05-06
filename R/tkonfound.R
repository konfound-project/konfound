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
  if (a < 5 || b < 5 || c < 5 || d < 5){
    test <- "fisher"
  }

  # odds_ratio <- a*d/(b*c)
  n_cnt <- a+b
  n_trm <- c+d
  n_obs <- n_cnt + n_trm
  # est <- log(odds_ratio)

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
    solution <- getswitch_chisq(a, b, c, d, alpha, switch_trm)
    chisq_final <- solution$chisq_final
  }

  if (test == "fisher"){
    solution <- getswitch_fisher(a, b, c, d, alpha, switch_trm)
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
  total_switch <- solution$total_switch

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
  RIR_extra <- 0
  RIR_extra <- final_extra <- NA

  if (allnotenough) {
      # if need two rows, then do not report RIR_pi
      ## because denominator is tricky
      RIR_pi <- NA
      final_extra <- solution$final_extra
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

  if (p_ob < alpha) {
    change <- "To invalidate the inference, "
  } else {
    change <- "To sustain an inference, "
  }

  if (!allnotenough & final > 1) {
    conclusion1 <- paste0(
      change, sprintf("one would need to replace %d ", RIR), RIRway, "data")

    if (replace == "control") {
      conclusion1b <- paste0(
        sprintf("points for which the probability of failure in the control group applies (RIR = %d). ", RIR))
    } else {
      conclusion1b <- paste0(
        sprintf("points for which the probability of failure in the entire group applies (RIR = %d). ", RIR))
    }

    conclusion1c <- paste0(
      sprintf("This is equivalent to transferring %d", final),
      " data points from ", transferway, "."
    )
  }

  if (!allnotenough & final == 1) {
    conclusion1 <- paste0(
      change, sprintf("one would need to replace %d ", RIR), RIRway)

    if (replace == "control") {
      conclusion1b <- paste0(
        sprintf("data points for which the probability of failure in the control group applies (RIR = %d). ", RIR))
    } else {
      conclusion1b <- paste0(
        sprintf("data points for which the probability of failure in the entire group applies (RIR = %d). ", RIR))
      }

    conclusion1c <- paste0(
      sprintf("This is equivalent to transferring %d", final),
      " data points from ", transferway, ".")
  }

  if (allnotenough){
    conclusion1 <- paste(
      change, c("only transferring data points from" ), transferway,
      sprintf(" is not enough. We also need to transfer %d data points from ", final_extra))

    conclusion1b <- paste0(
      transferway_extra, c("as shown, from the User-entered Table to the Transfer Table."))

    conclusion1c <- paste0(sprintf(" This means we need to replace %d of ", RIR), RIRway,
    sprintf( "with null hypothesis data points; and replace %d ", RIR_extra), RIRway_extra,
    c(" with null hypothesis data points to change the inference."))
  }

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

  info1 <- "This function calculates the number of data points that would have to be replaced"
  info2 <- "with zero effect data points (RIR) to invalidate an inference made about the"
  info3 <- "association between the rows and columns in a 2x2 table."
  info4 <- "One can also interpret this as switches from one cell to another, such as from"
  info5 <- "the treatment success cell to the treatment failure cell."

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
                     perc_bias_to_change = NA,
                     RIR_primary = RIR,
                     RIR_supplemental = RIR_extra,
                     RIR_perc = RIR_pi,
                     fragility_primary = final,
                     fragility_supplemental = final_extra,
                     starting_table = table_start,
                     final_table = table_final,
                     user_SE = NA,
                     analysis_SE = NA,
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
    cat("\n")
    cat(conclusion2)
    cat("\n")
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
    cat(crayon::bold("RIR:"))
    cat("\n")
    cat("RIR =", RIR)
    cat("\n")
    
  }
  
}
