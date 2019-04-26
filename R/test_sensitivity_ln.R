#' # helpers for the core sensitivity analysis function
#' 
#' create_konfound_class <- function(x) {
#'   structure(x, class = "konfound")
#' }
#' 
#' #' Concise summary of konfound output
#' #' @details Prints a concise summary of konfound output with multiple types of data specified in the to_return argument
#' #' @param object A `konfound` object
#' #' @param ... Additional arguments
#' #' @export
#' 
#' summary.konfound <- function(object, ...) {
#'   cat("Created", length(object), "forms of output. To access type: \n")
#'   cat("\n")
#' 
#'   for (name in names(object)) {
#'     cat(rlang::expr_text(substitute(object)), "$", name, "\n", sep = "")
#'   }
#' }

# Main function to test sensitivity to be wrapped with pkonfound(), konfound(), and mkonfound()

test_sensitivity_nl <- function(est_eff,
                             std_err,
                             n_obs,
                             n_covariates,
                             alpha,
                             tails,
                             nu,
                             to_return,
                             model_object,
                             tested_variable) {

  # calculating statistics used in every case
  if (est_eff < 0) {
    critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 1) * -1
  }
  else {
    critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 1)
  }

  beta_threshold <- critical_t * std_err
  # dealing with cases where hypotheses other than whether est_eff differs from 0
  if (nu != 0) {
    est_eff <- abs(est_eff - nu)
  } else {
    est_eff <- est_eff - 0
  } # this is just to make what this is doing evident

  # for replacement of cases approach

  # calculating percentage of effect and number of observations to sustain or invalidate inference
  if (abs(est_eff) > abs(beta_threshold)) {
    bias <- 100 * (1 - (beta_threshold / est_eff))
    recase <- round(n_obs * (bias / 100))
  }
  else if (abs(est_eff) < abs(beta_threshold)) {
    sustain <- 100 * (1 - (est_eff / beta_threshold))
    recase <- round(n_obs * (sustain / 100))
  }
  else if (est_eff == beta_threshold) {
    stop("The coefficient is exactly equal to the threshold.")
  }

  # for correlation-based approach

  # transforming t into r
  obs_r <- (est_eff / std_err) / sqrt(((n_obs - n_covariates - 3) + ((est_eff / std_err)^2)))
  # finding critical r
  critical_r <- critical_t / sqrt((critical_t^2) + (n_obs - n_covariates - 3))
  # calculating threshold
  if ((abs(obs_r) > abs(critical_r)) & ((obs_r * critical_r) > 0)) {
    mp <- -1
  } else {
    mp <- 1
  }
  # calculating impact of the confounding variable
  itcv <- (obs_r - critical_r) / (1 + mp * abs(critical_r))
  # finding correlation of confound to invalidate / sustain inference
  r_con <- round(sqrt(abs(itcv)), 3)

  # output dispatch

  # if (length(to_return) > 1) {
  #   to_return <- to_return[!(to_return == "print")]
  # 
  #   konfound_output <- purrr::map(
  #     to_return,
  #     ~ test_sensitivity(
  #       est_eff = est_eff,
  #       std_err = std_err,
  #       n_obs = n_obs,
  #       n_covariates = n_covariates,
  #       alpha = alpha,
  #       tails = tails,
  #       nu = nu,
  #       to_return = .
  #     )
  #   )
    konfound_output <- create_konfound_class(konfound_output)
    names(konfound_output) <- to_return
    output_print(est_eff, beta_threshold, bias, sustain, nu, recase, obs_r, critical_r, r_con, itcv, alpha)

    cat("\n")
    message(paste("Print output created by default. Created", length(konfound_output), "other forms of output. Use list indexing or run summary() on the output to see how to access."))

    return(konfound_output)
  }

  else if (to_return == "raw_output") {
    return(output_df(est_eff, beta_threshold, est_eff, bias, sustain, recase, obs_r, critical_r, r_con, itcv))
  } else if (to_return == "thresh_plot") { # this still makes sense for NLMs (just not quite as accurate)
    return(plot_threshold(beta_threshold = beta_threshold, est_eff = est_eff))
  # } else if (to_return == "corr_plot") {
  #   return(plot_correlation(r_con = r_con, obs_r = obs_r, critical_r = critical_r))
  } else if (to_return == "print") {
    return(output_print(est_eff, beta_threshold, bias, sustain, nu, recase, obs_r, critical_r, r_con, itcv, alpha))
  } else if (to_return == "table") {
    return(output_table(model_object, tested_variable))
  } else if (to_return == "nl_table") {
      return(output_table(model_object, tested_variable))
  } else {
    stop("to_return must be set to 'raw_output', 'print', 'table', 'thresh_plot', or 'corr_plot' or some combination thereof")
  }
}

konfoundl <- function(est_eff, std_err, n_obs, n_trm, switch_trm = TRUE, thr_t = 1.96) {
# Get input for estimate, standard error, sample size & one marginal
# est_eff <- -0.2
# std_err <- 0.10383
# n_obs <- 20888
# n_trm <- 17888
# switch_trm <- F
# thr_t <- -15

# stop message
if (n_obs <= 0 || n_trm <= 0) 
  {stop("Please enter positive integers for sample size and number of treatment group cases!")}
if (n_obs <= n_trm) 
  {stop("The total sample size should be larger than the number of treatment group cases.")}

odds_ratio <- exp(est_eff)
# n_trm is the number of observations in the treatment group (c+d)
# n_cnt is the number of observations in the control group (a+b)
n_cnt <- n_obs - n_trm
# t_ob is the t value calculated based on observed estimate and standard error
t_ob <- est_eff/std_err
# invalidate_ob is true - the observed result is significant - we are invalidating the observed result
invalidate_ob <- isinvalidate(thr_t,t_ob)
# dcroddsratio_ob is true - our goal is to decrease the odds ratio
dcroddsratio_ob <- isdcroddsratio(thr_t, t_ob)

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
### remove the solution if one cell has fewerer than 5 cases or negative cells 
check1 <- check2 <- TRUE
if (!(n_cnt>=a1 && a1>5 && n_cnt>=b1 && b1>5 && n_trm>=c1 && c1>5 && n_trm>=d1 && d1>5))
  {check1 <- FALSE}
if (!(n_cnt>=a2 && a2>5 && n_cnt>=b2 && b2>5 && n_trm>=c2 && c2>5 && n_trm>=d2 && d2>5))
  {check2<- FALSE}
if (check1) {
  table_bstart1 <- get_abcd_kfnl(a1, b1, c1, d1)
  solution1 <- getswitch(table_bstart1, thr_t, switch_trm, n_obs)
  }
if (check2) {
  table_bstart2 <- get_abcd_kfnl(a2, b2, c2, d2)
  solution2 <- getswitch(table_bstart2, thr_t, switch_trm, n_obs)
  }
if (!check1 && !check2)
  {stop("Cannot generate a reasonable contingency table!")}

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


if (switch_trm && dcroddsratio_ob) 
{transferway <- "treatment success to treatment failure,"} 
if (switch_trm && !dcroddsratio_ob) 
{transferway <- "treatment failure to treatment success,"} 
if (!switch_trm && dcroddsratio_ob) 
{transferway <- "control failure to control success,"} 
if (!switch_trm && !dcroddsratio_ob) 
{transferway <- "control success to control failure,"}

if (final_solution$needtworows) {
  if (switch_trm && dcroddsratio_ob) 
  {transferway_extra <- "control failure to control success,"} 
  if (switch_trm && !dcroddsratio_ob) 
  {transferway_extra <- "control success to control failure,"} 
  if (!switch_trm && dcroddsratio_ob) 
  {transferway_extra <- "treatment success to treatment failure,"} 
  if (!switch_trm && !dcroddsratio_ob) 
  {transferway_extra <- "treatment failure to treatment success,"}
  }

if (invalidate_ob) {
  change <- "To invalidate the inference,"
} else {
  change <- "To sustain the inference,"
}

if (!final_solution$needtworows) {
  conclusion1 <- paste(change, sprintf("%d cases need to be transferred from", final_solution$final_switch), 
                    transferway, c("as shown from the Implied Table to the Transfer Table."))
} else {
  conclusion1 <- paste(change, c("only transferring cases from"), transferway,
                      sprintf("is not enough. We also need to transfer %d cases from", final_solution$final_extra), 
                      transferway_extra, c("as shown from the Implied Table to the Transfer Table."))
  
}

conclusion2 <- sprintf("For the Implied Table, we have estimate of %.3f, with standard error of %.3f and t-ratio of %.3f.", 
                       final_solution$est_eff_start, final_solution$std_err_start, final_solution$t_start)
conclusion3 <- sprintf("For the Transferred Table, we have estimate of %.3f, with standard error of %.3f and t-ratio of %.3f.", 
                            final_solution$est_eff_final, final_solution$std_err_final, final_solution$t_final)

result <- list(conclusion1, Implied_Table = final_solution$table_start, Transfer_Table = final_solution$table_final, 
               conclusion2, conclusion3, Implied_Estimate = final_solution$est_eff_start, Transfer_Estimate = final_solution$est_eff_final, 
               Implied_SE = final_solution$std_err_start, Transfer_SE = final_solution$std_err_final, 
               Implied_tratio = final_solution$t_start, Transfer_tratio = final_solution$t_final, 
               Taylor_predict = final_solution$taylor_pred, Percent_bias_predict = final_solution$perc_bias_pred)

return(result)
}

