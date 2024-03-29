# Function to output the data frame

#' Output data frame based on model estimates and thresholds
#'
#' @param est_eff estimated effect
#' @param beta_threshhold threshold for beta
#' @param unstd_beta unstandardized beta value
#' @param bias bias to change inference
#' @param sustain sustain to change inference
#' @param recase number of cases to replace null
#' @param obs_r observed correlation
#' @param critical_r critical correlation
#' @param r_con correlation for omitted variable
#' @param itcv inferential threshold for confounding variable
#' @param non_linear flag for non-linear models
#' @return data frame with model information
#' @importFrom dplyr tibble
output_df <- function(est_eff, 
                      beta_threshhold,
                      unstd_beta, 
                      bias = NULL, 
                      sustain = NULL, 
                      recase, obs_r, 
                      critical_r, 
                      r_con, 
                      itcv, 
                      non_linear) {
  if (abs(est_eff) > abs(beta_threshhold)) {
    df <- dplyr::tibble(
      action = "to_invalidate",
      inference = "reject_null",
      percent_bias_to_change_inference = round(bias, 3),
      replace_null_cases = round(recase, 3),
      unstd_beta = unstd_beta,
      beta_threshhold = beta_threshhold,
      omitted_variable_corr = r_con,
      itcv = itcv
    )
  }
  else if (abs(est_eff) < abs(beta_threshhold)) {
    df <- dplyr::tibble(
      action = "to_sustain",
      inference = "fail_to_reject_null",
      percent_bias_to_change_inference = round(sustain, 3),
      replace_null_cases = round(recase, 3),
      unstd_beta = unstd_beta,
      beta_threshhold = beta_threshhold,
      omitted_variable_corr = r_con,
      itcv = itcv
    )
  }
  else if (est_eff == beta_threshhold) {
    warning("The coefficient is exactly equal to the threshold.")
  }

  # if (abs(obs_r) > abs(critical_r)) {
  #     df$omitted_variable_corr <- r_con
  # }
  # else if (abs(obs_r) < abs(critical_r)) {
  #     df$omitted_variable_corr <- r_con
  # }
  else if (est_eff == beta_threshhold) {
    warning("The coefficient is exactly equal to the threshold.\n")
  }

  df
}
