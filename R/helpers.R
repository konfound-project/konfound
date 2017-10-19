# Function to output the data frame

output_df <- function(beta_diff, beta_threshhold, bias = NULL, sustain = NULL, recase, obs_r, critical_r, r_con) {
    if (abs(beta_diff) > abs(beta_threshhold)) {
        df  <- dplyr::data_frame(replacement_of_cases_inference = "to_invalidate", 
                                 percent_bias = round(bias, 3), 
                                 replace_null_cases = round(recase, 3),
                                 correlation_inference = NA,
                                 omitted_variable_corr = NA)
    } 
    else if (abs(beta_diff) < abs(beta_threshhold)) {
        df <- dplyr::data_frame(replacement_of_cases_inference = "to_sustain", 
                                percent_bias = round(sustain, 3), 
                                replace_null_cases = round(recase, 3),
                                correlation_inference = NA,
                                omitted_variable_corr = NA)
    } 
    else if (beta_diff == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.") } 
    if (abs(obs_r) > abs(critical_r)) {
        df$correlation_inference <- "to_invalidate"
        df$omitted_variable_corr <- r_con
    }
    else if (abs(obs_r) < abs(critical_r)) {
        df$correlation_inference <- "to_sustain"
        df$omitted_variable_corr <- r_con
    }
    else if (beta_diff == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.\n") }
    df
}

# Function to output the plot

output_plot <- function(beta_diff, beta_threshhold, bias = NULL, sustain = NULL, recase, obs_r, critical_r, r_con) {
    to_plot <- output_df(beta_diff, beta_threshhold, bias, sustain, recase)
    to_plot <- dplyr::mutate(to_plot, remainder = 100 - percent_bias)
    to_plot <- tidyr::gather(to_plot, key, val, -inference, -replace_null_cases)
    # to_plot <- dplyr::mutate(to_plot, inference = dplyr::case_when(
    #     inference == "to_invalidate" ~ "To Invalidate",
    #     inference == "to_sustain" ~ "To Sustain"),
    #     key = as.factor(key))
    return(to_plot)
    ggplot2::ggplot(to_plot, ggplot2::aes_string(x = "inference", y = "val", fill = "key")) +
        ggplot2::geom_col(position = ggplot2::position_fill(reverse = TRUE)) +
        ggplot2::scale_fill_manual("", values = c("cyan4", "lightgray"), breaks = c("percent_bias", "remainder")) +
        ggplot2::xlab("") +
        ggplot2::ylab("%") +
        ggplot2::ggtitle("Percentage of Effect Needed to Invalidate or Sustain the Inference") 
} 

# Function to output printed text

output_print <- function(beta_diff, beta_threshhold, bias = NULL, sustain = NULL, recase, obs_r, critical_r, r_con) {
    if (abs(beta_diff) > abs(beta_threshhold)) {
        cat("To invalidate the inference,", round(bias, 3), "% of the estimate would have to be due to bias.\n")
        cat("To invalidate the inference,", round(recase, 3), "observations would have to be replaced with cases for which there is no effect.\n") } 
    else if (abs(beta_diff) < abs(beta_threshhold)) {
        cat("To sustain the inference, ", round(sustain, 3), "% of the estimate would have to be due to bias.\n")
        cat("To sustain the inference, ", round(recase, 3), " of the cases with 0 effect would have to be replaced with cases at the threshold of inference.\n") } 
    else if (beta_diff == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.\n") }
    
    # output
    if (abs(obs_r) > abs(critical_r)) {
        cat("An omitted variable would have to be correlated at", r_con, "with the outcome and at", r_con, "with the predictor of interest (conditioning on observed covariates) to invalidate an inference.\n") }
    else if (abs(obs_r) < abs(critical_r)) {
        cat("An omitted variable would have to be correlated at", r_con, "with the outcome and at", r_con, "with the predictor of interest (conditioning on observed covariates) to sustain an inference.\n") }
    else if (r_con == itcv) {
        warning("The correlation is exactly equal to the threshold.\n") }
    
}

# Main function to test sensitivity to be wrapped with pkonfound(), konfound(), and mkonfound()

test_sensitivity <- function(unstd_beta,
                             std_err,
                             n_obs, 
                             n_covariates,
                             alpha,
                             tails,
                             nu,
                             to_return) {
    # calculating statistics used in every case
    if (unstd_beta < 0) {
        critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 1) * -1 } 
    else {critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 1) }
    beta_threshhold <- critical_t * std_err
    # dealing with cases where hypotheses other than whether unstd_beta differs from 0
    if (nu != 0) {
        beta_diff <- abs(unstd_beta - nu) } else {
            beta_diff <- unstd_beta - 0 } # this is just to make what this is doing evident
    
    # for replacement of cases approach
    
    # calculating percentage of effect and number of observations to sustain or invalidate inference
    if (abs(beta_diff) > abs(beta_threshhold)) {
        bias <- 100 * (1 - (beta_threshhold / beta_diff))
        recase <- round(n_obs * (bias / 100)) }
    else if (abs(beta_diff) < abs(beta_threshhold)) {
        sustain <- 100 * (1 - (beta_diff / beta_threshhold))
        recase <- round(n_obs * (sustain / 100)) }
    else if (beta_diff == beta_threshhold) {
        stop("The coefficient is exactly equal to the threshold.") }
    
    # for correlation-based approach
    
    # transforming t into r
    obs_r <- (beta_diff / std_err) / sqrt(((n_obs - n_covariates - 3) + ((beta_diff / std_err) ^ 2)))
    # finding critical r
    critical_r <- critical_t / sqrt((critical_t ^ 2) + (n_obs - n_covariates - 3))
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
    
    # needs obs_r, critical_r, and r_con
    
    # output dispatch
    if (to_return == "df") return(output_df(beta_diff, beta_threshhold, bias, sustain, recase, obs_r, critical_r, r_con))
    else if (to_return == "plot") return(output_plot(beta_diff, beta_threshhold, bias, sustain, recase, obs_r, critical_r, r_con)) 
    else if (to_return == "print") return(output_print(beta_diff, beta_threshhold, bias, sustain, recase, obs_r, critical_r, r_con))
    else stop("to_return must be set to df, print, or plot") }    

## quiets concerns (notes) of R CMD check re: the vars that are evaluated using non-standard evaluation
if (getRversion() >= "2.15.1")  utils::globalVariables(c("inference", "key", "replace_null_cases", "percent_bias", "val"))

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Sensitivity analysis as described in Frank, Maroulis, Duong, and Kelcey (2013).\nFind more information at https://jmichaelrosenberg.shinyapps.io/shinykonfound/")
}

#' Open codebook
#' @details Open the Shiny interactive web application in a browser
#' @return Launches a web browser
#' @export

launch_shiny <- function(){
    browseURL("https://jmichaelrosenberg.shinyapps.io/shinykonfound/")
}