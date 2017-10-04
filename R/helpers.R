# Function to output the data frame

output_df <- function(beta_diff, beta_threshhold, bias = NULL, sustain = NULL, recase) {
    if (abs(beta_diff) > abs(beta_threshhold)) {
        return(dplyr::data_frame(inference = "to_invalidate", 
                                 percent_bias = round(bias, 2), 
                                 replace_null_cases = round(recase, 3))) } 
    else if (abs(beta_diff) < abs(beta_threshhold)) {
        return(dplyr::data_frame(inference = "to_sustain", 
                                 percent_bias = round(sustain, 2), 
                                 replace_null_cases = round(recase, 3))) } 
    else if (beta_diff == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.") } }

# Function to output the plot

output_plot <- function(beta_diff, beta_threshhold, bias = NULL, sustain = NULL, recase) {
    output_df(beta_diff, beta_threshhold, bias, sustain, recase) %>% 
        dplyr::mutate(remainder = 100 - percent_bias) %>% 
        tidyr::gather(key, val, -inference, -replace_null_cases) %>%
        dplyr::mutate(inference = dplyr::case_when(
            inference == "to_invalidate" ~ "To Invalidate",
            inference == "to_sustain" ~ "To Sustain")) %>% 
    ggplot2::ggplot(ggplot2::aes_string(x = "inference", y = "val", fill = "key")) +
        ggplot2::geom_col(position = ggplot2::position_fill(reverse = TRUE)) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::scale_fill_manual("", values = c("cyan4", "lightgray"), breaks = "percent_bias") +
        ggplot2::xlab("") +
        ggplot2::ylab("%") +
        ggplot2::ggtitle("Percentage of Effect Needed to Invalidate or Sustain the Inference") 
    } 

# Function to output printed text

output_print <- function(beta_diff, beta_threshhold, bias = NULL, sustain = NULL, recase) {
    if (abs(beta_diff) > abs(beta_threshhold)) {
        cat("To invalidate the inference,", round(bias, 2), "% of the estimate would have to be due to bias.\n")
        cat("To invalidate the inference,", round(recase, 3), "observations would have to be replaced with cases for which there is no effect.") } 
    else if (abs(beta_diff) < abs(beta_threshhold)) {
        cat("To sustain the inference, ", round(sustain, 2), "% of the estimate would have to be due to bias.\n")
        cat("To sustain the inference, ", round(recase, 3), " of the cases with 0 effect would have to be replaced with cases at the threshold of inference.") } 
    else if (beta_diff == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.") } }

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
    # calculating percentage of effect and number of observations to sustain or invalidate inference
    if (abs(beta_diff) > abs(beta_threshhold)) {
        bias <- 100 * (1 - (beta_threshhold / beta_diff))
        recase <- round(n_obs * (bias / 100)) }
    else if (abs(beta_diff) < abs(beta_threshhold)) {
        sustain <- 100 * (1 - (beta_diff / beta_threshhold))
        recase <- round(n_obs * (sustain / 100)) }
    else if (beta_diff == beta_threshhold) {
        stop("The coefficient is exactly equal to the threshold.") }
    # output dispatch
    if (to_return == "df") return(output_df(beta_diff, beta_threshhold, bias, sustain, recase))
    else if (to_return == "plot") return(output_plot(beta_diff, beta_threshhold, bias, sustain, recase)) 
    else if (to_return == "print") return(output_print(beta_diff, beta_threshhold, bias, sustain, recase))
    else stop("to_return must be set to df, print, or plot") }    

## quiets concerns (notes) of R CMD check re: the vars that are evaluated using non-standard evaluation
if (getRversion() >= "2.15.1")  utils::globalVariables(c("inference", "key", "replace_null_cases", "percent_bias", "val"))