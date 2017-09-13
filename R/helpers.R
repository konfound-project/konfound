# Main function to test sensitivity to be wrapped with pkonfound() and konfound()

test_sensitivity <- function(unstd_beta,
                             standard_error,
                             n_obs, 
                             n_covariates,
                             alpha,
                             tails,
                             nu,
                             to_return) {
    
    # calculating statistics used in every case
    if (unstd_beta < 0) {
        critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates) * -1 } 
    else {critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates) }
    beta_threshhold <- critical_t * standard_error

    # dealing with cases where hypotheses other than whether unstd_beta differs from 0
    if (nu != 0) {
        beta_diff <- abs(unstd_beta - nu)
    } else {
        beta_diff <- unstd_beta - 0 # this is just to make what this is doing evident
    }

    # calculating percentage of effect and number of observations to sustain or invalidate inference
    if (abs(beta_diff) > abs(beta_threshhold)) {
        bias <- 100 * (1 - (beta_threshhold / beta_diff))
        recase <- round(n_obs * (bias / 100)) }
    else if (abs(beta_diff) < abs(beta_threshhold)) {
        sustain <- 100 * (1 - (beta_diff / beta_threshhold))
        recase <- round(n_obs * (sustain / 100)) }
    else if (beta_diff == beta_threshhold) {
        stop("The coefficient is exactly equal to the threshold.") }
    
    # output for a data.frame - should pull this and the next two sections into separate functions
    if (to_return == "df") {
        if (abs(beta_diff) > abs(beta_threshhold)) {
            return(dplyr::data_frame(inference = "to_invalidate", 
                                     percent = round(bias, 2), 
                                     observations = round(recase, 3))) } 
        else if (abs(beta_diff) < abs(beta_threshhold)) {
            return(dplyr::data_frame(inference = "to_sustain", 
                                     percent = round(sustain, 2), 
                                     observations = round(recase, 3))) } 
        else if (beta_diff == beta_threshhold) {
            warning("The coefficient is exactly equal to the threshold.") } }
        
    # output for a plot 
    else if (to_return == "plot") {
        if (abs(beta_diff) > abs(beta_threshhold)) {
            x <- dplyr::data_frame(inference = "to_invalidate", 
                                     percent = round(bias, 2), 
                                     observations = round(recase, 3)) } 
        else if (abs(beta_diff) < abs(beta_threshhold)) {
            x <- dplyr::data_frame(inference = "to_sustain", 
                                     percent = round(sustain, 2), 
                                     observations = round(recase, 3)) } 
        else if (beta_diff == beta_threshhold) {
            warning("The coefficient is exactly equal to the threshold.") }
        x <- dplyr::mutate(x, remainder = 100 - percent)
        x <- tidyr::gather(x, key, val, -inference, -observations)
        x <- dplyr::mutate(x, inference = dplyr::case_when(
            inference == "to_invalidate" ~ "To Invalidate",
            inference == "to_sustain" ~ "To Sustain"
        )) 
        ggplot2::ggplot(x, ggplot2::aes(x = inference, y = val, fill = key)) +
            ggplot2::geom_col(position = ggplot2::position_fill(reverse = TRUE)) +
            ggplot2::theme(legend.position = "none") +
            ggplot2::scale_fill_manual("", values = c("cyan4", "lightgray"), breaks = "percent") +
            ggplot2::xlab("") +
            ggplot2::ylab("%") +
            ggplot2::ggtitle("Percentage of Effect Needed to Invalidate or Sustain the Inference") }
    
    # output for printing
    else if (to_return == "print") {
        if (abs(beta_diff) > abs(beta_threshhold)) {
            cat("To invalidate the inference,", round(bias, 2), "% of the estimate would have to be due to bias.\n")
            cat("To invalidate the inference,", round(recase, 3), "observations would have to be replaced with cases for which there is no effect.") } 
        else if (abs(beta_diff) < abs(beta_threshhold)) {
            cat("To sustain the inference, ", round(sustain, 2), "% of the estimate would have to be due to bias.\n")
            cat("To sustain the inference, ", round(recase, 3), " of the cases with 0 effect would have to be replaced with cases at the threshold of inference.") } 
        else if (beta_diff == beta_threshhold) {
            warning("The coefficient is exactly equal to the threshold.") } }
    else {
        stop("to_return must be set to df, print, or plot") } }