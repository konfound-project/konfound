# calculating percentage bias and number of observations

calc_pct_and_n <- function(unstd_beta, beta_threshhold, n_obs) {
    if (abs(unstd_beta) > abs(beta_threshhold)) {
        bias <- 100 * (1 - (beta_threshhold / unstd_beta))
        recase <- round(n_obs * (bias / 100))
        return(list(bias, recase)) } 
    else if (abs(unstd_beta) < abs(beta_threshhold)) {
        sustain <- 100 * (1 - (unstd_beta / beta_threshhold))
        recase <- round(n_obs * (sustain / 100))
        return(list(sustain, recase)) } 
    else if (unstd_beta == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.")} }

# function to create dataframe output

create_dataframe <- function(unstd_beta, beta_threshhold, n_obs) {
    out <- calc_pct_and_n(unstd_beta = unstd_beta, beta_threshhold = beta_threshhold, n_obs = n_obs) 
    if (abs(unstd_beta) > abs(beta_threshhold)) {
        return(dplyr::data_frame(inference = "to_invalidate", 
                                 percent = round(out[[1]], 2), 
                                 observations = round(out[[2]], 3))) } 
    else if (abs(unstd_beta) < abs(beta_threshhold)) {
        return(dplyr::data_frame(inference = "to_sustain", 
                                 percent = round(out[[1]], 2), 
                                 observations = round(out[[2]], 3))) } 
    else if (unstd_beta == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.")} }

# function to create printed output

create_print <- function(unstd_beta, beta_threshhold, n_obs) {
    out <- calc_pct_and_n(unstd_beta = unstd_beta, beta_threshhold = beta_threshhold, n_obs = n_obs) 
    if (abs(unstd_beta) > abs(beta_threshhold)) {
        cat("To invalidate the inference,", round(out[[1]], 2), "% of the estimate would have to be due to bias.\n")
        cat("To invalidate the inference,", round(out[[2]], 3), "observations would have to be replaced with cases for which there is no effect.") } 
    else if (abs(unstd_beta) < abs(beta_threshhold)) {
        cat("To sustain the inference, ", round(out[[1]], 2), "% of the estimate would have to be due to bias.\n")
        cat("To sustain the inference, ", round(out[[2]], 3), " of the cases with 0 effect would have to be replaced with cases at the threshold of inference.") } 
    else if (unstd_beta == beta_threshhold) {
        warning("The coefficient is exactly equal to the threshold.")} }

# function to create plot

create_plot <- function(unstd_beta, beta_threshhold, n_obs) {
    x <- create_dataframe(unstd_beta = unstd_beta, beta_threshhold = beta_threshhold, n_obs = n_obs)
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

# Main function to test sensitivity to be wrapped with pkonfound() and konfound()

test_sensitivity <- function(unstd_beta,
                             standard_error,
                             n_obs, 
                             n_covariates,
                             alpha,
                             tails,
                             to_return) {
    if (unstd_beta < 0) {
        critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates) * -1 } 
    else {critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates) }
    beta_threshhold <- critical_t * standard_error
    if (to_return == "df") {
        create_dataframe(unstd_beta = unstd_beta, beta_threshhold = beta_threshhold, n_obs = n_obs) } 
    else if (to_return == "plot") { 
        create_plot(unstd_beta = unstd_beta, beta_threshhold = beta_threshhold, n_obs = n_obs) } 
    else if (to_return == "print") {
        create_print(unstd_beta = unstd_beta, beta_threshhold = beta_threshhold, n_obs = n_obs) } 
    else {
        stop("to_return must be set to df, print, or plot") } }