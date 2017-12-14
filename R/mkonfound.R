core_sensitivity_mkonfound <- function(t, df, alpha = .05, tails = 2) {
    critical_t <- stats::qt(1 - (alpha / tails), df)
    critical_r <- critical_t / sqrt((critical_t ^ 2) + df)
    
    obs_r <- t / sqrt(df + (t ^ 2))
    
    if (abs(obs_r) > abs(critical_r)) {
        action <- "to_invalidate"
        inference <- "reject_null"
        pct_bias <- 100 * (1 - (critical_r / obs_r)) }
    else if (abs(obs_r) < abs(critical_r)) {
        action <- "to_sustain"
        inference <- "fail_to_reject_null"
        pct_bias <- 100 * (1 - (obs_r / critical_r)) }
    else if (obs_r == critical_r) {
        action <- NA
        inference <- NA
        pct_bias <- NA
    }
    
    if ((abs(obs_r) > abs(critical_r)) & ((obs_r * critical_r) > 0)) {
        mp <- -1
    } else {
        mp <- 1
    }
    # calculating impact of the confounding variable
    itcv <- (obs_r - critical_r) / (1 + mp * abs(critical_r))
    # finding correlation of confound to invalidate / sustain inference
    r_con <- round(sqrt(abs(itcv)), 3)
    
    out <- dplyr::data_frame(t, df, action, inference, pct_bias, itcv, r_con)
    names(out) <- c("t", "df", "action", "inference", "pct_bias_to_change_inference", "itcv", "r_con")
    out$action <- as.character(out$action)
    out$inference <- as.character(out$inference)
    return(out)
}

#' Perform meta-analyses including sensitivity analysis
#' @description For fitted models, this command carries out sensitivity analysis for a number of models, when their parameters stored in a data.frame. 
#' @param df a data.frame with columns for the 'unstd_beta', 'std_err', 'n_obs', and 'n_covariates' arguments (see ?pkonfound for more information on these)
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @param return_plot whether to return a plot of the percent bias; defaults to FALSE
#' @param component_correlations whether to return the component correlations as part of the correlation-based approach
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference for each of the cases in the data.frame
#' @examples 
#' df <- data.frame(unstd_beta = c(2, 10, 1.7, .4, 3.2, 1.0, 2.3, 4.1, .9),
#'                  std_error = c(.3, 2.9, 1.5, 2, .1, .04, .1, 3, .5),
#'                  n_obs = c(70, 405, 200, 100, 103, 20, 50, 721, 320),
#'                  n_covs = c(3, 4, 1, 3, 10, 4, 1, 1, 0))
#' mkonfound(df)
#' mkonfound(df, return_plot = TRUE)
#' 
#' d <- read.csv("https://msu.edu/~kenfrank/example%20dataset%20for%20mkonfound.csv")
#' d
#' mkonfound(d$t, d$df)
#' @export
#'

mkonfound <- function(t, df, alpha = .05, tails = 2, return_plot = FALSE) {
    
    args <- list(as.list(t),
                 as.list(df))
    
    results_df <- purrr::pmap_dfr(args, core_sensitivity_mkonfound, alpha = alpha, tails = tails)
    
        if (return_plot == TRUE) {

            results_df$action <- dplyr::case_when(
                results_df$action == "to_invalidate" ~ "To Invalidate",
                results_df$action == "to_sustain" ~ "To Sustain"
            )

            p <- ggplot2::ggplot(results_df, ggplot2::aes(x = pct_bias_to_change_inference, fill = action)) +
                ggplot2::geom_histogram() +
                ggplot2::scale_fill_manual("", values = c("#1F78B4", "#A6CEE3")) +
                ggplot2::theme_bw() +
                ggplot2::ggtitle("Histogram of Percent Bias") +
                ggplot2::facet_grid(~ action) +
                ggplot2::scale_y_continuous(breaks = 1:nrow(results_df)) +
                ggplot2::theme(legend.position = "none") +
                ggplot2::ylab("Count") +
                ggplot2::xlab("Percent Bias")

            return(p)
        }
    
    return(dplyr::as_tibble(results_df))
    
}

#' Perform meta-analyses including sensitivity analysis
#' @description For fitted models, this command carries out sensitivity analysis for a number of models, when their parameters stored in a data.frame. 
#' @param df a data.frame with columns for the 'unstd_beta', 'std_err', 'n_obs', and 'n_covariates' arguments (see ?pkonfound for more information on these)
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @param return_plot whether to return a plot of the percent bias; defaults to FALSE
#' @param component_correlations whether to return the component correlations as part of the correlation-based approach
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference for each of the cases in the data.frame
#' @examples 
#' df <- data.frame(unstd_beta = c(2, 10, 1.7, .4, 3.2, 1.0, 2.3, 4.1, .9),
#'                  std_error = c(.3, 2.9, 1.5, 2, .1, .04, .1, 3, .5),
#'                  n_obs = c(70, 405, 200, 100, 103, 20, 50, 721, 320),
#'                  n_covs = c(3, 4, 1, 3, 10, 4, 1, 1, 0))
#' mkonfound(df)
#' mkonfound(df, return_plot = TRUE)
#' @export

mkonfound_d <- function(df, alpha = .05, tails = 2, return_plot = FALSE, component_correlations = FALSE) {
    args <- list(as.list(dplyr::pull(df, 1)), 
                 as.list(dplyr::pull(df, 2)), 
                 as.list(dplyr::pull(df, 3)), 
                 as.list(dplyr::pull(df, 4)))
    x <- purrr::pmap_dfr(args, pkonfound, alpha = alpha, tails = tails, to_return = "df")
    results_df <- dplyr::bind_cols(df, x)
    results_df <- dplyr::select(results_df, -.data$unstd_beta1)
    if (return_plot == TRUE) {
        
        results_df$action <- dplyr::case_when(
            results_df$action == "to_invalidate" ~ "To Invalidate",
            results_df$action == "to_sustain" ~ "To Sustain"
        )
        
        p <- ggplot2::ggplot(results_df, ggplot2::aes(x = percent_bias_to_change_inference, fill = action)) +
            ggplot2::geom_histogram() +
            ggplot2::scale_fill_manual("", values = c("#1F78B4", "#A6CEE3")) +
            ggplot2::theme_bw() +
            ggplot2::ggtitle("Histogram of Percent Bias") +
            ggplot2::facet_grid(~ action) +
            ggplot2::scale_y_continuous(breaks = 1:nrow(results_df)) +
            ggplot2::theme(legend.position = "none") +
            ggplot2::ylab("Count") +
            ggplot2::xlab("Percent Bias")
        
        return(p)
    }
    return(results_df)
} 
