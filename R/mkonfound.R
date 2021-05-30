#' Perform meta-analyses including sensitivity analysis
#' @description For fitted models, this command carries out sensitivity analysis for a number of models, when their parameters stored in a data.frame.
#' @param d data.frame or tibble with the t-statistics and associated degrees of freedom
#' @param t t-statistic or vector of t-statistics
#' @param df degrees of freedom or vector of degrees of freedom associated with the t-statistics in the t argument
#' @param return_plot whether to return a plot of the percent bias; defaults to FALSE
#' @inheritParams pkonfound
#' @import rlang
#' @import dplyr
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference for each of the cases in the data.frame
#' @examples
#' \dontrun{
#' d <- read.csv("https://msu.edu/~kenfrank/example%20dataset%20for%20mkonfound.csv")
#' d <- d[1:3, ] # this is only so that the example runs more quickly
#' str(d)
#' mkonfound(d, t, df)
#' }
#' @export
#'

mkonfound <- function(d, t, df, alpha = .05, tails = 2, return_plot = FALSE) {
  t_enquo <- enquo(t)
  df_enquo <- enquo(df)

  t_vec <- pull(select(d, !!t_enquo))
  df_vec <- pull(select(d, !!df_enquo))

  if (length(t_vec) <= 1) {
    stop("To carry out sensitivity analysis for a single study, use pkonfound()")
  }

  results_df <- suppressWarnings(purrr::map2_dfr(.x = t_vec, .y = df_vec, .f = core_sensitivity_mkonfound))

  if (return_plot == TRUE) {
    results_df$action <- dplyr::case_when(
      results_df$action == "to_invalidate" ~ "To Invalidate",
      results_df$action == "to_sustain" ~ "To Sustain"
    )

    p <- ggplot2::ggplot(results_df, ggplot2::aes_string(x = "pct_bias_to_change_inference", fill = "action")) +
      ggplot2::geom_histogram() +
      ggplot2::scale_fill_manual("", values = c("#1F78B4", "#A6CEE3")) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("Histogram of Percent Bias") +
      ggplot2::facet_grid(~action) +
      ggplot2::scale_y_continuous(breaks = 1:nrow(results_df)) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylab("Count") +
      ggplot2::xlab("Percent Bias")

    return(p)
  } else {
    return(results_df)
  }
}

core_sensitivity_mkonfound <- function(t, df, alpha = .05, tails = 2) {
  critical_t <- stats::qt(1 - (alpha / tails), df)
  critical_r <- critical_t / sqrt((critical_t^2) + df)

  # For replacement of cases approach
  obs_r <- abs(t / sqrt(df + (t^2)))

  if (abs(obs_r) > abs(critical_r)) {
    action <- "to_invalidate"
    inference <- "reject_null"
    pct_bias <- 100 * (1 - (critical_r / obs_r))
  }
  else if (abs(obs_r) < abs(critical_r)) {
    action <- "to_sustain"
    inference <- "fail_to_reject_null"
    pct_bias <- 100 * (1 - (obs_r / critical_r))
  }
  else if (obs_r == critical_r) {
    action <- NA
    inference <- NA
    pct_bias <- NA
  }

  # # For correlation based approach (for calculating ITCV)
  if ((abs(obs_r) > abs(critical_r)) & ((obs_r * critical_r) > 0)) {
    mp <- -1
  } else {
    mp <- 1
  }
  itcv <- (obs_r - critical_r) / (1 + mp * abs(critical_r))
  r_con <- round(sqrt(abs(itcv)), 3)

  out <- dplyr::data_frame(t, df, action, inference, pct_bias, itcv, r_con)
  names(out) <- c("t", "df", "action", "inference", "pct_bias_to_change_inference", "itcv", "r_con")
  out$pct_bias_to_change_inference <- round(out$pct_bias_to_change_inference, 3)
  out$itcv <- round(out$itcv, 3)
  out$action <- as.character(out$action)
  out$inference <- as.character(out$inference)

  return(out)
}
