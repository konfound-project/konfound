#' Meta-Analysis and Sensitivity Analysis for Multiple Studies
#'
#' Performs sensitivity analysis for multiple models, where parameters 
#' are stored in a data frame. It calculates the amount of bias required to 
#' invalidate or sustain an inference for each case in the data frame.
#'
#' @param d A data frame or tibble containing t-statistics and associated 
#' degrees of freedom.
#' @param t Column name or vector of t-statistics.
#' @param df Column name or vector of degrees of freedom associated 
#' with t-statistics.
#' @param alpha Significance level for hypothesis testing.
#' @param tails Number of tails for the test (1 or 2).
#' @param return_plot Whether to return a plot of the percent bias 
#' (default is `FALSE`).
#' @return Depending on `return_plot`, either returns a data frame with 
#' analysis results or a plot.
#' @importFrom rlang enquo
#' @importFrom dplyr select pull case_when
#' @importFrom purrr map2_dfr
#' @importFrom ggplot2 ggplot aes_string geom_histogram scale_fill_manual 
#' theme_bw ggtitle facet_grid scale_y_continuous theme ylab xlab
#' @importFrom stats qt
#' @examples
#' \dontrun{
#' data <- data.frame(t = c(2.1, 2.3, 1.9), df = c(30, 40, 35))
#' mkonfound(data, t, df)
#' }
#' @export

mkonfound <- function(d, t, df, alpha = .05, tails = 2, return_plot = FALSE) {
  t_enquo <- enquo(t)
  df_enquo <- enquo(df)

  t_vec <- pull(select(d, !!t_enquo))
  df_vec <- pull(select(d, !!df_enquo))

  if (length(t_vec) <= 1) {
    stop("To carry out sensitivity analysis 
         for a single study, use pkonfound()")
  }

  results_df <- suppressWarnings(
  purrr::map2_dfr(
    .x = t_vec, .y = df_vec, .f = core_sensitivity_mkonfound)
)
  if (return_plot == TRUE) {
    results_df$action <- dplyr::case_when(
      results_df$action == "to_invalidate" ~ "To Invalidate",
      results_df$action == "to_sustain" ~ "To Sustain"
    )

    p <- ggplot2::ggplot(results_df, ggplot2::aes_string(
  x = "pct_bias_to_change_inference", fill = "action"
)) +
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
  names(out) <- c("t", 
                  "df", 
                  "action", 
                  "inference", 
                  "pct_bias_to_change_inference", 
                  "itcv", 
                  "r_con")
  out$pct_bias_to_change_inference <- round(
    out$pct_bias_to_change_inference, 3)
  out$itcv <- round(out$itcv, 3)
  out$action <- as.character(out$action)
  out$inference <- as.character(out$inference)

  return(out)
}
