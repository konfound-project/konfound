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
#' mkonfound_ex
#' str(d)
#' mkonfound(mkonfound_ex, t, df)
#' }
#' @export
#'
mkonfound <- function(d, t, df, alpha = .05, tails = 2, return_plot = FALSE) {
  t_enquo <- enquo(t)
  df_enquo <- enquo(df)

  t_vec <- pull(select(d, !!t_enquo))
  df_vec <- pull(select(d, !!df_enquo))

  if (length(t_vec) <= 1) {
    stop("To carry out sensitivity analysis 
         for a single study, use pkonfound()")
  }
  
  results_df <- purrr::map2_dfr(.x = t_vec, .y = df_vec, 
                                .f = core_sensitivity_mkonfound)

  if (return_plot == TRUE) {
    results_df$action <- dplyr::case_when(
      results_df$action == "to_invalidate" ~ "To Invalidate",
      results_df$action == "to_sustain" ~ "To Sustain"
    )

    p <- ggplot2::ggplot(results_df, 
                         ggplot2::aes(x = results_df$pct_bias_to_change_inference, 
                                      fill = results_df$action)) +
      ggplot2::geom_histogram() +
      ggplot2::scale_fill_manual("", values = c("#1F78B4", "#A6CEE3")) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("Histogram of Percent Bias") +
      ggplot2::facet_grid(~action) +
      ggplot2::scale_y_continuous(breaks = seq_len(nrow(results_df))) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ylab("Count") +
      ggplot2::xlab("Percent Bias")

    return(p)
  } else {
    return(results_df)
  }
}
