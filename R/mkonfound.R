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
    stop("To carry out sensitivity analysis for a single study, use pkonfound()")
  }
  
  results_df <- purrr::map2_dfr(.x = t_vec, .y = df_vec, .f = core_sensitivity_mkonfound)

  if (return_plot == TRUE) {
    results_df$action <- dplyr::case_when(
      results_df$action == "to_invalidate" ~ "To Invalidate",
      results_df$action == "to_sustain" ~ "To Sustain"
    )

    p <- ggplot2::ggplot(results_df, ggplot2::aes(x = results_df$pct_bias_to_change_inference, fill = results_df$action)) +
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
