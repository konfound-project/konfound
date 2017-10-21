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
