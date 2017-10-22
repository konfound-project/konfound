library(tidyverse)

d <- data_frame(beta_threshold = 4.2,
                beta = 4.6)

dd <- dpylr::mutate(d, bias = case_when(
    beta - beta_threshold > 0 ~
    beta - beta_threshold < 0 ~
    beta == beta_threshold ~
    ))
    mutate(inference = "test") %>%
    gather(key, val, -beta_threshold, -inference) %>%
    mutate(key = as.factor(key),
           key = relevel(key, ref = "bias"))

ggplot(dd, aes(x = inference, y = val, fill = key)) +
    geom_col() +
    geom_hline(yintercept=d$beta, color = "black")

# output_plot(beta_diff, beta_threshhold, unstd_beta, obs_r, critical_r, r_con)
# 
# output_plot <- function(beta_diff, beta_threshhold, bias = NULL, sustain = NULL, recase, obs_r, critical_r, r_con) {
#     to_plot <- output_df(beta_diff, beta_threshhold, bias, sustain, recase)
#     to_plot <- dplyr::mutate(to_plot, remainder = 100 - percent_bias)
#     to_plot <- tidyr::gather(to_plot, key, val, -inference, -replace_null_cases)
#     # to_plot <- dplyr::mutate(to_plot, inference = dplyr::case_when(
#     #     inference == "to_invalidate" ~ "To Invalidate",
#     #     inference == "to_sustain" ~ "To Sustain"),
#     #     key = as.factor(key))
#     return(to_plot)
#     ggplot2::ggplot(to_plot, ggplot2::aes_string(x = "inference", y = "val", fill = "key")) +
#         ggplot2::geom_col(position = ggplot2::position_fill(reverse = TRUE)) +
#         ggplot2::scale_fill_manual("", values = c("cyan4", "lightgray"), breaks = c("percent_bias", "remainder")) +
#         ggplot2::xlab("") +
#         ggplot2::ylab("%") +
#         ggplot2::ggtitle("Percentage of Effect Needed to Invalidate or Sustain the Inference")
# }
# 
# 
# pkonfound(2, .4, 100, 3, to_return = "df") %>% 
#     
#     
#     

# pkonfound(2, .4, 100, 3, to_return="df")
# 
# pkonfound(2, .4, 100, 3, to_return="df") %>% 
#     mutate(remainder = 100 - percent_bias) %>% 
#     select(replacement_of_cases_inference, percent_bias, remainder) %>% 
#     gather(key, val, -replacement_of_cases_inference) %>% 
#     ggplot(aes(x = key, y = val, fill = key)) +
#     geom_col()
# scale_fill_manual("Inference", values = c("green4", "darkgray")) +
#     ggplot2::ylab("%")
# 
# # x <- pkonfound(.4, 2, 100, 3, to_return="plot")
# # x <- x %>% mutate(key = ifelse(key == "percent_bias", "To Sustain", "Effect"))
# # x$key <- factor(x$key, levels = c("To Sustain", "Effect"))
# # ggplot(x, aes(x = inference, y = val, fill = key)) +
# #     geom_col() +
# #     scale_fill_manual("Inference", values = c("red4", "darkgray")) +
# #     ggplot2::ylab("%") +
# #     theme(axis.title.x=element_blank(),
# #           axis.text.x=element_blank(),
# #           axis.ticks.x=element_blank())
