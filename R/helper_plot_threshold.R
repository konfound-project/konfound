# Function to output the plot

plot_threshold <- function(beta_threshold, unstd_beta) {
    
    if (unstd_beta > beta_threshold) {
        dd <- dplyr::data_frame(unstd_beta = unstd_beta, beta_threshold = beta_threshold)
        dd <- dplyr::mutate(dd, `Above Threshold` = unstd_beta - beta_threshold)
        dd <- dplyr::rename(dd, `Below Threshold` = beta_threshold)
    } else if (unstd_beta < beta_threshold) {
        dd <- dplyr::data_frame(unstd_beta = unstd_beta, beta_threshold = beta_threshold)
        dd <- dplyr::mutate(dd, `Above Threshold` = abs(unstd_beta - beta_threshold))
        dd <- dplyr::mutate(dd, `Below Threshold` = unstd_beta)
        dd <- dplyr::select(dd, -beta_threshold)
    }
    
    dd <- dplyr::select(dd, -unstd_beta)
    dd <- tidyr::gather(dd, key, val)
    dd <- dplyr::mutate(dd, inference = "group")
    
    p <- ggplot2::ggplot(dd, ggplot2::aes(x = inference, y = val, fill = key)) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::geom_hline(yintercept=unstd_beta, color = "black") +
        ggplot2::scale_fill_brewer("", type = "qual", palette = 3, direction = 1) +
        ggplot2::labs(caption = "The solid black line represents the effect") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank()) +
        ggplot2::xlab(NULL) +
        ggplot2::ylab("Unstandardized Beta Coefficient")
    
    return(p)
} 