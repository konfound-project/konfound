# Function to output the plot

plot_threshold <- function(beta_threshold, unstd_beta) {
    
    if (unstd_beta > beta_threshold) { # beta is above threshold
        dd <- dplyr::data_frame(unstd_beta = unstd_beta, beta_threshold = beta_threshold)
        dd <- dplyr::mutate(dd, `Above Threshold` = unstd_beta - beta_threshold)
        dd <- dplyr::rename(dd, `Below Threshold` = beta_threshold)
        
        dd <- dplyr::select(dd, -unstd_beta)
        dd <- tidyr::gather(dd, key, val)
        dd <- dplyr::mutate(dd, inference = "group")
        
        y_thresh <- dplyr::filter(dd, key == "Below Threshold")
        y_thresh <- dplyr::pull(dplyr::select(y_thresh, val))
        y_thresh_text <- y_thresh + sqrt(.005 * y_thresh)
        
        cols <- c("#A6CEE3", "#1F78B4") # dark blue and light blue
        
    } else if (unstd_beta < beta_threshold) { # beta is below threshold
        dd <- dplyr::data_frame(unstd_beta = unstd_beta, beta_threshold = beta_threshold)
        dd <- dplyr::mutate(dd, `Above Threshold` = abs(unstd_beta - beta_threshold))
        dd <- dplyr::mutate(dd, `Below Threshold` = unstd_beta)
        dd <- dplyr::select(dd, -beta_threshold)
        
        dd <- dplyr::select(dd, -unstd_beta)
        dd <- tidyr::gather(dd, key, val)
        dd <- dplyr::mutate(dd, inference = "group")
        
        y_thresh <- sum(dd$val)
        y_thresh_text <- y_thresh + sqrt(.005 * y_thresh)
        
        cols <- c("#B2DF8A", "#1F78B4") # dark blue and green
    
    }
    
    effect_text <- unstd_beta + (.025 * unstd_beta) # y-value of text

    p <- ggplot2::ggplot(dd, ggplot2::aes(x = inference, y = val, fill = key)) +
        ggplot2::geom_col(position = "stack") +
        
        ggplot2::geom_hline(yintercept = unstd_beta, color = "black") +
        ggplot2::annotate("text", x = 1, y = effect_text, label = "Effect") +
        
        ggplot2::geom_hline(yintercept = y_thresh, color = "red") +
        ggplot2::annotate("text", x = 1, y = y_thresh_text, label = "Threshold") +
        
        ggplot2::scale_fill_manual("", values = cols) +
        ggplot2::labs(caption = "The solid black line represents the effect") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank()) +
        ggplot2::xlab(NULL) +
        ggplot2::ylab("Unstandardized Beta Coefficient") +
        
        ggplot2::labs(caption = "Correlations are conditional on observed covariates")
    
    return(p)
} 