# Function to output the plot

plot_threshold <- function(beta_threshold, unstd_beta) {
    
    if (unstd_beta > beta_threshold) {
        dd <- data_frame(unstd_beta = unstd_beta, beta_threshold = beta_threshold) %>% 
            dplyr::mutate(`Above Threshold` = unstd_beta - beta_threshold) %>% 
            rename(`Below Threshold` = beta_threshold)
    } else if (unstd_beta < beta_threshold) {
        dd <- data_frame(unstd_beta = unstd_beta, beta_threshold = beta_threshold) %>% 
            dplyr::mutate(`Above Threshold` = abs(unstd_beta - beta_threshold)) %>% 
            mutate(`Below Threshold` = unstd_beta) %>% 
            select(-beta_threshold)
    }
    
    p <- dd %>% select(-unstd_beta) %>% 
        gather(key, val) %>% 
        mutate(inference = "group") %>% 
        ggplot(aes(x = inference, y = val, fill = key)) +
        geom_col(position = "stack") +
        geom_hline(yintercept=unstd_beta, color = "black") +
        scale_fill_brewer("", type = "qual", palette = 3, direction = 1) +
        labs(caption = "The solid black line represents the effect") +
        theme_bw() +
        theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
        xlab(NULL) +
        ylab("Unstandardized Beta Coefficient")
    
    return(p)
} 