# plot correlation

plot_correlation <- function(r_con, obs_r, critical_r){
    d <- data.frame(x = c(0), y = c(0))
    
    if (abs(obs_r) > abs(critical_r)) {
        the_title <- "To sustain the inference"
    } else if (abs(obs_r) < abs(critical_r)) {
        the_title <- "To invalidate the inference"    
    } else {
        the_title <- "Inference and critical r are exactly equal"
    }

    p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_segment(ggplot2::aes(y = .1), xend = 0, yend = .9, arrow = ggplot2::arrow(), size = 2.5, color = "#1F78B4") + # straight up
        ggplot2::geom_segment(ggplot2::aes(x = .1), xend = 1, yend = .9, arrow = ggplot2::arrow(), size = 2.5, color = "#1F78B4") + # hypotenuse
        ggplot2::geom_segment(ggplot2::aes(x = .15, y = 1), xend = .9, yend = 1, arrow = ggplot2::arrow(), size = 2.5, color = "#1F78B4") + # straight across
        
        ggplot2::geom_segment(ggplot2::aes(x = .05, y = .25), xend = .275, yend = .65, arrow = ggplot2::arrow(), size = 2.5, color = "#1F78B4") + # straight across
        ggplot2::geom_segment(ggplot2::aes(x = .175, y = .15), xend = .3, yend = .625, arrow = ggplot2::arrow(), size = 2.5, color = "#1F78B4") + # straight across
        
        ggplot2::annotate("text", x = 0, y = 0, label = paste0("CV"), fontface = 3) +
        ggplot2::annotate("text", x = 0, y = 1, label = paste0("Predictor of Interest"), fontface = 3) +
        ggplot2::annotate("text", x = 1, y = 1, label = paste0("Outcome"), fontface = 3) +
        # annotate("text", x = .35, y = .775, label = paste0("Impact")) +
        
        ggplot2::annotate("text", x = -.1, y = .5, label = paste0("Rx.cv = ", r_con), fontface = 1) +
        ggplot2::annotate("text", x = .575, y = .35, label = paste0("Ry.cv = ", r_con), fontface = 1) +
        ggplot2::annotate("text", x = .35, y = .7, label = paste0("Rx.cv X Ry.cv =\n", round(sqrt(r_con), 3)), fontface = 1) +
        
        ggplot2::xlim(-.15, 1.1) +
        ggplot2::ylim(-.05, 1) +
        ggplot2::theme_void() +
        ggplot2::ggtitle(the_title)
        
    p
}