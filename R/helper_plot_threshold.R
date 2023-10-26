# Function to output the plot

plot_threshold <- function(beta_threshold, est_eff) {
  beta_threshold <- abs(beta_threshold)
  est_eff <- abs(est_eff)

  if (est_eff > beta_threshold) { # beta is above threshold
    dd <- dplyr::tibble(
      est_eff <- est_eff,
      beta_threshold <- beta_threshold
    )

    dd <- dplyr::mutate(dd, `Above Threshold` = est_eff - beta_threshold)
    dd <- dplyr::rename(dd, `Below Threshold` = beta_threshold)

    dd <- dplyr::select(dd, -est_eff)
    dd <- tidyr::gather(dd, key, val)
    dd <- dplyr::mutate(dd, inference = "group")

    y_thresh <- dplyr::filter(dd, key == "Below Threshold")
    y_thresh <- dplyr::pull(dplyr::select(y_thresh, val))
    y_thresh_text <- y_thresh + sqrt(.005 * abs(y_thresh))

    effect_text <- est_eff + (.025 * est_eff) # y-value of text

    cols <- c("#A6CEE3", "#1F78B4") # dark blue and light blue
  } else if (est_eff < beta_threshold) { # beta is below threshold

    dd <- dplyr::tibble(est_eff = est_eff, beta_threshold = beta_threshold)
    dd <- dplyr::mutate(dd, `Above Estimated Effect, Below Threshold` = abs(est_eff - beta_threshold))
    dd <- dplyr::mutate(dd, `Below Threshold` = est_eff)
    dd <- dplyr::select(dd, -beta_threshold)

    dd <- dplyr::select(dd, -est_eff)
    dd <- tidyr::gather(dd, key, val)
    dd <- dplyr::mutate(dd, inference = "group")

    y_thresh <- sum(abs(dd$val))
    if (est_eff < 0) {
      y_thresh <- y_thresh * -1
    }

    y_thresh_text <- y_thresh + sqrt(.005 * abs(y_thresh))
    effect_text <- est_eff + sqrt(.025 * abs(est_eff)) # y-value of text

    cols <- c("lightgray", "#1F78B4") # dark blue and green (green not used right now)
  }

  p <- ggplot2::ggplot(dd, ggplot2::aes(x = inference, y = val, fill = key)) +
    ggplot2::geom_col(position = "stack") +

    ggplot2::geom_hline(yintercept = est_eff, color = "black") +
    ggplot2::annotate("text", x = 1, y = effect_text, label = "Estimated Effect") +

    ggplot2::geom_hline(yintercept = y_thresh, color = "red") +
    ggplot2::annotate("text", x = 1, y = y_thresh_text, label = "Threshold") +
    # ggplot2::geom_text(aes(label = "Effect"), vjust = -.5) + this is discussed here: https://github.com/jrosen48/konfound/issues/5

    ggplot2::scale_fill_manual("", values = cols) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank()) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab("Effect (abs. value)") +
    ggplot2::theme(legend.position = "top")

  return(p)
}
