# plot correlation

#' Plot Correlation Diagram
#'
#' This function creates a plot to illustrate the correlation between different 
#' variables,specifically focusing on the confounding variable, predictor of 
#' interest, and outcome.It uses ggplot2 for graphical representation.
#'
#' @param r_con Correlation coefficient related to the confounding variable.
#' @param obs_r Observed correlation coefficient.
#' @param critical_r Critical correlation coefficient for decision-making.
#' @return A ggplot object representing the correlation diagram.
#' @importFrom ggplot2 ggplot aes_string geom_segment geom_curve annotate 
#' theme_void ggtitle
plot_correlation <- function(r_con, obs_r, critical_r) {
  d <- data.frame(x = c(0), y = c(0))

  if (abs(obs_r) > abs(critical_r)) {
    the_title <- "To invalidate an inference"
  } else if (abs(obs_r) < abs(critical_r)) {
    the_title <- "To sustain an inference"
  } else {
    the_title <- "Inference and critical r are exactly equal"
  }

  p <- ggplot2::ggplot(d, ggplot2::aes_string(x = "x", y = "y")) +

# main arrows
ggplot2::geom_segment(
  ggplot2::aes(y = .1),
  xend = 0,
  yend = .9,
  arrow = ggplot2::arrow(),
  size = 2.5,
  color = "#A6CEE3"
) + # straight up
ggplot2::geom_segment(
  ggplot2::aes(x = .1),
  xend = 1,
  yend = .9,
  arrow = ggplot2::arrow(),
  size = 2.5,
  color = "#A6CEE3"
) + # hypotenuse
ggplot2::geom_segment(
  ggplot2::aes(x = .15, y = 1),
  xend = .9,
  yend = 1,
  arrow = ggplot2::arrow(),
  size = 2.5,
  color = "#A6CEE3"
) + # straight across


# label annotations
ggplot2::annotate(
  "text",
  x = 0,
  y = 0,
  label = paste0("Confounding\nVariable"),
  fontface = 3
) +
ggplot2::annotate(
  "text",
  x = 0,
  y = 1,
  label = paste0("Predictor of Interest"),
  fontface = 3
) +
ggplot2::annotate(
  "text",
  x = 1,
  y = 1,
  label = paste0("Outcome"),
  fontface = 3
) +


# CV arrows
# ggplot2::geom_segment(ggplot2::aes(x = .05, y = .25), xend = .275,
#yend = .65, arrow = ggplot2::arrow(), size = 2.5, color = "#1F78B4") + 
# straight across
# ggplot2::geom_segment(ggplot2::aes(x = .175, y = .15), xend = .3, 
#yend = .625, arrow = ggplot2::arrow(), size = 2.5, color = "#1F78B4") +
  # straight across
ggplot2::geom_curve(
  ggplot2::aes(x = .04, y = .325, xend = .35, yend = .825),
  curvature = -.35,
  size = 2.5,
  color = "#1F78B4",
  arrow = ggplot2::arrow()
) +
ggplot2::geom_curve(
  ggplot2::aes(x = .225, y = .23, xend = .4, yend = .8),
  curvature = .35,
  size = 2.5,
  color = "#1F78B4",
  arrow = ggplot2::arrow()
) +
ggplot2::geom_segment(
  ggplot2::aes(x = .37, y = .81),
  xend = .465,
  yend = .925,
  arrow = ggplot2::arrow(),
  size = 2.5,
  color = "#1F78B4"
) + # straight across


# corr. annotations
ggplot2::annotate(
  "text",
  x = -.125,
  y = .5,
  label = paste0("Rx.cv | Z =\n ", round(r_con, 3)),
  fontface = 1
) +
ggplot2::annotate(
  "text",
  x = .575,
  y = .35,
  label = paste0("Ry.cv | Z =\n", round(r_con, 3)),
  fontface = 1
) +
ggplot2::annotate(
  "text",
  x = .25,
  y = .525,
  label = paste0("Rx.cv | Z * Ry.cv | Z =\n", round(r_con^2, 3)),
  fontface = 1
) +


    # plot modifications
    ggplot2::xlim(-.15, 1.1) +
    ggplot2::ylim(-.05, 1) +
    ggplot2::theme_void() +
    ggplot2::ggtitle(the_title)

  p
}
