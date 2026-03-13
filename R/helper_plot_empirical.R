# helper_plot_empirical.R
# ============================================================================
# Plotting and print functions for empirical RIR output objects.
#
# This file has two layers:
#
#   Internal workers (prefixed with . or _internal):
#     .plot_shape()             k* histogram, shape-focused
#     plot_comp_stacked()       k* histogram stacked by supporter/suppressor
#     .plot_partial_internal()  partial regression plot (r_x vs r_y)
#     plot_influence_binned()   binned average influence across k* bins
#     plot_rir_dist_internal()  histogram of per-case psi_i values
#     print.rir_influence()     S3 print method for rir_influence objects
#
#   Public wrappers (user-facing, all take a konfound_empdist object):
#     plot.konfound_empdist()   S3 dispatch; routes to stacked or shape
#     plot_hist()               k* distribution (type = "stacked" or "shape")
#     plot_partial()            partial regression with influence coloring
#     plot_comp()               binned influence composition (type = "ci" or "rir")
#     plot_rir_dist()           per-case RIR influence distribution
#
# Key inputs:
#   result       A konfound_empdist object from konfound_empdist()
#   case_info    Output from label_cases(); provides r_x, r_y, ci, ci_share
#   rir_infl     Output from rir_influence(); provides psi_i per case
#
# Key outputs:
#   ggplot2 objects returned invisibly; printed to the plots panel on call.
#   All public wrappers accept verbose = TRUE to print a plain-text
#   description of the plot to the console (stderr via message()).
#
# Dependencies: ggplot2 (required), ggtext (subtitle rendering), ggrepel (labels)
# ============================================================================
#' @importFrom stats density aggregate median sd quantile IQR
#' @importFrom utils head

utils::globalVariables(c(
    "r_x", "r_y", "ci_share", "ci_rank", "label_text",
    "k_star", "xmin", "xmax", "ymin", "ymax", "group",
    "xmid", "avg_influence", "influence"
))

# S3 plot method dispatcher
# type = "stacked": composition histogram (requires case_info and tracker).
# type = "shape":   shape-focused histogram; always available as fallback.
#' @export
plot.konfound_empdist <- function(x,
                                  type = c("stacked", "shape"),
                                  title_str = NULL,
                                  ...) {
  
  stopifnot(inherits(x, "konfound_empdist"))
  type <- match.arg(type)
  
  # Graceful fallback: if stacked requested but composition unavailable
  
  if (type == "stacked") {
    if (is.null(x$tracker) || is.null(x$case_info)) {
      message("Composition tracking not available. Falling back to shape plot.",
              "\n  (Pass case_info to konfound_empdist() to enable composition tracking.)")
      type <- "shape"
    }
  }
  
  if (type == "stacked") {
    # Build a lightweight emp_res-like list for plot_comp_stacked()
    emp_res <- list(tracker = x$tracker, k_cf = x$k_cf, n = x$n)
    plot_comp_stacked(emp_res, x$case_info,
                      title_str = title_str %||% x$target_var, ...)
  } else {
    .plot_shape(x, title_str = title_str, ...)
  }
}


# Shape-focused k* histogram (type = "shape" fallback)
# Input:  konfound_empdist object (x).
# Output: ggplot histogram of k* with density overlay, dashed k_cf vline,
#         solid empirical median vline, and summary stats in subtitle.
.plot_shape <- function(x,
                        title_str = NULL,
                        bins = NULL,
                        binwidth = NULL,
                        density_adjust = 1.2,
                        show_density = TRUE,
                        show_rug = FALSE,
                        ...) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }
  
  # -- Data: separate censored (k* = N) from interior --
  k_all <- x$k_star[is.finite(x$k_star)]
  n_total <- length(k_all)
  if (n_total == 0) stop("No finite k_star values to plot.")
  
  n_sample <- x$n
  k_censored <- sum(k_all >= n_sample)
  pct_censored <- round(100 * k_censored / n_total, 1)
  
  # Plot only interior k* values (< N)
  df <- data.frame(k_star = k_all[k_all < n_sample])
  n_pts <- nrow(df)
  
  if (n_pts < 5) {
    warning("Fewer than 5 interior k* values. Most reps hit k*=N.")
    df <- data.frame(k_star = k_all)
    n_pts <- nrow(df)
    pct_censored <- 0
  }
  
  rng  <- range(df$k_star)
  span <- diff(rng)
  if (span <= 0) span <- 1
  
  # -- Binwidth --
  if (!is.null(binwidth)) {
    bw <- binwidth
  } else if (!is.null(bins)) {
    bw <- span / bins
  } else {
    bins_target <- max(40L, ceiling(1.5 * sqrt(n_pts)))
    bw <- max(1, span / bins_target)
  }
  
  # -- Summary stats (computed on ALL k*, including censored) --
  k_med  <- if (n_total >= 2) median(k_all) else NA_real_
  k_sd   <- if (n_total >= 2) sd(k_all) else NA_real_
  k_skew <- if (n_total >= 3) {
    m <- mean(k_all); s <- sd(k_all)
    (sum((k_all - m)^3) / n_total) / (s^3)
  } else NA_real_
  
  # -- Subtitle: rich info, ggtext-compatible --
  has_ggtext <- requireNamespace("ggtext", quietly = TRUE)
  
  if (has_ggtext) {
    sub_parts <- sprintf(
      paste0(
        "Model: %s &nbsp;|&nbsp; N = %d<br>",
        "Closed-form RIR = %d (dashed) &nbsp;|&nbsp; ",
        "Empirical RIR (median; solid line) = %.0f &nbsp;|&nbsp; ",
        "SD = %.1f &nbsp;|&nbsp; Skew = %.2f<br>",
        "<span style='font-size:7pt; color:grey50;'>",
        "Each bar shows the number of resampled replacements (k*) needed ",
        "to nullify the inference.",
        "</span>"
      ),
      title_str %||% x$target_var, x$n, x$k_cf, k_med, k_sd, k_skew
    )
    if (pct_censored > 0) {
      sub_parts <- paste0(
        sub_parts,
        sprintf("<br><span style='font-size:7pt; color:grey50;'>%d reps (%.1f%%) censored at k* = N</span>",
                k_censored, pct_censored)
      )
    }
    subtitle_elem <- ggtext::element_textbox_simple(
      size = 9, lineheight = 1.3, margin = ggplot2::margin(b = 8)
    )
  } else {
    # Fallback: plain text
    sub_parts <- paste(
      sprintf("k_cf = %d (dashed)", x$k_cf),
      sprintf("med = %.0f,  SD = %.1f,  skew = %.2f", k_med, k_sd, k_skew),
      sep = "  |  "
    )
    if (pct_censored > 0) {
      sub_parts <- paste(sub_parts,
                         sprintf("%d reps (%.1f%%) censored at k* = N",
                                 k_censored, pct_censored),
                         sep = "  |  ")
    }
    subtitle_elem <- ggplot2::element_text(size = 7.5, family = "mono",
                                           color = "#4A5568",
                                           margin = ggplot2::margin(b = 6))
  }
  
  # -- Colors (aligned with plot_comp_stacked) --
  col_fill    <- "#3AAFA9"
  col_border  <- "#2B7A78"
  col_density <- "#1A365D"
  col_vline   <- "gray30"
  
  # -- Build plot --
  p <- ggplot2::ggplot(df, ggplot2::aes(x = k_star)) +
    
    ggplot2::geom_histogram(
      binwidth = bw, boundary = 0, closed = "left",
      fill = col_fill, color = col_border,
      alpha = 0.40, linewidth = 0.25
    ) +
    ggplot2::geom_vline(xintercept = x$k_cf, linetype = "dashed",
                        color = col_vline, linewidth = 0.7) +
    
    ggplot2::labs(
      title    = "Empirical RIR Distribution",
      subtitle = sub_parts,
      x        = "k* (replacements to nullify)",
      y        = "Count"
    ) +
    
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.subtitle = subtitle_elem
    )
  
  # -- Rug --
  if (isTRUE(show_rug) || (is.character(show_rug) && show_rug == "always")) {
    p <- p + ggplot2::geom_rug(alpha = 0.15, linewidth = 0.3, color = col_fill)
  }
  
  # -- Density overlay --
  if (show_density && n_pts >= 10) {
    dens <- density(df$k_star, adjust = density_adjust)
    dens_df <- data.frame(x = dens$x, y = dens$y * n_pts * bw)
    p <- p + ggplot2::geom_line(
      data = dens_df, ggplot2::aes(x = x, y = y),
      color = col_density, linewidth = 0.6, alpha = 0.7
    )
  }
  
  # -- Vlines: k_cf (dashed) and median (solid), matching plot_comp_stacked --
  if (is.finite(k_med)) {
    p <- p + ggplot2::geom_vline(
      xintercept = k_med,
      color = col_vline, linewidth = 0.7, linetype = "solid"
    )
  }
  
  p
}


# Stacked composition histogram (default k* plot)
# Input:  emp_res (konfound_empdist or list with $tracker, $k_cf, $n),
#         case_info from label_cases(), title_str for subtitle.
# Output: ggplot histogram of k* with bars stacked by supporter/suppressor
#         fraction per bin. Dashed line = k_cf, solid line = empirical median.
plot_comp_stacked <- function(emp_res, case_info, title_str) {
  tk        <- emp_res$tracker
  k_cf      <- emp_res$k_cf
  k_emp     <- median(tk$k_star)
  k_sd      <- sd(tk$k_star)
  k_skew    <- {
    m <- mean(tk$k_star); s <- sd(tk$k_star)
    if (s > 0) (sum((tk$k_star - m)^3) / length(tk$k_star)) / (s^3) else NA_real_
  }
  ci        <- case_info$ci
  beta_sign <- sign(case_info$beta_hat)
  base_rate <- mean(sign(ci) == beta_sign)
  
  # Adaptive binwidth: same logic as .plot_shape for visual consistency
  k_finite    <- tk$k_star[is.finite(tk$k_star)]
  n_pts       <- length(k_finite)
  k_rng       <- range(k_finite)
  k_span      <- diff(k_rng); if (k_span <= 0) k_span <- 1
  bins_target <- max(40L, ceiling(1.5 * sqrt(n_pts)))
  bw          <- max(1, k_span / bins_target)
  lo          <- floor(k_rng[1] / bw) * bw
  hi          <- ceiling(k_rng[2] / bw) * bw
  breaks      <- seq(lo, hi + bw, by = bw)
  tk$k_bin <- cut(tk$k_star, breaks = breaks, include.lowest = TRUE)
  
  bin_agg <- aggregate(cbind(frac_supporter, k_star) ~ k_bin,
                       data = tk,
                       FUN = function(x) c(mean = mean(x), n = length(x)))
  
  n_bins <- nrow(bin_agg)
  bin_df <- data.frame(
    k_bin          = bin_agg$k_bin,
    frac_supporter = bin_agg$frac_supporter[, "mean"],
    count          = bin_agg$k_star[, "n"],
    xmin           = breaks[seq_len(n_bins)],
    xmax           = breaks[seq_len(n_bins) + 1]
  )
  
  stack_df <- rbind(
    data.frame(bin_df,
               ymin  = 0,
               ymax  = bin_df$count * bin_df$frac_supporter,
               group = "Supporters replaced"),
    data.frame(bin_df,
               ymin  = bin_df$count * bin_df$frac_supporter,
               ymax  = bin_df$count,
               group = "Suppressors replaced")
  )
  stack_df$group <- factor(stack_df$group,
                           levels = c("Suppressors replaced",
                                      "Supporters replaced"))
  
  subtitle_txt <- sprintf(
    paste0(
      "Model: %s &nbsp;|&nbsp; N = %.0f<br>",
      "Closed-form RIR = %.0f &nbsp;|&nbsp; ",
      "Empirical RIR (median; solid line) = %.0f &nbsp;|&nbsp; ",
      "SD = %.1f &nbsp;|&nbsp; Skew = %.2f<br>",
      "<span style='font-size:7pt; color:grey50;'>",
      "Each bar shows the number of resampled replacements (k*) needed ",
      "to nullify the inference.<br>",
      "Stacked colors show the average composition of replaced cases per bin.",
      "</span>"
    ),
    title_str, emp_res$n, k_cf, k_emp, k_sd, k_skew
  )
  
  ggplot2::ggplot(stack_df) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                   fill = group),
      color = "white", linewidth = 0.2, alpha = 0.75
    ) +
    ggplot2::geom_vline(xintercept = k_cf, linetype = "dashed",
                        color = "gray30", linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = k_emp, linetype = "solid",
                        color = "gray30", linewidth = 0.7) +
    ggplot2::scale_fill_manual(
      values = c("Supporters replaced"  = "#4878A8",
                 "Suppressors replaced" = "#C06060"),
      name = NULL
    ) +
    ggplot2::labs(
      title    = "Empirical RIR Distribution: Replacement Composition",
      subtitle = subtitle_txt,
      x = "k* (replacements to nullify)",
      y = "Count"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.subtitle   = ggtext::element_textbox_simple(
        size = 9, lineheight = 1.3, margin = ggplot2::margin(b = 8)
      )
    )
}

# Partial regression plot with optional RIR-influence labels
# Input:  case_info from label_cases() (provides r_x, r_y, ci, ci_share),
#         rir_infl from rir_influence() (optional; enables psi_i labels).
# Output: ggplot scatter of (r_x, r_y) with points colored and sized by c_i.
#         When show_rir_labels = TRUE, top cases are annotated with psi_i.
#         infl_threshold overrides top_n if provided.
.plot_partial_internal <- function(case_info, title_str,
                                   rir_infl = NULL,
                                   show_rir_labels = FALSE,
                                   top_n = 10,
                                   infl_threshold = NULL) {
  
  df <- data.frame(r_x = case_info$r_x, r_y = case_info$r_y,
                   ci = case_info$ci, ci_share = case_info$ci_share)
  ci_directed <- df$ci * sign(case_info$beta_hat)
  df$ci_rank <- rank(ci_directed) / nrow(df)
  
  n_support  <- sum(sign(case_info$ci) == sign(case_info$beta_hat))
  n_suppress <- sum(sign(case_info$ci) != sign(case_info$beta_hat))
  
  # Subtitle: add RIR-influence note if labels are shown
  if (show_rir_labels && !is.null(rir_infl)) {
    subtitle_txt <- sprintf(
      paste0(
        "Model: %s &nbsp;|&nbsp; N = %d<br>",
        "Estimated beta = %.3f &nbsp;|&nbsp; %d supporters, %d suppressors<br>",
        "<span style='font-size:7pt; color:grey50;'>",
        "Point size: influence magnitude. Color: support/opposition. ",
        "Labels: RIR-influence in replacement-equivalent units.",
        "</span>"
      ),
      title_str, nrow(df), case_info$beta_hat, n_support, n_suppress
    )
  } else {
    subtitle_txt <- sprintf(
      paste0(
        "Model: %s &nbsp;|&nbsp; N = %d<br>",
        "Estimated beta = %.3f &nbsp;|&nbsp; %d supporters, %d suppressors<br>",
        "<span style='font-size:7pt; color:grey50;'>",
        "Residuals from regressing predictor and outcome separately on covariates. ",
        "Point size: influence magnitude. Line: partial regression slope.",
        "</span>"
      ),
      title_str, nrow(df), case_info$beta_hat, n_support, n_suppress
    )
  }
  
  # Base plot (unchanged from original)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = r_x, y = r_y)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray80", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, color = "gray80", linewidth = 0.3) +
    ggplot2::geom_abline(intercept = 0, slope = case_info$beta_hat,
                         color = "gray40", linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(size = ci_share, color = ci_rank),
                        alpha = 0.65) +
    ggplot2::scale_size_continuous(range = c(0.5, 4), guide = "none") +
    ggplot2::scale_color_gradient2(
      low = "#A50F15", mid = "gray85", high = "#08519C",
      midpoint = 0.5,
      breaks = c(0.05, 0.50, 0.95),
      labels = c("Opposes effect", "Neutral", "Supports effect"),
      name = "Case Influence"
    ) +
    ggplot2::guides(color = ggplot2::guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 12, barheight = 0.5,
      label.position = "bottom"
    )) +
    ggplot2::labs(
      title    = "Partial Regression Plot: Case Influence Diagnostic",
      subtitle = subtitle_txt,
      x = "Predictor Residual",
      y = "Outcome Residual"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.subtitle   = ggtext::element_textbox_simple(
        size = 9, lineheight = 1.3, margin = ggplot2::margin(b = 8)
      )
    )
  
  # --- Add RIR-influence labels if requested ---
  if (show_rir_labels && !is.null(rir_infl)) {
    
    if (!requireNamespace("ggrepel", quietly = TRUE)) {
      warning("ggrepel package required for RIR-influence labels. ",
              "Install with install.packages('ggrepel').", call. = FALSE)
      return(p)
    }
    
    infl_vals <- rir_infl$influence
    
    # Determine which cases to label
    if (!is.null(infl_threshold)) {
      label_idx <- which(abs(infl_vals) > infl_threshold)
    } else {
      label_idx <- order(abs(infl_vals), decreasing = TRUE)[seq_len(min(top_n, length(infl_vals)))]
    }
    
    # Build label data.frame
    label_df <- data.frame(
      r_x   = df$r_x[label_idx],
      r_y   = df$r_y[label_idx],
      infl  = infl_vals[label_idx]
    )
    
    # Format: positive influence plain, negative in red-ish with sign
    label_df$label_text <- ifelse(
      label_df$infl >= 0,
      sprintf("+%.1f", label_df$infl),
      sprintf("%.1f", label_df$infl)
    )
    label_df$label_color <- ifelse(label_df$infl >= 0, "#08519C", "#A50F15")
    
    p <- p +
      ggrepel::geom_text_repel(
        data = label_df,
        ggplot2::aes(x = r_x, y = r_y, label = label_text),
        inherit.aes = FALSE,
        color = label_df$label_color,
        size = 2.8,
        fontface = "bold",
        segment.color = "gray60",
        segment.size = 0.3,
        segment.alpha = 0.5,
        min.segment.length = 0.3,
        box.padding = 0.4,
        point.padding = 0.3,
        max.overlaps = 20,
        seed = 42
      )
  }
  
  p
}


# Binned influence composition across k* bins
# Input:  emp_res (konfound_empdist or list with $tracker, $k_cf, $n),
#         case_info from label_cases(), rir_infl from rir_influence() (optional).
# Output: ggplot scatter of average influence per k* bin with LOESS trend.
#         metric = "ci":  y-axis is mean(c_i * sign(beta_hat)) per bin,
#                         optionally normalized by mean(|c_i|).
#         metric = "rir": y-axis is mean(psi_i) per bin (requires rir_infl).
plot_influence_binned <- function(emp_res, case_info, title_str,
                                  metric = c("ci", "rir"),
                                  rir_infl = NULL,
                                  normalize = FALSE) {
  tk        <- emp_res$tracker
  k_cf      <- emp_res$k_cf
  k_emp     <- median(tk$k_star)
  
  metric <- match.arg(metric)
  
  ci        <- case_info$ci
  beta_sign <- sign(case_info$beta_hat)
  ci_oriented <- ci * beta_sign
  
  if (metric == "rir") {
    # -- RIR influence metric --
    if (is.null(tk$mean_rir_infl) || all(is.na(tk$mean_rir_infl))) {
      stop("Tracker does not contain mean_rir_infl. ",
           "Re-run konfound_empdist() with case_info to enable RIR composition tracking.",
           call. = FALSE)
    }
    y_vals       <- tk$mean_rir_infl
    expected_val <- if (!is.null(rir_infl)) attr(rir_infl, "mean_influence") else mean(y_vals, na.rm = TRUE)
    y_label      <- expression("Avg. RIR influence of replaced cases  " * (bar(psi)[i]))
    ref_label    <- sprintf("Expected RIR influence (horizontal dashed line): %.3f", expected_val)
    norm_tag     <- " (RIR influence)"
    subtitle_note <- "Higher values = replaced cases had larger leave-one-out effect on k_cf."
  } else {
    # -- c_i influence metric (default) --
    if (is.null(tk$mean_ci_signed)) {
      stop("Tracker does not contain mean_ci_signed. ",
           "Re-run konfound_empdist() with case_info to enable composition tracking.",
           call. = FALSE)
    }
    # Scaling factor (ci only)
    if (normalize) {
      denom        <- mean(abs(ci))
      y_vals       <- tk$mean_ci_signed / denom
      expected_val <- mean(ci_oriented) / denom
      y_label      <- expression("Avg. relative influence " * (bar(c)[i] / mean("|" * c * "|")))
      ref_label    <- sprintf("Expected (dashed): %.2f", expected_val)
    } else {
      y_vals       <- tk$mean_ci_signed
      expected_val <- mean(ci_oriented)
      y_label      <- expression("Avg. signed influence " * (bar(c)[i] %.% sign(hat(beta))))
      ref_label    <- sprintf("Expected relative influence (horizontal dashed line): %.4f", expected_val)
    }
    norm_tag      <- if (normalize) " (normalized)" else " (raw)"
    subtitle_note <- "Higher values = replaced cases contributed more strongly to the effect."
  }
  
  # Bin by k*
  breaks <- pretty(tk$k_star, n = 40)
  bin_idx <- cut(tk$k_star, breaks = breaks, include.lowest = TRUE)
  
  bin_agg <- aggregate(
    data.frame(y = y_vals, k = tk$k_star),
    by = list(k_bin = bin_idx),
    FUN = function(x) c(mean = mean(x), n = length(x))
  )
  
  n_bins <- nrow(bin_agg)
  bin_df <- data.frame(
    avg_influence = bin_agg$y[, "mean"],
    count         = bin_agg$k[, "n"],
    xmid          = (breaks[seq_len(n_bins)] + breaks[seq_len(n_bins) + 1]) / 2
  )
  
  # Y-axis limits: symmetric around expected, minimum window
  max_dev <- max(abs(bin_df$avg_influence - expected_val),
                 0.15 * abs(expected_val))
  y_lo <- expected_val - max_dev * 1.3
  y_hi <- expected_val + max_dev * 1.3
  
  normalize_note <- if (metric == "ci" && normalize) {
    pct_oppose <- 100 * mean(ci * beta_sign < 0)
    sprintf("<br>Normalized by mean(|c<sub>i</sub>|); expected &lt; 1 because %.0f%% of cases oppose the effect.",
            pct_oppose)
  } else ""
  
  subtitle_txt <- sprintf(
    paste0(
      "Model: %s &nbsp;|&nbsp; N = %.0f<br>",
      "%s<br>",
      "<span style='font-size:7pt; color:grey50;'>",
      "Each point shows the average influence of replaced cases for that k* bin.%s<br>",
      "%s Trend line: LOESS.",
      "</span>"
    ),
    title_str, emp_res$n,
    ref_label,
    normalize_note,
    subtitle_note
  )
  
  ggplot2::ggplot(bin_df, ggplot2::aes(x = xmid, y = avg_influence)) +
    ggplot2::geom_hline(yintercept = expected_val, linetype = "dashed",
                        color = "gray40", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted",
                        color = "gray70", linewidth = 0.3) +
    ggplot2::geom_smooth(method = "loess", span = 0.5, se = TRUE,
                         color = "#4878A8", fill = "#4878A8",
                         alpha = 0.15, linewidth = 0.6) +
    ggplot2::geom_point(color = "#4878A8", size = 1.5, alpha = 0.8) +
    ggplot2::geom_vline(xintercept = k_cf, linetype = "dashed",
                        color = "gray30", linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = k_emp, linetype = "solid",
                        color = "gray30", linewidth = 0.5) +
    ggplot2::scale_y_continuous(limits = c(y_lo, y_hi)) +
    ggplot2::labs(
      title    = paste0("Influence of Replaced Cases Across k* Bins", norm_tag),
      subtitle = subtitle_txt,
      x = "k* (replacements to nullify)",
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "none",
      plot.subtitle   = ggtext::element_textbox_simple(
        size = 9, lineheight = 1.3, margin = ggplot2::margin(b = 8)
      )
    )
}


# S3 print method for rir_influence objects
#' @export
print.rir_influence <- function(x, top_n = 10, ...) {
  k_cf  <- attr(x, "k_cf")
  t_obs <- attr(x, "t_obs")
  n     <- attr(x, "n")
  fv    <- attr(x, "focal_var")
  mech  <- attr(x, "mechanical")
  mean_infl <- attr(x, "mean_influence")
  
  cat("RIR-Influence Diagnostic\n")
  cat(sprintf("  Focal: %s | N = %d | t = %.3f | k_cf = %.1f (continuous)\n",
              fv, n, t_obs, k_cf))
  cat(sprintf("  Mechanical (sample size) component: %.3f (constant for all cases)\n", mech))
  cat(sprintf("  Mean influence: %.3f (expected under uniform influence: ~1.0)\n\n", mean_infl))
  
  cat("--- Decomposition summary ---\n")
  cat(sprintf("  Mean |delta_beta|: %.3f\n", mean(abs(x$delta_beta))))
  cat(sprintf("  Mean |delta_se|:   %.3f\n", mean(abs(x$delta_se))))
  cat(sprintf("  Cor(delta_beta, delta_se): %.3f\n\n",
              cor(x$delta_beta, x$delta_se)))
  
  cat(sprintf("--- Top %d most influential cases ---\n", top_n))
  ord <- order(abs(x$influence), decreasing = TRUE)
  top <- head(ord, top_n)
  print.data.frame(x[top, c("case", "influence", "delta_beta", "delta_se")],
                   row.names = FALSE, digits = 3)
  
  cat(sprintf("\n--- Distribution of influence ---\n"))
  cat(sprintf("  Min: %.3f | Q25: %.3f | Median: %.3f | Q75: %.3f | Max: %.3f\n",
              min(x$influence), quantile(x$influence, 0.25),
              median(x$influence), quantile(x$influence, 0.75),
              max(x$influence)))
  cat(sprintf("  SD: %.3f | IQR: %.3f\n",
              sd(x$influence), IQR(x$influence)))
  
  invisible(x)
}


# RIR influence distribution histogram
# Input:  rir_infl from rir_influence() (provides psi_i per case, k_cf,
#         mean_influence, mechanical component).
# Output: ggplot histogram of psi_i = k_cf - k_cf^(-i) with density overlay
#         and mean vline.
plot_rir_dist_internal <- function(rir_infl, title_str) {
  infl <- rir_infl$influence
  n    <- length(infl)
  if (n == 0) stop("rir_infl$influence is empty.", call. = FALSE)
  
  k_cf       <- attr(rir_infl, "k_cf")
  mean_infl  <- attr(rir_infl, "mean_influence")
  focal_var  <- attr(rir_infl, "focal_var")
  mechanical <- attr(rir_infl, "mechanical")
  
  # Adaptive binwidth: same formula as .plot_shape
  rng  <- range(infl)
  span <- diff(rng); if (span <= 0) span <- 1
  bins_target <- max(40L, ceiling(1.5 * sqrt(n)))
  bw   <- max(0.001, span / bins_target)
  
  pct_positive <- 100 * mean(infl > 0)
  pct_negative <- 100 * mean(infl < 0)
  
  has_ggtext <- requireNamespace("ggtext", quietly = TRUE)
  
  sub_txt <- sprintf(
    paste0(
      "Model: %s &nbsp;|&nbsp; N = %d &nbsp;|&nbsp; k_cf = %.0f<br>",
      "Mean RIR influence = %.3f &nbsp;|&nbsp; Mechanical component = %.3f<br>",
      "<span style='font-size:7pt; color:grey50;'>",
      "%.0f%% of cases strengthen robustness (psi_i > 0: their removal lowers k_cf); ",
      "%.0f%% weaken it.",
      "</span>"
    ),
    title_str %||% focal_var, n, k_cf,
    mean_infl, mechanical,
    pct_positive, pct_negative
  )
  
  subtitle_elem <- if (has_ggtext) {
    ggtext::element_textbox_simple(size = 9, lineheight = 1.3,
                                   margin = ggplot2::margin(b = 8))
  } else {
    ggplot2::element_text(size = 7.5, family = "mono", color = "#4A5568",
                          margin = ggplot2::margin(b = 6))
  }
  
  col_fill    <- "#3AAFA9"
  col_border  <- "#2B7A78"
  col_density <- "#1A365D"
  col_mean    <- "#718096"
  col_zero    <- "#E53E3E"
  
  df <- data.frame(influence = infl)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = influence)) +
    ggplot2::geom_histogram(
      binwidth = bw, boundary = 0, closed = "left",
      fill = col_fill, color = col_border,
      alpha = 0.40, linewidth = 0.25
    ) +
    ggplot2::geom_vline(xintercept = mean_infl, linetype = "solid",
                        color = col_mean, linewidth = 0.6) +
    ggplot2::labs(
      title    = "Distribution of Per-Case RIR Influence",
      subtitle = sub_txt,
      x        = expression(psi[i] * "  (RIR influence = " * k[cf] - k[cf]^{(-i)} * ")"),
      y        = NULL
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.subtitle = subtitle_elem
    )
  
  # Density overlay (same as .plot_shape)
  if (n >= 10) {
    dens    <- density(infl, adjust = 1)
    dens_df <- data.frame(x = dens$x, y = dens$y * n * bw)
    p <- p + ggplot2::geom_line(
      data = dens_df, ggplot2::aes(x = x, y = y),
      color = col_density, linewidth = 0.6, alpha = 0.7
    )
  }
  
  p
}


# Public plot interface
# All wrappers take a konfound_empdist object as their first argument and
# delegate to the internal workers above.

#' Plot the empirical k* distribution
#'
#' @param result A konfound_empdist object
#' @param type "stacked" (default, requires case_info) or "shape" (always available)
#' @param verbose If TRUE, prints a description of the plot to the console.
#'   Default is FALSE.
#' @param ... Passed to the underlying plot function
plot_hist <- function(result, type = c("stacked", "shape"),
                      verbose = FALSE, ...) {
  if (!inherits(result, "konfound_empdist")) {
    stop("result must be a konfound_empdist object.", call. = FALSE)
  }
  type <- match.arg(type)
  
  if (verbose) {
    if (type == "stacked") {
        message(
            "This stacked histogram plot shows the empirical distribution of k*\n",
            "(the number of cases that must be replaced to nullify the inference)\n",
            "across random replacement scenarios. Bars are stacked by the proportion\n",
            "of supporters (cases that strengthen the focal effect) or suppressors\n",
            "(cases that weaken it) replaced in each run. The dashed vertical line\n",
            "marks the analytic k_cf (the replacement threshold derived from the\n",
            "closed-form solution); the solid line marks the empirical median. A\n",
            "roughly symmetric distribution centered near k_cf indicates the analytic\n",
            "estimate is a reliable approximation and the inference is stable. Left\n",
            "skew suggests specific influential cases are load-bearing: when those\n",
            "cases happen to be replaced, nullification occurs unusually quickly.\n",
            "Right skew is more commonly seen with near-marginal significance, often\n",
            "due to many suppressors in the data, whether systematic or not."
        )
    } else {
        message(
            "This histogram plot shows the shape and spread of the empirical\n",
            "distribution of k* (the number of cases that must be replaced to\n",
            "nullify the inference) across random replacement scenarios. The solid\n",
            "vertical line marks the empirical median. A roughly symmetric\n",
            "distribution centered near k_cf (the closed-form replacement threshold)\n",
            "indicates the analytic estimate is a reliable approximation and the\n",
            "inference is stable. Left skew suggests specific influential cases are\n",
            "load-bearing: when those cases are selected for replacement,\n",
            "nullification occurs unusually quickly. Right skew is more commonly\n",
            "seen with near-marginal significance, often due to the presence of\n",
            "suppressors in the data, whether systematic or not."
        )
    }
  }
  
  plot.konfound_empdist(result, type = type, ...)
}


#' Partial regression plot with per-case influence coloring
#'
#' @param result A konfound_empdist object (must have been run with case_info)
#' @param show_rir_labels If TRUE (default when rir_infl available), label top
#'   cases by RIR influence. Set FALSE to suppress labels.
#' @param top_n Number of cases to label (default: 10)
#' @param verbose If TRUE, prints a description of the plot to the console.
#'   Default is FALSE.
#' @param ... Passed to the underlying plot function
plot_partial <- function(result, show_rir_labels = NULL, top_n = 10,
                         verbose = FALSE, ...) {
  if (!inherits(result, "konfound_empdist")) {
    stop("result must be a konfound_empdist object.", call. = FALSE)
  }
  if (is.null(result$case_info)) {
    stop(
      "case_info not available. Pass case_info = label_cases(model, target_var) ",
      "to konfound_empdist() when running.",
      call. = FALSE
    )
  }
  
  if (verbose) {
    message(
        "This partial regression plot shows each case as a point in the space\n",
        "of (r_x, r_y), where r_x is the residual of the focal predictor after\n",
        "regressing on all other covariates, and r_y is the residual of the\n",
        "outcome regressed on the same covariates. These are the raw\n",
        "covariate-adjusted residuals from the Frisch-Waugh-Lovell\n",
        "decomposition. The per-case influence on the focal coefficient is\n",
        "c_i = r_x * r_y; point size and color reflect the magnitude and\n",
        "direction of this influence. Cases whose c_i shares the sign of the\n",
        "focal coefficient are colored as supporters (they strengthen the\n",
        "effect); those whose c_i opposes the sign of the focal coefficient\n",
        "are colored as suppressors (they weaken it).\n",
        "\n",
        "When RIR influence labels are shown, each labeled case also reports\n",
        "psi_i in replacement-equivalent units. A large positive psi_i means\n",
        "the case is more influential in sustaining robustness than a typical\n",
        "data point in the sample. A large negative psi_i means the case is\n",
        "negatively influential in RIR terms, actively reducing k* relative\n",
        "to what the rest of the data would imply."
    )
  }
  
  # Default: label with RIR influence when available
  use_labels <- if (is.null(show_rir_labels)) !is.null(result$rir_infl) else show_rir_labels
  .plot_partial_internal(
    result$case_info,
    title_str       = result$target_var,
    rir_infl        = result$rir_infl,
    show_rir_labels = use_labels,
    top_n           = top_n,
    ...
  )
}


#' Replacement composition plot across k* bins
#'
#' @param result A konfound_empdist object (must have been run with case_info)
#' @param type "ci" (default): average signed c_i influence per bin;
#'             "rir": average RIR influence (leave-one-out) per bin
#' @param verbose If TRUE, prints a description of the plot to the console.
#'   Default is FALSE.
#' @param ... Passed to the underlying plot function
plot_comp <- function(result, type = c("ci", "rir"),
                      verbose = FALSE, ...) {
  if (!inherits(result, "konfound_empdist")) {
    stop("result must be a konfound_empdist object.", call. = FALSE)
  }
  if (is.null(result$tracker) || nrow(result$tracker) == 0) {
    stop(
      "Composition tracking not available. Pass case_info = label_cases(model, target_var) ",
      "to konfound_empdist() when running.",
      call. = FALSE
    )
  }
  if (is.null(result$case_info)) {
    stop(
      "case_info not available. Pass case_info = label_cases(model, target_var) ",
      "to konfound_empdist() when running.",
      call. = FALSE
    )
  }
  type <- match.arg(type)
  if (type == "rir" && (is.null(result$rir_infl) || all(is.na(result$tracker$mean_rir_infl)))) {
    stop(
      "RIR influence not available. Ensure rir_influence() ran successfully ",
      "(requires lm model fit with model = TRUE and a significant effect).",
      call. = FALSE
    )
  }
  
  if (verbose) {
    message(
      "This binned composition plot shows how the average influence of\n",
      "replaced cases varies across k* bins. Each point summarizes the cases\n",
      "replaced in scenarios that fell within that k* range. When type =\n",
      "\"ci\", the y-axis shows the average signed c_i influence (r_x * r_y\n",
      "scaled by effect direction). When type = \"rir\", the y-axis shows the\n",
      "average psi_i (RIR influence) in replacement-equivalent units. The\n",
      "horizontal dashed line marks the expected value under no systematic\n",
      "pattern.\n",
      "\n",
      "If bins at the low end of k* (fragile runs) show average influence well\n",
      "above the reference line, it suggests fragility is driven by which\n",
      "specific high-influence cases were selected for replacement, not just\n",
      "random chance. A relatively flat profile across bins suggests influence\n",
      "is more evenly distributed and fragility is not concentrated in any\n",
      "particular subset of cases."
    )
  }
  
  emp_res <- list(tracker = result$tracker, k_cf = result$k_cf, n = result$n)
  ts <- result$target_var
  plot_influence_binned(
    emp_res, result$case_info,
    title_str = ts,
    metric    = type,
    rir_infl  = result$rir_infl,
    ...
  )
}


#' Distribution of per-case RIR influence
#'
#' @param result A konfound_empdist object (must have been run with case_info)
#' @param verbose If TRUE, prints a description of the plot to the console.
#'   Default is FALSE.
#' @param ... Passed to the underlying plot function
plot_rir_dist <- function(result, verbose = FALSE, ...) {
  if (!inherits(result, "konfound_empdist")) {
    stop("result must be a konfound_empdist object.", call. = FALSE)
  }
  if (is.null(result$rir_infl)) {
    stop(
      "RIR influence not available. Ensure case_info was passed to konfound_empdist() ",
      "and rir_influence() ran successfully (requires a significant lm effect).",
      call. = FALSE
    )
  }
  
  if (verbose) {
    message(sprintf(
      paste0(
        "This histogram plot shows the distribution of per-case RIR influence\n",
        "(psi_i) across all N = %d cases in the dataset. psi_i = k_cf -\n",
        "k_cf^(-i), interpreted in terms of the RIR metric: it is the change\n",
        "in the replacement threshold when case i is left out. A case with\n",
        "large positive psi_i is more influential than a typical data point in\n",
        "sustaining robustness, meaning its removal would noticeably lower k*.\n",
        "A case with large negative psi_i is negatively influential in RIR\n",
        "terms: its presence in the data reduces k* relative to what the rest\n",
        "of the sample would imply."
      ),
      result$n
    ))
  }
  
  plot_rir_dist_internal(result$rir_infl, title_str = result$target_var, ...)
}