# test_empirical_rir.R
# ============================================================================
# Main entry point, method runners, and print method for the empirical RIR
# distribution.
#
# Functions in this file (in order):
#   label_cases()            per-case FWL influence and supporter/suppressor labels
#   konfound_empdist()       public entry point; dispatches to direct or search
#   .empdist_direct()        direct method runner (LM only; fast)
#   .empdist_search()        search method runner (LM/GLM; finds k* per rep)
#   print.konfound_empdist() S3 print method for konfound_empdist objects
#
# Methods:
#   "direct"  Replace k_cf cases, refit once per rep, record nullified (yes/no).
#             Null rate ~50% if k_cf is well-calibrated. LM only.
#   "search"  Binary search + backward scan for k* per rep (see find_k_star()
#             in helpers_empirical_rir.R). Distribution of k* across reps.
#             Supports LM and GLM.
#
# Key inputs:
#   model       A fitted lm/glm object (must be fit with model = TRUE)
#   target_var  Name of the focal predictor coefficient
#   case_info   Output from label_cases(); enables composition tracking and
#               unlocks stacked plot. Auto-computed when not supplied.
#
# Key outputs:
#   An object of class "konfound_empdist" (a named list). Key fields:
#     $k_cf, $k_star, $tracker, $case_info, $rir_infl, $desc, $n, $method
#   Plot functions in helper_plot_empirical.R all accept this object.
# ============================================================================


# null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b


# Case-level influence labels (FWL decomposition)
# Input:  fitted lm object (model = TRUE required), focal_var, high_q threshold.
# Output: list with r_x, r_y (centered partial residuals), ci (r_x * r_y),
#         ci_share (normalized absolute contributions), beta_hat (FWL slope),
#         label (factor: high/typical x supporter/suppressor), high_threshold.
# Used by: plot_partial(), composition tracking in .empdist_search(),
#          plot_comp_stacked(), plot_supporter_binned().
label_cases <- function(fit, focal_var, high_q = 0.90) {
  mf   <- model.frame(fit)
  resp <- names(mf)[1]
  covs <- setdiff(names(mf), c(resp, focal_var))
  
  fml_x <- as.formula(paste(focal_var, "~", paste(covs, collapse = " + ")))
  fml_y <- as.formula(paste(resp, "~", paste(covs, collapse = " + ")))
  
  r_x <- residuals(lm(fml_x, data = mf))
  r_y <- residuals(lm(fml_y, data = mf))
  r_x_c <- r_x - mean(r_x)
  r_y_c <- r_y - mean(r_y)
  
  ci       <- r_x_c * r_y_c
  ci_share <- abs(ci) / sum(abs(ci))
  beta_hat <- sum(r_x_c * r_y_c) / sum(r_x_c^2)
  
  high_threshold <- quantile(ci_share, high_q)
  direction <- ifelse(sign(ci) == sign(beta_hat), "supporter", "suppressor")
  influence <- ifelse(ci_share >= high_threshold, "high", "typical")
  label <- factor(paste(influence, direction),
                  levels = c("high supporter", "typical supporter",
                             "typical suppressor", "high suppressor"))
  
  list(r_x = r_x_c, r_y = r_y_c, ci = ci, ci_share = ci_share,
       beta_hat = beta_hat, label = label, high_threshold = high_threshold)
}



# ============================================================================
# Main entry point
# ============================================================================

#' Empirical RIR Distribution
#'
#' @param model Fitted lm or glm object (must have model = TRUE)
#' @param target_var Name of the focal predictor coefficient
#' @param reps Number of simulation replications
#' @param method Either "direct" (fast, LM only) or "search" (original, LM/GLM)
#' @param k Override replacement count (default: k_cf). Only used for method="direct".
#' @param alpha Significance level for nullification decision
#' @param seed Random seed for reproducibility
#' @param sign_flip_nullifies If TRUE, sign reversal counts as nullification
#' @param engine For method="search": "auto", "fast", or "slow"
#' @param get_test For method="search": custom test extractor function
#' @param case_info Output from label_cases(). When provided with method="search",
#'                  enables per-rep composition tracking (supporter fraction, etc.)
#'                  and unlocks the stacked composition plot.
#' @param verbose Print detailed progress messages
#' @param progress Print progress every 100 reps
#' @return Object of class "konfound_empdist"
konfound_empdist <- function(model, target_var,
                             reps = 1000,
                             method = c("search", "direct"),
                             k = NULL,
                             alpha = 0.05,
                             seed = 123,
                             sign_flip_nullifies = TRUE,
                             engine = "auto",
                             get_test = NULL,
                             case_info = NULL,
                             verbose = FALSE,
                             progress = FALSE) {
  
  method <- match.arg(method)
  
  # Validate model
  if (is.null(model$model)) {
    stop("Fit the model with model=TRUE so model$model is available.", call. = FALSE)
  }
  
  summ <- summary(model)
  if (!target_var %in% rownames(summ$coefficients)) {
    stop("target_var '", target_var, "' not found in model coefficients.", call. = FALSE)
  }
  
  is_glm <- inherits(model, "glm")
  
  # Method validation
  if (method == "direct" && is_glm) {
    stop("method='direct' currently supports LM only. Use method='search' for GLM.", call. = FALSE)
  }
  
  # Auto-compute case_info when not supplied -- model and target_var are
  # already in hand, so there is no reason to require a separate call.
  if (is.null(case_info)) {
    case_info <- tryCatch(
      label_cases(model, focal_var = target_var),
      error = function(e) {
        message("note: label_cases() failed (", conditionMessage(e),
                "). Composition tracking disabled.")
        NULL
      }
    )
  } else {
    # Validate user-supplied case_info
    required <- c("ci", "ci_share", "beta_hat", "high_threshold")
    missing  <- setdiff(required, names(case_info))
    if (length(missing) > 0) {
      stop("case_info is missing required fields: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
  }
  
  # Dispatch to appropriate method
  if (method == "direct") {
    result <- .empdist_direct(
      model = model,
      target_var = target_var,
      reps = reps,
      k = k,
      alpha = alpha,
      seed = seed,
      sign_flip_nullifies = sign_flip_nullifies,
      case_info = case_info,
      verbose = verbose,
      progress = progress
    )
  } else {
    result <- .empdist_search(
      model = model,
      target_var = target_var,
      reps = reps,
      alpha = alpha,
      seed = seed,
      sign_flip_nullifies = sign_flip_nullifies,
      engine = engine,
      get_test = get_test,
      case_info = case_info,
      verbose = verbose,
      progress = progress
    )
  }
  
  result
}


# ============================================================================
# Direct method (FWL residualization approach)
# ============================================================================

.empdist_direct <- function(model, target_var, reps, k, alpha, seed,
                            sign_flip_nullifies, case_info,
                            verbose, progress) {
  
  # Prep using engine_resid.R
  prep <- prep_resid(model, target_var, alpha = alpha)
  
  n <- prep$n
  k_cf <- prep$k_cf
  
  # Run k* distribution simulation
  set.seed(seed)
  t_start <- Sys.time()
  
  result <- simulate_k_star_distribution(
    prep = prep,
    reps = reps,
    seed = seed,
    sign_flip_nullifies = sign_flip_nullifies,
    progress = progress
  )
  
  total_time <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  
  # Build status vector
  status <- ifelse(result$converged, "ok", "fail")
  
  # Summaries
  ok <- result$converged
  k_clean <- result$k_star[ok]
  
  desc <- list(
    reps_total = reps,
    reps_ok = sum(ok),
    reps_fail = sum(!ok),
    
    k_star_mean = if (length(k_clean) >= 2) mean(k_clean) else NA_real_,
    k_star_median = if (length(k_clean) >= 2) median(k_clean) else NA_real_,
    k_star_sd = if (length(k_clean) >= 2) sd(k_clean) else NA_real_,
    k_star_min = if (length(k_clean) >= 1) min(k_clean) else NA_integer_,
    k_star_max = if (length(k_clean) >= 1) max(k_clean) else NA_integer_,
    k_star_q25 = if (length(k_clean) >= 2) quantile(k_clean, 0.25) else NA_real_,
    k_star_q75 = if (length(k_clean) >= 2) quantile(k_clean, 0.75) else NA_real_,
    
    iterations_mean = mean(result$iterations),
    total_time_sec = total_time
  )
  
  # Build output object
  out <- list(
    target_var = target_var,
    method = "direct",
    k_cf = k_cf,
    
    # Per-rep results
    k_star = result$k_star,
    converged = result$converged,
    iterations = result$iterations,
    status = status,
    
    # Composition: not available for direct method
    tracker = NULL,
    case_info = case_info,
    
    # Summary statistics
    desc = desc,
    
    # Model info
    alpha = alpha,
    stat_obs = abs(prep$t_orig),
    crit = prep$t_crit,
    n = n,
    is_glm = FALSE,
    
    # Configuration
    seed = seed,
    reps = reps,
    sign_flip_nullifies = sign_flip_nullifies
  )
  
  class(out) <- "konfound_empdist"
  out
}


# ============================================================================
# Search method (original approach)
# ============================================================================

.empdist_search <- function(model, target_var, reps, alpha, seed,
                            sign_flip_nullifies, engine, get_test,
                            case_info, verbose, progress) {
  
  # Prep
  is_glm <- inherits(model, "glm")
  mf <- model$model
  n <- nrow(mf)
  summ <- summary(model)
  
  # Use the appropriate prep function based on model type
  prep <- prep_empirical_rir(model, target_var, alpha = alpha)
  
  # Closed-form anchor
  est <- coef(model)[[target_var]]
  se  <- summ$coefficients[target_var, "Std. Error"]
  stat_obs <- abs(est / se)
  crit <- if (is_glm) qnorm(1 - alpha/2) else qt(1 - alpha/2, df = model$df.residual)
  
  if (stat_obs <= crit) {
    stop("Effect is not currently significant at alpha = ", alpha, call. = FALSE)
  }
  
  prop_bias <- 1 - (crit / stat_obs)
  k_cf <- as.integer(round(prop_bias * n))
  k_cf <- max(0L, min(k_cf, n))
  
  # Determine whether to track composition
  track_comp <- !is.null(case_info)
  
  # Pre-extract case_info fields for speed (avoid repeated $ lookups)
  # Also compute RIR-influence (leave-one-out) once for the whole run
  rir_infl <- NULL
  if (track_comp) {
    ci_vals         <- case_info$ci
    ci_share_vals   <- case_info$ci_share
    high_thresh     <- case_info$high_threshold
    beta_sign       <- sign(case_info$beta_hat)
    rir_infl        <- tryCatch(
      rir_influence(model, target_var, alpha = alpha),
      error = function(e) NULL
    )
    if (!is.null(rir_infl)) {
      rir_infl_vals   <- rir_infl$influence
      rir_high_thresh <- quantile(abs(rir_infl_vals), 0.90)
    }
  }
  
  # Storage
  set.seed(seed)
  rep_seeds <- sample.int(.Machine$integer.max, reps)
  
  k_star         <- rep(NA_integer_, reps)
  k_bs           <- rep(NA_integer_, reps)
  converged      <- rep(NA, reps)
  iterations     <- rep(NA_integer_, reps)
  backward_evals <- rep(NA_integer_, reps)
  non_monotonic  <- rep(NA, reps)
  status         <- rep("ok", reps)
  
  # Composition tracker (only when case_info provided)
  if (track_comp) {
    comp_frac_supporter    <- rep(NA_real_, reps)
    comp_frac_high_infl    <- rep(NA_real_, reps)
    comp_mean_ci_share     <- rep(NA_real_, reps)
    comp_mean_ci_signed    <- rep(NA_real_, reps)
    comp_mean_rir_infl     <- rep(NA_real_, reps)
    comp_frac_high_rir     <- rep(NA_real_, reps)
  }
  
  t_start <- Sys.time()
  y0 <- prep$data[[prep$y_name]]
  
  for (r in seq_len(reps)) {
    
    if (progress && r %% 100 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
      rate <- r / elapsed
      eta <- (reps - r) / rate
      message(sprintf("Rep %d/%d (%.1f reps/sec, ETA: %.0f sec)", r, reps, rate, eta))
    }
    
    # Generate replacement scenario for this rep
    set.seed(rep_seeds[r])
    index_order <- sample.int(n)
    replacement_y_full <- generate_replacement_y(prep, seq_len(n), y0)
    
    # Find k* via binary search + backward scan
    res <- tryCatch(
      find_k_star(
        prep = prep,
        k_cf = k_cf,
        alpha = alpha,
        index_order = index_order,
        replacement_y_full = replacement_y_full,
        get_test = get_test,
        engine = engine,
        sign_flip_nullifies = sign_flip_nullifies
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    
    if (!is.null(res$error)) {
      status[r] <- "error"
      if (verbose) message("Rep ", r, " error: ", res$error)
      next
    }
    
    if (!isTRUE(res$converged) || !is.finite(res$k_star)) {
      status[r] <- "fail"
      next
    }
    
    k_star[r]         <- res$k_star
    k_bs[r]           <- res$k_bs
    converged[r]      <- res$converged
    iterations[r]     <- res$iterations
    backward_evals[r] <- res$backward_evals
    non_monotonic[r]  <- res$non_monotonic
    
    # Track composition: which cases were replaced?
    if (track_comp) {
      k <- res$k_star
      idx <- index_order[seq_len(k)]
      comp_frac_supporter[r] <- mean(sign(ci_vals[idx]) == beta_sign)
      comp_frac_high_infl[r] <- mean(ci_share_vals[idx] >= high_thresh)
      comp_mean_ci_share[r]  <- mean(ci_share_vals[idx])
      comp_mean_ci_signed[r] <- mean(ci_vals[idx] * beta_sign)
      if (!is.null(rir_infl)) {
        comp_mean_rir_infl[r] <- mean(rir_infl_vals[idx])
        comp_frac_high_rir[r] <- mean(abs(rir_infl_vals[idx]) >= rir_high_thresh)
      }
    }
  }
  
  total_time <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  
  # Summaries
  ok <- is.finite(k_star)
  k_clean <- k_star[ok]
  
  if (length(k_clean) < 5) {
    warning("Very few successful reps (", length(k_clean), "). Check model stability.", call. = FALSE)
  }
  
  desc <- list(
    reps_total = reps,
    reps_ok = sum(ok),
    reps_fail = sum(!ok),
    
    k_star_mean = if (length(k_clean) >= 2) mean(k_clean) else NA_real_,
    k_star_median = if (length(k_clean) >= 2) median(k_clean) else NA_real_,
    k_star_sd = if (length(k_clean) >= 2) sd(k_clean) else NA_real_,
    k_star_min = if (length(k_clean) >= 1) min(k_clean) else NA_integer_,
    k_star_max = if (length(k_clean) >= 1) max(k_clean) else NA_integer_,
    k_star_q25 = if (length(k_clean) >= 2) quantile(k_clean, 0.25) else NA_real_,
    k_star_q75 = if (length(k_clean) >= 2) quantile(k_clean, 0.75) else NA_real_,
    
    iterations_mean = if (any(ok)) mean(iterations[ok], na.rm = TRUE) else NA_real_,
    backward_evals_mean = if (any(ok)) mean(backward_evals[ok], na.rm = TRUE) else NA_real_,
    non_monotonic_rate = if (any(ok)) mean(non_monotonic[ok], na.rm = TRUE) else NA_real_,
    total_time_sec = total_time
  )
  
  # Build composition tracker data.frame (only successful reps)
  tracker <- NULL
  if (track_comp) {
    tracker <- data.frame(
      k_star         = k_star[ok],
      frac_supporter = comp_frac_supporter[ok],
      frac_high_infl = comp_frac_high_infl[ok],
      mean_ci_share  = comp_mean_ci_share[ok],
      mean_ci_signed = comp_mean_ci_signed[ok],
      mean_rir_infl  = if (!is.null(rir_infl)) comp_mean_rir_infl[ok] else rep(NA_real_, sum(ok)),
      frac_high_rir  = if (!is.null(rir_infl)) comp_frac_high_rir[ok] else rep(NA_real_, sum(ok)),
      stringsAsFactors = FALSE
    )
  }
  
  out <- list(
    target_var = target_var,
    method = "search",
    k_cf = k_cf,
    
    # Per-rep results
    k_star = k_star,
    k_bs = k_bs,
    converged = converged,
    iterations = iterations,
    backward_evals = backward_evals,
    non_monotonic = non_monotonic,
    status = status,
    
    # For compatibility
    nullified = rep(NA, reps),
    
    # Composition
    tracker = tracker,
    case_info = case_info,
    rir_infl = rir_infl,
    
    # Summary statistics
    desc = desc,
    
    # Model info
    alpha = alpha,
    stat_obs = stat_obs,
    crit = crit,
    n = n,
    is_glm = is_glm,
    
    # Configuration
    seed = seed,
    reps = reps,
    engine = engine,
    sign_flip_nullifies = sign_flip_nullifies
  )
  
  class(out) <- "konfound_empdist"
  out
}


# ============================================================================
# Print method
# ============================================================================
#' @export
print.konfound_empdist <- function(x, verbose = FALSE, ...) {
  
  stopifnot(inherits(x, "konfound_empdist"))
  
  use_crayon <- requireNamespace("crayon", quietly = TRUE)
  bold <- if (use_crayon) crayon::bold else identity
  
  # ---- Title -----------------------------------------------------------------
  cat(bold("Empirical Robustness of Inference to Replacement (RIR)"), "\n\n")
  
  # ---- Background ------------------------------------------------------------
  cat(bold("Background"), "\n")
  bg <- paste0(
    "The standard (closed-form) RIR gives a single threshold derived from the ",
    "t-statistic: the minimum number of cases that would need to be replaced ",
    "with null-effect cases to change the statistical inference. The empirical ",
    "RIR extends this by running repeated resampling iterations, each drawing ",
    "a random set of replacement cases and finding k*, the minimum ",
    "replacements needed to nullify the inference in that iteration. The ",
    "resulting distribution of k* is the empirical RIR, capturing how the ",
    "robustness threshold varies with which specific cases are replaced."
  )
  cat(strwrap(bg, width = 72), sep = "\n")
  cat("\n")
  
  # ---- Key Statistics --------------------------------------------------------
  cat(bold("Key Statistics"), "\n")
  cat(strwrap(sprintf(
    "The focal predictor is %s with N = %d and alpha = %.2f. The observed |t| is %.3f, which %s the critical value of %.3f.",
    x$target_var, x$n, x$alpha,
    x$stat_obs,
    if (x$stat_obs > x$crit) "exceeds" else "does not exceed",
    x$crit
  ), width = 72), sep = "\n")
  cat("\n")
  
  # ---- Closed-Form and Empirical RIR -----------------------------------------
  cat(bold("Closed-Form and Empirical RIR"), "\n")
  
  if (x$desc$reps_ok >= 2) {
    sd_pct  <- 100 * x$desc$k_star_sd / x$desc$k_star_median
    gap     <- x$desc$k_star_median - x$k_cf
    gap_abs <- abs(gap)
    gap_pct <- abs(100 * gap / x$k_cf)
    gap_dir <- if (gap > 0) "higher" else if (gap < 0) "lower" else "equal to"
    
    reps_str <- if (x$desc$reps_fail > 0) {
      sprintf("%d resampling iterations (%d successful, %d failed)",
              x$desc$reps_total, x$desc$reps_ok, x$desc$reps_fail)
    } else {
      sprintf("%d resampling iterations", x$desc$reps_total)
    }
    
    body <- sprintf(
      paste0(
        "The standard (closed-form) RIR is %.0f (%.1f%% of the sample). ",
        "To nullify the inference, %.0f or more cases would need to be ",
        "replaced with cases for which there is no effect. ",
        "Across %s, the median empirical k* is %.0f and the mean is %.1f ",
        "(SD = %.1f, %.1f%% of the median; IQR: [%.0f, %.0f]). ",
        "The empirical median is %.0f case%s %s than the closed-form RIR of %.0f, ",
        "a difference of %.1f%%."
      ),
      x$k_cf, 100 * x$k_cf / x$n,
      x$k_cf,
      reps_str,
      x$desc$k_star_median, x$desc$k_star_mean,
      x$desc$k_star_sd, sd_pct,
      x$desc$k_star_q25, x$desc$k_star_q75,
      gap_abs, if (gap_abs == 1) "" else "s", gap_dir, x$k_cf,
      gap_pct
    )
    cat(strwrap(body, width = 72), sep = "\n")
  } else {
    cat(strwrap(sprintf(
      "The standard (closed-form) RIR is %.0f (%.1f%% of the sample). Not enough successful resampling iterations to summarize the empirical distribution.",
      x$k_cf, 100 * x$k_cf / x$n
    ), width = 72), sep = "\n")
  }
  cat("\n")
  
  # ---- Replacement composition (when tracked) --------------------------------
  if (!is.null(x$tracker) && nrow(x$tracker) > 0) {
    cat(bold("Replacement Composition"), "\n")
    
    # -- c_i block --
    avg_supp      <- 100 * mean(x$tracker$frac_supporter, na.rm = TRUE)
    avg_high_ci   <- 100 * mean(x$tracker$frac_high_infl, na.rm = TRUE)
    avg_ci_signed <- mean(x$tracker$mean_ci_signed, na.rm = TRUE)
    exp_ci_signed <- mean(x$case_info$ci * sign(x$case_info$beta_hat))
    ci_diff       <- avg_ci_signed - exp_ci_signed
    ci_diff_pct   <- if (abs(exp_ci_signed) > 1e-10) 100 * ci_diff / abs(exp_ci_signed) else NA_real_
    
    cat("Case influence (c_i = r_x * r_y):\n")
    if (is.finite(ci_diff_pct)) {
      ci_body <- sprintf(
        paste0(
          "On average, %.1f%% of replaced cases were supporters of the effect ",
          "and %.1f%% were high-influence cases (top 10%% by |c_i|). ",
          "The average signed c_i of replaced cases was %.4f ",
          "(expected under random replacement: %.4f, ",
          "a difference of %.4f / %.1f%%)."
        ),
        avg_supp, avg_high_ci,
        avg_ci_signed, exp_ci_signed, ci_diff, ci_diff_pct
      )
    } else {
      ci_body <- sprintf(
        paste0(
          "On average, %.1f%% of replaced cases were supporters of the effect ",
          "and %.1f%% were high-influence cases (top 10%% by |c_i|). ",
          "The average signed c_i of replaced cases was %.4f ",
          "(expected under random replacement: %.4f)."
        ),
        avg_supp, avg_high_ci, avg_ci_signed, exp_ci_signed
      )
    }
    cat(strwrap(ci_body, width = 72, indent = 0, exdent = 0), sep = "\n")
    cat("\n")
    
    # -- RIR influence block (only when rir_infl was computed) --
    if (!is.null(x$rir_infl) && !all(is.na(x$tracker$mean_rir_infl))) {
      avg_rir      <- mean(x$tracker$mean_rir_infl, na.rm = TRUE)
      exp_rir      <- attr(x$rir_infl, "mean_influence")
      rir_diff     <- avg_rir - exp_rir
      rir_diff_pct <- if (abs(exp_rir) > 1e-10) 100 * rir_diff / abs(exp_rir) else NA_real_
      avg_high_rir <- 100 * mean(x$tracker$frac_high_rir, na.rm = TRUE)
      
      cat("RIR influence (leave-one-out k_cf change):\n")
      if (is.finite(rir_diff_pct)) {
        rir_body <- sprintf(
          paste0(
            "The average RIR influence of replaced cases was %.3f ",
            "(expected: %.3f, a difference of %.3f / %.1f%%). ",
            "High-influence cases (top 10%% by |RIR influence|) made up ",
            "%.1f%% of replaced cases on average."
          ),
          avg_rir, exp_rir, rir_diff, rir_diff_pct, avg_high_rir
        )
      } else {
        rir_body <- sprintf(
          paste0(
            "The average RIR influence of replaced cases was %.3f ",
            "(expected: %.3f). ",
            "High-influence cases (top 10%% by |RIR influence|) made up ",
            "%.1f%% of replaced cases on average."
          ),
          avg_rir, exp_rir, avg_high_rir
        )
      }
      cat(strwrap(rir_body, width = 72, indent = 0, exdent = 0), sep = "\n")
      cat("\n")
    }
  }
  
  # ---- Performance (verbose only) --------------------------------------------
  if (isTRUE(verbose)) {
    cat(bold("Performance"), "\n")
    if (is.finite(x$desc$iterations_mean)) {
      cat(sprintf("Avg iterations (Phase 1 binary search): %.1f\n",
                  x$desc$iterations_mean))
    }
    if (!is.null(x$desc$backward_evals_mean) && is.finite(x$desc$backward_evals_mean)) {
      cat(sprintf("Avg backward evals (Phase 2 scan): %.1f\n",
                  x$desc$backward_evals_mean))
    }
    if (!is.null(x$desc$non_monotonic_rate) && is.finite(x$desc$non_monotonic_rate)) {
      cat(sprintf("Non-monotonic reps: %.1f%%\n",
                  x$desc$non_monotonic_rate * 100))
    }
    cat(sprintf("Total time: %.1f sec  (%.1f reps/sec)\n\n",
                x$desc$total_time_sec, x$reps / x$desc$total_time_sec))
  }
  
  # ---- Supplemental plots ----------------------------------------------------
  cat(bold("Supplemental Plots"), "\n")
  cat(strwrap(
    "The empirical RIR distribution and case-level influence patterns can be explored further with the following plots. Use verbose = TRUE for a description of what each plot shows.",
    width = 72), sep = "\n")
  cat("\n")
  cat("plot_hist(result): k* distribution with replacement composition\n")
  cat("plot_partial(result): per-case influence on the focal coefficient\n")
  if (!is.null(x$rir_infl)) {
      cat("plot_rir_dist(result): distribution of per-case RIR influence (psi_i)\n")
  }
  cat("plot_comp(result):  signed c_i influence composition across k* bins\n")
  cat("plot_comp(result, \"rir\"): RIR influence composition across k* bins\n")
  invisible(x)
}