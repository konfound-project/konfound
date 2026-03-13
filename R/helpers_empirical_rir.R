# helpers_empirical_rir.R
# ============================================================================
# Utility functions supporting the empirical RIR distribution workflow.
#
# Functions in this file (in order):
#   refit_like()              refit an lm with new data, preserving weights/offsets
#   prep_empirical_rir()      precompute all quantities needed across replacement reps
#   `%||%`                    null-coalescing operator
#   generate_replacement_y()  construct Y* satisfying Y* perp X | Z
#   check_nullified()         test whether a single replacement scenario nullifies
#   find_k_star()             two-phase binary search + backward scan for k*
#   rir_influence()           leave-one-out RIR decomposition (psi_i per case)
#
# Key inputs:
#   model       A fitted lm object (must be fit with model = TRUE)
#   target_var  Name of the focal predictor coefficient
#   prep        Output from prep_empirical_rir(); passed into all engine functions
#
# Key outputs:
#   prep_empirical_rir()  Returns a named list used by engines and runners
#   find_k_star()         Returns a list with k_star, k_bs, diagnostics
#   rir_influence()       Returns a data.frame of class c("rir_influence",
#                         "data.frame") with psi_i and decomposition per case
#
# Design rules:
#   1. Always refit from an explicit formula + explicit data.
#   2. Always use the stored model frame (model$model) for reproducibility.
#   3. Precompute ingredients used repeatedly across reps.
# ============================================================================
#' @importFrom stats coef qnorm quantile median sd terms formula update as.formula
#'   lm fitted residuals reformulate model.matrix model.frame

# Refit an lm using explicit formula and data (avoids update() replaying the
# original call, which is fragile when data are modified mid-simulation).
# Preserves weights and offsets from the original model call if present.
refit_like <- function(model, formula, data, keep_model = TRUE) {
  
  
  w_expr   <- model$call$weights %||% NULL
  off_expr <- model$call$offset  %||% NULL
  
  w <- NULL
  off <- NULL
  
  if (!is.null(w_expr)) {
    w <- tryCatch(eval(w_expr, data, parent.frame()),
                  error = function(e) stop("Failed to evaluate weights in refit_like(): ", e$message, call. = FALSE))
  }
  if (!is.null(off_expr)) {
    off <- tryCatch(eval(off_expr, data, parent.frame()),
                    error = function(e) stop("Failed to evaluate offset in refit_like(): ", e$message, call. = FALSE))
  }
  
  if (inherits(model, "lm")) {
    args <- list(formula = formula, data = data, model = keep_model)
    if (!is.null(w))   args$weights <- w
    if (!is.null(off)) args$offset  <- off
    return(do.call(stats::lm, args))
  }
  
  stop("refit_like() currently supports lm models only.", call. = FALSE)
}


# Precompute all quantities needed repeatedly across replacement reps.
# Requires model fit with model = TRUE. Returns a named list (the "prep object")
# consumed by check_nullified(), find_k_star(), and the runner.
# r_x and r_y are partial-plot residuals: X and Y each residualized on Z
# via the reduced model (FWL decomposition). fast_lm caches the QR factorization
# and (X'X)^-1 diagonal for the target coefficient to skip full refits when possible.
prep_empirical_rir <- function(model, target_var, alpha = 0.05) {
  
  if (is.null(model$model)) stop("Model must contain model frame. Fit with model=TRUE.", call. = FALSE)
  
  mf <- model$model
  n  <- nrow(mf)
  
  f_full <- stats::formula(model)
  y_name <- all.vars(f_full)[1]
  
  if (!target_var %in% names(mf)) stop("target_var not found in model$model.", call. = FALSE)
  if (!y_name %in% names(mf)) stop("Outcome not found in model$model.", call. = FALSE)
  
  # Reduced formula (remove target_var from RHS)
  # Used to compute replacement outcomes: Y* = pred_red + resampled residual
  f_red <- stats::update(f_full, paste(". ~ . -", target_var))
  
  mod_red <- refit_like(model, f_red, data = mf, keep_model = TRUE)
  
  # Reduced-model predictions and residuals
  pred_red <- stats::fitted(mod_red)
  res_red  <- stats::resid(mod_red)
  
  # Partial plot ingredients (residualize X and Y on Z)
  z_terms <- attr(terms(f_red), "term.labels")
  
  residualize_on_z <- function(response_name) {
    if (length(z_terms) == 0) {
      v <- mf[[response_name]]
      return(v - mean(v, na.rm = TRUE))
    }
    f <- stats::reformulate(z_terms, response = response_name)
    fit <- stats::lm(f, data = mf)
    stats::resid(fit)
  }
  
  r_x <- residualize_on_z(target_var)
  r_y <- residualize_on_z(y_name)
  
  # Influence diagnostics from the full model (optional)
  hat <- tryCatch(stats::hatvalues(model), error = function(e) rep(NA_real_, n))
  cooks <- tryCatch(stats::cooks.distance(model), error = function(e) rep(NA_real_, n))
  
  dfbetas_target <- tryCatch({
    db <- stats::dfbetas(model)
    if (is.matrix(db) && (target_var %in% colnames(db))) {
      as.numeric(db[, target_var])
    } else {
      rep(NA_real_, n)
    }
  }, error = function(e) rep(NA_real_, n))
  
  # OPTIMIZATION: Precompute Linear Algebra Ingredients
  fast_lm <- NULL
  
  X_mat <- tryCatch(stats::model.matrix(model), error = function(e) NULL)
  
  if (!is.null(X_mat)) {
    target_idx <- which(colnames(X_mat) == target_var)
    
    if (length(target_idx) == 1) {
      
      if (inherits(model, "lm")) {
        
        qr_x <- qr(X_mat)
        
        if (qr_x$rank < ncol(X_mat)) {
          fast_lm <- NULL
        } else {
          XtX_inv_pivoted <- tryCatch(chol2inv(qr.R(qr_x)), error = function(e) NULL)
          
          target_pos_in_pivot <- which(qr_x$pivot == target_idx)
          
          if (!is.null(XtX_inv_pivoted) && length(target_pos_in_pivot) == 1) {
            
            XtX_inv_val <- XtX_inv_pivoted[target_pos_in_pivot, target_pos_in_pivot]
            
            fast_lm <- list(
              qr = qr_x,
              XtX_inv_target = XtX_inv_val,
              df_resid = model$df.residual,
              target_idx = target_idx,
              weights = if (!is.null(model$weights)) model$weights else NULL,
              offset = model$offset
            )
          }
        }
      }
    }
  }
  
  # Return a compact prep object
  list(
    # Core objects
    model = model,
    data = mf,
    n = n,
    
    # Target definition
    target_var = target_var,
    orig_sign = sign(coef(model)[[target_var]]),
    
    # Formulas
    f_full = f_full,
    f_red = f_red,
    
    # Aliases for backward compatibility
    formula_full = f_full,
    formula_red = f_red,
    
    # Outcome name
    y_name = y_name,
    
    # Reduced-model outputs
    pred_red = pred_red,
    res_red = res_red,
    
    # Test configuration
    alpha = alpha,
    
    # Partial plot ingredients
    r_x = as.numeric(r_x),
    r_y = as.numeric(r_y),
    
    # Diagnostics
    hat = as.numeric(hat),
    cooks = as.numeric(cooks),
    dfbetas_target = as.numeric(dfbetas_target),
    
    # Optimized ingredients
    fast_lm = fast_lm
  )
}



# Small utility: null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b


# Build Y* at all n positions: pred_red + bootstrap residual from reduced model.
# Satisfies Y* perp X | Z (replacement source assumption).
# idx: integer vector of indices to replace; y0: original outcome vector.
generate_replacement_y <- function(prep, idx, y0) {
  
  y_new <- y0
  k <- length(idx)
  
  # LM: pred_red + bootstrap residual from reduced model
  shuffled_res <- sample(prep$res_red, k, replace = TRUE)
  y_new[idx] <- prep$pred_red[idx] + shuffled_res
  
  y_new
}


# Check whether inference is nullified for one replacement scenario at fixed k.
# Tries the fast QR path first; falls back to full refit if unavailable.
# Returns a list: nullified (logical), pval, est, se, status.
check_nullified <- function(prep, idx, y_new = NULL,
                            alpha = 0.05,
                            get_test = NULL,
                            engine = c("auto", "slow", "fast")) {
  
  engine <- match.arg(engine)
  
  n <- prep$n
  y0 <- prep$data[[prep$y_name]]
  
  # Generate replacement if not provided
  if (is.null(y_new)) {
    y_new <- generate_replacement_y(prep, idx, y0)
  }
  
  # Determine fast path eligibility
  custom_test_requested <- !is.null(get_test)
  allow_fast <- FALSE
  
  if (!custom_test_requested && engine != "slow") {
    if (!is.null(prep$fast_lm)) {
      if (is.null(prep$fast_lm$weights) && is.null(prep$fast_lm$offset)) {
        allow_fast <- TRUE
      }
    }
  }
  
  if (engine == "fast" && !allow_fast) {
    stop("engine='fast' requested but fast path unavailable.", call. = FALSE)
  }
  
  # Default test extractor
  if (is.null(get_test)) {
    get_test <- function(fit, target_var, alpha) {
      cf <- tryCatch(summary(fit)$coefficients, error = function(e) NULL)
      if (is.null(cf) || !(target_var %in% rownames(cf))) {
        return(list(est = NA_real_, se = NA_real_, pval = NA_real_))
      }
      est <- tryCatch(unname(coef(fit)[[target_var]]), error = function(e) NA_real_)
      se  <- tryCatch(unname(cf[target_var, "Std. Error"]), error = function(e) NA_real_)
      pcol <- intersect(c("Pr(>|t|)", "Pr(>|z|)"), colnames(cf))
      pval <- if (length(pcol) == 1) suppressWarnings(as.numeric(cf[target_var, pcol])) else NA_real_
      list(est = est, se = se, pval = pval)
    }
  }
  
  # Initialize outputs
  est <- NA_real_
  se  <- NA_real_
  pval <- NA_real_
  status <- "ok"
  used_fast <- FALSE
  
  # =========================================================================
  # FAST LM PATH
  # =========================================================================
  if (allow_fast && !is.null(prep$fast_lm)) {
    
    qr_x <- prep$fast_lm$qr
    j_col <- prep$fast_lm$target_idx
    
    beta_fast <- tryCatch(qr.coef(qr_x, y_new), error = function(e) NULL)
    
    if (!is.null(beta_fast) && is.finite(j_col) && length(j_col) == 1L) {
      est_try <- beta_fast[j_col]
      
      if (is.finite(est_try)) {
        resid_fast <- qr.resid(qr_x, y_new)
        rss <- sum(resid_fast^2, na.rm = TRUE)
        df_res <- prep$fast_lm$df_resid
        
        if (is.finite(df_res) && df_res > 0) {
          sigma2 <- max(rss / df_res, .Machine$double.eps)
          xtxjj <- prep$fast_lm$XtX_inv_target
          
          if (is.finite(xtxjj) && xtxjj > 0) {
            se_fast <- sqrt(sigma2 * xtxjj)
            
            if (is.finite(se_fast) && se_fast > 0) {
              est <- unname(est_try)
              se  <- unname(se_fast)
              stat <- est / se
              pval <- 2 * stats::pt(-abs(stat), df = df_res)
              used_fast <- TRUE
            }
          }
        }
      }
    }
    
    if (!used_fast) status <- "fast_lm_fallback"
  }
  
  # =========================================================================
  # SLOW PATH (Fallback)
  # =========================================================================
  if (!used_fast) {
    
    new_data <- prep$data
    new_data[[prep$y_name]] <- y_new
    
    fit_new <- tryCatch(
      refit_like(prep$model, prep$f_full, data = new_data, keep_model = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(fit_new)) {
      return(list(nullified = NA, pval = NA_real_, est = NA_real_, se = NA_real_, status = "refit_fail"))
    }
    
    if (!(prep$target_var %in% names(stats::coef(fit_new)))) {
      return(list(nullified = NA, pval = NA_real_, est = NA_real_, se = NA_real_, status = "no_coef"))
    }
    
    tst <- tryCatch(
      get_test(fit_new, prep$target_var, alpha),
      error = function(e) list(est = NA_real_, se = NA_real_, pval = NA_real_)
    )
    
    est <- tst$est
    se  <- tst$se
    pval <- tst$pval
    status <- "slow"
  }
  
  # Compute p-value from est/se if missing
  if (!is.finite(pval) && is.finite(est) && is.finite(se) && se > 0) {
    stat <- est / se
    df <- prep$fast_lm$df_resid %||% prep$model$df.residual
    if (is.finite(df) && df > 0) {
      pval <- 2 * stats::pt(-abs(stat), df = df)
    }
  }
  
  # Nullification decision
  nullified <- if (is.finite(pval)) (pval >= alpha) else NA
  
  list(
    nullified = nullified,
    pval = pval,
    est = est,
    se = se,
    status = status
  )
}


# Find k* for a single replacement scenario using a two-phase search.
#
# Phase 1 (binary search):
#   Finds k_bs where k_bs nullifies and k_bs-1 does not. Assumes approximate
#   monotonicity: O(log N) refits. Binary search can only overshoot the true
#   first crossing (it verifies k_bs is a real crossing; the question is only
#   whether an earlier crossing exists that recovered).
#
# Phase 2 (backward scan):
#   Scans from (k_bs - delta) to k_bs to find the earliest nullification.
#   delta = max(5, ceiling(c * sqrt(k_bs))): scales with the random-walk
#   fluctuation of the replacement path's partial sum S_xy(k) ~ O(sqrt(k)).
#   Adaptive: if an earlier crossing is found, extends backward by delta/2
#   and repeats. Total cost: O(log N + sqrt(k_bs)) per rep.
#
# CRN within rep:
#   Replacement outcomes are pre-generated for all n indices; for any k, only
#   the first k values from index_order are applied. This ensures coherent
#   comparison across k values within the same rep.
#
# Returns a list: k_star, k_bs, converged, iterations, backward_delta,
#   backward_evals, non_monotonic, final_check.
find_k_star <- function(prep, k_cf,
                        alpha = 0.05,
                        index_order = NULL,
                        replacement_y_full = NULL,
                        get_test = NULL,
                        engine = "auto",
                        sign_flip_nullifies = TRUE,
                        backward_c = 3,
                        max_iter = 30) {
  
  n <- prep$n
  y0 <- prep$data[[prep$y_name]]
  orig_sign <- sign(stats::coef(prep$model)[[prep$target_var]])
  
  # Default: random permutation of indices
  if (is.null(index_order)) {
    index_order <- sample.int(n)
  }
  
  # Pre-generate replacement outcomes for all n indices
  # This is the "replacement scenario" that stays fixed as k varies
  if (is.null(replacement_y_full)) {
    replacement_y_full <- generate_replacement_y(prep, seq_len(n), y0)
  }
  
  # Helper: check nullification at k using first k indices from index_order
  check_at_k <- function(k) {
    if (k == 0L) {
      return(list(nullified = FALSE, pval = NA_real_, est = NA_real_, se = NA_real_, status = "k0"))
    }
    
    idx <- index_order[seq_len(k)]
    y_new <- y0
    y_new[idx] <- replacement_y_full[idx]
    
    res <- check_nullified(prep, idx, y_new = y_new, alpha = alpha,
                           get_test = get_test, engine = engine)
    
    # Sign flip as nullification
    if (sign_flip_nullifies && is.finite(res$est)) {
      if (sign(res$est) != orig_sign) {
        res$nullified <- TRUE
      }
    }
    
    res
  }
  
  # Early-return template for edge cases
  make_result <- function(k_star, k_bs, converged, iterations, note = NULL,
                          final_check = NULL, backward_delta = 0L,
                          backward_evals = 0L, non_monotonic = FALSE) {
    out <- list(
      k_star = as.integer(k_star),
      k_bs = as.integer(k_bs),
      converged = converged,
      iterations = iterations,
      backward_delta = as.integer(backward_delta),
      backward_evals = as.integer(backward_evals),
      non_monotonic = non_monotonic,
      final_check = final_check
    )
    if (!is.null(note)) out$note <- note
    out
  }
  
  # =========================================================================
  # PHASE 1: Binary search
  # =========================================================================
  lower <- 0L
  upper <- n
  iterations <- 0L
  
  # Check bounds
  check_upper <- check_at_k(upper)
  if (!isTRUE(check_upper$nullified)) {
    return(make_result(NA_integer_, NA_integer_, FALSE, 0L,
                       note = "k=n does not nullify", final_check = check_upper))
  }
  
  check_lower <- check_at_k(lower)
  if (isTRUE(check_lower$nullified)) {
    return(make_result(0L, 0L, TRUE, 0L,
                       note = "k=0 already nullifies", final_check = check_lower))
  }
  
  # Binary search: invariant is lower never nullifies, upper always nullifies
  while ((upper - lower) > 1L && iterations < max_iter) {
    iterations <- iterations + 1L
    mid <- as.integer(floor((lower + upper) / 2))
    check_mid <- check_at_k(mid)
    
    if (is.na(check_mid$nullified)) {
      # Ambiguous — treat conservatively as not nullified
      lower <- mid
      next
    }
    
    if (check_mid$nullified) {
      upper <- mid
    } else {
      lower <- mid
    }
  }
  
  k_bs <- as.integer(upper)
  
  # =========================================================================
  # PHASE 2: Backward scan for first crossing
  # =========================================================================
  # Binary search can only overshoot the first crossing (it finds a valid
  
  # crossing, but may miss an earlier one that recovered). Scan backward
  # from k_bs to find the earliest nullification in the window.
  #
  # Window size scales as sqrt(k_bs): the replacement path's partial sum
  # S_xy(k) has random-walk fluctuations of order O(sqrt(k)), so the
  # distance between a spurious first-crossing and the trend-crossing
  # scales similarly.
  
  delta <- max(5L, as.integer(ceiling(backward_c * sqrt(k_bs))))
  scan_lo <- max(1L, k_bs - delta)
  scan_hi <- k_bs - 1L
  
  k_first <- k_bs
  backward_evals <- 0L
  
  # Scan with adaptive extension: if we find an earlier crossing,
  # extend backward by delta/2 and repeat
  repeat {
    found_earlier <- FALSE
    
    if (scan_lo <= min(scan_hi, k_first - 1L)) {
      for (k in scan_lo:min(scan_hi, k_first - 1L)) {
        backward_evals <- backward_evals + 1L
        chk <- check_at_k(k)
        if (isTRUE(chk$nullified)) {
          k_first <- as.integer(k)
          found_earlier <- TRUE
          break  # Found earliest in this chunk; may extend further
        }
      }
    }
    
    if (!found_earlier || scan_lo <= 1L) break
    
    # Extend backward by delta/2
    ext <- max(1L, as.integer(ceiling(delta / 2)))
    new_lo <- max(1L, scan_lo - ext)
    if (new_lo >= scan_lo) break
    scan_hi <- scan_lo - 1L
    scan_lo <- new_lo
  }
  
  non_monotonic <- (k_first < k_bs)
  k_star <- k_first
  final_check <- check_at_k(k_star)
  
  make_result(k_star, k_bs, TRUE, iterations,
              final_check = final_check,
              backward_delta = delta,
              backward_evals = backward_evals,
              non_monotonic = non_monotonic)
}


# rir_influence: leave-one-out RIR decomposition
# Input:  fitted lm object (lm only, not glm), focal_var, alpha.
# Output: data.frame of class c("rir_influence", "data.frame") with one row
#         per case. Columns: case, influence (psi_i = k_cf - k_cf^(-i)),
#         delta_beta, delta_se, mechanical, beta_loo, se_loo, t_loo, k_cf_loo.
#         Attributes: k_cf, t_obs, beta_obs, se_obs, mechanical,
#         mean_influence, n, focal_var.
#
# psi_i decomposition: influence_i = mechanical + delta_beta_i + delta_se_i
#   mechanical:  change in k_cf from N -> N-1 (constant for all cases)
#   delta_beta:  change due to the coefficient shifting (beta channel)
#   delta_se:    change due to the SE shifting (SE channel)
rir_influence <- function(fit, focal_var, alpha = 0.05) {
  
  stopifnot(inherits(fit, "lm"), !inherits(fit, "glm"))
  
  smry   <- summary(fit)
  coefs  <- smry$coefficients
  idx    <- match(focal_var, rownames(coefs))
  if (is.na(idx)) stop("focal_var '", focal_var, "' not found in model.", call. = FALSE)
  
  beta_obs <- coefs[idx, "Estimate"]
  se_obs   <- coefs[idx, "Std. Error"]
  t_obs    <- coefs[idx, "t value"]
  
  n  <- stats::nobs(fit)
  p  <- length(stats::coef(fit))
  df <- n - p
  
  rir_continuous <- function(t_stat, n_obs, df_resid) {
    t_crit <- stats::qt(1 - alpha / 2, df = df_resid)
    if (abs(t_stat) <= t_crit) return(0)
    (1 - t_crit / abs(t_stat)) * n_obs
  }
  
  k_cf <- rir_continuous(t_obs, n, df)
  if (k_cf <= 0) {
    stop("Effect is not significant at alpha = ", alpha,
         ". rir_influence() requires a significant baseline.", call. = FALSE)
  }
  
  X      <- stats::model.matrix(fit)
  e      <- stats::residuals(fit)
  h      <- stats::hatvalues(fit)
  s2     <- smry$sigma^2
  XtXinv <- smry$cov.unscaled
  XtXinv_X <- XtXinv %*% t(X)
  
  df_loo <- df - 1
  
  beta_loo <- numeric(n)
  se_loo   <- numeric(n)
  t_loo    <- numeric(n)
  k_loo    <- numeric(n)
  
  for (i in seq_len(n)) {
    hi <- h[i]
    ei <- e[i]
    dfbeta_i    <- XtXinv_X[idx, i] * ei / (1 - hi)
    beta_loo[i] <- beta_obs - dfbeta_i
    s2_loo      <- (df * s2 - ei^2 / (1 - hi)) / df_loo
    var_inflate  <- XtXinv_X[idx, i]^2 / (1 - hi)
    var_beta_loo <- s2_loo * (XtXinv[idx, idx] + var_inflate)
    se_loo[i]   <- sqrt(max(var_beta_loo, .Machine$double.eps))
    t_loo[i]    <- beta_loo[i] / se_loo[i]
    k_loo[i]    <- rir_continuous(t_loo[i], n - 1, df_loo)
  }
  
  influence_total <- k_cf - k_loo
  
  k_mech     <- rir_continuous(t_obs, n - 1, df_loo)
  mechanical <- k_cf - k_mech
  
  t_beta_only <- beta_loo / se_obs
  k_beta_only <- vapply(t_beta_only, function(tb) {
    rir_continuous(tb, n - 1, df_loo)
  }, numeric(1))
  delta_beta <- k_mech - k_beta_only
  delta_se   <- k_beta_only - k_loo
  
  result <- data.frame(
    case       = seq_len(n),
    influence  = influence_total,
    delta_beta = delta_beta,
    delta_se   = delta_se,
    mechanical = mechanical,
    beta_loo   = beta_loo,
    se_loo     = se_loo,
    t_loo      = t_loo,
    k_cf_loo   = k_loo,
    stringsAsFactors = FALSE
  )
  
  attr(result, "k_cf")           <- k_cf
  attr(result, "t_obs")          <- t_obs
  attr(result, "beta_obs")       <- beta_obs
  attr(result, "se_obs")         <- se_obs
  attr(result, "mechanical")     <- mechanical
  attr(result, "mean_influence") <- mean(influence_total)
  attr(result, "n")              <- n
  attr(result, "focal_var")      <- focal_var
  
  class(result) <- c("rir_influence", "data.frame")
  result
}

# Generate distribution of k* using Direct/FWL method
# For each rep:
#  1. Generate a replacement scenario (permutation + replacement r_y values)
#  2. Binary search for k* where nullification first occurs
#  3. Store k*
#
# Input
# prep Prep object from prep_resid()
# reps Number of replications
# seed Random seed
# sign_flip_nullifies If TRUE, sign reversal counts as nullification
# progress Print progress
# Return: list with k_star vector and summaries
simulate_k_star_distribution <- function(prep, reps = 1000, seed = NULL,
                                         sign_flip_nullifies = TRUE,
                                         progress = TRUE) {
  
  if (!is.null(seed)) set.seed(seed)
  
  n <- prep$n
  t_start <- Sys.time()
  
  # Storage
  k_star <- integer(reps)
  converged <- logical(reps)
  iterations <- integer(reps)
  
  for (r in seq_len(reps)) {
    
    if (progress && r %% 100 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
      rate <- r / elapsed
      eta <- (reps - r) / rate
      message(sprintf("Rep %d/%d (%.1f reps/sec, ETA: %.0f sec)", r, reps, rate, eta))
    }
    
    # Generate replacement scenario
    perm <- sample.int(n)  # Random order of indices
    replacement_r_y <- sample(prep$r_y, n, replace = TRUE)  # Replacement values from full pool
    
    # Find k* for this scenario
    res <- find_k_star(prep, perm, replacement_r_y, sign_flip_nullifies)
    
    k_star[r] <- res$k_star
    converged[r] <- res$converged
    iterations[r] <- res$iterations
  }
  
  total_time <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  
  # Summaries
  list(
    k_star = k_star,
    converged = converged,
    iterations = iterations,
    
    k_cf = prep$k_cf,
    n = n,
    reps = reps,
    
    k_star_mean = mean(k_star),
    k_star_median = median(k_star),
    k_star_sd = sd(k_star),
    k_star_min = min(k_star),
    k_star_max = max(k_star),
    
    iterations_mean = mean(iterations),
    total_time_sec = total_time,
    seed = seed
  )
}


# Prepare ingredients for direct RIR simulation
# 
# Computes and caches:
#   - Residualized X (r_x): target variable regressed on covariates
#   - Residualized Y (r_y): outcome regressed on covariates  
#   - Reduced model predictions and residuals
#   - QR decomposition for fast refitting
# 
# Input:
# model Fitted lm object (must have model = TRUE)
# target_var Name of the focal predictor coefficient
# alpha Significance level for nullification decision
# Return: A prep list used by simulation functions
prep_resid <- function(model, target_var, alpha = 0.05) {
  
  
  # Validate inputs
  
  if (!inherits(model, "lm")) {
    stop("prep_resid() currently supports lm models only.", call. = FALSE)
  }
  if (inherits(model, "glm")) {
    stop("prep_resid() does not support glm models yet.", call. = FALSE)
  }
  if (is.null(model$model)) {
    stop("Model must be fit with model = TRUE.", call. = FALSE)
  }
  
  # Extract model frame (fixed data used throughout)
  mf <- model$model
  n <- nrow(mf)
  
  # Formula components
  f_full <- formula(model)
  y_name <- all.vars(f_full)[1]
  
  # Validate target_var
  if (!target_var %in% names(coef(model))) {
    stop("target_var '", target_var, "' not found in model coefficients.", call. = FALSE)
  }
  if (!target_var %in% names(mf)) {
    stop("target_var '", target_var, "' not found in model frame.", call. = FALSE)
  }
  
  # Reduced formula: remove target_var from RHS
  f_red <- update(f_full, paste(". ~ . -", target_var))
  
  # Fit reduced model (Y ~ Z, excluding target)
  mod_red <- lm(f_red, data = mf)
  pred_red <- fitted(mod_red)
  res_red <- residuals(mod_red)
  
  # Residualize target variable on covariates (X ~ Z)
  # If no covariates remain, just center X
  z_terms <- attr(terms(f_red), "term.labels")
  
  if (length(z_terms) == 0) {
    r_x <- mf[[target_var]] - mean(mf[[target_var]])
    r_y <- mf[[y_name]] - mean(mf[[y_name]])
  } else {
    # Regress X on Z
    f_x_on_z <- reformulate(z_terms, response = target_var)
    r_x <- residuals(lm(f_x_on_z, data = mf))
    
    # r_y is just res_red (Y regressed on Z)
    r_y <- res_red
  }
  
  # Original coefficient and test statistics
  summ <- summary(model)
  coef_table <- summ$coefficients
  
  est_orig <- coef(model)[[target_var]]
  se_orig <- coef_table[target_var, "Std. Error"]
  t_orig <- coef_table[target_var, "t value"]
  p_orig <- coef_table[target_var, "Pr(>|t|)"]
  df_resid <- model$df.residual
  
  # Critical value for two-sided test
  t_crit <- qt(1 - alpha / 2, df = df_resid)
  
  # Closed-form RIR (k_cf)
  # From: |t_new| < t_crit where t_new ≈ t_orig * (n - k) / n
  # Solving: k_cf = n * (1 - t_crit / |t_orig|)
  if (abs(t_orig) <= t_crit) {
    # Already non-significant
    k_cf <- 0L
  } else {
    prop_bias <- 1 - (t_crit / abs(t_orig))
    k_cf <- as.integer(round(prop_bias * n))
    k_cf <- max(0L, min(k_cf, n))
  }
  
  # Cache QR decomposition for fast refitting
  X_mat <- model.matrix(model)
  qr_X <- qr(X_mat)
  
  # Precompute (X'X)^{-1} diagonal element for target coefficient
  # Handle pivot in QR decomposition
  target_idx <- which(colnames(X_mat) == target_var)
  
  XtX_inv <- chol2inv(qr.R(qr_X))
  target_pos_in_pivot <- which(qr_X$pivot == target_idx)
  XtX_inv_target <- XtX_inv[target_pos_in_pivot, target_pos_in_pivot]
  
  # Return prep object
  list(
    # Model components
    model = model,
    data = mf,
    n = n,
    
    # Variable names
    target_var = target_var,
    y_name = y_name,
    
    # Formulas
    f_full = f_full,
    f_red = f_red,
    
    # Reduced model ingredients
    pred_red = as.numeric(pred_red),
    res_red = as.numeric(res_red),
    
    # Residualized variables
    r_x = as.numeric(r_x),
    r_y = as.numeric(r_y),
    
    # Original test results
    est_orig = est_orig,
    se_orig = se_orig,
    t_orig = t_orig,
    p_orig = p_orig,
    orig_sign = sign(est_orig),
    
    # Inference setup
    alpha = alpha,
    t_crit = t_crit,
    df_resid = df_resid,
    k_cf = k_cf,
    
    # Fast refit ingredients
    qr_X = qr_X,
    X_mat = X_mat,
    target_idx = target_idx,
    target_pos_in_pivot = target_pos_in_pivot,
    XtX_inv_target = XtX_inv_target
  )
}