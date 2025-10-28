# Closed-form correlation-based RIR for binary outcome models
# Works for continuous or binary focal; link name is recorded for context.
# Computes r-based RIR by default, and also t-based for comparison.
# Message text is generated inside and returned as $message. Optionally printed.

test_correlation_rir <- function(
        est_eff,            # coefficient estimate for the focal predictor
        std_err,            # standard error of the estimate
        n_obs,              # number of observations used in the model
        n_covariates,       # number of fitted coefficients including intercept
        alpha = 0.05,       # significance level
        tails = 2,          # 1 or 2
        #crit = c("t","z"),  # critical family for p and threshold
        nu   = 0,           # null value
        to_return = c("print", "raw_output")
) {
    # argument checks
    stopifnot(is.finite(est_eff),
              is.finite(std_err), std_err > 0,
              is.finite(n_obs), n_obs >= 2,
              is.finite(n_covariates), n_covariates >= 1,
              tails %in% c(1,2))
    
    digits = 3
    to_return <- match.arg(to_return)
    
    # helpers
    clamp01 <- function(x) pmax(0, pmin(1, x))
    r_from_t <- function(t, df) t / sqrt(t^2 + df)
    sf  <- function(x) signif(x, digits)
    pct <- function(x) round(100 * x, digits)
    
    # df, observed stats
    df    <- max(1L, as.integer(n_obs - n_covariates))
    t_obs <- (est_eff - nu) / std_err
    r_obs <- r_from_t(t_obs, df)
    
    # t critical and r critical
    t_crit <- stats::qt(1 - alpha / tails, df = df)
    r_crit <- r_from_t(t_crit, df)
    
    # p value on t
    p_obs <- if (tails == 2) 2 * stats::pt(-abs(t_obs), df = df) else 1 - stats::pt(abs(t_obs), df = df)
    is_sig <- p_obs < alpha
    
    # r policy fraction and count
    eps <- .Machine$double.eps
    if (is_sig) {
        # nullify
        pi_r <- clamp01(1 - abs(r_crit) / pmax(abs(r_obs), eps))
    } else {
        # sustain
        pi_r <- clamp01(1 - pmax(abs(r_obs), eps) / pmax(abs(r_crit), eps))
    }
    k_r <- ceiling(n_obs * pi_r)
    
    # t policy analogue for reference
    if (is_sig) {
        pi_t <- clamp01(1 - abs(t_crit) / pmax(abs(t_obs), eps))
    } else {
        pi_t <- clamp01(1 - pmax(abs(t_obs), eps) / pmax(abs(t_crit), eps))
    }
    k_t <- ceiling(n_obs * pi_t)
    
    # r-based message only
    action_clause <- if (is_sig) {
        "nullify the inference (lose statistical significance)"
    } else {
        "sustain an inference of an effect (reach statistical significance)"
    }
    
    # replacement phrase and interpretation sentence by scenario
    replacement_phrase <- if (is_sig) "zero-effect data points.\n" else "data points at the \nthreshold for statistical significance."
    interpretation_line <- if (is_sig) {
        "This RIR value represents the proportion of the data that would have to be replaced \nwith zero-effect data points for the observed relationship to lose statistical significance."
    } else {
        "This RIR value represents the proportion of the data that would have to be replaced \nwith data points at the threshold for statistical significance for the result to become \nstatistically significant."
    }
    
    thr_r  <- abs(r_crit)
    
    fmt <- paste0("%.", digits, "f")
    
    frac_text_r <- if (is_sig) {
        sprintf(
            "pi_r = 1 - |r_crit| / |r_obs|
    = 1 - %s / %s
    = %s.",
            sprintf(fmt, abs(r_crit)),
            sprintf(fmt, abs(r_obs)),
            sprintf(fmt, pi_r)
        )
    } else {
        sprintf(
            "pi_r = 1 - |r_obs| / |r_crit|
            = 1 - %s / %s
            = %s.",
            sprintf(fmt, abs(r_obs)),
            sprintf(fmt, abs(r_crit)),
            sprintf(fmt, pi_r)
        )
    }
    
    # This function calculates the number of data points that would need to be replaced vv
    # to nullify the inference to lose statistical significance in a binary outcome model. vv
    # Replacement is assumed to occur uniformly across the distribution of observations.
    # 
    message_text <- paste0(
        crayon::bold("Correlation-Based Robustness of Inference to Replacement (RIR):"), "\n",
        "This function calculates the number of data points that would need to be replaced\n",
        "to ", action_clause, " in a binary outcome model.\n",
        "Replacement is assumed to occur uniformly across the distribution of observations.\n\n",
        
        "The closed-form fraction on the correlation scale is\n  ", frac_text_r, "\n",
        "where:\n",
        "  pi_r = replacement fraction on the correlation scale (proportion of data that must be replaced);\n",
        "  r_obs = observed absolute value of correlation implied by the test statistic (df = ", df, "):\n",
        "          r_obs = t_obs / sqrt(t_obs^2 + df) = ", sprintf(fmt, abs(r_obs)), ";\n",
        "  r_crit = critical correlation derived from the critical t-statistic at alpha = ", alpha, " (df = ", df, "):\n",
        "           r_crit = t_crit / sqrt(t_crit^2 + df) = ", sprintf(fmt, abs(r_crit)), ".\n\n",
        
        "Using a threshold of ", sf(thr_r), " for statistical significance (alpha = ", alpha, "),\n",
        sprintf(fmt, pct(pi_r)), "% of the observed estimate of ", sf(est_eff),
        if (is_sig) " would have to be due to bias.\n" else " would need to be strengthened by replacement.\n",
        "This implies replacing ", k_r, " of ", n_obs, " observations (",
        sprintf(fmt, pct(pi_r)), "%) with ", replacement_phrase,
        "Thus, RIR = ", k_r, ".\n\n",
        interpretation_line
    )
    
    out <- list(
        input = list(est_eff = est_eff, std_err = std_err, n_obs = n_obs,
                     n_covariates = n_covariates, alpha = alpha, tails = tails, nu = nu),
        df = df,
        t_obs = t_obs,
        r_obs = r_obs,
        t_crit = t_crit,
        r_crit = r_crit,
        p_obs = p_obs,
        is_sig = is_sig,
        # r policy
        pi_r = pi_r, k_r = k_r,
        # t policy for reference
        pi_t = pi_t, k_t = k_t,
        message = message_text
    )
    
    # Raw concise output branch
    raw_out <- list(
        observed_t = t_obs,
        critical_t = t_crit,
        observed_r = r_obs,
        critical_r = r_crit,
        p_value = p_obs,
        RIR_perc = pi_r,
        RIR = k_r
    )
    
    # Dispatch
    if (to_return == "print") {
        cat(message_text, "\n")
        invisible(out)
    } else if (to_return == "raw_output") { 
        return(raw_out)
    }
}

## Example
# Significant case
# ex1 <- test_correlation_rir(
#     est_eff = 0.30, std_err = 0.015,
#     n_obs = 5000, n_covariates = 10,
#     alpha = 0.05, tails = 2,
#     to_return = "print"
# )
# 
# # Significant case
# test_correlation_rir(
#     est_eff = 0.30, std_err = 0.015,
#     n_obs = 5000, n_covariates = 10,
#     alpha = 0.05, tails = 2,
#     to_return = "raw_output"
# )
# 
# # Non-significant case
# ex2 <- test_correlation_rir(
#     est_eff = 0.03, std_err = 0.05,
#     n_obs = 1200, n_covariates = 8,
#     alpha = 0.05, tails = 2
# )


# pkonfound(est_eff = 0.03, std_err = 0.05,
#           n_obs = 1200, n_covariates = 8,
#           alpha = 0.05, tails = 2,
#           index = "corr_RIR",
#           to_return = "print")
