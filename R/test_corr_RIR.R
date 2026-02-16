## test_corr_RIR.R
## Correlation-based Robustness of Inference to Replacement (RIR)
## Unified framework for LM (t-based) and GLM (z-based, logistic/probit)
##
## WHAT THIS FUNCTION DOES:
##   Converts a regression coefficient and its SE into a correlation-scale
##   effect size (r_obs), compares it to a critical correlation (r_crit),
##   and computes the fraction of observations (pi_r) that would need to be
##   replaced to nullify or sustain the inference.
##
## WHY THIS WORKS FOR BOTH LM AND GLM:
##   The RIR fraction pi_r = 1 - |r_crit|/|r_obs| depends only on the
##   monotonic mapping from the test statistic to [0,1]. The mapping
##   r = stat / sqrt(stat^2 + df) is valid for any Wald-type statistic;
##   it yields a partial correlation for LM and a normalized effect-size
##   index for GLM. The fraction pi_r is invariant to which monotonic
##   scale we use — only the ratio matters.

test_correlation_rir <- function(
        est_eff,            # coefficient estimate for the focal predictor
        std_err,            # standard error of the estimate
        n_obs,              # number of observations used in the model
        n_covariates,       # number of covariates (excluding intercept)
        alpha = 0.05,       # significance level
        tails = 2,          # 1 or 2
        nu   = 0,           # null value (non-zero supported for both LM/GLM)
        model_type = "lm",  # "lm" for linear models, "glm" for generalized
        link = NULL,        # NULL for LM; "logit" or "probit" for GLM
        to_return = c("print", "raw_output")
) {
    ## Argument validation 
    ## If model_type = "lm" and link is non-NULL, we warn and ignore link.
    
    # argument checks
    stopifnot(is.finite(est_eff),
              is.finite(std_err), std_err > 0,
              is.finite(n_obs), n_obs >= 2,
              is.finite(n_covariates), n_covariates >= 0,
              tails %in% c(1, 2))
    
    # model_type validation
    model_type <- match.arg(model_type, choices = c("lm", "glm"))
    
    # link validation: required for GLM, ignored for LM
    if (model_type == "glm") {
        if (is.null(link)) {
            stop("For model_type = 'glm', you must specify link = 'logit' or 'probit'.")
        }
        link <- match.arg(link, choices = c("logit", "probit"))
    }
    if (model_type == "lm" && !is.null(link)) {
        warning("link is ignored when model_type = 'lm'. Using t-distribution.")
        link <- NULL
    }
    
    digits <- 3
    to_return <- match.arg(to_return)
    
    ## Helper functions 
    clamp01 <- function(x) pmax(0, pmin(1, x)) # bound x to [0, 1]
    r_from_stat <- function(stat, df) stat / sqrt(stat^2 + df)  # t or z -> r
    sf  <- function(x) signif(x, digits) # round to significant digits
    pct <- function(x) round(100 * x, digits) # proportion -> percentage
    fmt <- paste0("%.", digits, "f") # format string for sprintf
    

    ## Degrees of freedom and test statistic 
    ## LM uses t; GLM uses z.
    ##
    ## For LM:
    ##   df = n - n_cov - 1  (residual df; subtract 1 for intercept)
    ##   The test statistic follows a t-distribution with these df.
    ##   r_obs is the partial correlation of X and Y given Z.
    ##
    ## For GLM:
    ##   We use the Wald z-statistic, which is asymptotically N(0,1).
    ##   The critical value comes from qnorm, NOT qt.
    ##   For the r conversion, we need an effective df to map z onto [0,1].
    ##   We use df_eff = n - p, where p = n_cov + 1 (total parameters
    ##   including intercept). This choice:
    ##     (a) parallels the LM residual df,
    ##     (b) ensures r shrinks toward 0 for large p relative to n,
    ##     (c) is the conventional choice in the meta-analysis literature
    ##         (e.g., Rosenthal & Rubin, 2003) for converting z to r.
    ##   Note that the resulting r is NOT a partial correlation in the GLM case;
    ##   it is a normalized effect-size index on [0,1] that enables the
    ##   closed-form RIR calculation.
    if (model_type == "lm") {
        # LM pathway: t-distribution
        df <- max(1L, as.integer(n_obs - n_covariates - 1)) # -1 accounts for the intercept
        stat_obs <- (est_eff - nu) / std_err # observed t
        stat_crit <- stats::qt(1 - alpha / tails, df = df) # critical t (positive)
        
        # p-value from t-distribution
        p_obs <- if (tails == 2) {
            2 * stats::pt(-abs(stat_obs), df = df)
        } else {
            stats::pt(-abs(stat_obs), df = df)
        }
        
        # label for output
        stat_label <- "t"
        r_interp   <- "partial correlation"
        
    } else {
        # GLM pathway: Wald z (normal approximation)
        # Effective df for the r conversion (see rationale above).
        # This does NOT affect the critical value or p-value, which use
        # the standard normal. It only controls the r = z/sqrt(z^2 + df)
        # mapping so that r remains bounded in [0,1] and comparable to
        # the LM case.
        df <- max(1L, as.integer(n_obs - n_covariates - 1))
        stat_obs <- (est_eff - nu) / std_err # observed z (Wald)
        stat_crit <- stats::qnorm(1 - alpha / tails) # critical z (positive)
        
        # p-value from normal distribution
        p_obs <- if (tails == 2) {
            2 * stats::pnorm(-abs(stat_obs))
        } else {
            stats::pnorm(-abs(stat_obs))
        }
        
        # label for output
        stat_label <- "z"
        r_interp   <- "correlation-equivalent index"
    }
    
    
    ## Convert to correlation scale
    r_obs  <- r_from_stat(stat_obs, df)
    r_crit <- r_from_stat(stat_crit, df)
    
    is_sig <- p_obs < alpha
    
    
    ## RIR fraction and count
    ## because r_from_stat is monotonic, pi_r = 1 - |r_crit|/|r_obs| 
    ## gives the same replacement fraction regardless of whether the
    ## underlying stat is t or z.
    eps <- .Machine$double.eps # machine epsilon; division-by-zero guard
    if (is_sig) {
        # nullify: how much of r_obs must be erased to reach r_crit
        pi_r <- clamp01(1 - abs(r_crit) / pmax(abs(r_obs), eps))
    } else {
        # sustain: how much must be boosted from r_obs to reach r_crit
        pi_r <- clamp01(1 - pmax(abs(r_obs), eps) / pmax(abs(r_crit), eps))
    }
    k_r <- ceiling(n_obs * pi_r)
    
    
    ## Raw-statistic-based RIR (diagnostic reference)
    ## This computes pi on the original test-statistic scale (t for LM, z for GLM),
    ## WITHOUT the r-conversion. It is retained alongside the r-based RIR for two
    ## purposes:
    ##
    ## For LM:  pi_stat (t-based) is equivalent to pi_beta (the beta-metric RIR)
    ##          because t = beta/SE and critical_t = beta_thr/SE, so the ratio
    ##          cancels identically. It serves as a cross-check.
    ##
    ## For GLM: pi_stat (z-based) is free of the effective-df dependency that the
    ##          r-conversion introduces. The z-to-r mapping r = z/sqrt(z^2 + df)
    ##          is nonlinear, which means pi_r depends on df even though pi_z does
    ##          not. The gap |pi_r - pi_z| therefore serves as a diagnostic for
    ##          how much the r-scale compression affects the RIR estimate.
    ##          - Large samples (n >> p): gap should be negligible, r-based RIR is safe.
    ##          - Small samples with large effects: gap may be substantive,
    ##            signaling that (a) the r-conversion is compressing the fraction,
    ##            and (b) the Wald z itself may be unreliable at this sample size.
    ##
    ## In the print output, we report pi_z / RIR_z alongside pi_r / RIR for GLM
    ## so the user can assess conversion quality.
    
    if (is_sig) {
        pi_stat <- clamp01(1 - abs(stat_crit) / pmax(abs(stat_obs), eps))
    } else {
        pi_stat <- clamp01(1 - pmax(abs(stat_obs), eps) / pmax(abs(stat_crit), eps))
    }
    k_stat <- ceiling(n_obs * pi_stat)
    
    
    ## Build print message
    action_clause <- if (is_sig) {
        "nullify the inference (lose statistical significance)"
    } else {
        "sustain an inference of an effect (reach statistical significance)"
    }
    
    replacement_phrase <- if (is_sig) {
        "zero-effect data points.\n"
    } else {
        "data points at the\nthreshold for statistical significance."
    }
    
    interpretation_line <- if (is_sig) {
        paste0(
            "This RIR value represents the proportion of the data that would have to be\n",
            "replaced with zero-effect data points for the observed relationship to lose\n",
            "statistical significance.")
    } else {
        paste0(
            "This RIR value represents the proportion of the data that would have to be\n",
            "replaced with data points at the threshold for statistical significance for\n",
            "the result to become statistically significant.")
    }
    
    thr_r <- abs(r_crit)
    
    # model-type description for the header 
    if (model_type == "lm") {
        model_desc <- "a linear model (OLS)"
        stat_detail <- sprintf(
            "  r_obs = observed partial correlation implied by %s (df = %d):\n          r_obs = %s_obs / sqrt(%s_obs^2 + df) = %s;",
            stat_label, df, stat_label, stat_label, sprintf(fmt, abs(r_obs)))
        crit_detail <- sprintf(
            "  r_crit = critical partial correlation at alpha = %s (%s-distribution, df = %d):\n           r_crit = %s_crit / sqrt(%s_crit^2 + df) = %s.",
            alpha, stat_label, df, stat_label, stat_label, sprintf(fmt, abs(r_crit)))
    } else {
        model_desc <- sprintf("a generalized linear model (GLM, %s link)", link)
        stat_detail <- sprintf(
            "  r_obs = observed %s implied by the Wald %s-statistic (eff. df = %d):\n          r_obs = %s_obs / sqrt(%s_obs^2 + df) = %s;",
            r_interp, stat_label, df, stat_label, stat_label, sprintf(fmt, abs(r_obs)))
        crit_detail <- sprintf(
            "  r_crit = critical %s at alpha = %s (standard normal):\n           r_crit = %s_crit / sqrt(%s_crit^2 + df) = %s.",
            r_interp, alpha, stat_label, stat_label, sprintf(fmt, abs(r_crit)))
    }
    
    # pi_r formula display (same logic as before, cleaner formatting)
    frac_text_r <- if (is_sig) {
        sprintf(
            "pi_r = 1 - |r_crit| / |r_obs|\n     = 1 - %s / %s\n     = %s.",
            sprintf(fmt, abs(r_crit)),
            sprintf(fmt, abs(r_obs)),
            sprintf(fmt, pi_r))
    } else {
        sprintf(
            "pi_r = 1 - |r_obs| / |r_crit|\n     = 1 - %s / %s\n     = %s.",
            sprintf(fmt, abs(r_obs)),
            sprintf(fmt, abs(r_crit)),
            sprintf(fmt, pi_r))
    }
    
    # z-based reference for GLM (conversion-quality diagnostic)
    # For GLM only, we append a reference line showing the z-based RIR
    # (which is free of the effective-df dependency) so users can assess how
    # much the r-scale compression affects their estimate.
    # For LM, this block produces an empty string (not shown in output).
    if (model_type == "glm") {
        z_ref_text <- paste0(
            "\n\n",
            crayon::bold("z-Based Reference (conversion-quality diagnostic):"), "\n",
            "For GLM, the correlation-equivalent index r is derived from the Wald z via a\n",
            "nonlinear mapping that depends on effective degrees of freedom (df = ", df, ").\n",
            "The z-based replacement fraction is free of this dependency:\n",
            "  pi_z = 1 - |z_crit| / |z_obs| = ", sprintf(fmt, pi_stat), ",  ",
            "RIR_z = ", k_stat, ".\n",
            "The r-based and z-based fractions converge for large samples. A large gap\n",
            "may indicate that the r-scale compression is substantively affecting the\n",
            "RIR estimate, and that the Wald z may also be unreliable at this sample size."
        )
    } else {
        z_ref_text <- ""
    }
    
    # assemble the full message
    # Structure: title -> model context -> formula -> where-block ->
    # plain-language summary -> interpretation -> [GLM z-ref if applicable]
    message_text <- paste0(
        crayon::bold("Correlation-Based Robustness of Inference to Replacement (RIR):"), "\n",
        "This function calculates the number of data points that would need to be replaced\n",
        "to ", action_clause, " based on ", model_desc, ".\n",
        "Replacement is assumed to occur uniformly across the distribution of observations.\n\n",
        
        "The closed-form fraction on the correlation scale is\n  ", frac_text_r, "\n",
        "where:\n",
        "  pi_r = replacement fraction on the correlation scale;\n",
        stat_detail, "\n",
        crit_detail, "\n\n",
        
        "Using a threshold of ", sf(thr_r), " for statistical significance (alpha = ", alpha, "),\n",
        sprintf(fmt, pct(pi_r)), "% of the observed estimate of ", sf(est_eff),
        if (is_sig) " would have to be due to bias.\n" else " would need to be strengthened by replacement.\n",
        "This implies replacing ", k_r, " of ", n_obs, " observations (",
        sprintf(fmt, pct(pi_r)), "%) with ", replacement_phrase,
        "Thus, RIR = ", k_r, ".\n\n",
        interpretation_line,
        z_ref_text  
    )
    
    
    ## Full output list (returned invisibly on print)
    out <- list(
        input = list(est_eff = est_eff, std_err = std_err, n_obs = n_obs,
                     n_covariates = n_covariates, alpha = alpha, tails = tails,
                     nu = nu, model_type = model_type, link = link),
        df = df,
        stat_type  = stat_label,
        stat_obs   = stat_obs, 
        stat_crit  = stat_crit,  
        r_obs  = r_obs,
        r_crit = r_crit,
        p_obs  = p_obs,
        is_sig = is_sig,
        # r-based RIR (primary)
        pi_r = pi_r, k_r = k_r,
        # raw-statistic-based RIR (reference)
        pi_stat = pi_stat, k_stat = k_stat,
        # z-based reference (GLM only; conversion-quality diagnostic)
        pi_z = if (model_type == "glm") pi_stat else NA,
        k_z = if (model_type == "glm") k_stat else NA,
        message = message_text
    )
    
    
    ## Raw concise output branch 
    raw_out <- list(
        model_type = model_type,    # [NEW]
        link = link,          # [NEW] NULL for LM
        stat_type = stat_label,    # [NEW] "t" or "z"
        observed_stat = stat_obs,      # [CHANGE] was observed_t
        critical_stat = stat_crit,     # [CHANGE] was critical_t
        observed_r = r_obs,
        critical_r = r_crit,
        p_value = p_obs,
        RIR_perc = pi_r,
        RIR = k_r,
        # z-based reference (GLM conversion-quality diagnostic)
        RIR_z_perc = if (model_type == "glm") pi_stat else NA,
        RIR_z = if (model_type == "glm") k_stat  else NA

    )
    
    
    ## Dispatch (print and raw_output)
    if (to_return == "print") {
        cat(message_text, "\n")
    } else if (to_return == "raw_output") {
        return(raw_out)
    }
}


## ==========================================================================
## EXAMPLES
## ==========================================================================

## LM examples (t-based, default)
# Significant LM case
# ex_lm1 <- test_correlation_rir(
#     est_eff = 0.30, std_err = 0.015,
#     n_obs = 5000, n_covariates = 10,
#     alpha = 0.05, tails = 2,
#     to_return = "print"
# )

# Non-significant LM case
# ex_lm2 <- test_correlation_rir(
#     est_eff = 0.03, std_err = 0.05,
#     n_obs = 1200, n_covariates = 8,
#     alpha = 0.05, tails = 2,
#     to_return = "print"
# )

## GLM examples (z-based)
# Logistic regression, significant
# ex_glm1 <- test_correlation_rir(
#     est_eff = -0.20, std_err = 0.103,
#     n_obs = 20888, n_covariates = 3,
#     alpha = 0.05, tails = 2,
#     model_type = "glm", link = "logit",
#     to_return = "print"
# )

# Probit regression, non-significant
# ex_glm2 <- test_correlation_rir(
#     est_eff = 0.05, std_err = 0.08,
#     n_obs = 500, n_covariates = 5,
#     alpha = 0.05, tails = 2,
#     model_type = "glm", link = "probit",
#     to_return = "print"
# )

# Raw output for GLM
# ex_glm3 <- test_correlation_rir(
#     est_eff = -0.20, std_err = 0.103,
#     n_obs = 20888, n_covariates = 3,
#     model_type = "glm", link = "logit",
#     to_return = "raw_output"
# )

## nu != 0 example (works for both LM and GLM) 
# Testing whether LM coefficient differs from 0.5
# ex_nu <- test_correlation_rir(
#     est_eff = 0.80, std_err = 0.15,
#     n_obs = 200, n_covariates = 4,
#     nu = 0.5,
#     to_return = "print"
# )

## pkonfound integration examples
# LM (default):
# pkonfound(est_eff = 0.03, std_err = 0.05,
#           n_obs = 1200, n_covariates = 8,
#           index = "corr_RIR", to_return = "print")

# GLM logistic:
# pkonfound(est_eff = -0.20, std_err = 0.103,
#           n_obs = 20888, n_covariates = 3,
#           index = "corr_RIR", link = "logit",
#           to_return = "print")

# GLM probit:
# pkonfound(est_eff = 0.05, std_err = 0.08,
#           n_obs = 500, n_covariates = 5,
#           index = "corr_RIR", link = "probit",
#           to_return = "print")
