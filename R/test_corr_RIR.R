## test_corr_RIR.R
## Robustness of Inference to Replacement (RIR) — t-scale and r-scale
## Unified handler for pkonfound(index = "RIR", scale = "t"/"r")
##
## WHAT THIS FUNCTION DOES:
##   Computes the fraction of observations that would need to be replaced
##   to nullify or sustain an inference, expressed on either the
##   t-statistic scale (scale = "t", LM only) or the correlation scale
##   (scale = "r", LM or GLM).
##
## SCALE CHOICE:
##   scale = "t" (default): pi_t = 1 - |t_crit|/|t_obs|. Algebraically
##     identical to the beta-metric RIR (ratio cancels via t = beta/SE).
##     Assumes constant SE across replacement. Available for LM only.
##     To relax the constant-SE assumption, use scale = "r".
##   scale = "r": converts the test statistic to a correlation via
##     r = stat / sqrt(stat^2 + df), then computes pi_r = 1 - |r_crit|/|r_obs|.
##     Accounts for SE inflation due to replacement. Supports LM and GLM.
##
## TODO: rename to test_sensitivity_rir.R / test_sensitivity_rir() once
##   the RIR refactor is complete and corr_RIR is fully retired.

test_correlation_rir <- function(
        est_eff,            # coefficient estimate for the focal predictor
        std_err,            # standard error of the estimate
        n_obs,              # number of observations used in the model
        n_covariates,       # number of covariates (excluding intercept)
        alpha = 0.05,       # significance level
        tails = 2,          # 1 or 2
        nu   = 0,           # null value (non-zero supported for both LM/GLM)
        scale = "t",        # "t" (default, LM only) or "r" (LM/GLM)
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
    
    # scale validation and GLM gate
    scale <- match.arg(scale, choices = c("t", "r"))
    if (scale == "t" && model_type == "glm") {
        stop("scale = 't' is only available for linear models (model_type = 'lm'). ",
             "For GLM, use scale = 'r' (z-based).")
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
        df <- max(1L, as.integer(n_obs - n_covariates - 2)) 
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
        df <- max(1L, as.integer(n_obs - n_covariates - 2))
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
        paste0("data points for which the effect is ", nu, ".")
    } else {
        paste0("data points at\nthe threshold for statistical significance.")
    }
    
    interpretation_line <- if (is_sig) {
        paste0(
            "That is, the RIR value represents the proportion of the data that\n",
            "would have to be replaced with data points for which the effect is ",
            nu, "\n",
            "for the observed relationship to lose statistical significance.")
    } else {
        paste0(
            "This RIR value represents the proportion of the data that would\n",
            "have to be replaced with data points at the threshold for\n",
            "statistical significance for the result to become statistically\n",
            "significant.")
    }
    
    thr_r <- abs(r_crit)
    
    # model-type description (shared)
    model_desc <- if (model_type == "lm") "a linear model (OLS)" else
        sprintf("a generalized linear model (GLM, %s link)", link)
    
    if (scale == "t") {
        # ── t-scale path ────────────────────────────────────────────────────
        # Primary metric: pi_stat / k_stat (t-based, identical to beta-metric RIR)
        frac_text <- if (is_sig) {
            sprintf(
                "pi_t = (|t_obs| - |t_crit|) / |t_obs|\n     = (%s - %s) / %s\n     = %s.",
                sprintf(fmt, abs(stat_obs)), sprintf(fmt, abs(stat_crit)),
                sprintf(fmt, abs(stat_obs)), sprintf(fmt, pi_stat))
        } else {
            sprintf(
                "pi_t = (|t_crit| - |t_obs|) / |t_crit|\n     = (%s - %s) / %s\n     = %s.",
                sprintf(fmt, abs(stat_crit)), sprintf(fmt, abs(stat_obs)),
                sprintf(fmt, abs(stat_crit)), sprintf(fmt, pi_stat))
        }
        stat_detail <- sprintf(
            "  t_obs = observed t-statistic (df = %d):\n          t_obs = (est_eff - nu) / std_err = %s;",
            df, sprintf(fmt, stat_obs))
        crit_detail <- sprintf(
            "  t_crit = critical t-value at alpha = %s (t-distribution, df = %d):\n           t_crit = %s.",
            alpha, df, sprintf(fmt, stat_crit))
        thr_val    <- abs(stat_crit)
        pi_primary <- pi_stat
        k_primary  <- k_stat
        pi_label   <- "pi_t"
        z_ref_text <- ""
        
    } else {
        # ── r-scale path ────────────────────────────────────────────────────
        # Primary metric: pi_r / k_r (correlation scale)
        frac_text <- if (is_sig) {
            sprintf(
                "pi_r = (|r_obs| - |r_crit|) / |r_obs|\n     = (%s - %s) / %s\n     = %s.",
                sprintf(fmt, abs(r_obs)), sprintf(fmt, abs(r_crit)),
                sprintf(fmt, abs(r_obs)), sprintf(fmt, pi_r))
        } else {
            sprintf(
                "pi_r = (|r_crit| - |r_obs|) / |r_crit|\n     = (%s - %s) / %s\n     = %s.",
                sprintf(fmt, abs(r_crit)), sprintf(fmt, abs(r_obs)),
                sprintf(fmt, abs(r_crit)), sprintf(fmt, pi_r))
        }
        if (model_type == "lm") {
            stat_detail <- sprintf(
                "  r_obs = observed partial correlation implied by %s (df = %d):\n          r_obs = %s_obs / sqrt(%s_obs^2 + df) = %s;",
                stat_label, df, stat_label, stat_label, sprintf(fmt, abs(r_obs)))
            crit_detail <- sprintf(
                "  r_crit = critical partial correlation at alpha = %s (%s-distribution, df = %d):\n           r_crit = %s_crit / sqrt(%s_crit^2 + df) = %s.",
                alpha, stat_label, df, stat_label, stat_label, sprintf(fmt, abs(r_crit)))
        } else {
            stat_detail <- sprintf(
                "  r_obs = observed %s implied by the Wald %s-statistic (eff. df = %d):\n          r_obs = %s_obs / sqrt(%s_obs^2 + df) = %s;",
                r_interp, stat_label, df, stat_label, stat_label, sprintf(fmt, abs(r_obs)))
            crit_detail <- sprintf(
                "  r_crit = critical %s at alpha = %s (standard normal):\n           r_crit = %s_crit / sqrt(%s_crit^2 + df) = %s.",
                r_interp, alpha, stat_label, stat_label, sprintf(fmt, abs(r_crit)))
        }
        thr_val    <- abs(r_crit)
        pi_primary <- pi_r
        k_primary  <- k_r
        pi_label   <- "pi_r"
        # z-based reference for GLM only (conversion-quality diagnostic)
        if (model_type == "glm") {
            z_ref_text <- paste0(
                "\n\n",
                crayon::bold(
                    "z-Based Reference (conversion-quality diagnostic):"), "\n",
                "For GLM, the correlation-equivalent index r is derived from\n",
                "the Wald z via a nonlinear mapping that depends on effective\n",
                "degrees of freedom (df = ", df, ").\n",
                "The z-based replacement fraction is free of this dependency:\n",
                "  pi_z = 1 - |z_crit| / |z_obs| = ",
                sprintf(fmt, pi_stat), ",  RIR_z = ", k_stat, ".\n",
                "The r-based and z-based fractions converge for large samples.\n",
                "A large gap may indicate that the r-scale compression is\n",
                "substantively affecting the RIR estimate and that the Wald z\n",
                "may also be unreliable at this sample size, affecting the\n",
                "interpretation of both pi_z and pi_r."
            )
        } else {
            z_ref_text <- ""
        }
    }
    
    # intro paragraph (shared; model_type-conditional only)
    if (model_type == "lm") {
        intro_text <- paste0(
            "This function calculates the number of data points that would need\n",
            "to be replaced to ", action_clause, ",\n",
            "based on ", model_desc, ". Replacement is assumed to occur\n",
            "uniformly across the distribution of observations.\n\n"
        )
    } else {
        intro_text <- paste0(
            "This function calculates the number of data points that would need\n",
            "to be replaced to ", action_clause, ",\n",
            "based on a generalized linear model (GLM, ", link, " link).\n",
            "Replacement is assumed to occur uniformly across the\n",
            "distribution of observations.\n\n"
        )
    }
    
    # title (scale-dependent)
    title_text <- if (scale == "t") {
        crayon::bold("Robustness of Inference to Replacement (RIR):")
    } else {
        crayon::bold("Correlation-Based Robustness of Inference to Replacement (RIR):")
    }
    
    # scale note and citation (appended after interpretation_line)
    scale_note <- if (scale == "t") {
        paste0(
            "\n\nNote: This RIR is computed via the t-statistic, which assumes\n",
            "a constant standard error (SE) across replacement. For an RIR\n",
            "that accounts for SE inflation due to replacement, use scale = \"r\".")
    } else if (model_type == "glm") {
        paste0(
            "\n\nNote: This RIR is computed by converting the Wald z-statistic\n",
            "to the correlation scale. It accounts for SE inflation due to\n",
            "replacement. The z-based reference above provides the unconverted\n",
            "RIR for comparison.")
    } else {
        paste0(
            "\n\nNote: This RIR is computed via the correlation scale and does\n",
            "not assume a constant standard error (SE). It accounts for SE\n",
            "inflation due to replacement. For an RIR that assumes a constant\n",
            "SE across replacement, use scale = \"t\".")
    }
    
    cite_text <- paste0(
        "\n\nSee Frank et al. (2013) for a description of the method.\n\n",
        crayon::underline("Citation:"), "\n",
        "Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. (2013).\n",
        "What would it take to change an inference? Using Rubin's causal\n",
        "model to interpret the robustness of causal inferences. ",
        crayon::italic("Educational\nEvaluation and Policy Analysis, 35"),
        "(4), 437-460."
    )
    
    # assemble the full message
    # Structure: title -> model context -> formula -> where-block ->
    #   plain-language summary -> interpretation ->
    #   [GLM z-ref if scale="r" GLM] -> scale note -> citation
    message_text <- paste0(
        title_text, "\n",
        intro_text,
        "The closed-form fraction on the ",
        if (scale == "t") "t-statistic" else "correlation",
        " scale is\n  ", frac_text, "\n",
        "where:\n",
        "  ", pi_label, " = replacement fraction on the ",
        if (scale == "t") "t-statistic" else "correlation", " scale;\n",
        stat_detail, "\n",
        crit_detail, "\n\n",
        "Using a threshold of ", sf(thr_val),
        " for statistical significance (alpha = ", alpha, "),\n",
        sprintf(fmt, pct(pi_primary)),
        "% of the observed estimate of ", sf(est_eff),
        if (is_sig) " would have to be due to bias\n"
        else " would need to be strengthened by replacement.\n",
        if (is_sig) "to nullify the inference. " else "",
        "This implies replacing ", k_primary, " of ", n_obs, " observations",
        if (is_sig) "\n(" else " (",
        sprintf(fmt, pct(pi_primary)), "%) with ", replacement_phrase,
        " Thus, RIR = ", k_primary, ".\n\n",
        interpretation_line,
        z_ref_text,
        scale_note,
        cite_text
    )
    
    
    
    ## Full output list (returned invisibly on print)
    out <- c(
        list(
            input = list(est_eff = est_eff, std_err = std_err, n_obs = n_obs,
                         n_covariates = n_covariates, alpha = alpha, tails = tails,
                         nu = nu, scale = scale, model_type = model_type, link = link),
            df        = df,
            stat_type = stat_label
        ),
        structure(list(stat_obs, stat_crit),
                  names = c(paste0(stat_label, "_obs"),
                            paste0(stat_label, "_crit"))),
        list(
            r_obs  = if (scale == "r") r_obs  else NA,
            r_crit = if (scale == "r") r_crit else NA,
            p_obs  = p_obs,
            is_sig = is_sig,
            # primary RIR (scale-dependent)
            pi_primary = pi_primary, k_primary = k_primary,
            # t-based (always computed; primary for scale="t", reference for scale="r")
            pi_stat = pi_stat, k_stat = k_stat,
            # r-based (scale="r" only)
            pi_r = if (scale == "r") pi_r else NA,
            k_r  = if (scale == "r") k_r  else NA,
            # z-based reference (GLM, scale="r" only; conversion-quality diagnostic)
            pi_z = if (scale == "r" && model_type == "glm") pi_stat else NA,
            k_z  = if (scale == "r" && model_type == "glm") k_stat  else NA,
            message = message_text
        )
    )
    
    
    ## Raw concise output branch 
    # stat field names are dynamic: observed_t/critical_t for LM, observed_z/critical_z for GLM
    raw_out <- c(
        list(
            scale      = scale,
            model_type = model_type,
            link       = link,
            stat_type  = stat_label
        ),
        structure(list(stat_obs,  stat_crit),
                  names = c(paste0("observed_", stat_label),
                            paste0("critical_", stat_label))),
        list(
            observed_r = if (scale == "r") r_obs  else NA,
            critical_r = if (scale == "r") r_crit else NA,
            p_value    = p_obs,
            RIR_perc   = pi_primary,
            RIR        = k_primary,
            # z-based reference (GLM, scale="r" only; conversion-quality diagnostic)
            RIR_z_perc = if (scale == "r" && model_type == "glm") pi_stat else NA,
            RIR_z      = if (scale == "r" && model_type == "glm") k_stat  else NA
        )
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
# LM, t-scale (default):
# pkonfound(est_eff = 0.03, std_err = 0.05,
#           n_obs = 1200, n_covariates = 8,
#           index = "RIR", to_return = "print")

# LM, r-scale (relaxed constant-SE assumption):
# pkonfound(est_eff = 0.03, std_err = 0.05,
#           n_obs = 1200, n_covariates = 8,
#           index = "RIR", scale = "r", to_return = "print")

# GLM logistic, r-scale:
# pkonfound(est_eff = -0.20, std_err = 0.103,
#           n_obs = 20888, n_covariates = 3,
#           index = "RIR", scale = "r", link = "logit",
#           to_return = "print")

# GLM probit, r-scale:
# pkonfound(est_eff = 0.05, std_err = 0.08,
#           n_obs = 500, n_covariates = 5,
#           index = "RIR", scale = "r", link = "probit",
#           to_return = "print")