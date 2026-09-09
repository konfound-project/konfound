## test_rir_glm.R
## Robustness of Inference to Replacement (RIR) for generalized linear
## models, computed on the average marginal effect (AME) scale.
## Handler for konfound(index = "RIR") when model_object is a binomial
## glm with a logit or probit link.
##
## WHAT THIS FUNCTION DOES:
##   Takes a fitted glm object, computes the AME of the tested variable
##   and its delta-method standard error, and then computes the fraction
##   (and count) of observations that would need to be replaced to
##   nullify or sustain the inference on the AME.
##
## WHY THE AME SCALE:
##   The RIR argument requires that the pooled effect after replacement
##   equal (1 - pi) times the effect among the kept cases. That holds
##   for an OLS coefficient and it holds for any per-case average, but
##   it does not hold for a log-odds or probit coefficient, which is a
##   single model parameter rather than an average over cases. The AME
##   is the mean over cases of each case's own marginal effect, so
##   proportional mixing under replacement holds by definition. No
##   scale conversion and no effective-df choice are needed.
##
## ASSUMPTIONS (inherited from the linear-model RIR):
##   (a) Replaced cases are a representative slice of the sample, so the
##       mean effect among kept cases equals the original AME.
##   (b) Replaced cases share the distribution of predictor and covariate
##       values with the cases they replace.
##   (c) Individual marginal effects of kept cases are held at their
##       estimated values rather than re-estimated after replacement.
##   The threshold uses the observed SE of the AME (constant-SE
##   assumption, as in the t-scale linear RIR). The SE itself is a
##   delta-method (first-order) quantity and the test is a Wald z.
##
## AME TYPE:
##   "derivative": mean over cases of d p_i / d x. For logit this is
##       beta_j * mean(p_i (1 - p_i)); for probit, beta_j * mean(phi(eta_i)).
##       Used for continuous predictors.
##   "difference": mean over cases of p_i(x = 1) - p_i(x = 0), holding each
##       case's covariates at their observed values. Used for 0/1 predictors.
##   "auto" (default) picks "difference" when the focal column of the model
##       matrix contains only 0 and 1, and "derivative" otherwise.
##
## TODO: add a threshold plot (AME with its CI against AME_thr) once the
##   print/raw_output interface is settled.

test_rir_glm <- function(
        model_object,                 # fitted glm, binomial family, logit or probit link
        tested_variable_string,       # name of the focal predictor (or its coefficient)
        alpha = 0.05,                 # significance level
        tails = 2,                    # 1 or 2
        nu = 0,                       # null value on the probability (AME) scale
        ame_type = c("auto", "derivative", "difference"),
        to_return = c("print", "raw_output")
) {
    ## Argument validation
    to_return <- match.arg(to_return)
    ame_type  <- match.arg(ame_type)
    
    if (!inherits(model_object, "glm")) {
        stop("test_rir_glm() requires a model fitted with glm().")
    }
    fam  <- stats::family(model_object)
    link <- fam$link
    if (!(fam$family %in% c("binomial", "quasibinomial"))) {
        stop("test_rir_glm() currently supports binomial (or quasibinomial) ",
             "models with a logit or probit link.")
    }
    if (!(link %in% c("logit", "probit"))) {
        stop("test_rir_glm() currently supports the logit and probit links; ",
             "the fitted model uses link = '", link, "'.")
    }
    stopifnot(is.finite(alpha), alpha > 0, alpha < 1,
              tails %in% c(1, 2),
              is.finite(nu))
    
    beta <- stats::coef(model_object)
    if (anyNA(beta)) {
        stop("The model has aliased (NA) coefficients. Refit without the ",
             "collinear terms before computing the RIR.")
    }
    V <- stats::vcov(model_object)
    X <- stats::model.matrix(model_object)
    w <- model_object$prior.weights
    if (is.null(w)) w <- rep(1, nrow(X))
    n_obs <- stats::nobs(model_object)
    cn    <- colnames(X)
    
    ## Locate the focal column
    ## Exact match on the coefficient name first; otherwise accept a factor
    ## variable whose single dummy column starts with the variable name.
    if (tested_variable_string %in% cn) {
        j <- match(tested_variable_string, cn)
    } else {
        cand <- cn[startsWith(cn, tested_variable_string)]
        if (length(cand) == 1L) {
            j <- match(cand, cn)
            message("Using coefficient '", cand, "' for variable '",
                    tested_variable_string, "'.")
        } else if (length(cand) > 1L) {
            stop("Variable '", tested_variable_string, "' maps to several ",
                 "coefficients (", paste(cand, collapse = ", "), "). ",
                 "Pass the coefficient name you want tested.")
        } else {
            stop("Variable '", tested_variable_string, "' was not found among ",
                 "the model coefficients.")
        }
    }
    focal_name <- cn[j]
    
    ## Warn if the focal variable also appears in other terms
    ## (interactions, polynomials, transformations). The AME below is
    ## computed from the focal column only, so it would be incomplete.
    var_pattern <- paste0("(^|[^A-Za-z0-9_.])",
                          gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", tested_variable_string),
                          "([^A-Za-z0-9_.]|$)")
    others <- cn[-j]
    if (any(grepl(var_pattern, others))) {
        warning("'", tested_variable_string, "' appears in other model terms (",
                paste(others[grepl(var_pattern, others)], collapse = ", "),
                "). The AME is computed from the main-effect column only, ",
                "so the result may be incomplete.")
    }
    
    ## Choose the AME type
    focal_x <- X[, j]
    if (ame_type == "auto") {
        ame_type <- if (all(focal_x %in% c(0, 1))) "difference" else "derivative"
    }
    
    ## Link-specific functions
    ## F = cdf (probability), f = density (slope), fprime = derivative of f
    if (link == "logit") {
        F_fun  <- stats::plogis
        f_fun  <- function(e) { p <- stats::plogis(e); p * (1 - p) }
        fp_fun <- function(e) { p <- stats::plogis(e); p * (1 - p) * (1 - 2 * p) }
        coef_scale <- "log-odds"
    } else {
        F_fun  <- stats::pnorm
        f_fun  <- stats::dnorm
        fp_fun <- function(e) -e * stats::dnorm(e)
        coef_scale <- "probit"
    }
    
    ## Helper functions
    clamp01 <- function(x) pmax(0, pmin(1, x))
    wmean   <- function(v) sum(w * v) / sum(w)          # weighted mean over cases
    wcmean  <- function(M) colSums(w * M) / sum(w)      # weighted column means
    digits  <- 3
    fmt     <- paste0("%.", digits, "f")
    sf      <- function(x) signif(x, digits)
    pct     <- function(x) round(100 * x, 1)
    eps     <- .Machine$double.eps
    
    ## AME and delta-method gradient
    eta <- as.vector(X %*% beta)
    if (ame_type == "derivative") {
        mean_f <- wmean(f_fun(eta))
        ame    <- unname(beta[j] * mean_f)
        grad   <- wcmean(beta[j] * fp_fun(eta) * X)
        grad[j] <- grad[j] + mean_f
    } else {
        X1 <- X; X1[, j] <- 1
        X0 <- X; X0[, j] <- 0
        eta1 <- as.vector(X1 %*% beta)
        eta0 <- as.vector(X0 %*% beta)
        ame  <- wmean(F_fun(eta1) - F_fun(eta0))
        grad <- wcmean(f_fun(eta1) * X1 - f_fun(eta0) * X0)
    }
    ame_se <- sqrt(as.numeric(t(grad) %*% V %*% grad))
    if (!is.finite(ame_se) || ame_se <= 0) {
        stop("The delta-method SE of the AME is not positive. Check the model fit.")
    }
    
    ## Test statistic on the AME scale (Wald z)
    delta_hat <- ame - nu
    z_obs     <- delta_hat / ame_se
    z_crit    <- stats::qnorm(1 - alpha / tails)
    p_obs     <- if (tails == 2) 2 * stats::pnorm(-abs(z_obs)) else stats::pnorm(-abs(z_obs))
    is_sig    <- p_obs < alpha
    
    ## Threshold, replacement fraction and count
    ## AME_thr is the smallest |AME - nu| still significant at the observed SE.
    ## Significant:     (1 - pi) * delta_hat = AME_thr  ->  pi = 1 - AME_thr / |delta_hat|
    ## Not significant: replace with threshold-level cases -> pi = 1 - |delta_hat| / AME_thr
    ame_thr <- z_crit * ame_se
    if (is_sig) {
        pi_ame <- clamp01(1 - ame_thr / pmax(abs(delta_hat), eps))
    } else {
        pi_ame <- clamp01(1 - pmax(abs(delta_hat), eps) / pmax(ame_thr, eps))
    }
    k_ame <- ceiling(n_obs * pi_ame)
    
    ## Reference: same arithmetic on the coefficient scale
    ## Reported only as a ratio of test statistics, not as a case count.
    coef_est <- unname(beta[j])
    coef_se  <- sqrt(V[j, j])
    z_coef   <- coef_est / coef_se
    pi_coef  <- if (abs(z_coef) > z_crit) {
        clamp01(1 - z_crit / abs(z_coef))
    } else {
        clamp01(1 - abs(z_coef) / z_crit)
    }
    k_coef <- ceiling(n_obs * pi_coef)
    
    ## Descriptive quantities for the message
    has_int      <- "(Intercept)" %in% cn
    n_covariates <- length(beta) - 1L - as.integer(has_int)
    model_label  <- if (link == "logit") "Logistic" else "Probit"
    pp           <- 100 * ame                       # percentage points
    tail_label   <- if (tails == 2) "two-tailed" else "one-tailed"
    var_label    <- tested_variable_string
    
    direction <- if (ame >= 0) "increase" else "decrease"
    effect_sentence <- if (ame_type == "derivative") {
        sprintf(paste0(
            "A one-unit increase in %s is associated with a %s percentage-point\n",
            "%s in the probability of the outcome, averaged over the sample."),
            var_label, sprintf("%.1f", abs(pp)), direction)
    } else {
        sprintf(paste0(
            "Moving %s from 0 to 1 is associated with a %s percentage-point\n",
            "%s in the probability of the outcome, averaged over the sample."),
            var_label, sprintf("%.1f", abs(pp)), direction)
    }
    p_text <- if (p_obs < 0.001) "p < 0.001" else sprintf("p = %.3f", p_obs)
    nu_sentence <- if (nu != 0) {
        sprintf(" The test compares the AME with a null value of %s.", sf(nu))
    } else ""
    sig_sentence <- if (is_sig) {
        sprintf("The result is statistically significant at alpha = %s (%s).",
                alpha, tail_label)
    } else {
        sprintf("The result is not statistically significant at alpha = %s (%s).",
                alpha, tail_label)
    }
    
    ## Build print message
    title_text <- crayon::bold("Robustness of Inference to Replacement (RIR)")
    
    header_text <- sprintf(paste0(
        "%s regression with %d observations and %d %s.\n",
        "Effect measured on the probability scale (average marginal effect, AME).\n\n"),
        model_label, n_obs, n_covariates,
        if (n_covariates == 1L) "covariate" else "covariates")
    
    result_text <- paste0(
        crayon::bold("Your result"), "\n",
        "  ", gsub("\n", "\n  ", effect_sentence), "\n",
        sprintf("  AME = %s, SE = %s, z = %s, %s.",
                sprintf(fmt, ame), sprintf(fmt, ame_se),
                sprintf("%.2f", z_obs), p_text),
        nu_sentence, "\n",
        "  ", sig_sentence, "\n\n")
    
    if (is_sig) {
        bias_text <- paste0(
            crayon::bold("How much bias would it take to change the inference?"), "\n",
            sprintf(paste0(
                "  The result would no longer be significant if the effect shrank to %s\n",
                "  (the critical value %s times the SE of %s). So %s%% of the\n",
                "  estimated effect would have to be due to bias.\n\n"),
                sprintf(fmt, if (delta_hat >= 0) nu + ame_thr else nu - ame_thr),
                sprintf("%.2f", z_crit), sprintf(fmt, ame_se), pct(pi_ame)))
        replace_text <- paste0(
            crayon::bold("In terms of replacing observations"), "\n",
            sprintf(paste0(
                "  You would need to replace %d of the %d observations (%s%%) with\n",
                "  cases in which %s has no effect on the outcome for the result to\n",
                "  lose significance.\n",
                "  RIR = %d\n\n"),
                k_ame, n_obs, pct(pi_ame), var_label, k_ame))
    } else {
        bias_text <- paste0(
            crayon::bold("How much stronger would the effect need to be?"), "\n",
            sprintf(paste0(
                "  The result would become significant if the effect reached %s\n",
                "  (the critical value %s times the SE of %s). The estimated effect\n",
                "  is %s%% of that threshold.\n\n"),
                sprintf(fmt, if (delta_hat >= 0) nu + ame_thr else nu - ame_thr),
                sprintf("%.2f", z_crit), sprintf(fmt, ame_se), pct(1 - pi_ame)))
        replace_text <- paste0(
            crayon::bold("In terms of replacing observations"), "\n",
            sprintf(paste0(
                "  You would need to replace %d of the %d observations (%s%%) with\n",
                "  cases in which the effect of %s is at the threshold for statistical\n",
                "  significance for the result to become significant.\n",
                "  RIR = %d\n\n"),
                k_ame, n_obs, pct(pi_ame), var_label, k_ame))
    }
    
    scale_text <- paste0(
        crayon::bold("Why the probability scale?"), "\n",
        "  When a share of cases is replaced by cases with no effect, the\n",
        "  average marginal effect shrinks by exactly that share. This is what\n",
        "  makes the replacement count meaningful. The same is not true of a\n",
        sprintf("  %s coefficient. For reference, the same arithmetic applied to\n", coef_scale),
        sprintf("  the %s coefficient (%s, SE %s) gives %d, but that figure is\n",
                coef_scale, sprintf(fmt, coef_est), sprintf(fmt, coef_se), k_coef),
        "  a ratio of test statistics, not a number of cases.\n\n")
    
    ame_note <- if (ame_type == "derivative") {
        "  The AME is the average slope of the probability curve (derivative)."
    } else {
        "  The AME is the average change in probability from x = 0 to x = 1."
    }
    assume_text <- paste0(
        crayon::bold("What this assumes"), "\n",
        "  Replaced cases are spread evenly across the data rather than\n",
        "  concentrated among unusual or influential cases.\n",
        "  Replaced cases have the same distribution of predictor and covariate\n",
        "  values as the cases they replace.\n",
        "  The AME and its delta-method SE are adequate summaries. With small\n",
        "  samples or near-separation, also check the likelihood-ratio test.\n",
        ame_note)
    
    cite_text <- paste0(
        "\n\nSee Frank et al. (2013) for a description of the method.\n\n",
        crayon::underline("Citation:"), "\n",
        "Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. (2013).\n",
        "What would it take to change an inference? Using Rubin's causal\n",
        "model to interpret the robustness of causal inferences. ",
        crayon::italic("Educational\nEvaluation and Policy Analysis, 35"),
        "(4), 437-460."
    )
    
    message_text <- paste0(
        title_text, "\n",
        header_text,
        result_text,
        bias_text,
        replace_text,
        scale_text,
        assume_text,
        cite_text
    )
    
    ## Full output list (returned invisibly on print)
    out <- list(
        input = list(tested_variable = tested_variable_string,
                     focal_coefficient = focal_name,
                     n_obs = n_obs, n_covariates = n_covariates,
                     alpha = alpha, tails = tails, nu = nu,
                     model_type = "glm", link = link, ame_type = ame_type),
        ame        = ame,
        ame_se     = ame_se,
        ame_thr    = ame_thr,
        z_obs      = z_obs,
        z_crit     = z_crit,
        p_obs      = p_obs,
        is_sig     = is_sig,
        pi_primary = pi_ame,
        k_primary  = k_ame,
        # coefficient-scale reference (ratio of test statistics, not a case count)
        coef_est   = coef_est,
        coef_se    = coef_se,
        pi_coef    = pi_coef,
        k_coef     = k_coef,
        message    = message_text
    )
    
    ## Raw concise output branch
    raw_out <- list(
        model_type   = "glm",
        link         = link,
        ame_type     = ame_type,
        AME          = ame,
        AME_se       = ame_se,
        AME_thr      = ame_thr,
        observed_z   = z_obs,
        critical_z   = z_crit,
        p_value      = p_obs,
        RIR_perc     = pi_ame,
        RIR          = k_ame,
        # coefficient-scale reference (ratio of test statistics, not a case count)
        RIR_coef_perc = pi_coef,
        RIR_coef      = k_coef
    )
    
    ## Dispatch (print and raw_output)
    if (to_return == "print") {
        cat(message_text, "\n")
        return(invisible(out))
    } else if (to_return == "raw_output") {
        return(raw_out)
    }
}


## ==========================================================================
## konfound() INTEGRATION (proposed edit)
## ==========================================================================
## In konfound(), replace the glm branch so that index = "RIR" on a
## binomial logit/probit model routes here. The two "we recommend
## pkonfound" / "only an approximation" messages are no longer needed on
## this path. The IT index and non-binomial families keep the existing
## konfound_glm() route.
##
## if (inherits(model_object, "glm") & two_by_two == FALSE) {
##     fam <- stats::family(model_object)
##     if (index == "RIR" &&
##         fam$family %in% c("binomial", "quasibinomial") &&
##         fam$link %in% c("logit", "probit")) {
##         output <- test_rir_glm(
##             model_object           = model_object,
##             tested_variable_string = tested_variable_string,
##             alpha                  = alpha,
##             tails                  = tails,
##             nu                     = nu,        # add nu = 0 to konfound() signature
##             ame_type               = ame_type,  # add ame_type = "auto" to konfound() signature
##             to_return              = to_return
##         )
##     } else {
##         message("If a dichotomous independent variable is used, consider using the 2X2 table approach enabled with the argument two_by_two = TRUE")
##         output <- konfound_glm(
##             model_object = model_object,
##             tested_variable_string = tested_variable_string,
##             alpha = alpha, tails = tails, to_return = to_return
##         )
##     }
## }
##
## pkonfound() note: no change required. If a summary-statistic entry point
## is wanted later, the arithmetic from "Test statistic" onward can be lifted
## into a small engine that accepts (AME, AME_se, n_obs, alpha, tails, nu).


## ==========================================================================
## EXAMPLES
## ==========================================================================

## Logistic, continuous predictor (derivative AME)
# m1 <- glm(am ~ wt + hp, data = mtcars, family = binomial(link = "logit"))
# test_rir_glm(m1, "wt")
# test_rir_glm(m1, "wt", to_return = "raw_output")

## Probit, continuous predictor
# m2 <- glm(am ~ wt + hp, data = mtcars, family = binomial(link = "probit"))
# test_rir_glm(m2, "wt")

## Logistic, 0/1 predictor (difference AME chosen automatically)
# m3 <- glm(am ~ vs + wt, data = mtcars, family = binomial(link = "logit"))
# test_rir_glm(m3, "vs")

## Force the derivative AME for a 0/1 predictor
# test_rir_glm(m3, "vs", ame_type = "derivative")

## Non-zero null on the probability scale (is the AME different from 0.05?)
# test_rir_glm(m1, "wt", nu = 0.05)

## Through konfound() once integrated
# konfound(m1, wt)
# konfound(m1, wt, to_return = "raw_output")