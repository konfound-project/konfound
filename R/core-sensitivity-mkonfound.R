core_sensitivity_mkonfound <- function(t, df, alpha = .05, tails = 2) {
    critical_t <- stats::qt(1 - (alpha / tails), df)
    critical_r <- critical_t / sqrt((critical_t^2) + df)
    
    # For replacement of cases approach
    obs_r <- abs(t / sqrt(df + (t^2)))
    
    if (abs(obs_r) > abs(critical_r)) {
        action <- "to_invalidate"
        inference <- "reject_null"
        pct_bias <- 100 * (1 - (critical_r / obs_r))
    }
    else if (abs(obs_r) < abs(critical_r)) {
        action <- "to_sustain"
        inference <- "fail_to_reject_null"
        pct_bias <- 100 * (1 - (obs_r / critical_r))
    }
    else if (obs_r == critical_r) {
        action <- NA
        inference <- NA
        pct_bias <- NA
    }
    
    # # For correlation based approach (for calculating ITCV)
    if ((abs(obs_r) > abs(critical_r)) & ((obs_r * critical_r) > 0)) {
        mp <- -1
    } else {
        mp <- 1
    }
    itcv <- (obs_r - critical_r) / (1 + mp * abs(critical_r))
    r_con <- round(sqrt(abs(itcv)), 3)
    
    out <- tibble::tibble(t, df, action, inference, pct_bias, itcv, r_con)
    names(out) <- c("t", "df", "action", "inference", "pct_bias_to_change_inference", "itcv", "r_con")
    out$pct_bias_to_change_inference <- round(out$pct_bias_to_change_inference, 3)
    out$itcv <- round(out$itcv, 3)
    out$action <- as.character(out$action)
    out$inference <- as.character(out$inference)
    
    return(out)
}