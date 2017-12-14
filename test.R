#' @importFrom stats coef pt pnorm
merMod_p <- function(fit, p.kr) {
    # retrieve sigificance level of independent variables (p-values)
    if (inherits(fit, "merModLmerTest") && requireNamespace("lmerTest", quietly = TRUE)) {
        cs <- suppressWarnings(stats::coef(lmerTest::summary(fit)))
    } else {
        cs <- stats::coef(summary(fit))
    }
    
    # remeber coef-names
    coef_names <- rownames(cs)
    
    # check if we have p-values in summary
    if (ncol(cs) >= 4) {
        # do we have a p-value column?
        pvcn <- which(colnames(cs) == "Pr(>|t|)")
        # if not, default to 4
        if (length(pvcn) == 0) pvcn <- 4
        pv <- cs[, pvcn]
    } else if (inherits(fit, "lmerMod") && requireNamespace("pbkrtest", quietly = TRUE) && p.kr) {
        # compute Kenward-Roger-DF for p-statistic. Code snippet adapted from
        # http://mindingthebrain.blogspot.de/2014/02/three-ways-to-get-parameter-specific-p.html
        message("Computing p-values via Kenward-Roger approximation. Use `p.kr = FALSE` if computation takes too long.")
        #first coefficients need to be data frame
        cs <- as.data.frame(cs)
        # get KR DF
        df.kr <- suppressMessages(pbkrtest::get_Lb_ddf(fit, lme4::fixef(fit)))
        # # compute p-values, assuming an approximate t-dist
        # pv <- 2 * stats::pt(abs(cs$`t value`), df = df.kr, lower.tail = FALSE)
        # # name vector
        # names(pv) <- coef_names
    pv
}

library(lme4)
library(railtrails)

d <- railtrails %>% 
    unnest(raw_reviews)
    
fit <- lmer(mean_review ~ distance + (1|state), data = d)
df.kr <- suppressMessages(pbkrtest::get_Lb_ddf(fm1, lme4::fixef(fm1)))
