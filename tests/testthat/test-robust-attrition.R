# test-robust-attrition.R
# Tests for the standalone robust_attrition() command
# (differential attrition robustness, beta, master-only).
#
# Reference inputs from paper's erified example:
#   std_err = 2.19, ntreatob = 1817, ncontrolob = 1981,
#   ntreattot = 2028, ncontroltot = 2311,
#   yobt = 54.72, yobc = 49.90, syob = 29,
#   n_covariates = 1, R2 = 0.01, R2xz = 0.479^2
#
# Supersedes the earlier engine-level tests that called test_attrition()
# with the pre-refactor se / ncovar argument names.

context("Checking robust_attrition (differential attrition, beta)")

# Canonical inputs, shared by the structure / arithmetic / defaults /
# finiteness tests so the long argument list is not repeated each time.
att_args <- list(
    std_err = 2.19,
    ntreatob = 1817, ncontrolob = 1981,
    ntreattot = 2028, ncontroltot = 2311,
    yobt = 54.72, yobc = 49.90, syob = 29,
    n_covariates = 1, R2 = 0.01
)

# Run robust_attrition() on the canonical inputs, overriding any named args
# and always returning the raw list.
run_att <- function(...) {
    args <- modifyList(att_args, list(...))
    args$to_return <- "raw_output"
    do.call(robust_attrition, args)
}

# Contract: raw_output returns the four documented top-level components, each
# carrying the fields the print method and downstream code read. A dropped or
# renamed field here breaks every consumer with no error.
test_that("robust_attrition() raw_output has the documented structure", {
    out <- run_att(R2xz = 0.479^2)
    expect_type(out, "list")
    expect_named(out, c("inputs", "nonpar", "correlation_based", "derived"))
    
    expect_true(all(c("deltami", "deltacomb") %in% names(out$nonpar)))
    
    cb_fields <- c("rbetami", "rbeta3", "p_value",
                   "positive_root_min_effect", "negative_root_min_effect")
    expect_true(all(cb_fields %in% names(out$correlation_based)))
})

# The new std_err / n_covariates argument names must reach the internal
# se / ncovar engine. These identities are exact (no tolerance to hide a
# mis-wired argument), so a rename regression fails here first. This is the
# guard the old test_attrition() tests lost in the standalone refactor.
test_that("robust_attrition() derived quantities match the input arithmetic", {
    out <- run_att(R2xz = 0.479^2)
    expect_equal(out$inputs$std_err,      2.19)
    expect_equal(out$inputs$n_covariates, 1)
    expect_equal(out$derived$esteffect, 54.72 - 49.90, tolerance = 1e-10)
    expect_equal(out$derived$tob, (54.72 - 49.90) / 2.19, tolerance = 1e-10)
    expect_equal(out$derived$nobs,  1817 + 1981)
    expect_equal(out$derived$fulln, 2028 + 2311)
    expect_equal(out$derived$nmiss, (2028 - 1817) + (2311 - 1981))
    expect_equal(out$derived$df, (1817 + 1981) - 1 - 2)   # nobs - n_covariates - 2
})

# R2xz / R2yz are computed internally when NULL. A supplied R2xz must override
# the default and actually move the correlation-based coefficient; otherwise
# the default-vs-supplied branch would be a silent no-op.
test_that("robust_attrition() uses internal R2xz/R2yz defaults, and supplied R2xz overrides", {
    out_default  <- run_att()               # R2xz, R2yz left NULL
    out_override <- run_att(R2xz = 0.479^2)
    
    expect_equal(out_default$inputs$R2xz, out_default$inputs$R2xz_default)
    expect_equal(out_default$inputs$R2yz, out_default$inputs$R2yz_default)
    
    expect_equal(out_override$inputs$R2xz, 0.479^2)
    expect_false(isTRUE(all.equal(
        out_default$correlation_based$rbetami,
        out_override$correlation_based$rbetami
    )))
})

# Smoke test over the full pipeline: the nonparametric threshold, the
# correlation-based coefficient and p-value, and both interaction roots must
# all resolve to finite numbers. The root step takes a sqrt that can go NaN,
# so this is the cheapest guard that the math stays well-defined.
test_that("robust_attrition() nonpar and correlation-based quantities are finite", {
    out <- run_att(R2xz = 0.479^2)
    expect_true(is.finite(out$nonpar$deltami))
    expect_true(is.finite(out$correlation_based$rbetami))
    expect_true(is.finite(out$correlation_based$p_value))
    expect_true(is.finite(out$correlation_based$positive_root_min_effect))
    expect_true(is.finite(out$correlation_based$negative_root_min_effect))
})

# Input guards, including the no-attrition case specific to this command
# (the analysis is undefined when nothing is missing). Each call must stop,
# not return a quietly wrong list.
test_that("robust_attrition() validation catches bad inputs", {
    # std_err must be positive
    expect_error(run_att(std_err = -1), "std_err must be")
    
    # observed cannot exceed intended (ntreattot = 2028)
    expect_error(run_att(ntreatob = 2100), "ntreatob cannot exceed")
    
    # R2 must lie in [0, 1)
    expect_error(run_att(R2 = 1.5), "R2 must satisfy")
    
    # no attrition: observed equals intended in both arms
    expect_error(
        robust_attrition(
            std_err = 1,
            ntreatob = 100, ncontrolob = 100,
            ntreattot = 100, ncontroltot = 100,
            yobt = 10, yobc = 8, syob = 5,
            n_covariates = 1, R2 = 0.1,
            to_return = "raw_output"
        ),
        "No attrition detected"
    )
})