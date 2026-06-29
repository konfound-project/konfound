context("Checking test_correlation_rir (corr RIR engine)")

# Canonical LM baseline: same inputs as the existing pkonfound test, so any
# numeric drift in the t-scale engine gets caught from two directions.
test_that("significant LM, scale='t' matches canonical RIR", {
    out <- test_correlation_rir(2, 0.4, 100, 3,
                                scale = "t", to_return = "raw_output")
    expect_equal(out$RIR,      61)
    expect_equal(out$RIR_perc, 0.603, tolerance = .001)
    expect_lt(out$p_value, 0.05)
})

# Non-significant path ("sustain" direction). Without this, only one of the
# two code branches in the pi calculation is ever exercised by the tests.
test_that("non-significant LM, scale='t' gives correct sustain count", {
    out <- test_correlation_rir(0.03, 0.05, 1200, 8,
                                scale = "t", to_return = "raw_output")
    # df = 1190; stat_obs = 0.6 < qt(0.975, 1190) ~ 1.962 -> not significant
    expect_gte(out$p_value, 0.05)
    expect_equal(out$RIR, 833, tolerance = 2)
})

# Confirms the r-conversion is actually doing something: if scale="r" were
# accidentally identical to scale="t", the whole r-scale branch would be
# silently broken with no numeric signal.
test_that("scale='r' gives a different (lower) RIR than scale='t'", {
    out_t <- test_correlation_rir(2, 0.4, 100, 3,
                                  scale = "t", to_return = "raw_output")
    out_r <- test_correlation_rir(2, 0.4, 100, 3,
                                  scale = "r", to_return = "raw_output")
    expect_lt(out_r$RIR, out_t$RIR)
    expect_equal(out_r$RIR, 57, tolerance = 1)
})

# The GLM gate is the one place where a wrong argument produces a silently
# wrong answer rather than an error. Verifying it stops is the single most
# important guard in this function.
test_that("scale='t' with model_type='glm' errors with informative message", {
    expect_error(
        test_correlation_rir(0.5, 0.05, 500, 3,
                             model_type = "glm", link = "logit",
                             scale = "t"),
        regexp = "only available for linear models"
    )
})

# Smoke test: the GLM pathway runs end-to-end and p_value is populated.
# Keeps the GLM code path exercised without pinning fragile numeric values.
test_that("GLM logistic, scale='r' runs without error and returns p_value", {
    out <- test_correlation_rir(0.5, 0.05, 500, 3,
                                model_type = "glm", link = "logit",
                                scale = "r", to_return = "raw_output")
    expect_lt(out$p_value, 0.05)
    expect_equal(out$stat_type, "z")
})