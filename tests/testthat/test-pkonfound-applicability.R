# test-pkonfound-applicability.R
# Tests for pkonfound()'s argument-applicability messaging layer.
#
# pkonfound() dispatches to different engines based on `index`,
# `model_type`, and several sentinel arguments (`a`, `n_treat`,
# `two_by_two_table`, `link`, `scale`). Each branch only uses a
# subset of pkonfound()'s formals. When a user supplies arguments
# that are not relevant to the dispatched branch, pkonfound() now
# emits a single informational message() listing them.
#
# These tests verify that:
#   (1) irrelevant arguments trigger the message,
#   (2) relevant arguments pass silently,
#   (3) multiple irrelevant arguments are combined into one message.
#
# The applicability matrix itself lives in R/pkonfound_applicability.R.
# When adding a new dispatch branch or formal to pkonfound(), update
# the matrix and add a corresponding test here.

test_that("pkonfound messages when replace is passed with linear model", {
    expect_message(
        pkonfound(est_eff = 2, std_err = 0.4, n_obs = 100,
                  n_covariates = 3, replace = "entire"),
        "replace"
    )
})

test_that("pkonfound messages when FR2max is passed with non-COP", {
    expect_message(
        pkonfound(est_eff = 2, std_err = 0.4, n_obs = 100,
                  n_covariates = 3, FR2max = 0.7),
        "FR2max"
    )
})

test_that("pkonfound messages when scale is passed with index = 'IT'", {
    expect_message(
        pkonfound(est_eff = 0.5, std_err = 0.056, n_obs = 6174,
                  sdx = 0.22, sdy = 1, R2 = 0.3,
                  index = "IT", scale = "r"),
        "scale"
    )
})

test_that("pkonfound is silent when all args apply", {
    expect_no_message(
        pkonfound(est_eff = 2, std_err = 0.4, n_obs = 100,
                  n_covariates = 3),
        message = "not applicable"
    )
})

test_that("combined message lists multiple irrelevant args at once", {
    msgs <- capture_messages(
        pkonfound(est_eff = 2, std_err = 0.4, n_obs = 100,
                  n_covariates = 3,
                  replace = "entire", FR2max = 0.7, n_treat = 50)
    )
    applicability_msg <- msgs[grepl("not applicable", msgs)]
    
    # exactly one combined message, not one per irrelevant arg
    expect_length(applicability_msg, 1)
    
    # all three irrelevant args are named, order-agnostic
    expect_match(applicability_msg, "replace")
    expect_match(applicability_msg, "FR2max")
    expect_match(applicability_msg, "n_treat")
})