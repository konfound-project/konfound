context("Checking test_sensitivity_ln")

source("~/konfound_newitcv_testcoverage/R/test_sensitivity_ln.R")
source("~/konfound_newitcv_testcoverage/R/nonlinear_auxiliary.R")
#source("~/newitcv_testcoverage/R/test_sensitivity_ln.R")
#source("~/newitcv_testcoverage/R/nonlinear_auxiliary.R")

# Test for standard error condition
test_that("test_sensitivity_ln handles standard error validation", {
    expect_error(
        test_sensitivity_ln(
            est_eff = 1.0,
            std_err = 0,  # Incorrect value that should trigger an error
            n_obs = 100,
            n_covariates = 5,
            n_treat = 1,
            alpha = 0.05,
            tails = 2,
            nu = 20,
            to_return = "raw_output"
        ),
        "Standard error needs to be greater than zero"
    )
})

# Test for the number of observations relative to covariates condition
test_that("test_sensitivity_ln handles observation-to-covariates validation", {
    expect_error(
        test_sensitivity_ln(
            est_eff = 1.0,
            std_err = 0.1,
            n_obs = 8,  # Fewer than n_covariates + 3; should trigger an error
            n_covariates = 5,
            n_treat = 1,
            alpha = 0.05,
            tails = 2,
            nu = 0,
            to_return = "raw_output"
        ),
        "Did not run! There are too few observations relative to\n   the number of observations and covariates. Please specify a\n    less complex model to use KonFound-It."
    )
})

# Test for invalid sample size and treatment cases
test_that("test_sensitivity_ln handles non-positive integers for sample size and treatment group cases", {
    # First case: Invalid sample size
    expect_error(
        test_sensitivity_ln(
            est_eff = 0.5,
            std_err = 0.2,
            n_obs = 0,  # Invalid sample size
            n_covariates = 5,
            n_treat = 1,
            alpha = 0.05,
            tails = 2,
            nu = 0,
            to_return = "raw_output"
        ),
        "Did not run! There are too few observations relative to\n   the number of observations and covariates. Please specify a\n    less complex model to use KonFound-It."
    )
    
    # Second case: Invalid number of treatment cases
    expect_error(
        test_sensitivity_ln(
            est_eff = 0.5,
            std_err = 0.2,
            n_obs = 100,
            n_covariates = 5,
            n_treat = 0,  # Invalid number of treatment cases
            alpha = 0.05,
            tails = 2,
            nu = 20,
            to_return = "raw_output"
        ),
        "Please enter positive integers for sample size\n     and number of treatment group cases."
    )
})

# Test input validation for n_obs and n_treat
test_that("test_sensitivity_ln handles invalid n_obs and n_treat inputs correctly", {
    # Test for non-positive n_treat
    expect_error(
        test_sensitivity_ln(
            est_eff = 0.5, std_err = 0.1, n_obs = 10, n_covariates = 3, n_treat = 0,
            alpha = 0.05, tails = 2, nu = 1, to_return = "value",
            model_object = lm(mpg ~ wt, data = mtcars), tested_variable = "wt"),
        "Please enter positive integers for sample size\n     and number of treatment group cases.",
        fixed = TRUE
    )
    
    # Test for n_obs <= n_treat
    expect_error(
        test_sensitivity_ln(
            est_eff = 0.5, std_err = 0.1, n_obs = 10, n_covariates = 3, n_treat = 50,
            alpha = 0.05, tails = 2, nu = 1, to_return = "value",
            model_object = lm(mpg ~ wt, data = mtcars), tested_variable = "wt"),
        "The total sample size should be larger than\n     the number of treatment group cases.",
        fixed = TRUE
    )
})



# Test for the calculation of thr_t based on est_eff
test_that("thr_t is calculated correctly for positive and negative est_eff", {
    # Mock data setup
    n_obs <- 100
    n_covariates <- 5
    alpha <- 0.05
    tails <- 2
    # Positive est_eff
    est_eff_positive <- 0.1
    expected_thr_t_positive <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 2)
    expect_equal(expected_thr_t_positive, 1.985802, tolerance = .001)
    
    # Negative est_eff
    est_eff_negative <- -0.1
    expected_thr_t_negative <- -stats::qt(1 - (alpha / tails), n_obs - n_covariates - 2)
    expect_equal(expected_thr_t_negative, -1.985802, tolerance = .001)
})

# Test when the total sample size is not larger than the number of treatment group cases
test_that("test_sensitivity_ln checks that total sample size is larger than the number of treatment group cases", {
    expect_error(
        test_sensitivity_ln(
            est_eff = 0.5,
            std_err = 0.2,
            n_obs = 5,  # Sample size equal to the number of treatments
            n_covariates = 3,
            n_treat = 5,
            alpha = 0.05,
            tails = 2,
            nu = 0,
            to_return = "raw_output"
        ),
        "Did not run! There are too few observations relative to\n   the number of observations and covariates. Please specify a\n    less complex model to use KonFound-It."
    )
})

# Test input validation for n_obs and n_treat
test_that("test_sensitivity_ln handles invalid n_obs and n_treat inputs correctly", {
    # Test for non-positive n_obs or n_treat
    expect_error(
        test_sensitivity_ln(
            est_eff = 0.5, std_err = 0.1, n_obs = 0, n_covariates = 3, n_treat = 5,
            alpha = 0.05, tails = 2, nu = 1, to_return = "value",
            model_object = lm(mpg ~ wt, data = mtcars), tested_variable = "wt"),
        "Did not run! There are too few observations relative to\n   the number of observations and covariates. Please specify a\n    less complex model to use KonFound-It."
    )
    
    expect_error(
        test_sensitivity_ln(
            est_eff = 0.5, std_err = 0.1, n_obs = 10, n_covariates = 3, n_treat = 0,
            alpha = 0.05, tails = 2, nu = 1, to_return = "value",
            model_object = lm(mpg ~ wt, data = mtcars), tested_variable = "wt"),
        "Please enter positive integers for sample size\n     and number of treatment group cases."
    )
    
    # Test for n_obs <= n_treat
    expect_error(
        test_sensitivity_ln(
            est_eff = 0.5, std_err = 0.1, n_obs = 10, n_covariates = 3, n_treat = 15,
            alpha = 0.05, tails = 2, nu = 1, to_return = "value",
            model_object = lm(mpg ~ wt, data = mtcars), tested_variable = "wt"),
        "The total sample size should be larger than\n     the number of treatment group cases."
    )
})
    