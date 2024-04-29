context("Testing Nonlinear_auxiliary.R")

source("~/konfound_newitcv_testcoverage/R/nonlinear_auxiliary.R")
#source("~/newitcv_testcoverage/R/nonlinear_auxiliary.R")

test_that("get_t_kfnl adjusts zero cells correctly", {
    # Check adjustment when 'a' is 0
    expect_equal(get_t_kfnl(0, 1, 1, 1), get_t_kfnl(0.5, 1, 1, 1))
    
    # Check adjustment when 'b' is 0
    expect_equal(get_t_kfnl(1, 0, 1, 1), get_t_kfnl(1, 0.5, 1, 1))
    
    # Check adjustment when 'c' is 0
    expect_equal(get_t_kfnl(1, 1, 0, 1), get_t_kfnl(1, 1, 0.5, 1))
    
    # Check adjustment when 'd' is 0
    expect_equal(get_t_kfnl(1, 1, 1, 0), get_t_kfnl(1, 1, 1, 0.5))
})


test_that("get_pi calculates intermediate variables and final result correctly", {
    # Setup mock values
    odds_ratio <- 2.0
    std_err <- 0.6  # Chosen to ensure discriminant is non-negative
    n_obs <- 200
    n_trm <- 80  # Less than half of n_obs
    
    # Expected intermediate values
    a <- odds_ratio * n_obs^2 * std_err^4
    b <- -a
    c <- 4 + 4 * odds_ratio^2 + odds_ratio * (-8 + 4 * n_obs * std_err^2)
    discriminant <- b^2 - 4 * a * c
    
    # Check if the discriminant is non-negative
    expect_true(discriminant >= 0, info = "Discriminant should be non-negative to ensure real solutions.")
    
    # Calculate expected x1 and x2
    x1_expected <- (-b - sqrt(discriminant)) / (2 * a)
    x2_expected <- (-b + sqrt(discriminant)) / (2 * a)
    
    # Act: Run function to get actual output
    actual_x <- get_pi(odds_ratio, std_err, n_obs, n_trm)
    
    # Expected result based on the number of treatment cases ratio
    expected_x <- if (n_trm / n_obs <= 0.5) x1_expected else x2_expected
    
    # Assert: Check intermediate variables
    expect_equal(a, odds_ratio * n_obs^2 * std_err^4, info = "Check 'a' calculation.")
    expect_equal(b, -a, info = "Check 'b' calculation.")
    expect_equal(c, 4 + 4 * odds_ratio^2 + odds_ratio * (-8 + 4 * n_obs * std_err^2), info = "Check 'c' calculation.")
    expect_equal(x1_expected, (-b - sqrt(discriminant)) / (2 * a), info = "Check 'x1' calculation.")
    expect_equal(x2_expected, (-b + sqrt(discriminant)) / (2 * a), info = "Check 'x2' calculation.")
    
    # Check if the actual output matches expected (handle NA)
    if (is.na(expected_x)) {
        expect_true(is.na(actual_x), info = "Output should be NA when expected is NA.")
    } else {
        expect_equal(actual_x, expected_x, tolerance = 0.01, info = "Check final output.")
    }
})

test_that("cal_minse calculates minimum standard error correctly", {
    # Setup mock values
    n_obs <- 1000
    n_treat <- 500
    odds_ratio <- 1.5
    
    # Expected calculation based on the function's formula
    expected_minse <- sqrt((4 * n_obs + 
                                sqrt(16 * n_obs^2 + 4 * n_treat * (n_obs - n_treat) * 
                                         ((4 + 4 * odds_ratio^2) / odds_ratio - 7.999999)))/
                               (2 * n_treat * (n_obs - n_treat)))
    
    # Act: Run the function to get actual output
    actual_minse <- cal_minse(n_obs, n_treat, odds_ratio)
    
    # Assert: Check if the actual output matches expected
    expect_equal(actual_minse, expected_minse, tolerance = 1e-6,
                 info = "Check if the calculated minimum standard error matches the expected value.")
})


test_that("cal_thr_t calculates threshold t correctly", {
    # Setup mock values for positive estimated effect
    est_eff_pos <- 0.5
    alpha <- 0.05
    tails <- 2
    n_obs <- 100
    n_covariates <- 5
    
    # Expected calculation for positive estimated effect
    expected_thr_t_pos <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 3)
    
    # Act: Run the function for positive estimated effect
    actual_thr_t_pos <- cal_thr_t(est_eff_pos, alpha, tails, n_obs, n_covariates)
    
    # Assert: Check if the actual output matches expected for positive estimated effect
    expect_equal(actual_thr_t_pos, expected_thr_t_pos,
                 info = "Check if the calculated threshold t for positive estimated effect matches the expected value.")
    
    # Setup mock values for negative estimated effect
    est_eff_neg <- -0.5
    
    # Expected calculation for negative estimated effect
    expected_thr_t_neg <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 3) * -1
    
    # Act: Run the function for negative estimated effect
    actual_thr_t_neg <- cal_thr_t(est_eff_neg, alpha, tails, n_obs, n_covariates)
    
    # Assert: Check if the actual output matches expected for negative estimated effect
    expect_equal(actual_thr_t_neg, expected_thr_t_neg,
                 info = "Check if the calculated threshold t for negative estimated effect matches the expected value.")
})

test_that("check_starting_table handles invalid input cases correctly", {
    # Setup test cases where the check should be FALSE
    # Case 1: Values less than 5
    expect_false(check_starting_table(100, 50, 4, 6, 7, 8))
    expect_false(check_starting_table(100, 50, 6, 4, 7, 8))
    expect_false(check_starting_table(100, 50, 6, 6, 4, 8))
    expect_false(check_starting_table(100, 50, 6, 6, 7, 4))
    
    # Case 2: Values greater than n_cnt or n_treat
    expect_false(check_starting_table(10, 8, 11, 5, 5, 5))
    expect_false(check_starting_table(10, 8, 5, 5, 5, 9))
    
    # Case 3: Values that are NaN
    expect_false(check_starting_table(10, 8, NaN, 5, 6, 7))
    expect_false(check_starting_table(10, 8, 5, NaN, 6, 7))
    expect_false(check_starting_table(10, 8, 5, 6, NaN, 7))
    expect_false(check_starting_table(10, 8, 5, 6, 7, NaN))
    
    # Case 4: Values equal to 5, which should pass, negating the test to ensure proper failure response
    expect_true(check_starting_table(10, 8, 5, 5, 5, 5))
})



