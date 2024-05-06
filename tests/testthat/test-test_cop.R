context("Testing test_cop.R")

# Error Case 1: Standard error is zero or negative
test_that("Standard error check", {
    expect_error(test_cop(est_eff = 0.4, std_err = 0, n_obs = 300, n_covariates = 5,
                          sdx = 1, sdy = 1, R2 = 0.5, eff_thr = 0, FR2max = 0.6, alpha = 0.05, tails = 2),
                 "Did not run! Standard error needs to be \n                            greater than zero.")
})

# Error Case 2: Standard deviation of x is zero or negative
test_that("Standard deviation of x check", {
    expect_error(test_cop(est_eff = 0.4, std_err = 0.1, n_obs = 300, n_covariates = 5,
                          sdx = 0, sdy = 1, R2 = 0.5, eff_thr = 0, FR2max = 0.6, alpha = 0.05, tails = 2),
                 "Did not run! Standard deviation of x needs to be \n                        greater than zero.")
})

# Error Case 3: Standard deviation of y is zero or negative
test_that("Standard deviation of y check", {
    expect_error(test_cop(est_eff = 0.4, std_err = 0.1, n_obs = 300, n_covariates = 5,
                          sdx = 1, sdy = 0, R2 = 0.5, eff_thr = 0, FR2max = 0.6, alpha = 0.05, tails = 2),
                 "Did not run! Standard deviation of y needs to be \n                        greater than zero.")
})

# Error Case 4: Insufficient observations relative to number of covariates
test_that("Observations relative to covariates check", {
    expect_error(test_cop(est_eff = 0.4, std_err = 0.1, n_obs = 8, n_covariates = 5,
                          sdx = 1, sdy = 1, R2 = 0.5, eff_thr = 0, FR2max = 0.6, alpha = 0.05, tails = 2),
                 "Did not run! There are too few observations relative to the \n         number of observations and covariates. Please specify a less \n         complex model to use KonFound-It.")
})

# Error Case 5: R2 is not less than FR2max
test_that("R2 less than FR2max check", {
    expect_error(test_cop(est_eff = 0.4, std_err = 0.1, n_obs = 300, n_covariates = 5,
                          sdx = 1, sdy = 1, R2 = 0.6, eff_thr = 0, FR2max = 0.5, alpha = 0.05, tails = 2),
                 "R2 Max needs to be greater than R2")
})

# Error Case 6: FR2max is not less than 1
test_that("FR2max less than 1 check", {
    expect_error(test_cop(est_eff = 0.4, std_err = 0.1, n_obs = 300, n_covariates = 5,
                          sdx = 1, sdy = 1, R2 = 0.5, eff_thr = 0, FR2max = 1, alpha = 0.05, tails = 2),
                 "Did not run! R2 Max needs to be less than 1.")
})

# Error Case 7: Check for non-positive Rxz^2
test_that("Non-positive Rxz^2 check", {
    expect_error(test_cop(est_eff = 0.4, std_err = 0.1, n_obs = 300, n_covariates = 5,
                          sdx = 1, sdy = 1, R2 = 0.99, eff_thr = 0, FR2max = 1.3, alpha = 0.05, tails = 2),
                 "Did not run! R2 Max needs to be less than 1.")
})



test_that("test_cop outputs the correct language and values when to_return is 'print'", {
    # Mock input values
    est_eff <- 0.5
    std_err <- 0.1
    n_obs <- 100
    n_covariates <- 2
    sdx <- 1
    sdy <- 1
    R2 <- 0.25
    eff_thr <- 0.3
    FR2max_multiplier <- 1.3
    FR2max <- 0.3
    alpha <- 0.05
    tails <- 2
    to_return <- "print"
    delta_star <- 0.1  # example values
    delta_exact <- 0.2
    delta_pctbias <- 10.5
    eff_x_M3_oster <- 0.15
    eff_x_M3 <- 0.12
    cond_RIR_fixedY <- 0.95
    cond_RIR_null <- 0.85
    cond_RIR_rxyz <- 0.75
    
    # Capture the output
    output <- capture.output(
        test_cop(est_eff, std_err, n_obs, n_covariates, sdx, sdy, R2, eff_thr, FR2max_multiplier, FR2max, alpha, tails, to_return)
        )

    # Define expected lines
    expected_lines <- c(
        "Coefficient of Proportionality (COP):",
        "",
        "This function calculates a correlation-based coefficient of",
        "proportionality (delta) as well as Oster's delta*.",
        "",
        "Delta* is 0.387 (assuming no covariates in the baseline model M1),", 
        "the correlation-based delta is 1.172, with a bias of -66.980%.",
        "Note that %bias = (delta* - delta) / delta.",
        "",
        "With delta*, the coefficient in the final model will be 0.447.",     
        "With the correlation-based delta, the coefficient will be 0.300.",
        "",
        "Use to_return = \"raw_output\" to see more specific results and graphic",             
        "presentation of the result.",                     
        "",                                                                    
        "This function also calculates conditional RIR that invalidates the statistical inference.",  
        "",                                                                    
        "If the replacement data points have a fixed value, then RIR = 80.035.",  
        "If the replacement data points follow a null distribution, then RIR = 55.317.",                                       
        "If the replacement data points satisfy rxy|Z = 0, then RIR = 56.317."
    )
    
    # Check each line against expected
    expect_equal(output, expected_lines)
})
