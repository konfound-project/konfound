context("Checking test_pse")

test_that("test_pse handles non-positive standard error correctly", {
    # Expect an error to be thrown with the specific message when std_err is zero or negative
    expect_error(
        test_pse(est_eff = 0.5, std_err = 0, n_obs = 6174, n_covariates = 3, 
                 eff_thr = 0.1, sdx = 0.22, sdy = 1, R2 = 0.3, to_return = "full"),
        "Did not run! Standard error needs\n                              to be greater than zero."
    )
})



test_that("test_pse error handling for input validation", {
    # Testing that the function stops with an error message when standard deviation of x is zero or negative
    expect_error(
        test_pse(est_eff = 0.5, std_err = 0.056, n_obs = 6174, n_covariates = 3, 
                 eff_thr = 0.1, sdx = 0, sdy = 1, R2 = 0.3, to_return = "full"),
        "Did not run! Standard deviation of\n                          x needs to be greater than zero."
    )
})

test_that("test_pse error handling for input validation", {
    # Testing that the function stops with an error message when standard deviation of y is zero or negative
    expect_error(
        test_pse(est_eff = 0.5, std_err = 0.056, n_obs = 6174, n_covariates = 3, 
                 eff_thr = 0.1, sdx = 0.22, sdy = 0, R2 = 0.3, to_return = "full"),
        "Did not run! Standard deviation of\n                          y needs to be greater than zero."
    )
})

test_that("test_pse error handling for input validation", {
    # Testing that the function stops with an error message when there are too few observations
    expect_error(
        test_pse(est_eff = 0.5, std_err = 0.056, n_obs = 5, n_covariates = 3, 
                 eff_thr = 0.1, sdx = 0.22, sdy = 1, R2 = 0.3, to_return = "full"),
        "Did not run! There are too few observations relative\n           to the number of observations and covariates.\n           Please specify a less complex model to use KonFound-It."
    )
})

test_that("test_pse error handling for input validation", {
    # Testing that the function stops with an error message when R2 is not within the valid range (0,1)
    expect_error(
        test_pse(est_eff = 0.5, std_err = 0.056, n_obs = 6174, n_covariates = 3, 
                 eff_thr = 0.1, sdx = 0.22, sdy = 1, R2 = -0.1, to_return = "full"),
        "R2 needs to be greater than zero."
    )
})
test_that("test_pse error handling for input validation", {
    expect_error(
        test_pse(est_eff = 0.5, std_err = 0.056, n_obs = 6174, n_covariates = 3, 
                 eff_thr = 0.1, sdx = 0.22, sdy = 1, R2 = 1.5, to_return = "full"),
        "R2 needs to be less than one."
    )
})

test_that("test_pse error handling for input validation", {
    # Testing that the function stops with an error message when Rxz^2 calculation leads to a non-positive result
    expect_error(
        test_pse(est_eff = 0.5, std_err = 0.056, n_obs = 7, n_covariates = 3, 
                 eff_thr = 0.1, sdx = 0.22, sdy = 1, R2 = 0.999, to_return = "full"),
        "Did not run! Entered values produced Rxz^2 <=0,\n           consider adding more significant digits to your entered values.",
        fixed = TRUE 
    )
})

test_that("test_pse print output is correct", {
    # Setup for the test
    est_eff <- 0.5
    std_err <- 0.056
    n_obs <- 6174
    n_covariates <- 3
    eff_thr <- 0.1
    sdx <- 0.22
    sdy <- 1
    R2 <- 0.3
    to_return <- "print"
    
    # Expected string
    expected_output <- c(
        "This function calculates the conditions that set the estimated effect",
        "approximately equal to the threshold while preserving the standard error.",
        "",
        "The correlation between X and CV is 0.214, and the correlation between",
        "Y and CV is 0.313.",
        "",
        "Conditional on the covariates, the correlation between X and CV is 0.248,",
        "and the correlation between Y and CV is 0.372.",
        "",
        "Including such CV, the coefficient changes to 0.097, and standard error",
        "is 0.054.",
        "",
        "Use to_return = \"raw_output\" to see more specific results."
    )
    
    # Capture the output of the print statement
    output <- capture.output(
        test_pse(est_eff, std_err, n_obs, n_covariates, eff_thr, sdx, sdy, R2, to_return)
    )
    
    # Convert output array into a single string for easier grepl checking
    # output_string <- paste(output, collapse = "\n")
    
    # Check if the output is as expected
    expect_equal(output, expected_output)
})

