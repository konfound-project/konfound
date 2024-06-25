context("Checking auxiliary")

test_that("cal_ryz works", {
    expect_equal(cal_ryz(0.1, 0.6), tolerance = .001, 0.7719842) 
    expect_equal(cal_ryz(0.2, 0.6), tolerance = .001, 0.7637626) 
    expect_error(expect_message(cal_ryz(0.6, 0.2)), "Error! R2yz < 0!")
})

test_that("cal_rxz works", {
    expect_equal(
        cal_rxz(var_x = 0.047089, var_y = 0.982081, R2 = 0.251, df = 6164, std_err = 0.05049), 
        0.07671882, 
        tolerance = .001
    )
    expect_equal(
        cal_rxz(var_x = 0.6, var_y = 0.982081, R2 = 0.251, df = 6164, std_err = 0.05049), 
        0.960198, 
        tolerance = .001
    )
    expect_error(
        cal_rxz(var_x = 0.0001, var_y = 1000, R2 = 0.999, df = 100, std_err = 0.01),
        "Error! R2xz < 0!\n                         Consider adding more significant digits to your input values."
    )
})

### cal_pse
test_that("cal_pse calculates correctly under nominal conditions", {
    result <- cal_pse(thr = 0.5, kryx = 0.3)
    expect_is(result, "list")
    expect_length(result, 2)
    
    # Check the specific numeric values returned
    if (!is.null(result[[1]]) && !is.null(result[[2]])) {
        expect_type(result[[1]], "double")
        expect_type(result[[2]], "double")
        
        # Checking the actual values
        expect_equal(result[[1]], 0.3605452, tolerance = 1e-5, 
                     info = "rxcvGz_sepreserve does not match expected value.")
        expect_equal(result[[2]], -0.3744429, tolerance = 1e-5,
                     info = "rycvGz_sepreserve does not match expected value.")
    } else {
        fail("One of the results is NULL.")
    }
})

test_that("cal_pse handles edge cases correctly", {
    # Check for valid response or silent handling instead of errors
    expect_silent(cal_pse(thr = 2, kryx = 2))
    expect_silent(cal_pse(thr = -1, kryx = -1))
    
    # Check the output type or a characteristic of the output to ensure correctness
    result1 <- cal_pse(thr = 2, kryx = 2)
    expect_is(result1, "list")
    expect_length(result1, 2)
    expect_type(result1[[1]], "double")
    
    result2 <- cal_pse(thr = -1, kryx = -1)
    expect_is(result2, "list")
    expect_length(result2, 2)
    expect_type(result2[[1]], "double")
    
    # Expect no warning if the function is confirmed to handle it silently
    result3 <- cal_pse(thr = 1, kryx = 1)
    expect_is(result3, "list")
    expect_length(result3, 2)
    expect_type(result3[[1]], "double")
})

### verify_manual
# Test for typical values
test_that("verify_manual handles typical input values correctly", {
    result <- verify_manual(rxy = 0.5, rxz = 0.3, rxcv = 0.2, ryz = 0.4, rycv = 0.1, rzcv = 0.5, sdy = 10, sdx = 5)
    expect_type(result, "double")  # Use 'double' to reflect the precise type in R
    expect_true(length(result) == 1)
    expect_true(result != 0)  # Check that result is not zero or NA
})

# Test for edge cases that could lead to division by zero
test_that("verify_manual handles division by zero correctly", {
    result <- verify_manual(rxy = 1, rxz = 1, rxcv = 1, ryz = 1, rycv = 1, rzcv = 1, sdy = 10, sdx = 5)
    expect_true(is.nan(result))
})

# Test for non-numeric inputs
test_that("verify_manual handles non-numeric inputs", {
    expect_error(verify_manual(rxy = "a", rxz = "b", rxcv = "c", ryz = "d", rycv = "e", rzcv = "f", sdy = "g", sdx = "h"))
})


### cal_delta_star
context("Testing cal_delta_star function")

test_that("cal_delta_star handles typical input values correctly", {
    result <- cal_delta_star(
        FR2max = 0.95, R2 = 0.9, R2_uncond = 0.85,
        est_eff = 0.5, eff_thr = 0.4,
        var_x = 10, var_y = 15,
        est_uncond = 0.45, rxz = 0.3, n_obs = 100
    )
    expect_true(is.numeric(result))
    expect_true(length(result) == 1)
    expect_false(is.nan(result))
})

test_that("cal_delta_star caps FR2max at 0.99", {
    capped_result <- cal_delta_star(
        FR2max = 0.999,  # This should get capped to 0.99
        R2 = 0.9, R2_uncond = 0.85,
        est_eff = 0.5, eff_thr = 0.4,
        var_x = 10, var_y = 15,
        est_uncond = 0.45, rxz = 0.3, n_obs = 100
    )
    uncapped_result <- cal_delta_star(
        FR2max = 0.99,  # Directly setting to the cap limit
        R2 = 0.9, R2_uncond = 0.85,
        est_eff = 0.5, eff_thr = 0.4,
        var_x = 10, var_y = 15,
        est_uncond = 0.45, rxz = 0.3, n_obs = 100
    )
    expect_equal(capped_result, uncapped_result, info = "Results should be identical when FR2max is capped at 0.99")
})

test_that("cal_delta_star handles all-zero input edge cases", {
    result_zero_values <- cal_delta_star(
        FR2max = 0, R2 = 0, R2_uncond = 0,
        est_eff = 0, eff_thr = 0,
        var_x = 0, var_y = 0,
        est_uncond = 0, rxz = 0, n_obs = 1  # Extreme case
    )
    expect_true(is.nan(result_zero_values), info = "Function should return NaN for zero input conditions")
})

# Test for division by zero
test_that("cal_delta_star handles division by zero", {
    result <- cal_delta_star(
        FR2max = 0.9, R2 = 0.9, R2_uncond = 0.9,
        est_eff = 0.5, eff_thr = 0.5,  # This makes b0_m_b1 = 0
        var_x = 10, var_y = 0,         # This makes rt_m_ro_t_syy = 0 and rm_m_rt_t_syy = 0
        est_uncond = 0.5, rxz = 1, n_obs = 100  # rxz = 1 makes t_x = 0
    )
    expect_true(is.nan(result), info = "Function should return NaN when division by zero occurs")
})

test_that("verify_reg_uncond handles errors, warnings, and stop conditions correctly", {
    library(lavaan)
    # Setup mock values that will likely cause an error in the covariance matrix calculation
    n_obs_error <- 50
    sdx_error <- 0.1
    sdy_error <- 0.1
    rxy_error <- 10  # Exaggerated correlation to force covariance matrix errors
    
    # Mock input to cause a warning or non-invertible matrix
    n_obs_warning <- 50
    sdx_warning <- 1
    sdy_warning <- 1
    rxy_warning <- 2  # Unrealistic correlation, but may trigger warnings depending on context
    
    # Mock values for correct run (control)
    n_obs_correct <- 100
    sdx_correct <- 1
    sdy_correct <- 1
    rxy_correct <- 0.5  # Valid correlation
    
    # Test error handling
    expect_error(verify_reg_uncond(n_obs_error, sdx_error, sdy_error, rxy_error), "Error!")
    
    # Test warning handling (depends on implementation, assuming it returns a FALSE)
    expect_error(verify_reg_uncond(n_obs_warning, sdx_warning, sdy_warning, rxy_warning), "Error!")
    
    # Test normal execution without errors or warnings (assuming the function returns a list if successful)
    expect_is(verify_reg_uncond(n_obs_correct, sdx_correct, sdy_correct, rxy_correct), "list")
    
    # Check for the stop condition if the function does not return a 'lavaan' object
    # Providing parameters that will not return a valid 'lavaan' object
    n_obs_stop <- 30
    sdx_stop <- 1
    sdy_stop <- 1
    rxy_stop <- 5  # Still unrealistic, but for demonstration
    expect_error(verify_reg_uncond(n_obs_stop, sdx_stop, sdy_stop, rxy_stop), "Error!")
})











