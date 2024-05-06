context("Checking helper_output_print")

test_that("output_print produces the correct language when index is 'RIR' with eff_thr specified", {
    # Define mock values for parameters
    n_covariates <- 3
    est_eff <- 0.6
    beta_threshhold <- 0.5
    bias <- 30
    nu <- 0.1
    eff_thr <- 0.5
    recase <- 5
    alpha <- 0.05
    index <- "RIR"
    
    # Capture the output
    output <- capture.output(
        output_print(n_covariates, est_eff, beta_threshhold, bias=bias, nu=nu, eff_thr=eff_thr, recase=recase, obs_r=NA, critical_r=NA, r_con=NA, itcv=NA, alpha=alpha, index=index)
    )
    
    # Define expected lines
    expected_lines <- c(
        "Robustness of Inference to Replacement (RIR):",
        "RIR = 5",
        "",
        "The estimated effect is 0.6, and specified threshold for inference is 0.5.",
        "To invalidate the inference based on your estimate, 30% of the (0.6)",
        "estimate would have to be due to bias. This implies that to invalidate",
        "the inference one would expect to have to replace 5 (30%) observation",
        "with data points for which the effect is 0.1 (RIR = 5).",
        "",
        "See Frank et al. (2013) for a description of the method.",
        "",
        "Citation: Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. (2013).",
        "What would it take to change an inference?",
        "Using Rubin's causal model to interpret the robustness of causal inferences.",
        "Education, Evaluation and Policy Analysis, 35 437-460.",
        "",
        "Accuracy of results increases with the number of decimals reported."
    )
    
    # Check each line against expected
    expect_equal(output, expected_lines)
})

test_that("output_print produces the correct language for 'TO SUSTAIN' when index is 'RIR' with eff_thr specified", {
    # Define mock values for parameters
    n_covariates <- 3
    est_eff <- 0.3
    beta_threshhold <- 0.5
    sustain <- 20
    nu <- 0.2
    eff_thr <- 0.4
    recase <- 10
    alpha <- 0.05
    index <- "RIR"
    
    # Capture the output
    output <- capture.output(
        output_print(n_covariates, est_eff, beta_threshhold, bias=NA, sustain=sustain, eff_thr=eff_thr, nu=nu, recase=recase, obs_r=NA, critical_r=NA, r_con=NA, itcv=NA, alpha=alpha, index=index)
    )
    
    # Define expected lines
    expected_lines <- c(
        "Robustness of Inference to Replacement (RIR):",
        "RIR = 10",
        "",
        "The estimated effect is 0.3, and specified threshold for inference is 0.4.",
        "To reach that threshold, 20% of the (0.3) estimate would have to be due",
        "to bias. This implies that to sustain an inference one would expect to have",
        "to replace 10 (20%) observations with effect of 0.2 with data points with",
        "effect of 0.5 (RIR = 10).",
        "",
        "See Frank et al. (2013) for a description of the method.",
        "",
        "Citation: Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. (2013).",
        "What would it take to change an inference?",
        "Using Rubin's causal model to interpret the robustness of causal inferences.",
        "Education, Evaluation and Policy Analysis, 35 437-460.",
        "",
        "Accuracy of results increases with the number of decimals reported."
    )
    
    # Check each line against expected
    expect_equal(output, expected_lines)
})


        