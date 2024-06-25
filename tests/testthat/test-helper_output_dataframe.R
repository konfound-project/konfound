context("Testing Output DataFrame Creation")

# Test for est_eff greater than beta_threshhold
test_that("output dataframe is correct when est_eff is greater than beta_threshhold", {
    est_eff <- 0.5
    beta_threshhold <- 0.3
    unstd_beta <- 0.2
    bias <- 20
    recase <- 5
    r_con <- 0.1
    itcv <- 0.05
    
    df <- output_df(
        est_eff = est_eff,
        beta_threshhold = beta_threshhold,
        unstd_beta = unstd_beta,
        bias = bias,
        recase = recase,
        r_con = r_con,
        itcv = itcv
    )
    
    expect_equal(df$action, "to_invalidate")
    expect_equal(df$inference, "reject_null")
    expect_equal(df$percent_bias_to_change_inference, round(bias, 3))
    expect_equal(df$replace_null_cases, round(recase, 3))
    expect_equal(df$unstd_beta, unstd_beta)
    expect_equal(df$beta_threshhold, beta_threshhold)
    expect_equal(df$omitted_variable_corr, r_con)
    expect_equal(df$itcv, itcv)
})

# Test for est_eff less than beta_threshhold
test_that("output dataframe is correct when est_eff is less than beta_threshhold", {
    est_eff <- 0.2
    beta_threshhold <- 0.3
    unstd_beta <- 0.2
    sustain <- 15
    recase <- 10
    r_con <- 0.2
    itcv <- 0.06
    
    df <- output_df(
        est_eff = est_eff,
        beta_threshhold = beta_threshhold,
        unstd_beta = unstd_beta,
        sustain = sustain,
        recase = recase,
        r_con = r_con,
        itcv = itcv
    )
    
    expect_equal(df$action, "to_sustain")
    expect_equal(df$inference, "fail_to_reject_null")
    expect_equal(df$percent_bias_to_change_inference, round(sustain, 3))
    expect_equal(df$replace_null_cases, round(recase, 3))
    expect_equal(df$unstd_beta, unstd_beta)
    expect_equal(df$beta_threshhold, beta_threshhold)
    expect_equal(df$omitted_variable_corr, r_con)
    expect_equal(df$itcv, itcv)
})

# Test for est_eff equal to beta_threshhold
test_that("correct warning when est_eff is equal to beta_threshhold", {
    est_eff <- 0.3
    beta_threshhold <- 0.3
    unstd_beta <- 0.3
    r_con <- 0.3
    itcv <- 0.07
    
    expect_warning(
        df <- output_df(
            est_eff = est_eff,
            beta_threshhold = beta_threshhold,
            unstd_beta = unstd_beta,
            r_con = r_con,
            itcv = itcv
        ),
        "The coefficient is exactly equal to the threshold."
    )
})
