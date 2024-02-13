context("Checking pkonfound")

test_that("pkonfound test positive, significant coefficient works", {
    expect_equal(dplyr::pull(pkonfound(2, .4, 100, 3, to_return = "raw_output")[1, 3]), tolerance = .001, .603 * 100) # pct bias
    expect_equal(dplyr::pull(pkonfound(2, .4, 100, 3, to_return = "raw_output")[1, 7]), tolerance = .001, 0.568) # r-corr
    expect_equal(dplyr::pull(pkonfound(2, .4, 100, 3, to_return = "raw_output")[1, 8]), tolerance = .001, 0.322) # impact
})

test_that("pkonfound test negative, significant coefficient works", {
    expect_equal(dplyr::pull(pkonfound(-2, .4, 100, 3, to_return = "raw_output")[1, 3]), tolerance = .001, .603 * 100)
    expect_equal(dplyr::pull(pkonfound(-2, .4, 100, 3, to_return = "raw_output")[1, 7]), tolerance = .001, 0.568)
    expect_equal(dplyr::pull(pkonfound(-2, .4, 100, 3, to_return = "raw_output")[1, 8]), tolerance = .001, -0.322)
})

test_that("pkonfound test positive, not significant coefficient works", {
    # expect_equal(dplyr::pull(pkonfound(1, .4, 100, 3, to_return = "raw_output")[1, 3]), tolerance = .001, .206 * 100)
    expect_equal(dplyr::pull(pkonfound(1, .4, 100, 3, to_return = "raw_output")[1, 7]), tolerance = .001, 0.248)
    expect_equal(dplyr::pull(pkonfound(1, .4, 100, 3, to_return = "raw_output")[1, 8]), tolerance = .001, 0.061)
})

test_that("pkonfound test negative, not significant coefficient works", {
    # expect_equal(dplyr::pull(pkonfound(-1, .4, 100, 3, to_return = "raw_output")[1, 3]), tolerance = .001, 0.206 * 100)
    expect_equal(dplyr::pull(pkonfound(-1, .4, 100, 3, to_return = "raw_output")[1, 7]), tolerance = .001, 0.248)
    expect_equal(dplyr::pull(pkonfound(-1, .4, 100, 3, to_return = "raw_output")[1, 8]), tolerance = .001, -.061)
})

test_that("pkonfound creates the threshhold plot", {
    thresh_plot <- pkonfound(2, .4, 100, 3, to_return = "thresh_plot")
    
    expect_s3_class(thresh_plot, "ggplot")
})

test_that("pkonfound creates the correlation plot", {
    corr_plot <- pkonfound(2, .4, 100, 3, to_return = "corr_plot")
    
    expect_s3_class(corr_plot, "ggplot")
})

expect_output(pkonfound(2, .4, 100, 3), ".")

## PSE and COP

test_that("PSE and COE work via pkonfound", {
    
    output4 <- pkonfound(est_eff = -.125,
                         std_err = .050,
                         n_obs = 6265,
                         n_covariates = 7,
                         sdx = .217,
                         sdy = .991,
                         R2 = .251,
                         eff_thr = 0,
                         FR2max = .61,
                         index = "COP",
                         to_return = "raw_output")
    
    output5 <- pkonfound(est_eff = .5,
                         std_err = .056,
                         n_obs = 6174,
                         eff_thr = .1,
                         sdx = 0.22,
                         sdy = 1,
                         R2 = .3,
                         index = "PSE",to_return = "raw_output")

    expect_equal(output4$delta_exact, tolerance = .001, 1.308) # COP - delta exact
    
    expect_equal(output5$`correlation between X and CV conditional on Z`, tolerance = .001, 0.247) # PSE
    expect_equal(output5$`correlation between Y and CV conditional on Z`, tolerance = .001, 0.372) # PSE
    expect_equal(output5$`correlation between X and CV`, tolerance = .001, 0.214) # PSE
    expect_equal(output5$`correlation between Y and CV`, tolerance = .001, 0.313) # PSE
})

test_that("PSE and COE work via pkonfound", {
    
    output6 <- pkonfound(est_eff = -.125,
                         std_err = .050,
                         n_obs = 6265,
                         n_covariates = 7,
                         sdx = .217,
                         sdy = .991,
                         R2 = .251,
                         eff_thr = 0,
                         FR2max = .61,
                         index = "COP",
                         to_return = "raw_output")
    
    expect_s3_class(output6$Figure, "ggplot")
})

## logistic

test_that("logistic models work with pkonfound", {
    
    output7 <- pkonfound(.273, .024, 16999, 3, n_treat = 16000, model_type = "logistic", to_return = "raw_output")$RIR
    
    expect_equal(output7, 1156)
    
    output8 <- pkonfound(.027, .024, 16999, 3, n_treat = 16000, model_type = "logistic", to_return = "raw_output")$RIR
    
    expect_equal(output8, 803)
})

test_that("pkonfound printed output works for a positive case", {
    outputa <- capture.output(pkonfound(2, .4, 100, 3, to_return = "print", index = "RIR"))
    expect_true(length(outputa) > 0)
    
    outputb <- capture.output(pkonfound(2, .4, 100, 3, to_return = "print", index = "IT"))
    expect_true(length(outputb) > 0)
})

test_that("pkonfound printed output works for a negative case", {
    output <- capture.output(pkonfound(-2.2, .65, 200, 3, to_return = "print", index = "RIR"))
    expect_true(length(output) > 0)
    
    output <- capture.output(pkonfound(-2.2, .65, 200, 3, to_return = "print", index = "IT"))
    expect_true(length(output) > 0)
})
