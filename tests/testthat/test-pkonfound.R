context("Checking pkonfound")

test_that("pkonfound test positive, significant coefficient works", {
    expect_equal(pkonfound(2, .4, 100, 3, to_return = "raw_output")$perc_bias_to_change, tolerance = .001, .603 * 100) # pct bias
    expect_equal(pkonfound(2, .4, 100, 3, to_return = "raw_output")$rxcvGz, tolerance = .001, 0.566) # rxcvGz
    expect_equal(pkonfound(2, .4, 100, 3, to_return = "raw_output")$itcvGz, tolerance = .001, 0.321) # itcvGz
})

test_that("pkonfound test negative, significant coefficient works", {
    expect_equal(pkonfound(-2, .4, 100, 3, to_return = "raw_output")$perc_bias_to_change, tolerance = .001, .603 * 100)
    expect_equal(pkonfound(-2, .4, 100, 3, to_return = "raw_output")$rxcvGz, tolerance = .001, 0.566)
    expect_equal(pkonfound(-2, .4, 100, 3, to_return = "raw_output")$itcvGz, tolerance = .001, -0.321)
})

test_that("pkonfound test positive, not significant coefficient works", {
    expect_equal(pkonfound(1, .4, 100, 3, to_return = "raw_output")$perc_bias_to_change, tolerance = .001, .206 * 100)
    expect_equal(pkonfound(1, .4, 100, 3, to_return = "raw_output")$rxcvGz, tolerance = .001, 0.247)
    expect_equal(pkonfound(1, .4, 100, 3, to_return = "raw_output")$itcvGz, tolerance = .001, 0.061)
})

test_that("pkonfound test negative, not significant coefficient works", {
    expect_equal(pkonfound(-1, .4, 100, 3, to_return = "raw_output")$perc_bias_to_change, tolerance = .001, 0.206 * 100)
    expect_equal(pkonfound(-1, .4, 100, 3, to_return = "raw_output")$rxcvGz, tolerance = .001, 0.247)
    expect_equal(pkonfound(-1, .4, 100, 3, to_return = "raw_output")$itcvGz, tolerance = .001, -.061)
})

test_that("pkonfound creates the threshhold plot", {
    thresh_plot <- pkonfound(2, .4, 100, 3, to_return = "thresh_plot")
    
    expect_s3_class(thresh_plot, "ggplot")
    
    thresh_plot_null <- pkonfound(.01, .4, 100, 3, to_return = "thresh_plot")
    
    expect_s3_class(thresh_plot_null, "ggplot")
})

test_that("pkonfound creates the correlation plot", {
    corr_plot <- pkonfound(2, .4, 100, 3, to_return = "corr_plot")
    
    expect_s3_class(corr_plot, "ggplot")
    
    corr_plot_null <- pkonfound(.01, .4, 100, 3, to_return = "corr_plot")
    
    expect_s3_class(corr_plot_null, "ggplot")
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

    expect_equal(output4$delta_exact, tolerance = .001, 1.309) # COP - delta exact
    
    expect_equal(output5$`correlation between X and CV conditional on Z`, tolerance = .001, 0.248) # PSE

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
    
    output7 <- pkonfound(.273, .024, 16999, 3, n_treat = 16000, model_type = "logistic", to_return = "raw_output")$RIR_primary
    
    expect_equal(output7, 1156)
    
    output8 <- pkonfound(.027, .024, 16999, 3, n_treat = 16000, model_type = "logistic", to_return = "raw_output")$RIR_primary
    
    expect_equal(output8, 803)
    
    output8_print <- capture.output(pkonfound(.027, .024, 16999, 3, n_treat = 16000, model_type = "logistic", to_return = "print"))
    
    expect_true(length(output8_print) > 0)
    
    output9 <- pkonfound(.027, .024, 16999, 3, n_treat = 16000, replace = "entire",
                         model_type = "logistic", to_return = "raw_output")$RIR_primary
    
    expect_equal(output9, 793)
    
    output10 <- pkonfound(.027, .024, 16999, 3, n_treat = 16000, switch_trm = FALSE,
                         model_type = "logistic", to_return = "raw_output")$RIR_primary
    
    expect_equal(output10, 52)

})

## two by two table 

test_that("two by two works with pkonfound", {
    
    output11 <- pkonfound(a = 18, b = 12, c = 12, d = 17, to_return = "raw_output")$RIR_primary
    expect_equal(output11, 8)
    
    output12 <- pkonfound(a = 18, b = 12, c = 12, d = 17, switch_trm = FALSE, to_return = "raw_output")$RIR_primary
    expect_equal(output12, 5)
    
    output13 <- pkonfound(a = 18, b = 3, c = 12, d = 1, test = "chisq", to_return = "raw_output")$RIR_primary
    expect_equal(output13, 6)
    
    output14 <- pkonfound(a = 18, b = 1, c = 12, d = 1, switch_trm = FALSE, to_return = "raw_output")$RIR_primary
    expect_equal(output14, 5)
})

test_that("pkonfound printed output works for a positive case", {
    outputa <- capture.output(pkonfound(2, .4, 100, 3, to_return = "print", index = "RIR"))
    expect_true(length(outputa) > 0)
    
    outputb <- capture.output(pkonfound(2, .4, 100, 3, to_return = "print", index = "IT"))
    expect_true(length(outputb) > 0)
    
    outputc <- capture.output(pkonfound(.01, .4, 100, 3, to_return = "print", index = "RIR"))
    expect_true(length(outputa) > 0)
    
    outputd <- capture.output(pkonfound(.01, .4, 100, 3, to_return = "print", index = "IT"))
    expect_true(length(outputb) > 0)
})

test_that("pkonfound printed output works for a negative case", {
    output <- capture.output(pkonfound(-2.2, .65, 200, 3, to_return = "print", index = "RIR"))
    expect_true(length(output) > 0)
    
    output <- capture.output(pkonfound(-2.2, .65, 200, 3, to_return = "print", index = "IT"))
    expect_true(length(output) > 0)
    
    output <- capture.output(pkonfound(-.01, .65, 200, 3, to_return = "print", index = "RIR"))
    expect_true(length(output) > 0)
    
    output <- capture.output(pkonfound(-.01, .65, 200, 3, to_return = "print", index = "IT"))
    expect_true(length(output) > 0)
})
