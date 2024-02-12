context("Checking pkonfound")

library(lme4)
library(mice)

popmis <- popmis[1:100, ]
testmod1 <- lm(teachpop ~ texp + sex, data = popmis)
testmod2 <- lmer(teachpop ~ texp + sex + (1 | school), data = popmis)

output1 <- konfound(testmod1, texp, test_all = TRUE, to_return = "raw_output")
output2 <- konfound(testmod2, texp, test_all = TRUE, to_return = "raw_output")

output3 <- pkonfound(a = 35, b = 17, c = 17, d = 38, to_return = "raw_output")

output3_b <- pkonfound(a = 5, b = 17, c = 8, d = 38, to_return = "raw_output")
output3_c <- pkonfound(a = 5, b = 17, c = 8, d = 38, to_return = "raw_output", switch_trm = FALSE)
output3_d <- pkonfound(a = 5, b = 17, c = 8, d = 38, to_return = "raw_output", 
                       switch_trm = FALSE,
                       test = "chisq")

output3_e <- pkonfound(a = 5, b = 17, c = 8, d = 38, to_return = "raw_output", 
                       switch_trm = FALSE,
                       test = "fisher")

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

# test_that("pkonfound test positive, significant coefficient, small sample size works", {
# expect_equal(dplyr::pull(pkonfound(2, .3, 20, 3, to_return = "raw_output")[1, 3]), tolerance = .001, .682 * 100)
# expect_equal(dplyr::pull(pkonfound(2, .3, 20, 3, to_return = "raw_output")[1, 7]), tolerance = .001, 0.865)
# expect_equal(dplyr::pull(pkonfound(2, .3, 20, 3, to_return = "raw_output")[1, 8]), tolerance = .001, 0.748)
# })
# above to be updated later 

test_that("tkonfound for two_by_two works", {
    expect_equal(output3$RIR, tolerance = .001, 14)
})

test_that("test_all works for lm and lmer outout", {
    expect_is(output1, "data.frame")
    expect_is(output2, "data.frame")
})

thresh_plot <- pkonfound(2, .4, 100, 3, to_return = "thresh_plot")

test_that("pkonfound creates the threshhold plot", {
    expect_s3_class(thresh_plot, "ggplot")
})

corr_plot <- pkonfound(2, .4, 100, 3, to_return = "corr_plot")

test_that("pkonfound creates the correlation plot", {
    expect_s3_class(corr_plot, "ggplot")
})

expect_output(pkonfound(2, .4, 100, 3), ".")

## PSE and COP

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

test_that("PSE and COE work via pkonfound", {
    expect_equal(output4$delta_exact, tolerance = .001, 1.308) # COP - delta exact
    
    expect_equal(output5$`correlation between X and CV conditional on Z`, tolerance = .001, 0.247) # PSE
    expect_equal(output5$`correlation between Y and CV conditional on Z`, tolerance = .001, 0.372) # PSE
    expect_equal(output5$`correlation between X and CV`, tolerance = .001, 0.214) # PSE
    expect_equal(output5$`correlation between Y and CV`, tolerance = .001, 0.313) # PSE
})

# test that the plot works for COP
# logistic - pkonfound
# add a few more different combinations of pkonfound inputs (helper_output_print)
# pkonfound(.273, .024, 16999, 3, n_treat = 16000, model_type = "logistic")
