context("Checking pkonfound")

library(lme4)
library(mice)

popmis <- popmis[1:100, ]
testmod1 <- lm(teachpop ~ texp + sex, data = popmis)
testmod2 <- lmer(teachpop ~ texp + sex + (1 | school), data = popmis)

output1 <- konfound(testmod1, texp, test_all = TRUE, to_return = "raw_output")
output2 <- konfound(testmod2, texp, test_all = TRUE, to_return = "raw_output")

output3 <- pkonfound(a = 35, b = 17, c = 17, d = 38, to_return = "raw_output")

test_that("pkonfound test positive, significant coefficient works", {
  expect_equal(pkonfound(2, .4, 100, 3, to_return = "raw_output")$perc_bias_to_change, tolerance = .001, 60.28953) # pct bias
  expect_equal(pkonfound(2, .4, 100, 3, to_return = "raw_output")$rxcvGz, tolerance = .001, 0.568) # r-corr
  expect_equal(pkonfound(2, .4, 100, 3, to_return = "raw_output")$itcvGz, tolerance = .001, 0.3224053) # impact
})

test_that("pkonfound test negative, significant coefficient works", {
  expect_equal(pkonfound(-2, .4, 100, 3, to_return = "raw_output")$perc_bias_to_change, tolerance = .001, 60.28953)
  expect_equal(pkonfound(-2, .4, 100, 3, to_return = "raw_output")$rxcvGz, tolerance = .001, 0.568)
  expect_equal(pkonfound(-2, .4, 100, 3, to_return = "raw_output")$itcvGz, tolerance = .001, -0.3224053)
})

test_that("pkonfound test positive, not significant coefficient works", {
  expect_equal(pkonfound(1, .4, 100, 3, to_return = "raw_output")$perc_bias_to_change, tolerance = .001, 20.579)
  expect_equal(pkonfound(1, .4, 100, 3, to_return = "raw_output")$rxcvGz, tolerance = .001, 0.248)
  expect_equal(pkonfound(1, .4, 100, 3, to_return = "raw_output")$itcvGz, tolerance = .001, 0.061)
})

test_that("pkonfound test negative, not significant coefficient works", {
  expect_equal(pkonfound(-1, .4, 100, 3, to_return = "raw_output")$perc_bias_to_change, tolerance = .001, 20.579)
  expect_equal(pkonfound(-1, .4, 100, 3, to_return = "raw_output")$rxcvGz, tolerance = .001, 0.248)
  expect_equal(pkonfound(-1, .4, 100, 3, to_return = "raw_output")$itcvGz, tolerance = .001, -.061)
})

test_that("pkonfound test positive, significant coefficient, small sample size works", {
  expect_equal(pkonfound(2, .3, 20, 3, to_return = "raw_output")$perc_bias_to_change, tolerance = .001, 67.8282)
  expect_equal(pkonfound(2, .3, 20, 3, to_return = "raw_output")$rxcvGz, tolerance = .001, 0.863)
  expect_equal(pkonfound(2, .3, 20, 3, to_return = "raw_output")$itcvGz, tolerance = .001, 0.74545)
})

test_that("tkonfound for two_by_two works", {
  expect_equal(output3$RIR, tolerance = .001, 14)
})

test_that("test_all works for lm and lmer outout", {
  expect_is(output1, "data.frame")
  expect_is(output2, "data.frame")
})
