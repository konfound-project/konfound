context("Checking mkonfound")

library(lme4)
library(mice)

popmis <- popmis[1:100, ]

testmod1 <- lm(teachpop ~ texp + sex, data = popmis)
testmod2 <- lmer(teachpop ~ texp + sex + (1 | school), data = popmis)

output1 <- konfound(testmod1, texp, test_all = TRUE, to_return = "raw_output")
output2 <- konfound(testmod2, texp, test_all = TRUE, to_return = "raw_output")

test_that("mkonfound works for lm and lmer outout", {
  expect_is(output1, "data.frame")
  expect_is(output2, "data.frame")
})
