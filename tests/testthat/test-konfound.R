context("Checking konfound")

library(forcats)
library(lme4)

m1 <- lm(mpg ~ wt + hp, data = mtcars)
output1 <- konfound(m1, wt, to_return = "raw_output")

# d <- forcats::gss_cat
# d$married <- ifelse(d$marital == "Married", 1, 0)
# m2 <- glm(married ~ age, data = d, family = binomial(link = "logit"))
# output2 <- konfound(m2, age, to_return = "raw_output")

m3 <- fm1 <- lme4::lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
output3 <- konfound(m3, Days, to_return = "raw_output")

m4 <- glm(outcome ~ condition, data = binary_dummy_data, family = binomial(link = "logit"))
output4 <- konfound(m4, condition, two_by_two = TRUE, n_treat = 55, to_return = "raw_output")

test_that("konfound works for linear model", {
    expect_equal(output1$percent_bias_to_change_inference, 66.629, tolerance = .001)
})

# test_that("konfound works for glm, 2x2 model", {
#     expect_equal(output2$percent_bias_to_change_inference, 35.357, tolerance = .001)
# })

test_that("konfound works for lme4 model", {
    expect_equal(output3$percent_bias_to_change_inference, 84.826, tolerance = .001)
})

test_that("konfound works for glm, 2x2 model", {
    expect_equal(output4$RIR, 15)
})
