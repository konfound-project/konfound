context("Checking konfound")

test_that("konfound works for linear model", {
    library(forcats)
    library(lme4)
    m1 <- lm(mpg ~ wt + hp, data = mtcars)
    output1 <- konfound(m1, wt, to_return = "raw_output")
    expect_equal(output1$RIR_perc, 66.629, tolerance = .01)
})

test_that("konfound works for lme4 model", {

    m3 <- lme4::lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
    output3 <- konfound(m3, Days, to_return = "raw_output")
    expect_equal(output3$RIR_perc, 84.826, tolerance = .001)
})

test_that("konfound works for glm, 2x2 model", {
    m4 <- glm(outcome ~ condition, data = binary_dummy_data, family = binomial(link = "logit"))
    output4 <- konfound(m4, condition, two_by_two = TRUE, n_treat = 55, to_return = "raw_output")
    expect_equal(output4$RIR_primary, 15)

    m4 <- glm(outcome ~ condition, data = binary_dummy_data, family = binomial(link = "logit"))
    output4_print <- capture.output(konfound(m4, condition, two_by_two = TRUE, n_treat = 55, to_return = "print"))
    expect_true(length(output4_print) > 0)
})


test_that("konfound returns a tibble", {
    m5 <- lm(mpg ~ wt + hp, data = mtcars)
    output5 <- konfound(m5, wt, to_return = "table")

    expect_s3_class(output5$Main_Output, "tbl_df")

    mtcars$my_var <- runif(nrow(mtcars))
    m5b <- lm(wt ~ my_var, data = mtcars)
    output5b <- konfound(m5b, my_var, to_return = "table")

    expect_s3_class(output5b$Main_Output, "tbl_df")
})

test_that("konfound glm works", {
    gss_cat$married <- ifelse(gss_cat$marital == "Married", 1, 0)

    m6 <- glm(married ~ age, data = gss_cat, family = binomial(link = "logit"))
    m6_output <- konfound(m6, age, to_return = "raw_output")

    expect_equal(as.vector(m6_output$RIR_perc), 84.006, tolerance = .001)
})
