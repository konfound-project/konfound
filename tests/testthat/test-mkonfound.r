context("Checking mkonfound")

test_that("mkonfound produces table", {
    d <- mkonfound(mkonfound_ex, t, df)
    expect_is(d, "data.frame")
})

test_that("mkonfound produces plot", {
    p <- mkonfound(mkonfound_ex, t, df, return_plot = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("test_all works for lm and lmer output", {

    library(mice)

    popmis <- popmis[1:100, ]

    testmod1 <- lm(teachpop ~ texp + sex, data = popmis)
    
    output1 <- konfound(testmod1, texp, test_all = TRUE, to_return = "raw_output")
    
    library(lme4)
    
    testmod2 <- lmer(teachpop ~ texp + sex + (1 | school), data = popmis)

    output2 <- konfound(testmod2, texp, test_all = TRUE, to_return = "raw_output")

    expect_is(output1, "data.frame")
    expect_is(output2, "data.frame")
})
