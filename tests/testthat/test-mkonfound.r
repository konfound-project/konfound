context("Checking mkonfound")

test_that("mkonfound produces table", {
    d <- mkonfound(mkonfound_ex, t, df)
    expect_is(d, "data.frame")
})

test_that("mkonfound produces plot", {
    p <- mkonfound(mkonfound_ex, t, df, return_plot = TRUE)
    expect_s3_class(p, "ggplot")
})
