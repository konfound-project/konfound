context("Checking tkonfound")

tkonfound_fig <- tkonfound_fig(35, 17, 17, 38)[[3]]

test_that("tkonfound creates the correlation plot", {
    expect_s3_class(tkonfound_fig, "ggplot")
})
