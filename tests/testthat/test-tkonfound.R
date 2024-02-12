context("Checking tkonfound")

tkonfound_fig <- tkonfound_fig(35, 17, 17, 38)[[3]]

test_that("tkonfound creates the correlation plot", {
    expect_s3_class(tkonfound_fig, "ggplot")
})

tkonfound_fig <- tkonfound_fig(35, 17, 17, 38)[[3]]

#' tkonfound_fig(35, 17, 17, 38)
#' tkonfound_fig(35, 17, 17, 38, thr_p = 0.01)
#' tkonfound_fig(35, 17, 17, 38, thr_p = 0.01, switch_trm = FALSE)
#' tkonfound_fig(35, 17, 17, 38, thr_p = 0.01, switch_trm = TRUE, test = "chisq")
#' tkonfound_fig(35, 17, 17, 38, thr_p = 0.01, switch_trm = TRUE, test = "chisq", replace = "entire")