context("Checking tkonfound")

test_that("tkonfound creates the correlation plot", {
    
    tkonfound_fig <- tkonfound_fig(a = 35, b = 17, c = 17, d = 38)[[3]]
    
    expect_s3_class(tkonfound_fig, "ggplot")
})

test_that("tkonfound creates the correlation plot when switch_trm = FALSE", {
    
    tkonfound_fig2 <-  tkonfound_fig(a = 35, b = 17, c = 17, d = 38, switch_trm = FALSE)[[3]]
    
    expect_s3_class(tkonfound_fig2, "ggplot")
})

test_that("tkonfound creates the correlation plot with test = chisq", {
    
    tkonfound_fig3 <- tkonfound_fig(a = 35, b = 17, c = 17, d = 38, switch_trm = TRUE, test = "chisq")[[3]]

    expect_s3_class(tkonfound_fig3, "ggplot")
})

test_that("tkonfound creates the correlation plot with test = chisq and replace = entire", {
    
    tkonfound_fig4 <- tkonfound_fig(a = 35, b = 17, c = 17, d = 38, switch_trm = TRUE, test = "chisq", replace = "entire")[[3]]
    
    expect_s3_class(tkonfound_fig4, "ggplot")
})

test_that("tkonfound creates the correlation plot when treatment is small", {
    
    tkonfound_fig5 <- tkonfound_fig(a = 35, b = 17, c = 17, d = 38, switch_trm = TRUE, test = "chisq", replace = "entire")[[3]]
    
    expect_s3_class(tkonfound_fig5, "ggplot")
})

test_that("tkonfound for two_by_two works for raw output", {
    output3 <- pkonfound(a = 35, b = 17, c = 17, d = 38, to_return = "raw_output")
    
    expect_equal(output3$RIR_primary, tolerance = .001, 14)

    output3_null <- pkonfound(a = 5, b = 17, c = 17, d = 10, to_return = "raw_output")

    expect_equal(output3_null$RIR_primary, tolerance = .001, 6)
    
    pkonfound_two_by_two <- capture.output(pkonfound(a = 35, b = 17, c = 17, d = 38, to_return = "print")) # printed output works

    expect_true(length(pkonfound_two_by_two) > 0)
    
})
