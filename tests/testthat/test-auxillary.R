context("Checking auxiliary")

test_that("cal_ryz works", {
    expect_equal(cal_ryz(0.1, 0.6), tolerance = .001, 0.7719842) 
    expect_equal(cal_ryz(0.2, 0.6), tolerance = .001, 0.7637626) 
    expect_error(expect_message(cal_ryz(0.6, 0.2)), "The calculated variance in Y explained by Z is less than 0. This can occur if Z\n suppresses the relationship between X and Y. That is, if partialling on Z increases\n the relationship between X and Y. The unconditional ITCV is not conceptualized for\n this scenario.")
})

test_that("cal_rxz works", {
    expect_equal(cal_rxz(0.047089, 0.982081, 0.251, 6164, 0.05049), 
                 tolerance = .001, 0.07671882) 
    expect_equal(cal_rxz(0.6, 0.982081, 0.251, 6164, 0.05049), 
                 tolerance = .001, 0.960198) 
})

