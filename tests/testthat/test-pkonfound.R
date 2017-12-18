context("Checking pkonfound")

test_that("pkonfound test positive, significant coeficient works",  {
    expect_equal(dplyr::pull(pkonfound(2, .4, 100, 3, to_return = "raw_output")[1, 3]), tolerance = .001, .603 * 100)
    expect_equal(dplyr::pull(pkonfound(2, .4, 100, 3, to_return = "raw_output")[1, 7]), tolerance = .001, 0.568)
    expect_equal(dplyr::pull(pkonfound(2, .4, 100, 3, to_return = "raw_output")[1, 8]), tolerance = .001, 0.322)
})

test_that("pkonfound test negative, significant coeficient works",  {
    expect_equal(dplyr::pull(pkonfound(-2, .4, 100, 3, to_return = "raw_output")[1, 3]), tolerance = .001, .603 * 100)
    expect_equal(dplyr::pull(pkonfound(-2, .4, 100, 3, to_return = "raw_output")[1, 7]), tolerance = .001, 0.568)
    expect_equal(dplyr::pull(pkonfound(-2, .4, 100, 3, to_return = "raw_output")[1, 8]), tolerance = .001, -0.322)
})

test_that("pkonfound test positive, not significant coeficient works",  {
    expect_equal(dplyr::pull(pkonfound(1, .4, 100, 3, to_return = "raw_output")[1, 3]), tolerance = .001, .206 * 100)
    expect_equal(dplyr::pull(pkonfound(1, .4, 100, 3, to_return = "raw_output")[1, 7]), tolerance = .001, 0.248)
    expect_equal(dplyr::pull(pkonfound(1, .4, 100, 3, to_return = "raw_output")[1, 8]), tolerance = .001, 0.061)
})

test_that("pkonfound test negative, not significant coeficient works",  {
    expect_equal(dplyr::pull(pkonfound(-1, .4, 100, 3, to_return = "raw_output")[1, 3]), tolerance = .001, 0.206 * 100)
    expect_equal(dplyr::pull(pkonfound(-1, .4, 100, 3, to_return = "raw_output")[1, 7]), tolerance = .001, 0.248)
    expect_equal(dplyr::pull(pkonfound(-1, .4, 100, 3, to_return = "raw_output")[1, 8]), tolerance = .001, -.061)
})

test_that("pkonfound test positive, significant coeficient, small sample size works",  {
    expect_equal(dplyr::pull(pkonfound(2, .3, 20, 3, to_return = "raw_output")[1, 3]), tolerance = .1, .682 * 100)
    expect_equal(dplyr::pull(pkonfound(2, .3, 20, 3, to_return = "raw_output")[1, 7]), tolerance = .1, 0.851)
    expect_equal(dplyr::pull(pkonfound(2, .3, 20, 3, to_return = "raw_output")[1, 8]), tolerance = .1, 0.725)
})