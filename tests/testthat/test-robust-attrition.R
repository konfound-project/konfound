# test-robust-attrition.R
# Tests for robust_attrition() (standalone, beta).
#
# Reference values from Ken Frank's verified example:
#   std_err = 2.19, ntreatob = 1817, ncontrolob = 1981,
#   ntreattot = 2028, ncontroltot = 2311,
#   yobt = 54.72, yobc = 49.90, syob = 29,
#   n_covariates = 1, R2 = 0.01, R2xz = 0.479^2

test_that("attrition engine returns expected structure via raw_output", {
  out <- robust_attrition(
    std_err = 2.19,
    ntreatob = 1817,
    ncontrolob = 1981,
    ntreattot = 2028,
    ncontroltot = 2311,
    yobt = 54.72,
    yobc = 49.90,
    syob = 29,
    n_covariates = 1,
    R2 = 0.01,
    R2xz = 0.479^2,
    to_return = "raw_output"
  )

  expect_type(out, "list")
  expect_named(out, c("inputs", "nonpar", "correlation_based", "derived"))
  expect_true("deltami" %in% names(out$nonpar))
  expect_true("rbetami" %in% names(out$correlation_based))
  expect_true("positive_root_min_effect" %in% names(out$correlation_based))
  expect_true("negative_root_min_effect" %in% names(out$correlation_based))
})

test_that("attrition engine computes correct observed effect", {
  out <- robust_attrition(
    std_err = 2.19,
    ntreatob = 1817,
    ncontrolob = 1981,
    ntreattot = 2028,
    ncontroltot = 2311,
    yobt = 54.72,
    yobc = 49.90,
    syob = 29,
    n_covariates = 1,
    R2 = 0.01,
    R2xz = 0.479^2,
    to_return = "raw_output"
  )

  expect_equal(out$derived$esteffect, 54.72 - 49.90, tolerance = 1e-10)
  expect_equal(out$derived$nobs, 1817 + 1981)
  expect_equal(out$derived$fulln, 2028 + 2311)
  expect_equal(out$derived$nmiss, (2028 - 1817) + (2311 - 1981))
})

test_that("attrition engine: nonpar deltami is finite and nonzero", {
  out <- robust_attrition(
    std_err = 2.19,
    ntreatob = 1817,
    ncontrolob = 1981,
    ntreattot = 2028,
    ncontroltot = 2311,
    yobt = 54.72,
    yobc = 49.90,
    syob = 29,
    n_covariates = 1,
    R2 = 0.01,
    R2xz = 0.479^2,
    to_return = "raw_output"
  )

  expect_true(is.finite(out$nonpar$deltami))
  expect_false(out$nonpar$deltami == 0)
})

test_that("attrition engine: correlation-based quantities are finite", {
  out <- robust_attrition(
    std_err = 2.19,
    ntreatob = 1817,
    ncontrolob = 1981,
    ntreattot = 2028,
    ncontroltot = 2311,
    yobt = 54.72,
    yobc = 49.90,
    syob = 29,
    n_covariates = 1,
    R2 = 0.01,
    R2xz = 0.479^2,
    to_return = "raw_output"
  )

  expect_true(is.finite(out$correlation_based$rbetami))
  expect_true(is.finite(out$correlation_based$rbeta3))
  expect_true(is.finite(out$correlation_based$p_value))
  expect_true(is.finite(out$correlation_based$positive_root_min_effect))
  expect_true(is.finite(out$correlation_based$negative_root_min_effect))
})

test_that("attrition engine: R2xz/R2yz defaults are used when not supplied", {
  out <- robust_attrition(
    std_err = 2.19,
    ntreatob = 1817,
    ncontrolob = 1981,
    ntreattot = 2028,
    ncontroltot = 2311,
    yobt = 54.72,
    yobc = 49.90,
    syob = 29,
    n_covariates = 1,
    R2 = 0.01,
    to_return = "raw_output"
  )

  # When R2xz not supplied, used == default
  expect_equal(out$inputs$R2xz, out$inputs$R2xz_default)
  expect_equal(out$inputs$R2yz, out$inputs$R2yz_default)
})

test_that("attrition engine: user-supplied R2xz overrides default", {
  out_default <- robust_attrition(
    std_err = 2.19,
    ntreatob = 1817, ncontrolob = 1981,
    ntreattot = 2028, ncontroltot = 2311,
    yobt = 54.72, yobc = 49.90, syob = 29,
    n_covariates = 1, R2 = 0.01,
    to_return = "raw_output"
  )

  out_override <- robust_attrition(
    std_err = 2.19,
    ntreatob = 1817, ncontrolob = 1981,
    ntreattot = 2028, ncontroltot = 2311,
    yobt = 54.72, yobc = 49.90, syob = 29,
    n_covariates = 1, R2 = 0.01,
    R2xz = 0.479^2,
    to_return = "raw_output"
  )

  expect_equal(out_override$inputs$R2xz, 0.479^2)
  # Results should differ when R2xz differs
  expect_false(
    isTRUE(all.equal(
      out_default$correlation_based$rbetami,
      out_override$correlation_based$rbetami
    ))
  )
})

test_that("attrition engine: print runs without error", {
  expect_output(
    robust_attrition(
      std_err = 2.19,
      ntreatob = 1817, ncontrolob = 1981,
      ntreattot = 2028, ncontroltot = 2311,
      yobt = 54.72, yobc = 49.90, syob = 29,
      n_covariates = 1, R2 = 0.01,
      R2xz = 0.479^2,
      to_return = "print"
    ),
    "Robustness to Differential Attrition"
  )
})

test_that("attrition engine: validation catches bad inputs", {
  # se must be positive

  expect_error(
    robust_attrition(
      std_err = -1,
      ntreatob = 100, ncontrolob = 100,
      ntreattot = 120, ncontroltot = 120,
      yobt = 10, yobc = 8, syob = 5,
      n_covariates = 1, R2 = 0.1,
      to_return = "raw_output"
    ),
    "std_err must be"
  )

  # ntreatob cannot exceed ntreattot
  expect_error(
    robust_attrition(
      std_err = 1,
      ntreatob = 150, ncontrolob = 100,
      ntreattot = 120, ncontroltot = 120,
      yobt = 10, yobc = 8, syob = 5,
      n_covariates = 1, R2 = 0.1,
      to_return = "raw_output"
    ),
    "ntreatob cannot exceed"
  )

  # R2 out of range
  expect_error(
    robust_attrition(
      std_err = 1,
      ntreatob = 100, ncontrolob = 100,
      ntreattot = 120, ncontroltot = 120,
      yobt = 10, yobc = 8, syob = 5,
      n_covariates = 1, R2 = 1.5,
      to_return = "raw_output"
    ),
    "R2 must satisfy"
  )
})
