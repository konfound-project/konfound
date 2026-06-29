# Robustness of inferences to differential attrition (beta)

Quantifies what would need to be true in the attritted (missing) data to
nullify an inference based on the observed data, using two complementary
approaches: a nonparametric decomposition of the combined-sample effect,
and a correlation-based framework adapted from Frank and Min (2007).

## Usage

``` r
robust_attrition(
  std_err,
  ntreatob,
  ncontrolob,
  ntreattot,
  ncontroltot,
  yobt,
  yobc,
  syob,
  n_covariates,
  R2,
  R2xz = NULL,
  R2yz = NULL,
  alpha = 0.05,
  pad_frac = 0.15,
  verbose = FALSE,
  to_return = "print"
)
```

## Arguments

- std_err:

  standard error of the observed treatment effect.

- ntreatob:

  number of observed treatment cases.

- ncontrolob:

  number of observed control cases.

- ntreattot:

  intended total treatment cases.

- ncontroltot:

  intended total control cases.

- yobt:

  observed treatment-group mean.

- yobc:

  observed control-group mean.

- syob:

  observed outcome standard deviation.

- n_covariates:

  number of covariates in the observed-data model.

- R2:

  unadjusted R-squared of the observed-data model (0 \<= R2 \< 1).

- R2xz:

  optional predictor-covariate fit; computed internally from other
  inputs when NULL (default).

- R2yz:

  optional outcome-covariate fit; computed internally when NULL
  (default).

- alpha:

  significance level (default 0.05).

- pad_frac:

  half-width of the plotting window for `to_return = "plot"`, as a
  fraction of the observed pooled mean. Defaults to 0.15 (a +/- 15
  values draw a longer line.

- verbose:

  if TRUE, the printed output includes a short description of what each
  block computes and inline notes about internally-computed defaults.
  Defaults to FALSE.

- to_return:

  one of "print" (default) to display output and return the result list
  invisibly, "raw_output" to return the full list, or "plot" to return a
  ggplot object showing the effect required in the missing data to
  nullify the inference across assumed missing-data means.

## Value

A list with components `inputs`, `nonpar`, `correlation_based`, and
`derived`. Returned invisibly when `to_return = "print"`. When
`to_return = "plot"`, a ggplot object is returned instead.

## Details

This is a beta (development) version. Calculations and output are under
review.

## Examples

``` r
if (FALSE) { # \dontrun{
robust_attrition(
  std_err = 2.19,
  ntreatob = 1817, ncontrolob = 1981,
  ntreattot = 2028, ncontroltot = 2311,
  yobt = 54.72, yobc = 49.90, syob = 29,
  n_covariates = 1, R2 = 0.01, R2xz = 0.479^2
)
} # }
```
