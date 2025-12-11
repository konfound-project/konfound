# Perform Sensitivity Analysis on 2x2 Tables

This function performs a sensitivity analysis on a 2x2 contingency
table. It calculates the number of cases that need to be replaced to
invalidate or sustain the statistical inference. The function also
allows switching between treatment success and failure or control
success and failure based on the provided parameters.

## Usage

``` r
tkonfound(
  a,
  b,
  c,
  d,
  alpha = 0.05,
  switch_trm = TRUE,
  test = "fisher",
  replace = "control",
  to_return = to_return
)
```

## Arguments

- a:

  Number of unsuccessful cases in the control group.

- b:

  Number of successful cases in the control group.

- c:

  Number of unsuccessful cases in the treatment group.

- d:

  Number of successful cases in the treatment group.

- alpha:

  Significance level for the statistical test, default is 0.05.

- switch_trm:

  Boolean indicating whether to switch treatment row cells, default is
  TRUE.

- test:

  Type of statistical test to use, either "fisher" (default) or "chisq".

- replace:

  Indicates whether to use the entire sample or the control group for
  base rate calculation, default is "control".

- to_return:

  Type of output to return, either "raw_output" or "print".

## Value

Returns detailed information about the sensitivity analysis, including
the number of cases to be replaced (RIR), user-entered table, transfer
table, and conclusions.
