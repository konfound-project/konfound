# Output data frame based on model estimates and thresholds

Output data frame based on model estimates and thresholds

## Usage

``` r
output_df(
  est_eff,
  beta_threshhold,
  unstd_beta,
  bias = NULL,
  sustain = NULL,
  recase,
  obs_r,
  critical_r,
  r_con,
  itcv,
  non_linear
)
```

## Arguments

- est_eff:

  estimated effect

- beta_threshhold:

  threshold for beta

- unstd_beta:

  unstandardized beta value

- bias:

  bias to change inference

- sustain:

  sustain to change inference

- recase:

  number of cases to replace null

- obs_r:

  observed correlation

- critical_r:

  critical correlation

- r_con:

  correlation for omitted variable

- itcv:

  inferential threshold for confounding variable

- non_linear:

  flag for non-linear models

## Value

data frame with model information
