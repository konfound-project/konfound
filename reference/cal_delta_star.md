# Calculate delta star for sensitivity analysis

Calculate delta star for sensitivity analysis

## Usage

``` r
cal_delta_star(
  FR2max,
  R2,
  R2_uncond,
  est_eff,
  eff_thr,
  var_x,
  var_y,
  est_uncond,
  rxz,
  n_obs
)
```

## Arguments

- FR2max:

  maximum R2

- R2:

  current R2

- R2_uncond:

  unconditional R2

- est_eff:

  estimated effect

- eff_thr:

  effect threshold

- var_x:

  variance of X

- var_y:

  variance of Y

- est_uncond:

  unconditional estimate

- rxz:

  correlation coefficient between X and Z

- n_obs:

  number of observations

## Value

delta star value
