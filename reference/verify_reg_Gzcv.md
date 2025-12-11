# Verify regression model with control variable Z

Verify regression model with control variable Z

## Usage

``` r
verify_reg_Gzcv(n_obs, sdx, sdy, sdz, sdcv, rxy, rxz, rzy, rcvy, rcvx, rcvz)
```

## Arguments

- n_obs:

  number of observations

- sdx:

  standard deviation of X

- sdy:

  standard deviation of Y

- sdz:

  standard deviation of Z

- sdcv:

  sd between C and V

- rxy:

  correlation coefficient between X and Y

- rxz:

  correlation coefficient between X and Z

- rzy:

  correlation coefficient between Z and Y

- rcvy:

  correlation coefficient between V and Y

- rcvx:

  correlation coefficient between V and X

- rcvz:

  correlation coefficient between V and Z

## Value

list of model parameters
