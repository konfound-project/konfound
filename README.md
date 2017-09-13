
<!-- README.md is generated from README.Rmd. Please edit that file -->
konfound
========

The goal of konfound is to carry out sensitivity analysis as described in Frank, Maroulis, Duong, and Kelcey (2013) based on Rubin's (1974) causal model.

Installation
============

You can install konfound from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("jrosen48/rsensitivity")
```

Use of konfound
===============

### pkonfound() for published studies

`pkonfound()`, for published studies, calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient:

``` r
library(konfound)
pkonfound(2, .4, 100, 3, to_return = "df")
#> # A tibble: 1 x 3
#>       inference percent observations
#>           <chr>   <dbl>        <dbl>
#> 1 to_invalidate   60.31           60
pkonfound(2, .4, 100, 3, to_return = "print")
#> To invalidate the inference, 60.31 % of the estimate would have to be due to bias.
#> To invalidate the inference, 60 observations would have to be replaced with cases for which there is no effect.
pkonfound(2, .4, 100, 3, to_return = "plot")
```

![](README-unnamed-chunk-2-1.png)

``` r
pkonfound(.4, 2, 100, 3, to_return = "print")
#> To sustain the inference,  89.92 % of the estimate would have to be due to bias.
#> To sustain the inference,  90  of the cases with 0 effect would have to be replaced with cases at the threshold of inference.
```

### konfound() for models fit in R

`konfound()` calculates the same for models fit in R. For example, here is the output from a linear model fit with `lm()` using the built-in dataset `mtcars`:

``` r
m1 <- lm(mpg ~ wt + hp, data = mtcars)
arm::display(m1)
#> lm(formula = mpg ~ wt + hp, data = mtcars)
#>             coef.est coef.se
#> (Intercept) 37.23     1.60  
#> wt          -3.88     0.63  
#> hp          -0.03     0.01  
#> ---
#> n = 32, k = 3
#> residual sd = 2.59, R-Squared = 0.83
```

Sensitivity analysis for the effect for `wt` on `mpg` can be carried out as follows, specifying the fitted model object:

``` r
konfound(m1, wt)
#> To invalidate the inference, 66.72 % of the estimate would have to be due to bias.
#> To invalidate the inference, 21 observations would have to be replaced with cases for which there is no effect.
```

### mkonfound for meta-analyses including sensitivity analysis

``` r
library(dplyr, warn.conflicts = FALSE)

df <- tribble(
  ~unstd_beta, ~standard_error, ~n_obs, ~n_covariates,
  2,           .3,              70,      3,
  10,          2.9,             405,     4,
  1.7,         1.5,             200,     1,
  -3,          1.3,             125,     2
)

out <- mkonfound(df)
knitr::kable(out)
```

|  unstd\_beta|  standard\_error|  n\_obs|  n\_covariates| inference      |  percent|  observations|
|------------:|----------------:|-------:|--------------:|:---------------|--------:|-------------:|
|          2.0|              0.3|      70|              3| to\_invalidate |    70.06|            49|
|         10.0|              2.9|     405|              4| to\_invalidate |    42.99|           174|
|          1.7|              1.5|     200|              1| to\_sustain    |    42.53|            85|
|         -3.0|              1.3|     125|              2| to\_invalidate |    14.22|            18|

Shiny Version for published studies
===================================

A shiny version for sensitivity analysis for published studies is available [here](https://jmichaelrosenberg.shinyapps.io/shinykonfound/).
