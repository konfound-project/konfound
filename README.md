
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

Use
===

### pkonfound() for published studies

`pkonfound()`, for published studies, calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient:

``` r
library(konfound)
pkonfound(2, .4, 100, 3)
#> To invalidate the inference, 60.31% of the estimate would have to be due to bias.
#> To invalidate the inference, 60 observations would have to be replaced with cases for which there is no effect.
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
#> To invalidate the inference, 66.72% of the estimate would have to be due to bias.
#> To invalidate the inference, 21 observations would have to be replaced with cases for which there is no effect.
```

Shiny Version for published studies
===================================

A shiny version for sensitivity analysis for published studies is available [here](https://jmichaelrosenberg.shinyapps.io/shinykonfound/)
