
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

``` r
devtools::load_all(".")
#> Loading konfound
#> Sensitivity analysis as described in Frank, Maroulis, Duong, and Kelcey (2013).
#> For more information visit https://jmichaelrosenberg.shinyapps.io/shinykonfound/.
#> You can also find more information by running the function launch_shiny()
```

Use of konfound
===============

#### pkonfound() for published studies

`pkonfound()`, for published studies, calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient:

``` r
library(konfound)
```

``` r
pkonfound(2, .4, 100, 3)
#> To invalidate the inference, 60.3 % of the estimate would have to be due to bias.
#> To invalidate the inference, 60 observations would have to be replaced with cases for which there is no effect.
#> 
#> An omitted variable would have to be correlated at 0.568 with the outcome and at 0.568 with the predictor of interest (conditioning on observed covariates) to invalidate an inference.
#> Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be 0.754 to invalidate an inference.
```

#### konfound() for models fit in R

`konfound()` calculates the same for models fit in R. For example, here are the coefficients for a linear model fit with `lm()` using the built-in dataset `mtcars`:

``` r
m1 <- lm(mpg ~ wt + hp, data = mtcars)
m1
#> 
#> Call:
#> lm(formula = mpg ~ wt + hp, data = mtcars)
#> 
#> Coefficients:
#> (Intercept)           wt           hp  
#>    37.22727     -3.87783     -0.03177
```

Sensitivity analysis for the effect for `wt` on `mpg` can be carried out as follows, specifying the fitted model object:

``` r
konfound(m1, wt)
#> To invalidate the inference, 66.664 % of the estimate would have to be due to bias.
#> To invalidate the inference, 21 observations would have to be replaced with cases for which there is no effect.
#> 
#> An omitted variable would have to be correlated at 0.787 with the outcome and at 0.787 with the predictor of interest (conditioning on observed covariates) to invalidate an inference.
#> Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be 0.887 to invalidate an inference.
# we need to be able to list multiple variables--one or all may cover most cases
# or, just return the statistics for all variables
```

#### mkonfound for meta-analyses including sensitivity analysis

``` r
library(dplyr, warn.conflicts = FALSE)

df <- tribble(
  ~unstd_beta, ~std_err, ~n_obs, ~n_covs,
  2,           .3,       70,     3,
  10,          2.9,      405,    4,
  1.7,         1.5,      200,    1
)

mkonfound(df)
#> # A tibble: 3 x 11
#>   unstd_beta std_err n_obs n_covs replacement_of_cases_inference
#>        <dbl>   <dbl> <dbl>  <dbl>                          <chr>
#> 1        2.0     0.3    70      3                  to_invalidate
#> 2       10.0     2.9   405      4                  to_invalidate
#> 3        1.7     1.5   200      1                     to_sustain
#> # ... with 6 more variables: percent_bias <dbl>, replace_null_cases <dbl>,
#> #   unstd_beta1 <dbl>, beta_threshhold <dbl>, correlation_inference <chr>,
#> #   omitted_variable_corr <dbl>
```

How to find more information
============================

For more information, visit:

-   The Introduction to konfound vignette, with detailed information about each of the functions (`pkonfound()`, `konfound()`, and `mkounfound()`)
-   The [konfound interactive web application](https://jmichaelrosenberg.shinyapps.io/shinykonfound/), with links to PowerPoints and key publications
