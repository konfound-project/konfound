
<!-- README.md is generated from README.Rmd. Please edit that file -->
konfound
========

The goal of konfound is to carry out sensitivity analysis as described in Frank, Maroulis, Duong, and Kelcey (2013) based on Rubin's (1974) causal model.

Installation
============

You can install konfound from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("jrosen48/konfound")
```

``` r
library(konfound)
```

    #> Loading konfound
    #> Sensitivity analysis as described in Frank, Maroulis, Duong, and Kelcey (2013).
    #> For more information visit https://jmichaelrosenberg.shinyapps.io/shinykonfound/.
    #> You can also find more information by running the function launch_shiny()

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
#> # A tibble: 3 x 10
#>   unstd_beta std_err n_obs n_covs replacement_of_cases_inference
#>        <dbl>   <dbl> <dbl>  <dbl>                          <chr>
#> 1        2.0     0.3    70      3                  to_invalidate
#> 2       10.0     2.9   405      4                  to_invalidate
#> 3        1.7     1.5   200      1                     to_sustain
#> # ... with 5 more variables: percent_bias <dbl>, replace_null_cases <dbl>,
#> #   beta_threshhold <dbl>, correlation_inference <chr>,
#> #   omitted_variable_corr <dbl>
```

Other information
=================

How to learn more about sensitivity analysis
--------------------------------------------

For more information, please see:

-   The [Introduction to konfound vignette](https://jrosen48.github.io/konfound/articles/Introduction_to_konfound.html), with detailed information about each of the functions (`pkonfound()`, `konfound()`, and `mkounfound()`)
-   Ken Frank's website [here](https://msu.edu/~kenfrank/research.htm)
-   The [konfound interactive web application](https://jmichaelrosenberg.shinyapps.io/shinykonfound/), with links to PowerPoints and key publications

Feedback, issues, and feature requests
--------------------------------------

We prefer for issues to be filed via GitHub (link to the issues page for konfound [here](https://github.com/jrosen48/konfound/issues)) though we also welcome questions or feedback via [email](jrosen@msu.edu).

Here are a few things we are working on at present:

-   Adding the option to output component correlations
-   Providing options for `konfound()` to work with mixed effects or multi-level models
-   Fixing threshplot for negative coefficients (presently does not work)

Code of Conduct
---------------

Please note that this project is released with a Contributor Code of Conduct. By participating in this project you agree to abide by its terms below. This Code of Conduct is adapted from the Contributor Covenant (<http:contributor-covenant.org>), version 1.0.0, available at <http://contributor-covenant.org/version/1/0/0/>

> As contributors and maintainers of this project, we pledge to respect all people who contribute through reporting issues, posting feature requests, updating documentation, submitting pull requests or patches, and other activities. We are committed to making participation in this project a harassment-free experience for everyone, regardless of level of experience, gender, gender identity and expression, sexual orientation, disability, personal appearance, body size, race, ethnicity, age, or religion. Examples of unacceptable behavior by participants include the use of sexual language or imagery, derogatory comments or personal attacks, trolling, public or private harassment, insults, or other unprofessional conduct. Project maintainers have the right and responsibility to remove, edit, or reject comments, commits, code, wiki edits, issues, and other contributions that are not aligned to this Code of Conduct. Project maintainers who do not follow the Code of Conduct may be removed from the project team. Instances of abusive, harassing, or otherwise unacceptable behavior may be reported by opening an issue or contacting one or more of the project maintainers.
