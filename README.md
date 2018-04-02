
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/jrosen48/konfound.svg?branch=master)](https://travis-ci.org/jrosen48/konfound)

konfound
========

In social science (and educational) research, we often wish to understand how robust inferences about effects are to unobserved (or controlled for) covariates, possible problems with measurement, and other sources of bias. The goal of `konfound` is to carry out sensitivity analysis to help analysts to *quantify how robust inferences are to potential sources of bias*. This R package provides tools to carry out sensitivity analysis as described in Frank, Maroulis, Duong, and Kelcey (2013) based on Rubin’s (1974) causal model as well as in Frank (2000) based on the impact threshold for a confounding variable.

Installation
============

Presently, `konfound` is available only on GitHub. You can install `konfound` from GitHub with:

``` r
install.packages("devtools")
devtools::install_github("jrosen48/konfound")
```

    #> Loading konfound
    #> Sensitivity analysis as described in Frank, Maroulis, Duong, and Kelcey (2013) and in Frank (2000).
    #> For more information visit https://jmichaelrosenberg.shinyapps.io/shinykonfound/.

Use of konfound
===============

#### pkonfound() for published studies

`pkonfound()`, for published studies, calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient:

``` r
library(konfound)
```

``` r
pkonfound(2, .4, 100, 3)
#> Replacement of Cases Approach:
#> To invalidate an inference, 60.3% of the estimate would have to be due to bias. This is based on a threshold of 0.794 for statistical significance (alpha = 0.05).
#> To invalidate an inference, 60 observations would have to be replaced with cases for which the effect is 0.
#> 
#> Correlation-based Approach:
#> An omitted variable would have to be correlated at 0.568 with the outcome and at 0.568 with the predictor of interest (conditioning on observed covariates) to invalidate an inference based on a threshold of 0.201 for statistical significance (alpha = 0.05).
#> Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be 0.568 X 0.568 = 0.323 to invalidate an inference.
#> For other forms of output, change `to_return` to table, raw_output, thres_plot, or corr_plot.
#> For models fit in R, consider use of konfound().
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
summary(m1)
#> 
#> Call:
#> lm(formula = mpg ~ wt + hp, data = mtcars)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -3.941 -1.600 -0.182  1.050  5.854 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 37.22727    1.59879  23.285  < 2e-16 ***
#> wt          -3.87783    0.63273  -6.129 1.12e-06 ***
#> hp          -0.03177    0.00903  -3.519  0.00145 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 2.593 on 29 degrees of freedom
#> Multiple R-squared:  0.8268, Adjusted R-squared:  0.8148 
#> F-statistic: 69.21 on 2 and 29 DF,  p-value: 9.109e-12
```

Sensitivity analysis for the effect for `wt` on `mpg` can be carried out as follows, specifying the fitted model object:

``` r
konfound(m1, wt)
#> Note that this output is calculated based on the correlation-based approach used in mkonfound()
#> Replacement of Cases Approach:
#> To invalidate an inference, 66.664% of the estimate would have to be due to bias. This is based on a threshold of -1.293 for statistical significance (alpha = 0.05).
#> To invalidate an inference, 21 observations would have to be replaced with cases for which the effect is 0.
#> 
#> Correlation-based Approach:
#> An omitted variable would have to be correlated at 0.787 with the outcome and at 0.787 with the predictor of interest (conditioning on observed covariates) to invalidate an inference based on a threshold of -0.36 for statistical significance (alpha = 0.05).
#> Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be 0.787 X 0.787 = 0.619 to invalidate an inference.
#> For more detailed output, consider setting `to_return` to table
#> To consider other predictors of interest, consider setting `test_all` to TRUE.
```

#### mkonfound for meta-analyses including sensitivity analysis

We can use an existing dataset, such as the CSV file [here](https://msu.edu/~kenfrank/example%20dataset%20for%20mkonfound.csv).

``` r
d <- read.csv("https://msu.edu/~kenfrank/example%20dataset%20for%20mkonfound.csv")
head(d)
#>           t  df
#> 1  7.076763 178
#> 2  4.127893 193
#> 3  1.893137  47
#> 4 -4.166395 138
#> 5 -1.187599  97
#> 6  3.585478  87
mkonfound(d, t, df)
#> # A tibble: 30 x 7
#>         t    df action        inference  pct_bias_to_chan…     itcv  r_con
#>     <dbl> <int> <chr>         <chr>                  <dbl>    <dbl>  <dbl>
#>  1  7.08    178 to_invalidate reject_nu…             68.8   0.378   0.614 
#>  2  4.13    193 to_invalidate reject_nu…             50.6   0.168   0.410 
#>  3  1.89     47 to_sustain    fail_to_r…              5.47 -0.0120  0.110 
#>  4 -4.17    138 to_invalidate reject_nu…             50.3   0.202   0.449 
#>  5 -1.19     97 to_sustain    fail_to_r…             39.4  -0.0650  0.255 
#>  6  3.59     87 to_invalidate reject_nu…             41.9   0.190   0.436 
#>  7  0.282   117 to_sustain    fail_to_r…             85.5  -0.131   0.361 
#>  8  2.55     75 to_invalidate reject_nu…             20.6   0.0750  0.274 
#>  9 -4.44    137 to_invalidate reject_nu…             53.0   0.225   0.475 
#> 10 -2.05    195 to_invalidate reject_nu…              3.51  0.00600 0.0770
#> # ... with 20 more rows
```

Other information
=================

### How to learn more about sensitivity analysis

To learn more about sensitivity analysis, please visit:

-   The [Introduction to konfound vignette](https://jrosen48.github.io/konfound/articles/Introduction_to_konfound.html), with detailed information about each of the functions (`pkonfound()`, `konfound()`, and `mkounfound()`)
-   The causal inference section of Ken Frank's website [here](https://msu.edu/~kenfrank/research.htm#causal)
-   The [konfound interactive web application](https://jmichaelrosenberg.shinyapps.io/shinykonfound/), with links to PowerPoints and key publications

### Feedback, issues, and feature requests

We prefer for issues to be filed via GitHub (link to the issues page for `konfound` [here](https://github.com/jrosen48/konfound/issues)) though we also welcome questions or feedback via [email](jrosen@msu.edu).

### Code of Conduct

Please note that this project is released with a Contributor Code of Conduct available at <http://contributor-covenant.org/version/1/0/0/>
