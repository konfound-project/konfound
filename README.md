
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/konfound)](https://cran.r-project.org/package=konfound)
[![R-CMD-check](https://github.com/konfound-project/konfound/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/konfound-project/konfound/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/konfound-project/konfound/graph/badge.svg?token=ARijYlxn7O)](https://app.codecov.io/gh/konfound-project/konfound)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.05779/status.svg)](https://doi.org/10.21105/joss.05779)
<!-- badges: end -->

# konfound

In social science (and educational) research, we often wish to
understand how robust inferences about effects are to unobserved (or
controlled for) covariates, possible problems with measurement, and
other sources of bias. The goal of `konfound` is to carry out
sensitivity analysis to help analysts to *quantify how robust inferences
are to potential sources of bias*. This R package provides tools to
carry out sensitivity analysis as described in Frank, Maroulis, Duong,
and Kelcey (2013) based on Rubin’s (1974) causal model as well as in
Frank (2000) based on the impact threshold for a confounding variable.

# Installation

You can install the CRAN version of konfound with:

``` r
install.packages("konfound")
```

You can install the development version from GitHub with:

``` r
install.packages("devtools")
devtools::install_github("konfound-project/konfound")
```

# Use of konfound

## pkonfound() for published studies

`pkonfound()`, for published studies, calculates (1) how much bias there
must be in an estimate to invalidate/sustain an inference; (2) the
impact of an omitted variable necessary to invalidate/sustain an
inference for a regression coefficient:

``` r
library(konfound)
#> Sensitivity analysis as described in Frank, 
#> Maroulis, Duong, and Kelcey (2013) and in 
#> Frank (2000).
#> For more information visit http://konfound-it.com.
```

``` r
pkonfound(est_eff = 2, 
          std_err = .4, 
          n_obs = 100, 
          n_covariates = 3)
#> Robustness of Inference to Replacement (RIR):
#> TO INVALIDATE:
#> 
#> RIR = 60
#> 
#> The estimated effect is 2. To invalidate the inference of an effect using
#> the threshold of 0.794 for statistical significance with alpha = 0.05, 60.295% of
#> the (2) estimate would have to be due to bias. This implies that to invalidate
#> the inference one would expect to have to replace 60 (60.295%) observations
#> with cases for which the treatment effect is 0 (RIR = 60).
#> 
#> See Frank et al. (2013) for a description of the method.
#> 
#> Citation: Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. (2013).
#> What would it take to change an inference?
#> Using Rubin's causal model to interpret the robustness of causal inferences.
#> Education, Evaluation and Policy Analysis, 35 437-460.
#> 
#> Accuracy of results increases with the number of decimals reported.
#> For other forms of output, run
#>           ?pkonfound and inspect the to_return argument
#> For models fit in R, consider use of konfound().
```

## konfound() for models fit in R

`konfound()` calculates the same for models fit in R. For example, here
are the coefficients for a linear model fit with `lm()` using the
built-in dataset `mtcars`:

``` r
m1 <- lm(mpg ~ wt + disp, data = mtcars)
m1
#> 
#> Call:
#> lm(formula = mpg ~ wt + disp, data = mtcars)
#> 
#> Coefficients:
#> (Intercept)           wt         disp  
#>    34.96055     -3.35083     -0.01772
summary(m1)
#> 
#> Call:
#> lm(formula = mpg ~ wt + disp, data = mtcars)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -3.4087 -2.3243 -0.7683  1.7721  6.3484 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 34.96055    2.16454  16.151 4.91e-16 ***
#> wt          -3.35082    1.16413  -2.878  0.00743 ** 
#> disp        -0.01773    0.00919  -1.929  0.06362 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 2.917 on 29 degrees of freedom
#> Multiple R-squared:  0.7809, Adjusted R-squared:  0.7658 
#> F-statistic: 51.69 on 2 and 29 DF,  p-value: 2.744e-10
```

Sensitivity analysis for the effect for `wt` on `mpg` can be carried out
as follows, specifying the fitted model object:

``` r
konfound(m1, wt)
#> Robustness of Inference to Replacement (RIR):
#> TO INVALIDATE:
#> 
#> RIR = 9
#> 
#> The estimated effect is -3.351. To invalidate the inference of an effect using
#> the threshold of -2.381 for statistical significance with alpha = 0.05, 28.946% of
#> the (-3.351) estimate would have to be due to bias. This implies that to invalidate
#> the inference one would expect to have to replace 9 (28.946%) observations
#> with cases for which the treatment effect is 0 (RIR = 9).
#> 
#> See Frank et al. (2013) for a description of the method.
#> 
#> Citation: Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. (2013).
#> What would it take to change an inference?
#> Using Rubin's causal model to interpret the robustness of causal inferences.
#> Education, Evaluation and Policy Analysis, 35 437-460.
#> 
#> Accuracy of results increases with the number of decimals reported.
#> NULL
```

## mkonfound for meta-analyses including sensitivity analysis

We can use an existing (and built-in) dataset, such as `mkonfound_ex`.

``` r
mkonfound_ex
#> # A tibble: 30 × 2
#>         t    df
#>     <dbl> <dbl>
#>  1  7.08    178
#>  2  4.13    193
#>  3  1.89     47
#>  4 -4.17    138
#>  5 -1.19     97
#>  6  3.59     87
#>  7  0.282   117
#>  8  2.55     75
#>  9 -4.44    137
#> 10 -2.05    195
#> # ℹ 20 more rows
mkonfound(mkonfound_ex, t, df)
#> # A tibble: 30 × 7
#>         t    df action        inference      pct_bias_to_change_i…¹   itcv r_con
#>     <dbl> <dbl> <chr>         <chr>                           <dbl>  <dbl> <dbl>
#>  1  7.08    178 to_invalidate reject_null                     68.8   0.378 0.614
#>  2  4.13    193 to_invalidate reject_null                     50.6   0.168 0.41 
#>  3  1.89     47 to_sustain    fail_to_rejec…                   5.47 -0.012 0.11 
#>  4 -4.17    138 to_invalidate reject_null                     50.3   0.202 0.449
#>  5 -1.19     97 to_sustain    fail_to_rejec…                  39.4  -0.065 0.255
#>  6  3.59     87 to_invalidate reject_null                     41.9   0.19  0.436
#>  7  0.282   117 to_sustain    fail_to_rejec…                  85.5  -0.131 0.361
#>  8  2.55     75 to_invalidate reject_null                     20.6   0.075 0.274
#>  9 -4.44    137 to_invalidate reject_null                     53.0   0.225 0.475
#> 10 -2.05    195 to_invalidate reject_null                      3.51  0.006 0.077
#> # ℹ 20 more rows
#> # ℹ abbreviated name: ¹​pct_bias_to_change_inference
```

# Other information

### How to learn more about sensitivity analysis

To learn more about sensitivity analysis, please visit:

- The [Introduction to konfound
  vignette](https://konfound-it.org/konfound/articles/introduction-to-konfound.html),
  with detailed information about each of the functions (`pkonfound()`,
  `konfound()`, and `mkounfound()`)
- The [Konfound-It! interactive web
  application](https://konfound-project.shinyapps.io/konfound-it/), with
  links to PowerPoints and key publications

### Issues, feature requests, and contributing

We prefer for issues to be filed via GitHub (link to the issues page for
`konfound` [here](https://github.com/konfound-project/konfound/issues))
though we also welcome questions or feedback requests via email (see the
DESCRIPTION file).

Contributing guidelines are
[here](https://github.com/konfound-project/konfound/blob/master/.github/CONTRIBUTING.md).

### Code of Conduct

Please note that the konfound project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
