
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://travis-ci.org/jrosen48/konfound.svg?branch=master)](https://travis-ci.org/jrosen48/konfound)
[![CRAN
status](https://www.r-pkg.org/badges/version/konfound)](https://cran.r-project.org/package=konfound)

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
devtools::install_github("jrosen48/konfound")
```

# Use of konfound

#### pkonfound() for published studies

`pkonfound()`, for published studies, calculates (1) how much bias there
must be in an estimate to invalidate/sustain an inference; (2) the
impact of an omitted variable necessary to invalidate/sustain an
inference for a regression coefficient:

``` r
library(konfound)
#> Sensitivity analysis as described in Frank, Maroulis, Duong, and Kelcey (2013) and in Frank (2000).
#> For more information visit http://konfound-it.com.
```

``` r
pkonfound(est_eff = 2, 
          std_err = .4, 
          n_obs = 100, 
          n_covariates = 3)
#> Percent Bias Necessary to Invalidate the Inference:
#> To invalidate an inference, 60.3% of the estimate would have to be due to bias. This is based on a threshold of 0.794 for statistical significance (alpha = 0.05).
#> To invalidate an inference, 60 observations would have to be replaced with cases for which the effect is 0.
#> See Frank et al. (2013) for a description of the method
#> Citation: Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. 2013. What would it take to change an inference? Using Rubin's causal model to interpret the robustness of causal inferences. Education, Evaluation and Policy Analysis, 35 437-460.
#> Impact Threshold for a Confounding Variable:
#> The minimum impact to invalidate an inference for a null hypothesis of 0 effect is based on a correlation of 0.568 with the outcome and at 0.568 with the predictor of interest (conditioning on observed covariates) based on a threshold of 0.201 for statistical significance (alpha = 0.05).
#> Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be 0.568 X 0.568 = 0.323 to invalidate an inference for a null hypothesis of 0 effect.
#> See Frank (2000) for a description of the method
#> Citation: Frank, K. 2000. Impact of a confounding variable on the inference of a regression coefficient. Sociological Methods and Research, 29 (2), 147-194
#> For other forms of output, change `to_return` to table, raw_output, thres_plot, or corr_plot.
#> For models fit in R, consider use of konfound().
```

#### konfound() for models fit in R

`konfound()` calculates the same for models fit in R. For example, here
are the coefficients for a linear model fit with `lm()` using the
built-in dataset `mtcars`:

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

Sensitivity analysis for the effect for `wt` on `mpg` can be carried out
as follows, specifying the fitted model object:

``` r
konfound(m1, wt)
#> Percent Bias Necessary to Invalidate the Inference:
#> To invalidate an inference, 66.664% of the estimate would have to be due to bias. This is based on a threshold of -1.293 for statistical significance (alpha = 0.05).
#> To invalidate an inference, 21 observations would have to be replaced with cases for which the effect is 0.
#> See Frank et al. (2013) for a description of the method
#> Citation: Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. 2013. What would it take to change an inference? Using Rubin's causal model to interpret the robustness of causal inferences. Education, Evaluation and Policy Analysis, 35 437-460.
#> Impact Threshold for a Confounding Variable:
#> The minimum impact to invalidate an inference for a null hypothesis of 0 effect is based on a correlation of 0.787 with the outcome and at 0.787 with the predictor of interest (conditioning on observed covariates) based on a threshold of -0.36 for statistical significance (alpha = 0.05).
#> Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be 0.787 X 0.787 = 0.619 to invalidate an inference for a null hypothesis of 0 effect.
#> See Frank (2000) for a description of the method
#> Citation: Frank, K. 2000. Impact of a confounding variable on the inference of a regression coefficient. Sociological Methods and Research, 29 (2), 147-194
#> For more detailed output, consider setting `to_return` to table
#> To consider other predictors of interest, consider setting `test_all` to TRUE.
```

#### mkonfound for meta-analyses including sensitivity analysis

We can use an existing dataset, such as the CSV file
[here](https://msu.edu/~kenfrank/example%20dataset%20for%20mkonfound.csv).

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
#>         t    df action      inference       pct_bias_to_change_inf…   itcv r_con
#>     <dbl> <int> <chr>       <chr>                             <dbl>  <dbl> <dbl>
#>  1  7.08    178 to_invalid… reject_null                       68.8   0.378 0.614
#>  2  4.13    193 to_invalid… reject_null                       50.6   0.168 0.41 
#>  3  1.89     47 to_sustain  fail_to_reject…                    5.47 -0.012 0.11 
#>  4 -4.17    138 to_invalid… reject_null                       50.3   0.202 0.449
#>  5 -1.19     97 to_sustain  fail_to_reject…                   39.4  -0.065 0.255
#>  6  3.59     87 to_invalid… reject_null                       41.9   0.19  0.436
#>  7  0.282   117 to_sustain  fail_to_reject…                   85.5  -0.131 0.361
#>  8  2.55     75 to_invalid… reject_null                       20.6   0.075 0.274
#>  9 -4.44    137 to_invalid… reject_null                       53.0   0.225 0.475
#> 10 -2.05    195 to_invalid… reject_null                        3.51  0.006 0.077
#> # … with 20 more rows
```

# Features in-development

There is an in-development non-linear
option:

``` r
nl_output <- pkonfound(-0.2, 0.103, 20888, 3, n_trm = 17888, non_linear = TRUE)
#> Note that this output is from an approach for non-linear models that is developmental and unpublished
```

This function can also take a 2 x 2 table of treatment versus control
cases and success versus failure for the outcome, i.e.:

``` r
tkonfound(35, 17, 17, 38)
#> [[1]]
#> [1] "The tkonfound function calculates the number of cases that would have to be switched from one cell to another of a 2x2 table to invalidate an inference made about the association between the rows and columns. This can be applied to treatment vs control with successful vs unsuccessful outcomes."
#> 
#> [[2]]
#> [1] "To invalidate the inference, 10 cases need to be transferred from treatment success to treatment failure, as shown, from the Observed Table to the Transfer Table."
#> 
#> $User_enter_value
#>           Fail Success
#> Control     35      17
#> Treatment   17      38
#> 
#> $Transfer_Table
#>           Fail Success
#> Control     35      17
#> Treatment   27      28
#> 
#> [[5]]
#> [1] "For the Observed Table (based on user enter values), we have a Pearson's chi square of 14.176, with p value of 0.000."
#> 
#> [[6]]
#> [1] "For the Transfer Table, we have a Pearson's chi square of 3.640, with p value of 0.056."
```

Print `nl_output` to see the output.

# Other information

### How to learn more about sensitivity analysis

To learn more about sensitivity analysis, please visit:

  - The [Introduction to konfound
    vignette](https://jrosen48.github.io/konfound/articles/Introduction_to_konfound.html),
    with detailed information about each of the functions
    (`pkonfound()`, `konfound()`, and `mkounfound()`)
  - The causal inference section of Ken Frank’s website
    [here](https://msu.edu/~kenfrank/research.htm#causal)
  - The [konfound interactive web
    application](https://jmichaelrosenberg.shinyapps.io/shinykonfound/),
    with links to PowerPoints and key publications

### Feedback, issues, and feature requests

We prefer for issues to be filed via GitHub (link to the issues page for
`konfound` [here](https://github.com/jrosen48/konfound/issues)) though
we also welcome questions or feedback via email (see the DESCRIPTION
file).

### Code of Conduct

Please note that this project is released with a Contributor Code of
Conduct available at <http://contributor-covenant.org/version/1/0/0/>
