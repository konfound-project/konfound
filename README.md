
<!-- README.md is generated from README.Rmd. Please edit that file -->
konfound
========

The goal of konfound is to carry out sensitivity analysis based on Rubin's causal model.

### Installation

You can install konfound from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("jrosen48/rsensitivity")
```

### Use

`pkonfound()`, for published studies, calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient:

``` r
library(konfound)
pkonfound(2, .4, 100, 3)
#> To invalidate the inference, 60.31% of the estimate would have to be due to bias.
#> To invalidate the inference, 60 observations would have to be replaced with cases for which there is no effect.
```

`konfound()` calculates the same for models fit in R. For example, here is the output from a linear model fit with `lm()` using the built-in dataset `mtcars`:

``` r
m1 <- lm(mpg ~ wt + hp, data = mtcars)

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
#> To invalidate the inference, 66.72% of the estimate would have to be due to bias.
#> To invalidate the inference, 21 observations would have to be replaced with cases for which there is no effect.
```
