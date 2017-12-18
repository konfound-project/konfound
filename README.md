
<!-- README.md is generated from README.Rmd. Please edit that file -->
konfound
========

The goal of konfound is to carry out sensitivity analysis as described in Frank, Maroulis, Duong, and Kelcey (2013) based on Rubin’s (1974) causal model as well as in Frank (2000) based on the impact threshold for a confounding variable.

Installation
============

You can install konfound from GitHub with:

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
#> To invalidate the inference, 60.3% of the estimate would have to be due to bias based on a threshold of 0.794 and statistical significance.
#> To invalidate the inference, 60 observations would have to be replaced with cases for which the effect is 0.
#> 
#> Correlation-based Approach:
#> An omitted variable would have to be correlated at 0.568 with the outcome and at 0.568 with the predictor of interest (conditioning on observed covariates) to invalidate an inference based on a threshold of 0.201 and statistical significance.
#> Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be 0.568 X 0.568 = 0.323 to invalidate an inference.
#> NULL
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
#> To invalidate the inference, 66.664% of the estimate would have to be due to bias based on a threshold of -1.293 and statistical significance.
#> To invalidate the inference, 21 observations would have to be replaced with cases for which the effect is 0.
#> 
#> Correlation-based Approach:
#> An omitted variable would have to be correlated at 0.787 with the outcome and at 0.787 with the predictor of interest (conditioning on observed covariates) to invalidate an inference based on a threshold of -0.36 and statistical significance.
#> Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be 0.787 X 0.787 = 0.619 to invalidate an inference.
#> NULL
```

#### mkonfound for meta-analyses including sensitivity analysis

We can use an existing dataset, such as the CSV file [here](https://msu.edu/~kenfrank/example%20dataset%20for%20mkonfound.csv).

``` r
d <- read.csv("https://msu.edu/~kenfrank/example%20dataset%20for%20mkonfound.csv")
head(d)
mkonfound(d, t, df)
```

Other information
=================

How to learn more about sensitivity analysis
--------------------------------------------

For more information, please see:

-   The [Introduction to konfound vignette](https://jrosen48.github.io/konfound/articles/Introduction_to_konfound.html), with detailed information about each of the functions (`pkonfound()`, `konfound()`, and `mkounfound()`)
-   The causal inference section of Ken Frank's website [here](https://msu.edu/~kenfrank/research.htm#causal)
-   The [konfound interactive web application](https://jmichaelrosenberg.shinyapps.io/shinykonfound/), with links to PowerPoints and key publications

Feedback, issues, and feature requests
--------------------------------------

We prefer for issues to be filed via GitHub (link to the issues page for konfound [here](https://github.com/jrosen48/konfound/issues)) though we also welcome questions or feedback via [email](jrosen@msu.edu).

Code of Conduct
---------------

Please note that this project is released with a Contributor Code of Conduct available at <http://contributor-covenant.org/version/1/0/0/>

References
----------

-   Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. 2013. What would it take to change an inference?: Using Rubin’s causal model to interpret the robustness of causal inferences. *Education, Evaluation and Policy Analysis*. Vol 35: 437-460. <https://msu.edu/~kenfrank/What%20would%20it%20take%20to%20Change%20an%20Inference%20published.docx>

-   Frank, K.A., Gary Sykes, Dorothea Anagnostopoulos, Marisa Cannata, Linda Chard, Ann Krause, Raven McCrory. 2008. Extended influence: National Board Certified Teachers as help providers. *Education, Evaluation, and Policy Analysis*. Vol 30(1): 3-30. <https://msu.edu/~kenfrank/papers/Does%20NBPTS%20Certification%20Affect%20the%20Number%20of%20Colleagues%20a%20Teacher%20Helps%20with%20Instructional%20Matters%20acceptance%20version%202.doc>

-   Frank, K. A. and Min, K. 2007. Indices of Robustness for Sample Representation. *Sociological Methodology*. Vol 37, 349-392. <https://msu.edu/~kenfrank/papers/INDICES%20OF%20ROBUSTNESS%20TO%20CONCERNS%20REGARDING%20THE%20REPRESENTATIVENESS%20OF%20A%20SAMPLE.doc> (co first authors)

-   Frank, K. 2000. "Impact of a Confounding Variable on the Inference of a Regression Coefficient." *Sociological Methods and Research*, 29(2), 147-194 <https://msu.edu/~kenfrank/papers/impact%20of%20a%20confounding%20variable.pdf>
