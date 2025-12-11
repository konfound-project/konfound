# Meta-Analysis and Sensitivity Analysis for Multiple Studies

Performs sensitivity analysis for multiple models, where parameters are
stored in a data frame. It calculates the amount of bias required to
invalidate or sustain an inference for each case in the data frame.

## Usage

``` r
mkonfound(d, t, df, alpha = 0.05, tails = 2, return_plot = FALSE)
```

## Arguments

- d:

  A data frame or tibble containing t-statistics and associated degrees
  of freedom.

- t:

  Column name or vector of t-statistics.

- df:

  Column name or vector of degrees of freedom associated with
  t-statistics.

- alpha:

  Significance level for hypothesis testing.

- tails:

  Number of tails for the test (1 or 2).

- return_plot:

  Whether to return a plot of the percent bias (default is \`FALSE\`).

## Value

Depending on \`return_plot\`, either returns a data frame with analysis
results or a plot.

## Examples

``` r
if (FALSE) { # \dontrun{
mkonfound_ex
str(d)
mkonfound(mkonfound_ex, t, df)
} # }
```
