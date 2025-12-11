# Konfound Analysis for Linear Mixed-Effects Models

This function performs konfound analysis on a linear mixed-effects model
object produced by lme4::lmer. It calculates the sensitivity of
inferences for fixed effects in the model. It supports analysis for a
single variable or multiple variables.

## Usage

``` r
konfound_lmer(
  model_object,
  tested_variable_string,
  test_all,
  alpha,
  tails,
  index,
  to_return
)
```

## Arguments

- model_object:

  The mixed-effects model object produced by lme4::lmer.

- tested_variable_string:

  The name of the fixed effect being tested.

- test_all:

  Boolean indicating whether to test all fixed effects or not.

- alpha:

  Significance level for hypothesis testing.

- tails:

  Number of tails for the test (1 or 2).

- index:

  Type of sensitivity analysis ('RIR' by default).

- to_return:

  The type of output to return.

## Value

The results of the konfound analysis for the specified fixed effect(s).
