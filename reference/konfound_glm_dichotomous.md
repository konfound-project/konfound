# Konfound Analysis for Generalized Linear Models with Dichotomous Outcomes

This function performs konfound analysis on a generalized linear model
object with a dichotomous outcome. It uses 'broom' to tidy model outputs
and calculates the sensitivity of inferences.

## Usage

``` r
konfound_glm_dichotomous(
  model_object,
  tested_variable_string,
  alpha,
  tails,
  to_return,
  n_treat,
  switch_trm,
  replace
)
```

## Arguments

- model_object:

  The model object produced by glm.

- tested_variable_string:

  The name of the variable being tested.

- alpha:

  Significance level for hypothesis testing.

- tails:

  Number of tails for the test (1 or 2).

- to_return:

  The type of output to return.

- n_treat:

  Number of treatment cases.

- switch_trm:

  Term to switch for sensitivity analysis.

- replace:

  Boolean indicating whether to replace cases or not.

## Value

The results of the konfound analysis.
