# Konfound Analysis for Linear Models

This function performs konfound analysis on a linear model object
produced by lm. It calculates the sensitivity of inferences for
coefficients in the model. It supports analysis for a single variable or
multiple variables.

## Usage

``` r
konfound_lm(
  model_object,
  tested_variable_string,
  alpha,
  tails,
  index,
  to_return
)
```

## Arguments

- model_object:

  The linear model object produced by lm.

- tested_variable_string:

  The name of the variable being tested.

- alpha:

  Significance level for hypothesis testing.

- tails:

  Number of tails for the test (1 or 2).

- index:

  Type of sensitivity analysis ('RIR' by default).

- to_return:

  The type of output to return.

## Value

The results of the konfound analysis for the specified variable(s).
