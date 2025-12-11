# Konfound Analysis for Generalized Linear Models

This function performs konfound analysis on a generalized linear model
object. It uses 'broom' to tidy model outputs and calculates the
sensitivity of inferences. It supports analysis for a single variable or
multiple variables.

## Usage

``` r
konfound_glm(
  model_object,
  tested_variable_string,
  alpha,
  tails,
  index = "RIR",
  to_return
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

- index:

  Type of sensitivity analysis ('RIR' by default).

- to_return:

  The type of output to return.

## Value

The results of the konfound analysis for the specified variable(s).
