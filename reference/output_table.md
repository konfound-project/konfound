# Output a Tidy Table from a Model Object

This function takes a model object and the tested variable, tidies the
model output using \`broom::tidy\`, calculates the impact threshold for
confounding variables (ITCV) and impact for each covariate,and returns a
rounded, tidy table of model outputs.

## Usage

``` r
output_table(model_object, tested_variable)
```

## Arguments

- model_object:

  A model object from which to generate the output.

- tested_variable:

  The variable being tested in the model.

## Value

A tidy data frame containing model outputs, ITCV, and impacts for
covariates.
