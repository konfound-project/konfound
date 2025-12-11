# Plot Correlation Diagram

This function creates a plot to illustrate the correlation between
different variables,specifically focusing on the confounding variable,
predictor of interest, and outcome.It uses ggplot2 for graphical
representation.

## Usage

``` r
plot_correlation(r_con, obs_r, critical_r)
```

## Arguments

- r_con:

  Correlation coefficient related to the confounding variable.

- obs_r:

  Observed correlation coefficient.

- critical_r:

  Critical correlation coefficient for decision-making.

## Value

A ggplot object representing the correlation diagram.
