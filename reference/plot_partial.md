# Partial regression plot with per-case influence coloring

Partial regression plot with per-case influence coloring

## Usage

``` r
plot_partial(result, show_rir_labels = NULL, top_n = 10, verbose = FALSE, ...)
```

## Arguments

- result:

  A konfound_empdist object (must have been run with case_info)

- show_rir_labels:

  If TRUE (default when rir_infl available), label top cases by RIR
  influence. Set FALSE to suppress labels.

- top_n:

  Number of cases to label (default: 10)

- verbose:

  If TRUE, prints a description of the plot to the console. Default is
  FALSE.

- ...:

  Passed to the underlying plot function
