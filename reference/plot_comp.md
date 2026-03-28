# Replacement composition plot across k\* bins

Replacement composition plot across k\* bins

## Usage

``` r
plot_comp(result, type = c("ci", "rir"), verbose = FALSE, ...)
```

## Arguments

- result:

  A konfound_empdist object (must have been run with case_info)

- type:

  "ci" (default): average signed c_i influence per bin; "rir": average
  RIR influence (leave-one-out) per bin

- verbose:

  If TRUE, prints a description of the plot to the console. Default is
  FALSE.

- ...:

  Passed to the underlying plot function
