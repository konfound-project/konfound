# Empirical RIR Distribution

Empirical RIR Distribution

## Usage

``` r
konfound_empdist(
  model,
  target_var,
  reps = 1000,
  method = c("search", "direct"),
  k = NULL,
  alpha = 0.05,
  seed = 123,
  sign_flip_nullifies = TRUE,
  engine = "auto",
  get_test = NULL,
  case_info = NULL,
  verbose = FALSE,
  progress = FALSE
)
```

## Arguments

- model:

  Fitted lm or glm object (must have model = TRUE)

- target_var:

  Name of the focal predictor coefficient

- reps:

  Number of simulation replications

- method:

  Either "direct" (fast, LM only) or "search" (original, LM/GLM)

- k:

  Override replacement count (default: k_cf). Only used for
  method="direct".

- alpha:

  Significance level for nullification decision

- seed:

  Random seed for reproducibility

- sign_flip_nullifies:

  If TRUE, sign reversal counts as nullification

- engine:

  For method="search": "auto", "fast", or "slow"

- get_test:

  For method="search": custom test extractor function

- case_info:

  Output from label_cases(). When provided with method="search", enables
  per-rep composition tracking (supporter fraction, etc.) and unlocks
  the stacked composition plot.

- verbose:

  Print detailed progress messages

- progress:

  Print progress every 100 reps

## Value

Object of class "konfound_empdist"
