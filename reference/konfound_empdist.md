# Empirical RIR Distribution

Computes the empirical distribution of k\*, the minimum number of cases
that must be replaced with null-effect cases to nullify the inference,
across repeated resampling iterations. Extends the standard closed-form
RIR by capturing how the robustness threshold varies with which specific
cases are replaced.

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

## Examples

``` r
# \donttest{
  m <- lm(mpg ~ wt + cyl, data = mtcars, model = TRUE)
  result <- konfound_empdist(m, "wt", reps = 100, seed = 42)
  print(result)
#> Empirical Robustness of Inference to Replacement (RIR) 
#> 
#> Background 
#> The standard (closed-form) RIR gives a single threshold derived from
#> the t-statistic: the minimum number of cases that would need to be
#> replaced with null-effect cases to change the statistical inference.
#> The empirical RIR extends this by running repeated resampling
#> iterations, each drawing a random set of replacement cases and finding
#> k*, the minimum replacements needed to nullify the inference in that
#> iteration. The resulting distribution of k* is the empirical RIR,
#> capturing how the robustness threshold varies with which specific cases
#> are replaced.
#> 
#> Key Statistics 
#> The focal predictor is wt with N = 32 and alpha = 0.05. The observed
#> |t| is 4.216, which exceeds the critical value of 2.045.
#> 
#> Closed-Form and Empirical RIR 
#> The standard (closed-form) RIR is 16 (50.0% of the sample). To nullify
#> the inference, 16 or more cases would need to be replaced with cases
#> for which there is no effect. Across 100 resampling iterations (99
#> successful, 1 failed), the median empirical k* is 14 and the mean is
#> 14.5 (SD = 5.9, 41.9% of the median; IQR: [11, 18]). The empirical
#> median is 2 cases lower than the closed-form RIR of 16, a difference of
#> 12.5%.
#> 
#> Replacement Composition 
#> Case influence (c_i = r_x * r_y):
#> On average, 83.5% of replaced cases were supporters of the effect and
#> 14.4% were high-influence cases (top 10% by |c_i|). The average signed
#> c_i of replaced cases was 1.2604 (expected under random replacement:
#> 1.1474, a difference of 0.1130 / 9.9%).
#> 
#> RIR influence (leave-one-out k_cf change):
#> The average RIR influence of replaced cases was 0.824 (expected: 0.811,
#> a difference of 0.013 / 1.6%). High-influence cases (top 10% by |RIR
#> influence|) made up 14.4% of replaced cases on average.
#> 
#> Supplemental Plots 
#> The empirical RIR distribution and case-level influence patterns can be
#> explored further with the following plots. Use verbose = TRUE for a
#> description of what each plot shows.
#> 
#> plot_hist(result): k* distribution with replacement composition
#> plot_partial(result): per-case influence on the focal coefficient
#> plot_rir_dist(result): distribution of per-case RIR influence (psi_i)
#> plot_comp(result):  signed c_i influence composition across k* bins
#> plot_comp(result, "rir"): RIR influence composition across k* bins
# }
```
