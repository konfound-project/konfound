# Output printed text with formatting

This function outputs printed text for various indices such as RIR
(Robustness of Inference to Replacement) and IT (Impact Threshold for a
Confounding Variable) with specific formatting like bold, underline, and
italic using functions from the crayon package. It handles different
scenarios based on the effect difference, beta threshold, and other
parameters, providing formatted output for each case.

## Usage

``` r
output_print(
  n_covariates,
  est_eff,
  beta_threshhold,
  bias = NULL,
  sustain = NULL,
  nu,
  eff_thr,
  recase,
  obs_r,
  critical_r,
  r_con,
  itcv,
  alpha,
  index,
  far_bound,
  sdx = NA,
  sdy = NA,
  R2 = NA,
  rxcv = NA,
  rycv = NA,
  rxcvGz,
  rycvGz,
  benchmark_corr_product = NA,
  itcv_ratio_to_benchmark = NA
)
```

## Arguments

- n_covariates:

  number of covariates.

- est_eff:

  The estimated effect.

- beta_threshhold:

  The threshold value of beta, used for statistical significance
  determination.

- bias:

  The percentage of the estimate that could be due to bias (optional).

- sustain:

  The percentage of the estimate necessary to sustain an inference
  (optional).

- nu:

  The hypothesized effect size used in replacement analysis.

- eff_thr:

  Threshold for estimated effect.

- recase:

  The number of cases that need to be replaced to change the inference.

- obs_r:

  The observed correlation coefficient in the data.

- critical_r:

  The critical correlation coefficient for statistical significance.

- r_con:

  The correlation coefficient of an omitted variable with both the
  outcome and the predictor.

- itcv:

  The impact threshold for a confounding variable.

- alpha:

  The level of statistical significance.

- index:

  A character string indicating the index for which the output is
  generated ('RIR' or 'IT').

- far_bound:

  Indicator whether the threshold is towards the other side of nu or 0,
  by default is zero (same side), alternative is one (the other side).

- sdx:

  Standard deviation of x.

- sdy:

  Standard deviation of y.

- R2:

  the unadjusted, original R2 in the observed function.

- rxcv:

  the correlation between x and CV.

- rycv:

  the correlation between y and CV.

- rxcvGz:

  the correlation between predictor of interest and CV necessary to
  nullify the inference for smallest impact, conditioning on all
  observed covariates.

- rycvGz:

  the correlation between outcome and CV necessary to nullify the
  inference for smallest impact, conditioning on all observed
  covariates.

- benchmark_corr_product:

  the product of the correlations of covariates Z with X and Y (Rxz \*
  Ryz), measuring the observed association strength.

- itcv_ratio_to_benchmark:

  the ratio of the ITCV to the benchmark_corr_product, indicating the
  robustness of inference.
