# Changelog

## konfound 1.0.4 (IN DEVELOPMENT)

- add beta version for the VAM model
- add RIR index calculation for the PSE (component correlation) function
- include a global message linking to the konfound website and practical
  guide
- fix issue where “NULL” appeared at the end of printed output for
  konfound commands
- add delta threshold for statistical significance calculation and
  printed text for COP function
- add correlation-based RIR for logistic regression with continuous
  focal predictor

## konfound 1.0.3

CRAN release: 2025-05-28

- fix sign problems in print output for some cases
- separate out conditional RIR from COP
- add the ability to enter a confidence interval instead of an estimated
  effect
- add calculation code and print output for RIR/ITCV benchmark
  (pkonfound)
- add impact table for konfound
- update help file for pkonfound

## konfound 1.0.2

CRAN release: 2024-10-17

- edits to README and vignette
- small edit to DESCRIPTION

## konfound 1.0.1

CRAN release: 2024-10-07

- minor edits in advance of CRAN submit

## konfound 1.0.0

- major release of changes that were being made in the “newitcv” branch:
  - Includes option to specify non-zero null hypotheses
  - Includes option to directly specify threshold for inference
  - Improved output statements
  - Includes full raw results for RIR and ITCV
  - Calculation for unconditional ITCV when possible

## konfound 0.5.1

CRAN release: 2024-04-12

- minor patch for CRAN

## konfound 0.5.0

CRAN release: 2024-03-18

- edits in response to JOSS feedback, specifically:
  - improved testing suite
  - removal of test_all = TRUE to deal with high cyclomatic complexity
  - improvement of coding style to be consistent and in accordance with
    good practice

## konfound 0.4.0

CRAN release: 2021-06-01

- major updates in advance of initial submission to the R Journal

## konfound 0.3.1

- address minor bug introduced in the `index` argument

## konfound 0.3.0

CRAN release: 2020-12-17

- integrate non-linear functions in
  [`tkonfound()`](https://konfound-it.org/konfound/reference/tkonfound.md)
  into
  [`pkonfound()`](https://konfound-it.org/konfound/reference/pkonfound.md)
  and
  [`konfound()`](https://konfound-it.org/konfound/reference/konfound.md)

## konfound 0.2.1

CRAN release: 2020-02-26

- Refinements and bug fixes for the non-linear functions

## konfound 0.2.0

- Update to the non-linear functions.

## konfound 0.1.2

CRAN release: 2019-04-12

- Thanks to J. Murphy for pointing out a bug in how mkonfound works for
  lme4 output, a bug in the code of `konfound-lm` related to when the
  message is displayed when all coefficients are tested, and suggesting
  to add the name of the variable to the data frame returned when all
  variables are tested

## konfound 0.1.1

CRAN release: 2019-01-21

- Update license to include our names

## konfound 0.1.0

CRAN release: 2018-04-06

- Added a `NEWS.md` file to track changes to the package.
