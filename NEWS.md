# konfound 1.0.3 (IN DEVELOPMENT)

* fix sign problems in print output for some cases 
* separate out conditional RIR from COP
* add the ability to enter a confidence interval instead of an estimeted effect

# konfound 1.0.2

* edits to README and vignette
* small edit to DESCRIPTION

# konfound 1.0.1

* minor edits in advance of CRAN submit

# konfound 1.0.0

* major release of changes that were being made in the "newitcv" branch:
   * Includes option to specify non-zero null hypotheses
   * Includes option to directly specify threshold for inference
   * Improved output statements
   * Includes full raw results for RIR and ITCV
   * Calculation for unconditional ITCV when possible

# konfound 0.5.1

* minor patch for CRAN

# konfound 0.5.0

* edits in response to JOSS feedback, specifically:
    * improved testing suite
    * removal of test_all = TRUE to deal with high cyclomatic complexity
    * improvement of coding style to be consistent and in accordance with good practice

# konfound 0.4.0

* major updates in advance of initial submission to the R Journal

# konfound 0.3.1

* address minor bug introduced in the `index` argument

# konfound 0.3.0

* integrate non-linear functions in `tkonfound()` into `pkonfound()` and `konfound()`

# konfound 0.2.1

* Refinements and bug fixes for the non-linear functions

# konfound 0.2.0

* Update to the non-linear functions.

# konfound 0.1.2

* Thanks to J. Murphy for pointing out a bug in how mkonfound works for lme4 output, a bug in the code of `konfound-lm` related to when the message is displayed when all coefficients are tested, and suggesting to add the name of the variable to the data frame returned when all variables are tested

# konfound 0.1.1

* Update license to include our names

# konfound 0.1.0

* Added a `NEWS.md` file to track changes to the package.
