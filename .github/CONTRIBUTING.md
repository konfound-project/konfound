# Contributing to konfound

This outlines how to propose a change to konfound.

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue to notify the team.

If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).
See our guide on [how to create a great issue](https://code-review.tidyverse.org/issues/) for more advice.

You may also wish to contact the development team for bigger changes. Please see the contact information in the DESCRIPTION to do so.

### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("konfound-project/konfound", fork = TRUE)`.

*   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing.
    
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`. This is important: **even for team members, please make commits to branches, first**.

*   Ensure that all checks are passing. You can see this information within your PR (on GitHub). It will say passing or failing, and if failing, you can see what is the cause. If a check is not passing, correct the issue or contact a package maintainer for help.

*   Please run `goodpractice::gp()` to ensure code quality compliance. Some markers from this can justifiably be ignored, whereas others must be addressed. See a discussion [here](https://github.com/konfound-project/konfound/issues/50). Some things to be aware of:
    * avoiding long code lines (more than 80 characters)
    * using TRUE and FALSE instead of T and F
    * using roxygen2 syntax to import specific functions from packages
    * avoiding functions that are overly complex (i.e., avoiding high cyclomatic complexity)
 
*   For new functions or functionality, write examples and tests to cover the core of the functionality. Aim for 80% or higher test coverage for new functions. Check with `covr::package_coverage()`. 

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.
    
*   A member of the team will then review your PR.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Follow the style described in <https://style.tidyverse.org/news.html>.

### Code style

*   New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
    You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles, but please don't restyle code that has nothing to do with your PR.  

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  

## Code of Conduct

Please note that the konfound project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
