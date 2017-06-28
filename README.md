# rsensitivity

Package for carrying out sensitivity analysis using R

Because this package is not on [CRAN](https://cran.r-project.org/), it must be downloaded from GitHub using the following commands (note that `devtools` must be installed first):

    # install.packages("devtools")
    devtools::install_github("jrosen48/konfound")
    
Currently, one function is in-development, `pkonfound`, for published studies, this command calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient.

    konfound(unstd_beta = 2, standard_error = .4, n_obs = 100, n_covariates = 3)

    # To invalidate the inference, 60.31 % of the estimate would have to be due to bias.
    # To invalidate the inference, 60 observations would have to be replaced with cases for which there is no effect.

    pkonfound(unstd_beta = .4, standard_error = 2, n_obs = 100, n_covariates = 3)
    
    # To sustain the inference, 89.92 % of the estimate would have to be due to bias.
    # To sustain the inference, 90 of the cases with 0 effect would have to be replaced with cases at the threshold of inference.
