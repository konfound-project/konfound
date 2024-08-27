## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)

## ----gh-installation, eval = FALSE--------------------------------------------
#  install.packages("konfound")

## ----eval = TRUE--------------------------------------------------------------
library(konfound)

## -----------------------------------------------------------------------------
pkonfound(est_eff = 2, std_err = .4, n_obs = 100, n_covariates = 3)

## -----------------------------------------------------------------------------
pkonfound(est_eff = 2, std_err = .4, n_obs = 100, n_covariates = 3, index = "IT")

## ----fig.width = 6, fig.height = 6--------------------------------------------
pkonfound(est_eff = 2, std_err = .4, n_obs = 100, n_covariates = 3, to_return = "thresh_plot")

## ----fig.width = 6, fig.height = 6--------------------------------------------
pkonfound(est_eff = 2, std_err = .4, n_obs = 100, n_covariates = 3, to_return = "corr_plot")

## ----fig.width = 6, fig.height = 6--------------------------------------------
pkonfound(est_eff = 2, std_err = .4, n_obs = 100, n_covariates = 3, to_return = "raw_output")

## -----------------------------------------------------------------------------
pkonfound(a = 35, b = 17, c = 17, d = 38)

## -----------------------------------------------------------------------------
my_table <- tibble::tribble(
~unsuccess, ~success,
35,         17,
17,         38,
)
pkonfound(two_by_two_table = my_table)

## -----------------------------------------------------------------------------
pkonfound(est_eff = 0.4, std_err = 0.103, 
          n_obs = 20888, n_covariates = 3, 
          n_treat = 17888, model_type = 'logistic')

## -----------------------------------------------------------------------------
m1 <- lm(mpg ~ wt + hp + qsec, data = mtcars)
m1

konfound(model_object = m1, 
         tested_variable = hp)

## -----------------------------------------------------------------------------
konfound(model_object = m1, tested_variable = wt, to_return = "table")

## -----------------------------------------------------------------------------
# View summary stats for condition variable
table(binary_dummy_data$condition)
# Fit the logistic regression model
m4 <- glm(outcome ~ condition + control, 
          data = binary_dummy_data, family = binomial)
# View the summary of the model
summary(m4)

## -----------------------------------------------------------------------------
konfound(model_object = m4, 
         tested_variable = condition,
         two_by_two = TRUE, n_treat = 55)

## -----------------------------------------------------------------------------
if (requireNamespace("lme4")) {
    library(lme4)
    m3 <- fm1 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
    konfound(m3, Days)
}

## ----eval = TRUE--------------------------------------------------------------
mkonfound_ex
mkonfound(mkonfound_ex, t, df)

## ----eval = TRUE--------------------------------------------------------------
mkonfound(mkonfound_ex, t, df, return_plot = TRUE)

