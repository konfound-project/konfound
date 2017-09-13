
# # library(konfound)
# # 
# # # pkonfound for published studies
# # pkonfound(-3, .4, 100, 1)
# # 
# # ?help
# # 
# # ?pkonfound
# # 
# # help("pkonfound")
# # 
# # # linear model
# # 
# # m1 <- lm(mpg ~ wt + hp, data = mtcars)
# # 
# # 
# # arm::display(m1)
# 

alpha = .05
tails = 2
n_obs = 100
n_covariates = 1
standard_error = .4
unstd_beta = 2

critical_t <- qt(1 - (alpha / tails), n_obs - n_covariates) * -1 
beta_threshhold <- critical_t * standard_error

unstd_beta
beta_threshhold

pkonfound(.4, 2, 100, 3, to_return = "df")

# df <- tibble::tribble(
#     ~unstd_beta, ~standard_error, ~n_obs, ~n_covariates,
#     2,           .3,              70,      3,
#     10,          2.9,             405,     4,
#     1.7,         1.5,             200,     1,
#     -3,          1.3,             125,     2
# )
# 
# mkonfound(df)

# 
# args <- list(as.list(pull(df, 1)), as.list(pull(df, 2)), as.list(pull(df, 3)), as.list(pull(df, 4)))
# 
# args
# 
# pmap()
# 
# 
# 
# pmap(df)
# 
# x <- list(1, 10, 100)
# y <- list(1, 2, 3)
# z <- list(5, 50, 500)
# 
# pkonfound(2, .4, 100, 3)
# pkonfound(.4, 2, 100, 3)
