# t_sensitivity <- function(unstd_beta,
#                           std_err,
#                           n_obs,
#                           n_covariates,
#                           alpha,
#                           tails,
#                           nu,
#                           to_return) {
#     
#     # NOTE: the next two parts are the same for both approaches
#     
#     # calculating statistics used in every case
#     if (unstd_beta < 0) {
#         critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 2) * -1 }
#     else {critical_t <- stats::qt(1 - (alpha / tails), n_obs - n_covariates - 2) }
#     
#     # dealing with cases where hypotheses other than whether unstd_beta differs from 0
#     if (nu != 0) {
#         beta_diff <- abs(unstd_beta - nu) } else {
#             beta_diff <- unstd_beta - 0 } # this is just to make what this is doing evident
#     
#     # transforming t into r and finding critical t
#     obs_r <- (beta_diff / std_err) / sqrt(((n_obs - n_covariates - 3) + ((beta_diff / std_err) ^ 2)))
#     critical_r <- critical_t / sqrt((critical_t ^ 2) + (n_obs - n_covariates - 3))
#     # calculating threshold
#     if ((abs(obs_r) > abs(critical_r)) & ((obs_r * critical_r) > 0)) {
#         mp <- -1
#     } else { 
#         mp <- 1
#     }
#     itcv <- (obs_r - critical_r) / (1 + mp * abs(critical_r))
#     # finding correlation of confound to invalidate / sustain inference
#     r_con <- round(sqrt(abs(itcv)), 3)
#     
#     # also need to add component correlations
#     
#     # output
#     if (abs(obs_r) > abs(critical_r)) {
#         cat("An omitted variable would have to be correlated at", r_con, "with the outcome and at", r_con, "with the predictor of interest (conditioning on observed covariates) to invalidate an inference.") }
#     else if (abs(obs_r) < abs(critical_r)) {
#         cat("An omitted variable would have to be correlated at", r_con, "with the outcome and at", r_con, "with the predictor of interest (conditioning on observed covariates) to sustain an inference.") }
#     else if (r_con == itcv) {
#         warning("The correlation is exactly equal to the threshold.") }
# 
# }
# 
# t_sensitivity(-2, 3, 100, 3, .05, 2, 0)
# t_sensitivity(.4, 2, 100,0, .05, 2, 0)