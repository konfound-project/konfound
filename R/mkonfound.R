#' Perform meta-analyses including sensitivity analysis
#' @description For fitted models, this command carries out sensitivity analysis for a number of models, when their parameters stored in a data.frame. 
#' @param df a data.frame with columns for the 'unstd_beta', 'std_err', 'n_obs', and 'n_covariates' arguments (see ?pkonfound for more information on these)
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @param return_plot whether to return a plot of the percent bias; defaults to FALSE
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference for each of the cases in the data.frame
#' @examples 
#' df <- data.frame(unstd_beta = c(2, 10, 1.7),
#'                  std_error = c(.3, 2.9, 1.5), 
#'                  n_obs = c(70, 405, 200), 
#'                  n_covs = c(3, 4, 1))
#' mkonfound(df)
#' @export

mkonfound <- function(df, alpha = .05, tails = 2, return_plot = FALSE) {
    args <- list(as.list(dplyr::pull(df, 1)), 
                 as.list(dplyr::pull(df, 2)), 
                 as.list(dplyr::pull(df, 3)), 
                 as.list(dplyr::pull(df, 4)))
    x <- purrr::pmap_dfr(args, pkonfound, alpha = alpha, tails = tails, to_return = "df")
    results_df <- dplyr::bind_cols(df, x)
    results_df <- dplyr::select(results_df, -unstd_beta1)
    if (return_plot == TRUE) {
        to_plot <- dplyr::filter(results_df, inference == "to_invalidate")
        p <- ggplot2::ggplot(to_plot, ggplot2::aes(x = percent_bias)) +
            ggplot2::geom_histogram(color = "#1F78B4") +
            ggplot2::theme_bw()
        return(p)
    }
    return(results_df)
} 
