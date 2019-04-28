#' Perform sensitivity analysis for published studies
#' @description For published studies, this command calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient.
#' @param est_eff the estimated effect (such as an unstandardized beta coefficient or a group mean difference)
#' @param std_err the standard error of the estimate of the unstandardized regression coefficient
#' @param n_obs the number of observations in the sample
#' @param n_covariates the number of covariates in the regression model
#' @param alpha probability of rejecting the null hypothesis (defaults to 0.05)
#' @param tails integer whether hypothesis testing is one-tailed (1) or two-tailed (2; defaults to 2)
#' @param nu what hypothesis to be tested; defaults to testing whether est_eff is significantly different from 0
#' @param n_trm the number of cases associated with the treatment condition; applicable only when non_linear = TRUE
#' @param switch_trm whether to switch the treatment and control cases; defaults to FALSE; applicable only when non_linear - TRUE
#' @param non_linear whether the model is a non-linear model; defaults to FALSE
#' @param to_return whether to return a data.frame (by specifying this argument to equal "raw_output" for use in other analyses) or a plot ("plot"); default is to print ("print") the output to the console; can specify a vector of output to return
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference
#' @examples
#' pkonfound(2, .4, 100, 3)
#' pkonfound(-2.2, .65, 200, 3)
#' pkonfound(.5, 3, 200, 3)
#' pkonfound(-0.2, 0.103, 20888, 3, n_trm = 17888, non_linear = TRUE)
#'
#' pkonfound(2, .4, 100, 3, to_return = "thresh_plot")
#' pkonfound(2, .4, 100, 3, to_return = "corr_plot")
#'
#' pkonfound_output <- pkonfound(2, .4, 200, 3,
#'                               to_return = c("raw_output", "thresh_plot", "corr_plot"))
#' summary(pkonfound_output)
#' pkonfound_output$raw_output
#' pkonfound_output$thresh_plot
#' pkonfound_output$corr_plot
#' @export

pkonfound <- function(est_eff,
                      std_err,
                      n_obs,
                      n_covariates = 1,
                      alpha = .05,
                      tails = 2,
                      nu = 0,
                      n_trm = NULL,
                      switch_trm = FALSE,
                      non_linear = FALSE,
                      to_return = "print") {
  if ("table" %in% to_return) stop("a table can only be output when using konfound")

  if (non_linear == TRUE) {
      out <- test_sensitivity_ln(
          est_eff = est_eff,
          std_err = std_err,
          n_obs = n_obs,
          n_covariates = n_covariates,
          alpha = alpha,
          tails = tails,
          nu = nu,
          to_return = to_return,
          n_trm = n_trm,
          switch_trm = switch_trm
      )
  } else {
      out <- test_sensitivity(
          est_eff = est_eff,
          std_err = std_err,
          n_obs = n_obs,
          n_covariates = n_covariates,
          alpha = alpha,
          tails = tails,
          nu = nu,
          to_return = to_return
      )   
  }

  if (!is.null(out)) { # dealing with a strange print issue
    return(out)
  }

  if (to_return == "print") {
    message("For other forms of output, change `to_return` to table, raw_output, thres_plot, or corr_plot.")
  }

  message("For models fit in R, consider use of konfound().")
}
