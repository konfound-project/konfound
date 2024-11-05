# Function to output printed text

#' Output printed text with formatting
#'
#' This function outputs printed text for various indices such as RIR
#' (Robustness of Inference to Replacement)
#' and IT (Impact Threshold for a Confounding Variable) with specific formatting
#'  like bold, underline, and italic
#' using functions from the crayon package. It handles different scenarios based
#'  on the effect difference,
#' beta threshold, and other parameters, providing formatted
#' output for each case.
#'
#' @param n_covariates number of covariates. 
#' @param est_eff The estimated effect.
#' @param beta_threshhold The threshold value of beta, used for
#' statistical significance determination.
#' @param bias The percentage of the estimate that could be due to bias (optional).
#' @param sustain The percentage of the estimate necessary to sustain an inference (optional).
#' @param nu The hypothesized effect size used in replacement analysis.
#' @param eff_thr Threshold for estimated effect. 
#' @param recase The number of cases that need to be replaced to change the inference.
#' @param obs_r The observed correlation coefficient in the data.
#' @param critical_r The critical correlation coefficient for statistical significance.
#' @param r_con The correlation coefficient of an omitted variable with both the outcome and the predictor.
#' @param itcv The impact threshold for a confounding variable.
#' @param alpha The level of statistical significance.
#' @param index A character string indicating the index for which the output is generated ('RIR' or 'IT').
#' @param far_bound Indicator whether the threshold is towards the other side of nu or 0, by default is zero (same side), alternative is one (the other side).
#' @param sdx Standard deviation of x.
#' @param sdy Standard deviation of y. 
#' @param R2 the unadjusted, original R2 in the observed function.
#' @param rxcv the correlation between x and CV. 
#' @param rycv the correlation between y and CV. 
#' @importFrom crayon bold underline italic

output_print <- function(n_covariates,
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
                         rycvGz) {
  if (index == "RIR"){
    cat(crayon::bold("Robustness of Inference to Replacement (RIR):\n"))
    if ((abs(est_eff) > abs(beta_threshhold)) & is.na(eff_thr) == TRUE) {
      cat(paste0("RIR = ", round(recase, 3), "\n"))
      cat("\n")
      cat(paste0("To invalidate the inference of an effect using the threshold of ", round(beta_threshhold, 3), " for"))
      cat("\n")
      cat(paste0("statistical significance (with null hypothesis = ", nu, " and alpha = ", alpha, "), ", round(bias, 3), "%"))
      cat("\n")
      cat(paste0("of the (", round(est_eff, 3), ") estimate would have to be due to bias. This implies that to"))
      cat("\n")
      cat(paste0("invalidate the inference one would expect to have to replace ", round(recase, 3), " (", round(bias, 3), "%)"))
      cat("\n")
      cat(paste0("observations with data points for which the effect is ", nu, " (RIR = ", round(recase, 3), ").\n"))
      cat("\n")
    } else if ((abs(est_eff) > abs(beta_threshhold)) & is.na(eff_thr) == FALSE) {
      cat(paste0("RIR = ", round(recase, 3), "\n"))
      cat("\n")
      if ((far_bound == 0) & (est_eff * eff_thr < 0)) {
          cat(sprintf("Sign for effect threshold changed to be that of estimated effect. The threshold\n is now %.3f. Different signs would require replacement values to be arbitrarily\n more extreme than the threshold (%.3f) to achieve the threshold value.\n Consider using ITCV.", beta_threshhold, eff_thr)
              )
          cat("\n")
          cat("\n")
      }
      cat(paste0("The estimated effect is ", round(est_eff, 3), ", and specified threshold for inference is ", round(eff_thr, 3), "."))
      cat("\n")
      if ((far_bound == 0) & (est_eff * eff_thr < 0)) {
          cat("The threshold used takes the same sign as the estimated effect. See comment above.")
          cat("\n")
      }
      cat(paste0("To invalidate the inference based on your estimate, ", round(bias, 3), "% of the (", round(est_eff, 3), ")"))
      cat("\n")
      cat(paste0("estimate would have to be due to bias. This implies that to invalidate"))
      cat("\n")
      cat(paste0("the inference one would expect to have to replace ", round(recase, 3), " (", round(bias, 3), "%) observation"))
      cat("\n")
      cat(paste0("with data points for which the effect is ", nu, " (RIR = ", round(recase, 3), ").\n"))
      cat("\n")
    } else if ((abs(est_eff) < abs(beta_threshhold)) & is.na(eff_thr) == TRUE) {
      cat(paste0("RIR = ", round(recase, 3), "\n"))
      cat("\n")
      cat(paste0("The estimated effect is ", round(est_eff, 3), ". The threshold value for statistical significance"))
      cat("\n")
      cat(paste0("is ", round(beta_threshhold, 3), " (with null hypothesis = ", nu, " and alpha = ", alpha, "). To reach that threshold,"))
      cat("\n")
      cat(paste0(round(sustain, 3), "% of the (", round(est_eff, 3), ") estimate would have to be due to bias. This implies to sustain"))
      cat("\n")
      cat(paste0("an inference one would expect to have to replace ", round(recase, 3), " (", round(sustain, 3), "%) observations with"))
      cat("\n")
      cat(paste0("effect of ", nu, " with data points with effect of ", round(beta_threshhold, 3), " (RIR = ", round(recase, 3), ").\n"))
      cat("\n")
    } else if ((abs(est_eff) < abs(beta_threshhold)) & is.na(eff_thr) == FALSE) {
      cat(paste0("RIR = ", round(recase, 3), "\n"))
      cat("\n")
      if ((far_bound == 0) & (est_eff * eff_thr < 0)) {
          cat(sprintf("Sign for effect threshold changed to be that of estimated effect. The threshold\n is now %.3f. Different signs would require replacement values to be arbitrarily\n more extreme than the threshold (%.3f) to achieve the threshold value.\n Consider using ITCV.", beta_threshhold, eff_thr)
          )
          cat("\n")
          cat("\n")
      }
      cat(paste0("The estimated effect is ", round(est_eff, 3), ", and specified threshold for inference is ", round(eff_thr, 3), "."))
      cat("\n")
      if ((far_bound == 0) & (est_eff * eff_thr < 0)) {
          cat("The threshold used takes the same sign as the estimated effect. See comment above.")
          cat("\n")
      }
      cat(paste0("To reach that threshold, ", round(sustain, 3), "% of the (", round(est_eff, 3), ") estimate would have to be due"))
      cat("\n")
      cat(paste0("to bias. This implies that to sustain an inference one would expect to have"))
      cat("\n")
      cat(paste0("to replace ", round(recase, 3), " (", round(sustain, 3), "%) observations with effect of ", nu, " with data points with"))
      cat("\n")
      cat(paste0("effect of ", round(beta_threshhold, 3), " (RIR = ", round(recase, 3), ").\n"))
      cat("\n")
    } else if (est_eff == beta_threshhold) {
      warning("The coefficient is exactly equal to the threshold.\n")
    }
    cat("See Frank et al. (2013) for a description of the method.")
    cat("\n")
    cat("\n")
    cat(crayon::underline("Citation:"), "Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. (2013).")
    cat("\n")
    cat("What would it take to change an inference?")
    cat("\n")
    cat("Using Rubin's causal model to interpret the robustness of causal inferences.")
    cat("\n")
    cat(crayon::italic("Education, Evaluation and Policy Analysis, 35"), "437-460.")
    cat("\n")
    cat("\n")
    cat("Accuracy of results increases with the number of decimals reported.")
    cat("\n")


#link_html <- '<html><body><a href="https://journals.sagepub.com/doi/10.3102/0162373713493129">Click here for the article</a></body></html>'
#if (requireNamespace("htmltools", quietly = TRUE)) {
#    htmltools::html_print(htmltools::HTML(link_html))
#} else {
#    message("htmltools package is required")
#}

  }
  if (index == "IT") { 
    cat(crayon::bold("Impact Threshold for a Confounding Variable (ITCV):\n"))
    cat("\n")
    if (abs(obs_r) > abs(critical_r) & obs_r > 0) {
        
        if (!is.na(sdx) && !is.na(sdy) && !is.na(R2) && n_covariates != 0) {
            
            cat("Unconditional ITCV:")
            cat("\n")
            cat("The minimum impact of an omitted variable to invalidate an inference for")
            cat("\n")
            cat(paste0("a null hypothesis of an effect of nu (", nu, ") is based on a correlation of ", round(rycv, 3)))
            cat("\n")
            cat(paste0("with the outcome and ", round(rxcv, 3), " with the predictor of interest (BEFORE conditioning"))
            cat("\n")
            cat("on observed covariates; signs are interchangeable). This is based on a threshold")
            cat("\n")
            cat(paste0("effect of ", round(critical_r, 3), " for statistical significance (alpha = ", alpha, ").\n"))
            cat("\n")
            cat("Correspondingly the UNCONDITIONAL impact of an omitted variable (as defined in Frank 2000) must be")
            cat("\n")
            cat(paste0(round(rycv, 3), " X ", round(rxcv, 3), " = ", round(rycv * rxcv, 3), " to invalidate an inference for a null hypothesis of an effect of nu (", nu, ").\n", sep = ""))
            cat("\n")
            
            cat("Conditional ITCV:")
            cat("\n")
        }
        
      cat("The minimum impact of an omitted variable to invalidate an inference for")
      cat("\n")
      cat(paste0("a null hypothesis of an effect of nu (", nu, ") is based on a correlation of ", round(rycvGz, 3)))
      cat("\n")
      cat(paste0("with the outcome and ", round(rxcvGz, 3), " with the predictor of interest (conditioning"))
      cat("\n")
      cat("on all observed covariates in the model; signs are interchangeable). This is")
      cat("\n")
      cat(paste0("based on a threshold effect of ", round(critical_r, 3), " for statistical significance (alpha = ", alpha, ").\n"))
      cat("\n")
      cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be ") 
      cat("\n")
      cat(paste0(round(rycvGz, 3), " X ", round(rxcvGz, 3), " = ", round(rycvGz*rxcvGz, 3), " to invalidate an inference for a null hypothesis of an effect of nu (", nu, ").\n", sep = ""))
      
    } else if (abs(obs_r) > abs(critical_r) & obs_r < 0) {
        
        if (!is.na(sdx) && !is.na(sdy) && !is.na(R2) && n_covariates != 0) {
        
            cat("Unconditional ITCV:")
            cat("\n")
            cat("The minimum (in absolute value) impact of an omitted variable to invalidate")
            cat("\n")
            cat(paste0("an inference for a null hypothesis of an effect of nu (", nu, ") is based on"))
            cat("\n")
            cat(paste0("a correlation of ", round(rycv, 3), " with the outcome and ", round(rxcv, 3), " with the predictor of"))
            cat("\n")
            cat("interest (BEFORE conditioning on observed covariates; signs are interchangeable).")
            cat("\n")
            cat(paste0("This is based on a threshold effect of ", round(critical_r, 3), " for statistical significance (alpha = ", alpha, ").\n"))
            cat("\n")
            cat("Correspondingly the UNCONDITIONAL impact of an omitted variable (as defined in Frank 2000) must be") 
            cat("\n")
            cat(paste0(round(rycv, 3), " X ", round(rxcv, 3), " = ", round(rycv * rxcv, 3), " to invalidate an inference for a null hypothesis of an effect of nu (", nu, ").\n", sep = ""))
            cat("\n")
            
            cat("Conditional ITCV:")
            cat("\n")

        }
        
      cat("The minimum (in absolute value) impact of an omitted variable to invalidate")
      cat("\n")
      cat(paste0("an inference for a null hypothesis of an effect of nu (", nu, ") is based on"))
      cat("\n")
      cat(paste0("a correlation of ", round(rycvGz, 3), " with the outcome and ", round(rxcvGz, 3), " with the predictor of"))
      cat("\n")
      cat("interest (conditioning on all observed covariates in the model; signs are")
      cat("\n")
      cat(paste0("interchangeable). This is based on a threshold effect of ", round(critical_r, 3), " for statistical"))
      cat("\n")
      cat("significance (alpha = ", alpha, ").\n", sep = "")
      cat("\n")
      cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be") 
      cat("\n")
      cat(paste0(round(rycvGz, 3), " X ", round(rxcvGz, 3), " = ", round(rycvGz*rxcvGz, 3), " to invalidate an inference for a null hypothesis of an effect of nu (", nu, ").\n", sep = ""))
      
    } else if (abs(obs_r) < abs(critical_r) & obs_r >= 0) {
        
        if (!is.na(sdx) && !is.na(sdy) && !is.na(R2) && n_covariates != 0) {
        
            cat("Unconditional ITCV:")
            cat("\n")
            cat("The maximum impact (in absolute value) of an omitted variable to sustain")
            cat("\n")
            cat(paste0("an inference for a null hypothesis of an effect of nu (", nu, ") is based on"))
            cat("\n")
            cat(paste0("a correlation of ", round(rycv, 3), " with the outcome and ", round(rxcv, 3), " with the predictor"))
            cat("\n")
            cat("of interest (BEFORE conditioning on observed covariates; signs are")
            cat("\n")
            cat(paste0("interchangeable). This is based on a threshold effect of ", round(beta_threshhold, 3), " for"))
            cat("\n")
            cat("statistical significance (alpha = ", alpha, ").\n", sep = "")
            cat("\n")
            cat("Correspondingly the UNCONDITIONAL impact of an omitted variable (as defined in Frank 2000) must be ")
            cat("\n")
            cat(paste0(round(rycv, 3), " X ", round(rxcv, 3), " = ", round(rycv * rxcv, 3), " to sustain an inference for a null hypothesis of an effect of nu (", nu, ").\n", sep = ""))
            cat("\n")
            
            cat("Conditional ITCV:")
            cat("\n")

            }
      cat("The maximum impact (in absolute value) of an omitted variable to sustain")
      cat("\n")
      cat(paste0("an inference for a null hypothesis of an effect of nu (", nu, ") is based on"))
      cat("\n")
      cat(paste0("a correlation of ", round(rycvGz, 3), " with the outcome and ", round(rxcvGz, 3), " with the predictor"))
      cat("\n")
      cat("of interest (conditioning on all observed covariates in the model;")
      cat("\n")
      cat(paste0("signs are interchangeable). This is based on a threshold effect of ", round(beta_threshhold, 3)))
      cat("\n")
      cat("for statistical significance (alpha = ", alpha, ").\n", sep = "")
      cat("\n")
      cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be ")
      cat("\n")
      cat(paste0(round(rycvGz, 3), " X ", round(rxcvGz, 3), " = ", round(rycvGz*rxcvGz, 3), " to sustain an inference for a null hypothesis of an effect of nu (", nu, ").\n", sep = ""))
      
    } else if (abs(obs_r) < abs(critical_r) & obs_r < 0) {
        
        if (!is.na(sdx) && !is.na(sdy) && !is.na(R2) && n_covariates != 0) {
        
            cat("Unconditional ITCV:")
            cat("\n")
            cat("The maximum impact of an omitted variable to sustain an inference for")
            cat("\n")
            cat(paste0("a null hypothesis of an effect of nu (", nu, ") is based on a correlation of ", round(rycv, 3)))
            cat("\n")
            cat(paste0("with the outcome and ", round(rxcv, 3), " with the predictor of interest (BEFORE conditioning"))
            cat("\n")
            cat("on observed covariates; signs are interchageable). This is based on a threshold")
            cat("\n")
            cat(paste0("effect of ", round(beta_threshhold, 3), " for statistical significance (alpha = ", alpha, ").\n"))
            cat("\n")
            cat("Correspondingly the UNCONDITIONAL impact of an omitted variable (as defined in Frank 2000) must be ")
            cat("\n")
            cat(paste0(round(rycv, 3), " X ", round(rxcv, 3), " = ", round(rycv * rxcv, 3), " to sustain an inference for a null hypothesis of an effect of nu (", nu, ").\n", sep = ""))
            cat("\n")
            
            cat("Conditional ITCV:")
            cat("\n")
            }
        
      cat("The maximum impact of an omitted variable to sustain an inference for")
      cat("\n")
      cat(paste0("a null hypothesis of an effect of nu (", nu, ") is based on a correlation of ", round(rycvGz, 3)))
      cat("\n")
      cat(paste0("with the outcome and ", round(rxcvGz, 3), " with the predictor of interest (conditioning"))
      cat("\n")
      cat("on all observed covariates in the model; signs are interchageable). This is")
      cat("\n")
      cat(paste0("based on a threshold effect of ", round(beta_threshhold, 3), " for statistical significance (alpha = ", alpha, ").\n"))
      cat("\n")
      cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be ")
      cat("\n")
      cat(paste0(round(rycvGz, 3), " X ", round(rxcvGz, 3), " = ", round(rycvGz*rxcvGz, 3), " to sustain an inference for a null hypothesis of an effect of nu (", nu, ").\n", sep = ""))
      
    } else if (obs_r == critical_r) {
      warning("The correlation is exactly equal to the threshold.\n")
    }
    cat("\n")

    if (is.na(sdx) && is.na(sdy) && is.na(R2)) {
        cat("For calculation of unconditional ITCV using pkonfound(), additionally include")
        cat("\n")
        cat("the R2, sdx, and sdy as input, and request raw output.")
        cat("\n")
        cat("\n")
      }

    if (n_covariates == 0) {
        cat("Note that sdx and sdy and R2 are only used to calculate the unconditional ITCV when")
        cat("\n")
        cat("there are covariates included (number of covariates > 0).")
        cat("\n")
        cat("\n")
    }
    
    cat("See Frank (2000) for a description of the method.")
    cat("\n")
    cat("\n")
    cat(crayon::underline("Citation:"))
    cat("\n")
    cat("Frank, K. (2000). Impact of a confounding variable on the inference of a")
    cat("\n")
    cat("regression coefficient.", crayon::italic("Sociological Methods and Research, 29"), "(2), 147-194")
    cat("\n")
    cat("\n")
    cat("Accuracy of results increases with the number of decimals reported.")
    cat("\n")
    cat("\n")
    cat("The ITCV analysis was originally derived for OLS standard errors. If the")
    cat("\n")
    cat("standard errors reported in the table were not based on OLS, some caution")
    cat("\n")
    cat("should be used to interpret the ITCV.")
  }
}
