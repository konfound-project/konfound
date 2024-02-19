# Function to output printed text

output_print <- function(est_eff, beta_threshhold, bias = NULL, sustain = NULL, nu, eff_thr, recase, obs_r, critical_r, r_con, itcv, alpha, index) {
  if (index == "RIR"){ 
    cat(crayon::bold("Robustness of Inference to Replacement (RIR):\n"))
    if ((abs(est_eff) > abs(beta_threshhold)) & is.na(eff_thr) == TRUE) {
      cat("TO INVALIDATE:\n")
      cat("\n")
      cat(paste0("RIR = ", recase, "\n"))
      cat("\n")
      cat(sprintf("You entered an estimated effect of %.3f. To invalidate\n", est_eff))
      cat(sprintf("the inference of an effect using the threshold of %.3f for\n", beta_threshhold))
      cat(sprintf("statistical significance with alpha = %s, %.3f%% of the\n", alpha, bias))
      cat(sprintf("(%.3f) estimate would have to be due to bias. This implies\n", est_eff))
      cat("that to invalidate the inference one would expect to have to\n")
      cat(sprintf("replace %.0f (%.3f%%) observations with cases for which the\n", recase, bias))
      cat(sprintf("treatment effect is %s (RIR = %.0f).\n\n", nu, recase))
    } else if ((abs(est_eff) > abs(beta_threshhold)) & is.na(eff_thr) == FALSE) {
      cat("TO INVALIDATE:\n")
      cat("\n")
      cat(paste0("RIR = ", recase, "\n"))
      cat(sprintf("You entered an effect of %.3f, and specified a threshold\n", est_eff))
      cat(sprintf("for inference of %.3f. To invalidate the inference based on your\n", eff_thr))
      cat(sprintf("estimate, %.3f%% of the (%.3f) estimate would have to be due to\n", bias, est_eff))
      cat("bias. This implies that to invalidate the inference one would\n")
      cat(sprintf("expect to have to replace %.0f (%.3f%%) observations with\n", recase, bias))
      cat(sprintf("cases for which the treatment effect is %s (RIR = %.0f).\n\n", nu, recase))
    } else if ((abs(est_eff) < abs(beta_threshhold)) & is.na(eff_thr) == TRUE) {
      cat("TO SUSTAIN:\n", sep = "")
      cat("\n")
      cat(paste0("RIR = ", recase, "\n"))
      cat(sprintf("You entered an estimated effect of %.3f. The threshold value for\n", est_eff))
      cat(sprintf("statistical significance is %.3f (alpha = %s). To reach that\n", beta_threshhold, alpha))
      cat(sprintf("threshold, %.3f%% of the (%.3f) estimate would have to be due to\n", sustain, est_eff))
      cat("bias. This implies that to sustain an inference one would expect to\n")
      cat(sprintf("have to replace %.0f (%.3f%%) observations with effect of %s with\n", recase, sustain, nu))
      cat(sprintf("cases with effect of %.3f (RIR = %.0f).\n\n", beta_threshhold, recase))
    } else if ((abs(est_eff) < abs(beta_threshhold)) & is.na(eff_thr) == FALSE) {
      cat("TO SUSTAIN:\n", sep = "")
      cat("\n")
      cat(paste0("RIR = ", recase, "\n"))
      cat("\n")
      cat(sprintf("You entered an effect size of %.3f, and specified a threshold\n", est_eff))
      cat(sprintf("for inference of %.3f. To reach that threshold, %.3f%% of the\n", eff_thr, sustain))
      cat(sprintf("(%.3f) estimate would have to be due to bias. This implies\n", est_eff))
      cat("that to sustain an inference one would expect to have to replace\n")
      cat(sprintf("%.0f (%.3f%%) observations with effect of %s with cases\n", recase, sustain, nu))
      cat(sprintf("with effect of %.3f (RIR = %.0f).\n\n", beta_threshhold, recase))
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


link_html <- '<html><body><a href="https://journals.sagepub.com/doi/10.3102/0162373713493129">Click here for the article</a></body></html>'
if (requireNamespace("htmltools", quietly = TRUE)) {
    htmltools::html_print(htmltools::HTML(link_html))
} else {
    message("htmltools package is required")
}
   
  } 
  if (index == "IT") { 
    cat(crayon::bold("Impact Threshold for a Confounding Variable:\n"))
    cat("\n")
    if (abs(obs_r) > abs(critical_r) & obs_r > 0) {
      cat("The minimum impact of an omitted variable to invalidate an inference\n")
      cat("for a null hypothesis of 0 effect is based on a correlation of ", sprintf("%.3f", r_con), "\n")
      cat("with the outcome and at ", sprintf("%.3f", r_con), " with the predictor of interest (conditioning\n")
      cat("on all observed covariates in the model) based on a threshold of ", sprintf("%.3f", critical_r), "\n")
      cat("for statistical significance (alpha = ", alpha, ").\n\n")
      cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be\n")
      cat(sprintf("%.3f X %.3f = %.3f", r_con, r_con, r_con^2), " to invalidate an inference for a null hypothesis of 0 effect.\n\n")
  } else if (abs(obs_r) > abs(critical_r) & obs_r < 0) {
      cat("The minimum (in absolute value) impact of an omitted variable to\n")
      cat("invalidate an inference for a null hypothesis of 0 effect is based on\n")
      cat("a correlation of ", sprintf("%.3f", -r_con), " with the outcome and at ", sprintf("%.3f", r_con), " with the predictor\n")
      cat("of interest (conditioning on all observed covariates in the model;\n")
      cat("signs are interchangeable) based on a threshold of ", sprintf("%.3f", beta_threshhold), " for\n")
      cat("statistical significance (alpha = ", alpha, ").\n\n")
      cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be\n")
      cat(sprintf("%.3f X %.3f = %.3f", -r_con, r_con, -r_con^2), " to invalidate an inference for a null hypothesis of 0 effect.\n\n")
  } else if (abs(obs_r) < abs(critical_r) & obs_r > 0) {
      cat("The maximum impact (in absolute value) of an omitted variable to\n")
      cat("sustain an inference for a null hypothesis of 0 effect is based on\n")
      cat("a correlation of ", sprintf("%.3f", -r_con), " with the outcome an at ", sprintf("%.3f", r_con), " with the predictor\n")
      cat("of interest (conditioning on all observed covariates in the model;\n")
      cat("signs are interchangeable) based on a threshold of ", sprintf("%.3f", beta_threshhold), " for\n")
      cat("statistical significance (alpha = ", alpha, ").\n\n")
      cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be\n")
      cat(sprintf("%.3f X %.3f = %.3f", -r_con, r_con, -r_con^2), " to sustain an inference for a null hypothesis of 0 effect.\n\n")
  } else if (abs(obs_r) < abs(critical_r) & obs_r < 0) {
      cat("The maximum impact of an omitted variable to sustain an inference\n")
      cat("for a null hypothesis of 0 effect is based on a correlation of ", sprintf("%.3f", r_con), "\n")
      cat("with the outcome and at ", sprintf("%.3f", r_con), " with the predictor of interest (conditioning\n")
      cat("on all observed covariates in the model) based on a threshold of ", sprintf("%.3f", beta_threshhold), "\n")
      cat("for statistical significance (alpha = ", alpha, ").\n\n")
      cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be\n")
      cat(sprintf("%.3f X %.3f = %.3f", r_con, r_con, r_con^2), " to sustain an inference for a null hypothesis of 0 effect.\n\n")
    } else if (obs_r == critical_r) {
      warning("The correlation is exactly equal to the threshold.\n")
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

link_html <- '<html><body><a href="https://journals.sagepub.com/doi/10.1177/0049124100029002001">Click here for the article</a></body></html>'
if (requireNamespace("htmltools", quietly = TRUE)) {
  # Redirect output to null
  sink("/dev/null")
  invisible(htmltools::html_print(htmltools::HTML(link_html)))
  sink()
} else {
  message("htmltools package is required")
}


  }
    
}

