# Function to output printed text

output_print <- function(est_eff, beta_threshhold, bias = NULL, sustain = NULL, nu, eff_thr, recase, obs_r, critical_r, r_con, itcv, alpha, index) {
  if (index == "RIR"){ 
    cat(crayon::bold("Robustness of Inference to Replacement (RIR):\n"))
    if ((abs(est_eff) > abs(beta_threshhold)) & is.na(eff_thr) == TRUE) {
      cat("TO INVALIDATE:\n")
      cat("\n")
      cat(paste0("RIR = ", round(recase, 3), "\n"))
      cat("\n")
      cat(paste0("You entered an estimated effect of ", round(est_eff, 3), ". To invalidate"))
      cat("\n")
      cat(paste0("the inference of an effect using the threshold of ", round(beta_threshhold, 3), " for"))
      cat("\n")
      cat(paste0("statistical significance with alpha = ", alpha, ", ", round(bias, 3), "% of the"))
      cat("\n")
      cat(paste0("(", round(est_eff, 3), ") estimate would have to be due to bias. This implies"))
      cat("\n")
      cat("that to invalidate the inference one would expect to have to\n")
      cat(paste0("replace ", round(recase, 3), " (", round(bias, 3), "%) observations with cases for which the"))
      cat("\n")    
      cat(paste0("treatment effect is ", nu, " (RIR = ", round(recase, 3), ").\n"))
      cat("\n")    
    } else if ((abs(est_eff) > abs(beta_threshhold)) & is.na(eff_thr) == FALSE) {
      cat("TO INVALIDATE:\n")
      cat("\n")
      cat(paste0("RIR = ", round(recase, 3), "\n"))
      cat("\n")
      cat(paste0("You entered an effect of ", round(est_eff, 3), ", and specified a threshold"))
      cat("\n")
      cat(paste0("for inference of ", round(eff_thr, 3), ". To invalidate the inference based on your"))
      cat("\n")
      cat(paste0("estimate, ", round(bias, 3), "% of the (", round(est_eff, 3), ") estimate would have to be due to"))
      cat("\n")
      cat(paste0("bias. This implies that to invalidate the inference one would"))
      cat("\n")
      cat(paste0("expect to have to replace ", round(recase, 3), " (", round(bias, 3), "%) observations with"))
      cat("\n")
      cat(paste0("cases for which the treatment effect is ", nu, " (RIR = ", round(recase, 3), ").\n"))
      cat("\n") 
    } else if ((abs(est_eff) < abs(beta_threshhold)) & is.na(eff_thr) == TRUE) {
      cat("TO SUSTAIN:\n", sep = "")
      cat("\n")
      cat(paste0("RIR = ", round(recase, 3), "\n"))
      cat("\n")
      cat(paste0("You entered an estimated effect of ", round(est_eff, 3), ". The threshold value for"))
      cat("\n")
      cat(paste0("statistical significance is ", round(beta_threshhold, 3), " (alpha = ", alpha, "). To reach that"))
      cat("\n")
      cat(paste0("threshold, ", round(sustain, 3), "% of the (", round(est_eff, 3), ") estimate would have to be due to"))
      cat("\n")
      cat("bias. This implies that to sustain an inference one would expect to")
      cat("\n")
      cat(paste0("have to replace ", round(recase, 3), " (", round(sustain, 3), "%) observations with effect of ", nu, " with\n"))
      cat(paste0("cases with effect of ", round(beta_threshhold, 3), " (RIR = ", round(recase, 3), ").\n"))
      cat("\n")
    } else if ((abs(est_eff) < abs(beta_threshhold)) & is.na(eff_thr) == FALSE) {
      cat("TO SUSTAIN:\n", sep = "")
      cat("\n")
      cat(paste0("RIR = ", round(recase, 3), "\n"))
      cat("\n")
      cat(paste0("You entered an effect size of ", round(est_eff, 3), ", and specified a threshold"))
      cat("\n")
      cat(paste0("for inference of ", round(eff_thr, 3), ". To reach that threshold, ", round(sustain, 3), "% of the" ))
      cat("\n")
      cat(paste0("(", round(est_eff, 3), ") estimate would have to be due to bias. This implies"))
      cat("\n")
      cat("that to sustain an inference one would expect to have to replace ")
      cat("\n")
      cat(paste0(round(recase, 3), " (", round(sustain, 3), "%) observations with effect of ", nu, " with cases"))
      cat("\n")
      cat(paste0("with effect of ", round(beta_threshhold, 3), " (RIR = ", round(recase, 3), ").\n"))
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
      cat("The minimum impact of an omitted variable to invalidate an inference")
      cat("\n")
      cat(paste0("for a null hypothesis of 0 effect is based on a correlation of ", r_con))
      cat("\n")
      cat(paste0("with the outcome and at ", r_con, " with the predictor of interest (conditioning"))
      cat("\n")
      cat(paste0("on all observed covariates in the model) based on a threshold of", round(critical_r, 3)))
      cat("\n")
      cat("for statistical significance (alpha = ", alpha, ").\n",
          sep = "")
      cat("\n")
      cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be ") 
      cat("\n")
      cat(r_con, " X ", r_con, " = ", round(r_con^2, 3), " to invalidate an inference for a null hypothesis of 0 effect.\n", sep = "")
    } else if (abs(obs_r) > abs(critical_r) & obs_r < 0) {
      cat("The minimum (in absolute value) impact of an omitted variable to")
      cat("\n")
      cat(paste0("invalidate an inference for a null hypothesis of 0 effect is based on"))
      cat("\n")
      cat(paste0("a correlation of ", -r_con, " with the outcome and at ", r_con, " with the predictor"))
      cat("\n")
      cat("of interest (conditioning on all observed covariates in the model;")
      cat("\n")
      cat(paste0("signs are interchangeable) based on a threshold of ", round(beta_threshhold, 3)))
      cat("\n")
      cat("for statistical significance (alpha = ", alpha, ").\n", sep = "")
      cat("\n")
      cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be ") 
      cat("\n")
      cat(-r_con, " X ", r_con, " = ", -round(r_con^2, 3), " to invalidate an inference for a null hypothesis of 0 effect.\n", sep = "")
    } else if (abs(obs_r) < abs(critical_r) & obs_r > 0) {
      cat("The maximum impact (in absolute value) of an omitted variable to") 
      cat("\n")
      cat("sustain an inference for a null hypothesis of 0 effect is based on")
      cat("\n")
      cat(paste0("a correlation of ", -r_con, " with the outcome an at ", r_con, " with the predictor"))
      cat("\n")
      cat("of interest (conditioning on all observed covariates in the model;")
      cat("\n")
      cat(paste0("signs are interchangeable) based on a threshold of ", round(beta_threshhold, 3)))
      cat("\n")
      cat("for statistical significance (alpha = ", alpha, ").\n", sep = "")
      cat("\n")
      cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be ")
      cat("\n")
      cat(-r_con, " X ", r_con, " = ", -round(r_con^2, 3), " to sustain an inference for a null hypothesis of 0 effect.\n", sep = "")
    } else if (abs(obs_r) < abs(critical_r) & obs_r < 0) {
      cat("The maximum impact of an omitted variable to sustain an inference")
      cat("\n")
      cat(paste0("for a null hypothesis of 0 effect is based on a correlation of ", r_con))
      cat("\n")
      cat(paste0("with the outcome and at ", r_con, " with the predictor of interest (conditioning"))
      cat("\n")
      cat(paste0("on all observed covariates in the model) based on a threshold of ", round(critical_r, 3)))
      cat("\n")
      cat("for statistical significance (alpha = ", alpha, ").\n", sep = "")
      cat("\n")
      cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be ")
      cat("\n")
      cat(r_con, " X ", r_con, " = ", round(r_con^2, 3), " to sustain an inference for a null hypothesis of 0 effect.\n", sep = "")
    } else if (obs_r == critical_r) {
      warning("The correlation is exactly equal to the threshold.\n")
    }
    cat("See Frank (2000) for a description of the method.")
    cat("\n")
    cat("\n")
    cat(crayon::underline("Citation:"))
    cat("\n")
    cat("Frank, K. (2000). Impact of a confounding variable on the")
    cat("\n")
    cat("inference of a regression coefficient.", crayon::italic("Sociological Methods and Research, 29"), "(2), 147-194")
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

