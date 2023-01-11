# Function to output printed text

output_print <- function(eff_diff, beta_threshhold, bias = NULL, sustain = NULL, nu, recase, obs_r, critical_r, r_con, itcv, alpha, index) {
  if (index == "RIR"){ 
    cat(crayon::bold("Robustness of Inference to Replacement (RIR):\n"))
    if (abs(eff_diff) > abs(beta_threshhold)) {
      cat("To invalidate an inference, ", round(bias, 3), "% of the estimate would have to be due to bias. ")
      cat("\n")
      cat("This is based on a threshold of ", round(beta_threshhold, 3), " for statistical significance (alpha = ", alpha, ").\n", sep = "")
      cat("\n")
      cat("To invalidate an inference, ", round(recase, 3), " observations would have to be replaced with cases")
      cat("\n")
      cat("for which the effect is ", nu, " (RIR = ", round(recase, 3), ").\n", sep = "")
      cat("\n")
    }
    else if (abs(eff_diff) < abs(beta_threshhold)) {
      cat("To sustain an inference, ", round(sustain, 3), "% of the estimate would have to be due to bias. ")
      cat("\n")
      cat("This is based on a threshold of ", round(beta_threshhold, 3), " for statistical significance (alpha = ", alpha, ").\n", sep = "")
      cat("\n")
      cat("To sustain an inference, ", round(recase, 3), " of the cases with ", nu, " effect would have to be replaced with cases at the threshold of inference"," (RIR = ", round(recase, 3), ").\n", sep = "")
    }
    else if (eff_diff == beta_threshhold) {
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
  } 
  if (index == "IT") { 
    cat(crayon::bold("Impact Threshold for a Confounding Variable:\n"))
    if (abs(obs_r) > abs(critical_r) & obs_r > 0) {
      cat("The minimum impact of an omitted variable to invalidate an inference for a null hypothesis of 0 effect is based on a correlation of ", r_con)
      cat("\n")
      cat(" with the outcome and at ", r_con,
          " with the predictor of interest (conditioning on observed covariates) based on a threshold of ")
      cat("\n")
      cat(round(critical_r, 3), " for statistical significance (alpha = ", alpha, ").\n",
          sep = "")
      cat("\n")
      cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be ") 
      cat("\n")
      cat(r_con, " X ", r_con, " = ", round(r_con^2, 3), " to invalidate an inference for a null hypothesis of 0 effect.\n", sep = "")
    } else if (abs(obs_r) > abs(critical_r) & obs_r < 0) {
      cat("The minimum (in absolute value) impact of an omitted variable to invalidate an inference for a null hypothesis of 0 effect is based on a correlation of ", -r_con)
      cat("\n")
      cat(" with the outcome and at ", r_con,
          " with the predictor of interest (conditioning on observed covariates; signs are interchangable) based on a threshold of ")
      cat("\n")
      cat(round(critical_r, 3), " for statistical significance (alpha = ", alpha, ").\n",
          sep = "")
      cat("\n")
      cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be ") 
      cat("\n")
      cat(-r_con, " X ", r_con, " = ", -round(r_con^2, 3), " to invalidate an inference for a null hypothesis of 0 effect.\n", sep = "")
    } else if (abs(obs_r) < abs(critical_r) & obs_r > 0) {
      cat("The maximum impact (in absolute value) of an omitted variable to sustain an inference for a null hypothesis of 0 effect is based on a correlation of ", -r_con)
      cat("\n")
      cat(" with the outcome and at ", r_con,
          " with the predictor of interest (conditioning on observed covariates; signs are interchangable) based on a threshold of ", round(beta_threshhold, 3))
      cat("\n")
      cat(" for statistical significance (alpha = ", alpha, ").\n",
          sep = "")
      cat("\n")
      cat("Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be ")
      cat("\n")
      cat(-r_con, " X ", r_con, " = ", -round(r_con^2, 3), " to sustain an inference for a null hypothesis of 0 effect.\n", sep = "")
    } else if (abs(obs_r) < abs(critical_r) & obs_r < 0) {
      cat("The maximum impact of an omitted variable to sustain an inference for a null hypothesis of 0 effect is based on a correlation of ", r_con)
      cat("\n")
      cat(" with the outcome and at ", r_con,
          " with the predictor of interest (conditioning on observed covariates) based on a threshold of ", round(beta_threshhold, 3))
      cat("\n")
      cat(" for statistical significance (alpha = ", alpha, ").\n",
          sep = "")
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
  }
}

