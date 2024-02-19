# Function to output the data frame

#' Output a Tidy Table from a Model Object
#'
#' This function takes a model object and the tested variable, 
#' tidies the model output using `broom::tidy`, 
#' calculates the impact threshold for confounding variables (ITCV) and impact 
#' for each covariate,and returns a rounded, tidy table of model outputs.
#'
#' @param model_object A model object from which to generate the output.
#' @param tested_variable The variable being tested in the model.
#' @return A tidy data frame containing model outputs, ITCV, 
#' and impacts for covariates.
#' @importFrom broom tidy
#' @importFrom purrr modify_if
#' @importFrom stats cor
#' @importFrom dplyr select filter mutate
#' @importFrom rlang !! enquo
output_table <- function(model_object, tested_variable) {
  p <- all.vars(model_object$call)[1]
  cat("Dependent variable is", p, "\n")
  model_output <- broom::tidy(model_object) # tidying output
  
  model_output$itcv <- NA
  
  var_row <- model_output$term == tested_variable
  model_output$itcv[var_row] <- abs(konfound(model_object, !!tested_variable, to_return = "raw_output")$itcv)

  covariate_names <- model_output$term[!(model_output$term %in% c("(Intercept)", tested_variable))]

  
  for (i in seq(covariate_names)) {
    cov_row <- model_output$term == covariate_names[i]
    d <- model_object$model
    cor_df <- as.data.frame(stats::cor(d))
    model_output$itcv[cov_row] <- round(abs(cor_df[cov_row, 1]) * abs(cor_df[cov_row, tested_variable]), 3) # r_zy * r_zx
  }

  model_output <- purrr::modify_if(model_output, is.numeric, round, digits = 3)

  return(model_output)
}
