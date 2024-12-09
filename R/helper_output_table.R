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
#' @importFrom ppcor pcor
#' @importFrom dplyr select filter mutate arrange
#' @importFrom magrittr %>%
#' @importFrom rlang !! enquo
output_table <- function(model_object, tested_variable) {
  p <- all.vars(model_object$call)[1]
  cat("Dependent variable is", p, "\n")
  model_output <- broom::tidy(model_object) # tidying output
  
  model_output$itcv <- NA
  
  var_row <- model_output$term == tested_variable
  model_output$itcv[var_row] <- abs(konfound(model_object, 
                                             !!tested_variable, 
                                             to_return = "raw_output")$itcv)

  covariate_names <- model_output$term[
    !(model_output$term %in% c("(Intercept)", tested_variable))]

  
  for (i in seq(covariate_names)) {
    cov_row <- model_output$term == covariate_names[i]
    d <- model_object$model
    cor_df <- as.data.frame(stats::cor(d))
    model_output$itcv[cov_row] <- round(
      abs(cor_df[cov_row, 1]) * abs(cor_df[cov_row, tested_variable]), 
      3) # r_zy * r_zx
  }

  model_output <- purrr::modify_if(model_output, 
                                   is.numeric, 
                                   round, 
                                   digits = 3)

  options(pillar.neg = FALSE)
  
  # Observed Impact Table
  impact_table <- tibble(
      term = covariate_names,
      `Cor(vX)` = NA_real_,
      `Cor(vY)` = NA_real_,
      Impact = NA_real_
  )
  
  for (i in seq_along(covariate_names)) {
      covariate <- covariate_names[i]
      if (all(c(covariate, tested_variable, p) %in% colnames(cor_df))) {
          cor_vx <- cor_df[covariate, tested_variable]
          cor_vy <- cor_df[covariate, p]
          impact <- cor_vx * cor_vy
          impact_table$`Cor(vX)`[i] <- round(cor_vx, 4)
          impact_table$`Cor(vY)`[i] <- round(cor_vy, 4)
          impact_table$Impact[i] <- round(impact, 4)
      }
  }
  
  # Partial Impact Table
  impact_table_partial <- tibble(
      term = covariate_names,
      `Partial Cor(vX)` = NA_real_,
      `Partial Cor(vY)` = NA_real_,
      Partial_Impact = NA_real_
  )
  
  # Compute partial correlations with error handling
  for (i in seq_along(covariate_names)) {
      covariate <- covariate_names[i]
      tryCatch({
          pcor_vx <- suppressWarnings(ppcor::pcor(model_object$model[, c(covariate, tested_variable, covariate_names)])$estimate[1, 2])
          pcor_vy <- suppressWarnings(ppcor::pcor(model_object$model[, c(covariate, p, covariate_names)])$estimate[1, 2])
          partial_impact <- pcor_vx * pcor_vy
          impact_table_partial$`Partial Cor(vX)`[i] <- round(pcor_vx, 4)
          impact_table_partial$`Partial Cor(vY)`[i] <- round(pcor_vy, 4)
          impact_table_partial$Partial_Impact[i] <- round(partial_impact, 4)
      }, error = function(e) {
          # Handle errors during partial correlation computation
          impact_table_partial$`Partial Cor(vX)`[i] <- NA
          impact_table_partial$`Partial Cor(vY)`[i] <- NA
          impact_table_partial$Partial_Impact[i] <- NA
      })
  }
  
  # Sort Partial Impact Table in descending order
  impact_table_partial <- impact_table_partial %>% dplyr::arrange(desc(Partial_Impact))
  
  cat(paste0("X represents ", tested_variable, ", Y represents ", p, 
             ", v represents each covariate.\n",
             "First table is based on unconditional correlations, second table is based on\n",
             "partial correlations.\n\n"))
  
  # Check if any row has all Partial Correlation components as NA
  if (any(is.na(impact_table_partial$`Partial Cor(vX)`) &
          is.na(impact_table_partial$`Partial Cor(vY)`) &
          is.na(impact_table_partial$Partial_Impact))) {
      stop(
          paste0(
              "Numerical instability detected in partial correlation. This indicates potential multicollinearity or scaling issues.\n",
              "To resolve this issue, consider:\n",
              "1) Standardize predictors with scale().\n",
              "2) Remove or combine highly correlated predictors.\n",
              "3) Apply regularization (e.g., ridge regression)."
          )
      )
  }
  
  # Return all three tables as a list
  return(list(Main_Output = model_output, Unconditional_Impact = impact_table, Partial_Impact = impact_table_partial))
  }
