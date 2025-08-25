#' Konfound Analysis for Various Model Types
#'
#' Performs sensitivity analysis on fitted models including
#' linear models (`lm`), generalized linear models (`glm`),
#' and linear mixed-effects models (`lmerMod`).
#' It calculates the amount of bias required to invalidate or
#' sustain an inference,and the impact of an omitted variable
#' necessary to affect the inference. For a full description of 
#' the command’s usage and additional examples, please refer to 
#' our \href{https://konfound-it.org/page/guide/}{practical guide}.
#'
#' @param model_object A model object produced by `lm`, `glm`, or `lme4::lmer`.
#' @param tested_variable Variable associated with the coefficient to be tested.
#' @param alpha Significance level for hypothesis testing.
#' @param tails Number of tails for the test (1 or 2).
#' @param index Type of sensitivity analysis ('RIR' by default).
#' @param to_return Type of output to return ('print', 'raw_output', 'table').
#' @param two_by_two Boolean; if `TRUE`, uses a 2x2 table approach
#' for `glm` dichotomous variables.
#' @param n_treat Number of treatment cases
#' (used only if `two_by_two` is `TRUE`).
#' @param switch_trm Boolean; switch treatment and control in the analysis.
#' @param replace Replacement method for treatment cases ('control' by default).
#' @return Depending on `to_return`, prints the result, returns a raw output,
#' or a summary table.
#' @importFrom rlang enquo quo_name
#' @importFrom lme4 fixef lmer
#' @importFrom broom tidy glance
#' @importFrom dplyr filter select bind_cols
#' @importFrom purrr map_dbl
#' @importFrom pbkrtest get_Lb_ddf
#' @examples
#' # using lm() for linear models
#' m1 <- lm(mpg ~ wt + hp, data = mtcars)
#' konfound(m1, wt)
#' konfound(m1, wt, to_return = "table")
#'
#' # using glm() for non-linear models
#' if (requireNamespace("forcats")) {
#'   d <- forcats::gss_cat
#'
#'   d$married <- ifelse(d$marital == "Married", 1, 0)
#'
#'   m2 <- glm(married ~ age, data = d, family = binomial(link = "logit"))
#'   konfound(m2, age)
#' }
#'
#' # using lme4 for mixed effects (or multi-level) models
#' if (requireNamespace("lme4")) {
#'   library(lme4)
#'   m3 <- fm1 <- lme4::lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
#'   konfound(m3, Days)
#' }
#' 
#' m4 <- glm(outcome ~ condition, data = binary_dummy_data, family = binomial(link = "logit"))
#' konfound(m4, condition, two_by_two = TRUE, n_treat = 55)
#' 

#' @export

konfound <- function(model_object,
                     tested_variable,
                     alpha = .05,
                     tails = 2,
                     index = "RIR",
                     to_return = "print",
                     two_by_two = FALSE,
                     n_treat = NULL,
                     switch_trm = TRUE,
                     replace = "control") {
  
  # Stop messages
  if (!(class(model_object)[1] %in% c("lm", "glm", "lmerMod"))) {
    stop("konfound() is currently implemented for models estimated with
         lm(), glm(), and lme4::lmer(); consider using pkonfound() instead")
  }
  
  # Dealing with non-standard evaluation
  tested_variable_enquo <- rlang::enquo(tested_variable)
  # dealing with non-standard evaluation
  #(so unquoted names for tested_variable can be used)
  tested_variable_string <- rlang::quo_name(tested_variable_enquo)
  
  # Dispatching based on class
  if (class(model_object)[1] == "lm") {
    output <- konfound_lm(
      model_object = model_object,
      tested_variable_string = tested_variable_string,
      alpha = alpha,
      tails = tails,
      index = index,
      to_return = to_return
    )
    
    #return(output)
  }
  
  if (inherits(model_object, "glm") & two_by_two == FALSE) {
    message("Note that if your model is a logistic regression, we recommend using the pkonfound command for logistic regression with manually entered parameter estimates and other quantities.")
    message("Note that this is only an approximation. For exact results in terms of the number of cases that must be switched from treatment success to treatment failure to invalidate the inference see: https://msu.edu/~kenfrank/non%20linear%20replacement%20treatment.xlsm")
    message("If a dichotomous independent variable is used, consider using the 2X2 table approach enabled with the argument two_by_two = TRUE")
    output <- konfound_glm(
      model_object = model_object,
      tested_variable_string = tested_variable_string,
      alpha = alpha,
      tails = tails,
      to_return = to_return
    )
    
    #return(output)
  } 
  
  if (inherits(model_object, "glm") & two_by_two == TRUE) {
    
    if(is.null(n_treat)) stop("Please provide a value for n_treat to use
                              this functionality with a dichotomous predictor")

    output <- konfound_glm_dichotomous(
      model_object = model_object,
      tested_variable_string = tested_variable_string,
      alpha = alpha,
      tails = tails,
      to_return = to_return,
      n_treat = n_treat,
      switch_trm = switch_trm,
      replace = replace
    )
    
    #return(output)
    
  }
  
  if (inherits(model_object, "lmerMod")) {
    output <- konfound_lmer(
      model_object = model_object,
      tested_variable_string = tested_variable_string,
      alpha = alpha,
      tails = tails,
      index = index,
      to_return = to_return
    )
    
    message("Note that the Kenward-Roger approximation is used to
            estimate degrees of freedom for the predictor(s) of interest.
            We are presently working to add other methods for calculating
            the degrees of freedom for the predictor(s) of interest.
            If you wish to use other methods now, consider those detailed here:
            https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
            #why-doesnt-lme4-display-denominator-degrees-of-freedomp-values-what-other-options-do-i-have.
            You can then enter degrees of freedom obtained from another method along with the coefficient,
            number of observations, and number of covariates to the pkonfound() function to quantify the robustness of the inference.")
    
    #return(output)
  }
  
  if (!("table" %in% to_return)) {
    cat("\n")
    message("For more detailed output, consider setting `to_return` to table")
  }
  
  cat("\n")
  message("For more information, visit https://konfound-it.org")
  message(paste0("To explore examples and interpretation tips,\n",
                 "see our Practical Guide at https://konfound-it.org/page/guide/")
          )
  
  if (to_return == "table") {
      return(output)        # Allow output to display in console
  } else {
      return(invisible(output))  # Don't print NULL for print mode
  }  
}
