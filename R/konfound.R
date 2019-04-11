#' Perform sensitivity analysis on fitted models
#' @description For fitted models, this command calculates (1) how much bias there must be in an estimate to invalidate/sustain an inference; (2) the impact of an omitted variable necessary to invalidate/sustain an inference for a regression coefficient. Currently works for: models created with lm() (linear models).
#' @param model_object output from a model (currently works for: lm)
#' @param tested_variable Variable associated with the unstandardized beta coefficient to be tested
#' @inheritParams pkonfound
#' @param test_all whether to carry out the sensitivity test for all of the coefficients (defaults to FALSE)
#' @return prints the bias and the number of cases that would have to be replaced with cases for which there is no effect to invalidate the inference
#' @importFrom rlang .data
#' @examples
#' # using lm() for linear models
#' m1 <- lm(mpg ~ wt + hp, data = mtcars)
#' konfound(m1, wt)
#' konfound(m1, wt, test_all = TRUE)
#' konfound(m1, wt, to_return = "table")
#'
#' # using glm() for non-linear models
#' if (requireNamespace("forcats")) {
#' d <- forcats::gss_cat
#'
#' d$married <- ifelse(d$marital == "Married", 1, 0)
#'
#' m2 <- glm(married ~ age, data = d, family = binomial(link = "logit"))
#' konfound(m2, age)
#' }
#'
#' # using lme4 for mixed effects (or multi-level) models
#' if (requireNamespace("lme4")) {
#' library(lme4)
#' m3 <- fm1 <- lme4::lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
#' konfound(m3, Days)
#' }
#'
#' @export

konfound <- function(model_object,
                     tested_variable,
                     alpha = .05,
                     tails = 2,
                     to_return = "print",
                     test_all = FALSE) {

  # Stop messages
  if (!(class(model_object)[1] %in% c("lm", "glm", "lmerMod"))) {
    stop("konfound() is currently implemented for models estimated with lm(), glm(), and lme4::lmer(); consider using pkonfound() instead")
  }

  if ("table" %in% to_return & test_all == TRUE) stop("cannot return a table when test_all is set to TRUE")

  # Dealing with non-standard evaluation
  tested_variable_enquo <- rlang::enquo(tested_variable) # dealing with non-standard evaluation (so unquoted names for tested_variable can be used)
  tested_variable_string <- rlang::quo_name(tested_variable_enquo)

  # Dispatching based on class
  if (class(model_object)[1] == "lm") {
    output <- konfound_lm(
      model_object = model_object,
      tested_variable_string = tested_variable_string,
      test_all = test_all,
      alpha = alpha,
      tails = tails,
      to_return = to_return
    )

    if (is.null(output)) {

    } else {
      return(output)
    }
  }

  if (inherits(model_object, "glm")) {
    message("Note that for a non-linear model, impact threshold should not be interpreted.")

    output <- konfound_glm(
      model_object = model_object,
      tested_variable_string = tested_variable_string,
      test_all = test_all,
      alpha = alpha,
      tails = tails,
      to_return = to_return
    )

    return(output)
  }

  if (inherits(model_object, "lmerMod")) {
    output <- konfound_lmer(
      model_object = model_object,
      tested_variable_string = tested_variable_string,
      test_all = test_all,
      alpha = alpha,
      tails = tails,
      to_return = to_return
    )

    message("Note that the Kenward-Roger approximation is used to estimate degrees of freedom for the predictor(s) of interest. We are presently working to add other methods for calculating the degrees of freedom for the predictor(s) of interest. If you wish to use other methods now, consider those detailed here: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#why-doesnt-lme4-display-denominator-degrees-of-freedomp-values-what-other-options-do-i-have. You can then enter degrees of obtained from another method along with the coefficient, number of observations, and number of covariates to the pkonfound() function to carry out the sensitivity analysis.")

    return(output)
  }

  if (!("table" %in% to_return)) {
    message("For more detailed output, consider setting `to_return` to table")
  }

  if (test_all == FALSE) {
    message("To consider other predictors of interest, consider setting `test_all` to TRUE.")
  } else {
    message("Note that presently these predictors of interest are tested independently; future output may use the approach used in mkonfound.")
  }
}
