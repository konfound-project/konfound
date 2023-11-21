#' Perform sensitivity analysis on fitted models
#' @description For fitted models, this command calculates 
#' (1) how much bias there must be in an estimate to invalidate/sustain 
#' an inference; (2) the impact of an omitted variable necessary to 
#' invalidate/sustain an inference for a regression coefficient. 
#' Currently works for: models created with lm() (linear models).
#' @param model_object output from a model (currently works for: lm)
#' @param tested_variable Variable associated with the unstandardized beta 
#' coefficient to be tested
#' @inheritParams pkonfound
#' @param index whether output is RIR or IT (impact threshold); 
#' defaults to "RIR"
#' @param two_by_two whether or not the tested variable is a dichotomous 
#' variable in a GLM; if so, the 2X2 table approach is used; only works for 
#' single variables at present (so test_all = TRUE will return an error)
#' @param test_all whether to carry out the sensitivity test for all of the 
#' coefficients (defaults to FALSE)
#' @return prints the bias and the number of cases that would have to be 
#' replaced with cases for which there is no effect to invalidate the inference
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
#' m4 <- glm(outcome ~ condition, data = binary_dummy_data, 
#' family = binomial(link = "logit"))
#' konfound(m4, condition, two_by_two = TRUE, n_treat = 55)
#' 

#' @export

konfound <- function(model_object,
                     tested_variable,
                     alpha = .05,
                     tails = 2,
                     index = "RIR",
                     to_return = "print",
                     test_all = FALSE,
                     two_by_two = FALSE,
                     n_treat = NULL,
                     switch_trm = TRUE,
                     replace = "control") {
    
    # Stop messages
    if (!(class(model_object)[1] %in% c("lm", "glm", "lmerMod"))) {
        stop("konfound() is currently implemented for models estimated with
         lm(), glm(), and lme4::lmer(); consider using pkonfound() instead")
    }
    
    if ("table" %in% to_return & test_all == TRUE){
        stop("cannot return a table when test_all is set to TRUE")
    }
    # Dealing with non-standard evaluation
    
    # dealing with non-standard evaluation
    #(so unquoted names for tested_variable can be used)
    tested_variable_enquo <- rlang::enquo(tested_variable) 
    tested_variable_string <- rlang::quo_name(tested_variable_enquo)
    
    # Dispatching based on class
    if (class(model_object)[1] == "lm") {
        output <- konfound_lm(
            model_object = model_object,
            tested_variable_string = tested_variable_string,
            test_all = test_all,
            alpha = alpha,
            tails = tails,
            index = index,
            to_return = to_return
        )
        
        if (is.null(output)) {
            
        } else {
            return(output)
        }
    }
    
    if (inherits(model_object, "glm") & two_by_two == FALSE) {
        message("Note that for a non-linear model, 
            impact threshold should not be interpreted.")
        message("Note that this is only an approximation. For exact results 
            in terms of the number of cases that must be switched from treatment 
            success to treatment failure to invalidate the inference see: 
        https://msu.edu/~kenfrank/non%20linear%20replacement%20treatment.xlsm")
        message("If a dichotomous independent variable is used, consider using 
            the 2X2 table approach enabled with the argument two_by_two = TRUE")
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
    
    if (inherits(model_object, "glm") & two_by_two == TRUE) {
        
        if(is.null(n_treat)){ 
            stop("Please provide a value for 
           n_treat to use this functionality with a dichotomous predictor")
        }
        if (test_all == TRUE) {
            stop("test_all = TRUE is not supported 
                 when two_by_two is specified")
        }
        output <- konfound_glm_dichotomous(
            model_object = model_object,
            tested_variable_string = tested_variable_string,
            test_all = test_all,
            alpha = alpha,
            tails = tails,
            to_return = to_return,
            n_treat = n_treat,
            switch_trm = switch_trm,
            replace = replace
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
            index = index,
            to_return = to_return
        )
        
        message("Note that the Kenward-Roger approximation is used to 
            estimate degrees of freedom for the predictor(s) of interest. 
            We are presently working to add other methods for calculating 
            the degrees of freedom for the predictor(s) of interest. 
            If you wish to use other methods now, consider those detailed here: 
            https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#why-doesnt-lme4-display-denominator-degrees-of-freedomp-values-what-other-options-do-i-have. 
            You can then enter degrees of freedom obtained from another method 
            along with the coefficient, 
            number of observations, and number of covariates to the pkonfound() 
                function to quantify the robustness of the inference.")
        
        return(output)
    }
    
    if (!("table" %in% to_return)) {
        message("For more detailed output, 
                consider setting `to_return` to table")
    }
    
    if (test_all == FALSE) {
        message("To consider other predictors of interest, 
            consider setting `test_all` to TRUE.")
    } else {
        message("Note that presently these predictors of interest are tested 
            independently; future output may use the approach used 
                in mkonfound.")
    }
}
