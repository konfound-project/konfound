# Sensitivity Analysis is an Important Causal Inference Technique

Sensitivity analysis, statistical methods which quantify the conditions
necessary to alter inferences, are becoming increasingly important to a
variety of disciplines within the quantitative sciences, including
causal inference (Razavi, et al., 2021). A series of recent works within
the causal inference framework (Frank 2000; Frank et al., 2013), extends
previous sensitivity analyses by considering the characteristics of
omitted variables or unobserved cases that would alter an inference if
such variables or cases were otherwise observed. These sensitivity
analyses within the causal inference framework would result in research
write-ups such as “an omitted variable must have a xx correlation with
the predictor/treatment and outcome variable to invalidate the inference
of the presence of a treatment effect” when investigating the impact of
a confounding variable. Furthermore, causal inference sample replacement
sensitivity analysis would result in write-ups such as “one would have
to replace pp percent of the observed data with null hypothesis to
invalidate the inference.” Both sensitivity analysis statements enable
researchers to quantify the robustness of their causal inferences.

# The Need for an R Package

We have implemented these recent developments of sensitivity analysis
for causal inferences within R via the konfound R package. This package
is used to calculate two causal robustness indices: omission of
confounding variable(s) and sample replacement. In particular, the
konfound R package is designed to quantify the statistical robustness of
causal inferences.

The package includes the following functions: konfound(), pkonfound(),
and mkonfound() which allows users to calculate the robustness of
inferences for a user’s own model, a single published study and multiple
studies, respectively. The package is available from the Comprehensive R
Archive Network (CRAN) at <https://CRAN.R-project.org/package=konfound>;
it can be installed ia the standard `install.packages(“konfound”)`
function. The following three functions within the konfound package are
briefly summarized below along with their functionality.

# Summary and Functionality

1.  **konfound**: This function is used for fitted models and
    calculates 1) how much bias there must be in an estimate to
    invalidate/sustain an inference; 2) the impact of an omitted
    variable necessary to invalidate/sustain an inference for a
    regression coefficient. This function currently works for linear
    models created with lm(), glm(), and lmer() fitted in R. The value
    that is printed in the R console is the bias and the number of cases
    that would have to be replaced with cases for which there is no
    effect to invalidate the inference.

## *Example for linear models fit with lm()*

For this example, we will use the `ChickWeight` dataset built within R.
This dataset comes from an experiment exploring the effect of diet on
growth of chicks. Each chick had their weight recorded for 20 days,
their diet group, and the day their weight was recorded since they were
born. More information on this dataset can be found at
<https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/ChickWeight>

We will model the impact time has on chick weight.

    m <- lm(weight ~ Diet + Time, data = ChickWeight)
    summary(m)

    ## 
    ## Call:
    ## lm(formula = weight ~ Diet + Time, data = ChickWeight)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -136.851  -17.151   -2.595   15.033  141.816 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  10.9244     3.3607   3.251  0.00122 ** 
    ## Diet2        16.1661     4.0858   3.957 8.56e-05 ***
    ## Diet3        36.4994     4.0858   8.933  < 2e-16 ***
    ## Diet4        30.2335     4.1075   7.361 6.39e-13 ***
    ## Time          8.7505     0.2218  39.451  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 35.99 on 573 degrees of freedom
    ## Multiple R-squared:  0.7453, Adjusted R-squared:  0.7435 
    ## F-statistic: 419.2 on 4 and 573 DF,  p-value: < 2.2e-16

As you can see, the variable “Time” has a significant effect on chick
weight.

Now let’s examine the robustness of this effect.

    konfound::konfound(m, Time)

    ## Robustness of Inference to Replacement (RIR):
    ## To invalidate an inference,  95.021 % of the estimate would have to be due to bias. 
    ## This is based on a threshold of 0.436 for statistical significance (alpha = 0.05).
    ## 
    ## To invalidate an inference,  549  observations would have to be replaced with cases
    ## for which the effect is 0 (RIR = 549).
    ## 
    ## See Frank et al. (2013) for a description of the method.
    ## 
    ## Citation: Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. (2013).
    ## What would it take to change an inference?
    ## Using Rubin's causal model to interpret the robustness of causal inferences.
    ## Education, Evaluation and Policy Analysis, 35 437-460.

    ## For more detailed output, consider setting `to_return` to table

    ## To consider other predictors of interest, consider setting `test_all` to TRUE.

As you can see, the effect Time has on chick weight is quite robust.
This output can be interpreted in two ways. First, 95.021% of the effect
Time has on weight would have to be attributed to another variable not
included in the model in order to invalidate this inference. We can also
say that out og the 578 recorded observations, we would have to replace
549 of these cases with a zero effect in order for this inference to be
invalidated. That’s a lot!

1.  **pkonfound**: This function performs sensitivity analysis for
    analysis which have already been doncuted, such as one in an
    already-published study or from analysis carried out using other
    software. This function calculates 1) how much bias there must be in
    an estimate to invalidate/sustain an inference; 2) the impact of an
    omitted variable necessary to invalidate/sustaintain an inference
    for a regression coefficient.

## *Example for for a regression analysis*

For this example, the following parameters from a regression analysis
would be entered into the pkonfound function: standard error (*SE* =
.4), unstandardized beta coefficient (*B* = 2), sample size (*n* = 100),
and the number of covariates (*cv* = 3).

    konfound::pkonfound(.4, 2, 100, 3)

    ## Robustness of Inference to Replacement (RIR):
    ## To sustain an inference,  89.924 % of the estimate would have to be due to bias. 
    ## This is based on a threshold of 3.97 for statistical significance (alpha = 0.05).
    ## 
    ## To sustain an inference, 90 of the cases with 0 effect would have to be replaced with cases at the threshold of inference (RIR = 90).
    ## See Frank et al. (2013) for a description of the method.
    ## 
    ## Citation: Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. (2013).
    ## What would it take to change an inference?
    ## Using Rubin's causal model to interpret the robustness of causal inferences.
    ## Education, Evaluation and Policy Analysis, 35 437-460.

    ## For other forms of output, run ?pkonfound and inspect the to_return argument

    ## For models fit in R, consider use of konfound().

The effect from this example is quite robust, too. 89.924% of the effect
(unstandardized beta coefficient) would have to be due to another
variable not included in this model to invalidate this inference. Also,
90 cases within the sample size would have to be replaced with zero
effect cases in order to invaldiate this inference. Therefore, this
inference is quite robust.

1.  **mkonfound**: This function performs a sensitivity analysis for a
    meta-analysis. This function is used for fitted models when
    parameters are stored in a data frame. The value that is printed in
    the R console is the bias and the number of cases that would have to
    be replaced with cases for which there is no effect to invalidate
    the inference for each of the cases in the data frame.

## *Example for a regression meta-analysis*

For this example, a CSV file will be read in from a website and saved as
d dataframe. t represents the t-statistic, and df represents the degrees
of freedom associated with the t-statistics in the t argument.

    #d<-read.csv("https://msu.edu/~kenfrank/example%20dataset%20for%20mkonfound.csv")
    #head(d)
    #konfound::mkonfound(d, t, df) #THIS DOESNT WORK. I THINK THE CSV IS BAD.

A plot summarizing the percent bias needed to sustain or invalidate the
inference across all of these studies can be returned using the syntax
below.

    #konfound::mkonfound(d, t, df, return_plot = T)

# Doing and Learning More

We have created a Shiny interactive web application at
<https://jmichaelrosenberg.shinyapps.io/konfound-it/>. We also have
additional documentation at <https://jrosen48.github.io/konfound>.

# Acknowledgements

# References
