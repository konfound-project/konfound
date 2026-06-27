{smcl}
{title:Title}

{phang}
{bf:robust_attrition} {hline 2} Robustness to differential attrition analysis

{title:Syntax}

{p 8 17 2}
{cmd:robust_attrition}
{it:se ntreatob ncontrolob ntreattot ncontroltot yobt yobc syob ncovar r2}
[{cmd:,} {opt r2xz(#)} {opt r2yz(#)} {opt alpha(#)} {opt verbose}]

{title:Description}

{pstd}
{cmd:robust_attrition} computes nonparametric and correlation-based robustness 
quantities for differential attrition using scalar inputs describing observed 
and intended treatment/control group sizes, observed group means, outcome 
variability, and model fit.

{pstd}
The command evaluates how treatment effects may change under assumptions
about missing outcomes and unobserved confounding.

{title:Required arguments}

{pstd}
The following 10 numeric inputs must be supplied in order:

{phang2}{it:se} Standard error of the observed treatment effect. Must be positive.

{phang2}{it:ntreatob} Number of observed treated units.

{phang2}{it:ncontrolob} Number of observed control units.

{phang2}{it:ntreattot} Intended total number of treated units.

{phang2}{it:ncontroltot} Intended total number of control units.

{phang2}{it:yobt} Mean outcome for observed treated units.

{phang2}{it:yobc} Mean outcome for observed control units.

{phang2}{it:syob} Standard deviation of observed outcome. Must be positive.

{phang2}{it:ncovar} Number of covariates in the model. Must be nonnegative.

{phang2}{it:r2} R-squared from the regression model.

{pstd}
Note: These inputs must be provided in the exact order shown above.
Named options such as {cmd:se()} are not supported.

{title:Options}

{phang}
{opt r2xz(#)} Optional user-specified predictor–covariate fit quantity.

{phang}
{opt r2yz(#)} Optional user-specified outcome–covariate fit quantity.

{phang}
{opt alpha(#)} significance level for hypothesis testing (default is 0.05)

{phang}
{opt verbose} Display interpretation guidance for each block and notes about
internally-computed defaults for {opt r2xz} and {opt r2yz}. Off by default.
This is a flag, not a numeric option (use {cmd:verbose}, not {cmd:verbose(1)}).

{title:Example}

{phang}
Basic usage:

{cmd}
robust_attrition 2.19 1817 1981 2028 2311 54.72 49.90 29 1 0.01
{txt}

{phang}
With optional parameters:

{cmd}
robust_attrition 2.19 1817 1981 2028 2311 54.72 49.90 29 1 0.01, r2xz(0.229441)
{txt}

{phang}
With all options:
{cmd}
robust_attrition 2.19 1817 1981 2028 2311 54.72 49.90 29 1 0.01, r2xz(0.229441) r2yz(0.1) alpha(0.05)
{txt}
{phang}
With verbose interpretation guidance:
{cmd}
robust_attrition 2.19 1817 1981 2028 2311 54.72 49.90 29 1 0.01, r2xz(0.229441) verbose
{txt}
