capture program drop pkonfound
program define pkonfound
version 13.1
syntax anything, [if] [in] [ sig(real 0.05) nu(real 0) onetail(real 0) rep_0(real 0) ]


local coef: word 1 of `anything'
local sd: word 2 of `anything'
local N: word 3 of `anything'
local Ncov: word 4 of `anything'

if `onetail'==1 {
local criticalt =sign(`coef' - `nu') *  invttail(`N'-`Ncov'-1,`sig')
local beta_threshold = `criticalt' * `sd' +`nu'
local t_critr = `beta_threshold'/`sd'
local r_crit = `t_critr'/sqrt((`t_critr')^2 + `N'-`Ncov'-3)
local r_obs = (`coef'/`sd')/sqrt((`coef'/`sd')^2 +`N'-`Ncov'-3)

  if (`r_obs' - `r_crit') >= 0 {
  local itcv = (`r_obs' - `r_crit')/(1-`r_crit')
  }
  else {
  local itcv = (`r_obs' - `r_crit')/(1 +`r_crit')
  }
  local impact_ =  string(`itcv',"%6.4f")
 local r_con = string(sqrt(abs(`itcv')),"%6.3f")
 local nr_con = -1 * `r_con'
 
   if abs((`coef' - `nu')/`sd') < abs(`criticalt')  {
  dis "------------------"
  dis "Your estimate is not statistically significant."
  dis "------------------"
  }
 
   dis "------------------"
  dis "Impact Threshold for Omitted Variable"
 dis ""
  if abs((`coef' - `nu')/`sd') >= abs(`criticalt')  {
  
  if `itcv'>0 {
  dis "An omitted variable would have to be correlated at `r_con' with the outcome and at `r_con' with the predictor" _newline "of interest (conditioning on observed covariates) to invalidate an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "`r_con' x `r_con'=`impact_' to invalidate an inference."
}
else {
  dis "An omitted variable would have to be correlated at `r_con' with the outcome and at `nr_con' with the predictor" _newline "of interest (conditioning on observed covariates. Signs are interchangeable) to invalidate an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "`r_con' x `nr_con'=`impact_' to invalidate an inference."

}
  }
  else {

  if `itcv'>0 {
  dis "An omitted variable would have to be correlated at `r_con' with the outcome and at `r_con' with the predictor" _newline "of interest (conditioning on observed covariates) to sustain an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "`r_con' x `r_con'=`impact_' to sustain an inference."
}
else {
  dis "An omitted variable would have to be correlated at `r_con' with the outcome and at `nr_con' with the predictor" _newline "of interest (conditioning on observed covariates. Signs are interchangeable) to sustain an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "`r_con' x `nr_con'=`impact_' to sustain an inference."

}
   
}
}

else {
local criticalt =sign(`coef' - `nu') *  invttail(`N'-`Ncov'-1,`sig'/2)
local beta_threshold = (`criticalt' * `sd') + `nu'
local t_critr = `beta_threshold'/`sd'
local r_crit = `t_critr'/sqrt((`t_critr')^2 + `N'-`Ncov'-3)
local r_obs = (`coef'/`sd')/sqrt((`coef'/`sd')^2 +`N'-`Ncov'-3)

  if (`r_obs' - `r_crit') >= 0 {
  local itcv = (`r_obs' - `r_crit')/(1-`r_crit')
  }
  else {
  local itcv = (`r_obs' - `r_crit')/(1 +`r_crit')
  }
  local impact_ =  string(`itcv',"%6.4f")
 local r_con = string(sqrt(abs(`itcv')),"%6.3f")
 local nr_con = -1 * `r_con'
 
    if abs((`coef' - `nu')/`sd') < abs(`criticalt')  {
  dis "------------------"
  dis "Your estimate is not statistically significant."
  dis "------------------"
  }
 
   dis "------------------"
  dis "Impact Threshold for Omitted Variable"
dis ""
  if  abs((`coef' - `nu')/`sd') >= abs(`criticalt') {
  
  if `itcv'>0 {
  dis "An omitted variable would have to be correlated at `r_con' with the outcome and at `r_con' with the predictor" _newline "of interest (conditioning on observed covariates) to invalidate an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "`r_con' x `r_con'=`impact_' to invalidate an inference."
}
else {
  dis "An omitted variable would have to be correlated at `r_con' with the outcome and at `nr_con' with the predictor" _newline "of interest (conditioning on observed covariates. Signs are interchangeable) to invalidate an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "`r_con' x `nr_con'=`impact_' to invalidate an inference."

}
  }
  else {

  if `itcv'>0 {
  dis "An omitted variable would have to be correlated at `r_con' with the outcome and at `r_con' with the predictor" _newline "of interest (conditioning on observed covariates) to sustain an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "`r_con' x `r_con'=`impact_' to sustain an inference."
}
else {
  dis "An omitted variable would have to be correlated at `r_con' with the outcome and at `nr_con' with the predictor" _newline "of interest (conditioning on observed covariates. Signs are interchangeable) to sustain an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "`r_con' x `nr_con'=`impact_' to sustain an inference."

}
   
}
}




local coef: word 1 of `anything'
local sd: word 2 of `anything'
local N: word 3 of `anything'
local Ncov: word 4 of `anything'

if `onetail'==1 {
local criticalt =sign(`coef' - `nu') *  invttail(`N'-`Ncov'-1,`sig')
local threshold = `criticalt' * `sd'
  if `rep_0'==1 {
  local bias = string(100*(1- ((`threshold'+`nu')/`coef')),"%6.2f")
  }
  else {
  local bias = string(100*(1- (`threshold'/(`coef'-`nu'))),"%6.2f")
  }
  if abs(`coef')> abs(`threshold'+`nu') {
    local sustain = string(100*(1- ((`threshold'+`nu')/(`coef'))),"%6.2f")
	}
	else {
	local sustain = string(100*(1- ((`coef')/((`threshold'+`nu')))),"%6.2f")
	}
  local recase = round(`N' * `bias'/100,1)
  
    dis "------------------"
  dis "The Threshold for % Bias to Invalidate/Sustain the Inference" 
  dis ""
  if (abs(`coef' - `nu')- abs(`threshold')) >=0  {
  
  if `rep_0'==0 {
  dis "To invalidate the inference `bias'% of the estimate would have to be due to bias; to invalidate the" _newline "inference `bias'% (`recase') cases would have to be replaced with cases for which there is an effect of `nu'."
}
else {
  dis "To invalidate the inference `bias'% of the estimate would have to be due to bias; to invalidate the " _newline "inference `bias'% (`recase') cases would have to be replaced with cases for which there is an zero effect."

}
  }
  else {
  dis "To sustain the inference `sustain'% of the estimate would have to be due to bias; to sustain the " _newline "inference `sustain'% of the cases with 0 effect would have to be replaced with cases at the threshold of inference."
  }
  
  
}

else {
  local criticalt = sign(`coef' - `nu') * invttail(`N'-`Ncov'-1,`sig'/2)
  local threshold = `criticalt' * `sd'
  if `rep_0'==1 {
  local bias = string(100*(1- ((`threshold'+`nu')/`coef')),"%6.2f")
  }
  else {
  local bias = string(100*(1- (`threshold'/(`coef'-`nu'))),"%6.2f")
  }
   if abs(`coef')> abs(`threshold'+`nu') {
    local sustain = string(100*(1- ((`threshold'+`nu')/(`coef'))),"%6.2f")
	}
	else {
	local sustain = string(100*(1- ((`coef')/((`threshold'+`nu')))),"%6.2f")
	}
 local recase = round(`N' * `bias'/100,1)
    dis "------------------"
  dis "The Threshold for % Bias to Invalidate/Sustain the Inference" 
  dis ""
  if (abs(`coef' - `nu')- abs(`threshold')) >=0  {
  
  if `rep_0'==0 {
  dis "To invalidate the inference `bias'% of the estimate would have to be due to bias; to invalidate the " _newline "inference `bias'% (`recase') cases would have to be replaced with cases for which there is an effect of `nu'."
}
else {
  dis "To invalidate the inference `bias'% of the estimate would have to be due to bias; to invalidate the " _newline "inference `bias'% (`recase') cases would have to be replaced with cases for which there is an zero effect."

}
  }
  else {
  dis "To sustain the inference `sustain'% of the estimate would have to be due to bias; to sustain the " _newline "inference `sustain'% of the cases with 0 effect would have to be replaced with cases at the threshold of inference."
  }
  }
  
  dis ""
  dis "Note:"
  dis "For non-linear models, the impact threshold should not be used."
  dis "The % bias calculation is based on the original coefficient, compare" _newline "with the use of average partial effects as in the [konfound] command."
  
  end

