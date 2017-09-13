capture program drop konfound
program define konfound
	version 13.1
syntax varlist (min=1 max=10), [if] [in] [ sig(real 0.05) nu(real 0)  onetail(real 0) uncond (real 0)  rep_0(real 0) non_li(real 0)]

capture drop esti_ 
capture drop thres_ 
 capture drop id_ 
 capture drop ttt_ 
 capture drop samused_
 capture drop _namelis 
  capture drop _count
quietly  gen esti_=.
quietly gen thres_=.
quietly gen id_=""

local i=0
foreach x in `varlist' {
local i= `i'+1

if `non_li' == 0  {
  local `x'coef = _b[`x']
  local `x'sd   = _se[`x']
  }
else  {
  margins, dydx(`x')
  local `x'coef=el(r(b),1,1)
  local `x'sd=sqrt(el(r(V),1,1))
  }
  
  if `onetail'==1 {
  if e(N_g)==. {
local `x'criticalt = sign(``x'coef' - `nu') *  invttail(e(N)-e(df_m),`sig')
}
else {
local `x'criticalt = sign(``x'coef' - `nu') *  invttail(e(N)-e(df_m)-e(N_g)+1,`sig')
}
local `x'threshold = ``x'criticalt' * ``x'sd'
  if `rep_0'==1 {
  local `x'bias = string(100*(1- ((``x'threshold'+`nu')/``x'coef')),"%6.2f")
  }
  else {
  local `x'bias = string(100*(1- (``x'threshold'/(``x'coef'-`nu'))),"%6.2f")
  }
    if abs(``x'coef')> abs(``x'threshold'+`nu') {
    local `x'sustain = string(100*(1- ((``x'threshold'+`nu')/(``x'coef'))),"%6.2f")
	}
	else {
    local `x'sustain = string(100*(1- ((``x'coef')/((``x'threshold'+`nu')))),"%6.2f")
	}
  local `x'recase = round(e(N) * ``x'bias'/100,1)
  if (abs(``x'coef' - `nu')- abs(``x'threshold')) <0  {
  dis "------------------"
  dis "For `x': Your estimate is not statistically significant."
  dis "------------------"
  }
  
  if `non_li' == 1  {
  dis "Following calculation is based on Average Partial Effect:"
  }
  dis "------------------"
  dis "The Threshold for % Bias to Invalidate/Sustain the Inference" 
  dis ""
  if (abs(``x'coef' - `nu')- abs(``x'threshold')) >=0  {
  
  if `rep_0'==0 {
  dis "For `x':" _newline "To invalidate the inference ``x'bias'% of the estimate would have to be due to bias; to invalidate the" _newline "inference ``x'bias'% (``x'recase') cases would have to be replaced with cases for which there is an effect of `nu'."
}
else {
  dis "For `x':" _newline "To invalidate the inference ``x'bias'% of the estimate would have to be due to bias; to invalidate the" _newline "inference``x'bias'% (``x'recase') cases would have to be replaced with cases for which there is an zero effect."

}
  }
  else {

  dis "For `x':" _newline "To sustain the inference ``x'sustain'% of the estimate would have to be due to bias; to sustain the" _newline "inference ``x'sustain'% of the cases with 0 effect would have to be replaced with cases at the threshold of inference."
  }
  
if e(r2_p)!=. & `non_li' == 0 {
dis ""
dis "Warnings:"
dis "For a non-linear model calculation based on average partial effect is recommended, in the options use non_li(1)."
dis ""
dis "For a non-linear model users might also want to consider alternative standard error estimation methods,"_newline "such as the bootstrapping method."
dis "To use the bootstrapping method type [bootstrap, reps(#):] before your original command."
}  
}
  
  
else {
    if e(N_g)==. {
local `x'criticalt = sign(``x'coef' - `nu') *  invttail(e(N)-e(df_m),`sig'/2)
}
else {
local `x'criticalt = sign(``x'coef' - `nu') *  invttail(e(N)-e(df_m)-e(N_g)+1,`sig'/2)
}
  local `x'threshold = ``x'criticalt' * ``x'sd'
  if `rep_0'==1 {
  local `x'bias = string(100*(1- ((``x'threshold'+`nu')/``x'coef')),"%6.2f")
  }
  else {
  local `x'bias = string(100*(1- (``x'threshold'/(``x'coef'-`nu'))),"%6.2f")
  }
     if abs(``x'coef') > abs(``x'threshold'+`nu') {
    local `x'sustain = string(100*(1- ((``x'threshold'+`nu')/(``x'coef'))),"%6.2f")
	}
	else {
    local `x'sustain = string(100*(1- ((``x'coef')/((``x'threshold'+`nu')))),"%6.2f")
	}
  local `x'recase = round(e(N) * ``x'bias'/100,1)
  
  if (abs(``x'coef' - `nu')- abs(``x'threshold')) <0  {
  dis "------------------"
  dis "For `x': Your estimate is not statistically significant."
  dis "------------------"
  }
  
    if `non_li' == 1  {
  dis "Following calculation is based on Average Partial Effect:"
  }
 dis "------------------"
    dis "The Threshold for % Bias to Invalidate/Sustain the Inference" 
  dis ""
  
  if (abs(``x'coef' - `nu')- abs(``x'threshold')) >=0 {
  
  if `rep_0'==0 {
  dis "For `x':" _newline "To invalidate the inference ``x'bias'% of the estimate would have to be due to bias; to invalidate the" _newline "inference ``x'bias'% (``x'recase') cases would have to be replaced with cases for which there is an effect of `nu'."
}
else {
  dis "For `x':" _newline "To invalidate the inference ``x'bias'% of the estimate would have to be due to bias; to invalidate the" _newline "inference ``x'bias'% (``x'recase') cases would have to be replaced with cases for which there is an zero effect."

}
  }
  else {

  dis "For `x':" _newline "To sustain the inference ``x'sustain'% of the estimate would have to be due to bias; to sustain the" _newline "inference ``x'sustain'% of the cases with 0 effect would have to be replaced with cases at the threshold of inference."
  }
  if e(r2_p)!=. & `non_li' == 0 {
  dis ""
  dis "Warnings:"
dis "For a non-linear model calculation based on average partial effect is recommended, in the options use non_li(1)."
dis ""
dis "For a non-linear model users might also want to consider alternative standard error estimation methods,"_newline "such as the bootstrapping method."
dis "To use the bootstrapping method type [bootstrap, reps(#):] before your original command."
} 
  }
  
if  ``x'bias' > 0 & `nu' == 0 & ``x'sd' != 0{

 

quietly replace esti_= abs(``x'coef') - abs(``x'threshold') in `i'
quietly replace thres_= abs(``x'threshold') in `i'
quietly replace id_ = "`x'" in `i'

 }

  }
  quietly egen ttt_= max(esti_)
if ttt_[1]!=. {
   graph bar thres_ esti_, over(id_) stack  legend( label(1 "threshold") label(2 "estimate") )
 }
  
 drop thres_ esti_ id_ ttt_ 


local NN = e(N)
local dfm = e(df_m)
local Ng = e(N_g)
local prsq= e(r2_p)
local Dep= e(depvar)
gen samused_=e(sample)
local Rsq = e(r2)
quietly sum `Dep' if samused_==1
local VarY= r(Var)

quietly indeplist
 local Ncov= wordcount(r(X))
quietly gen _namelis=r(X) in 1
quietly moss _namelis, match("([c0-9]+[\.].[^ ]*)") regex
local abc =_count[1]

forvalues xyz = 1/`abc'  {

quietly replace _namelis = subinword(_namelis,_match`xyz'[1],"",.) in 1
drop _pos`xyz' _match`xyz'
}


local namelist  = _namelis[1] 



local Nz= `Ncov' - _count[1] - 1
 
foreach x in `varlist' {

if `non_li' == 0  {
  local `x'coef = _b[`x']
  local `x'sd   = _se[`x']
  }
else  {
  quietly margins,dydx(`x')
  local `x'coef=el(r(b),1,1)
  local `x'sd=sqrt(el(r(V),1,1))
  }

}

foreach x in `varlist'  { 
  
 if `onetail'==1 {
 
  if `Ng'==. {
local `x'criticalt =sign(``x'coef' - `nu') *  invttail(`NN'-`dfm',`sig')
local `x'be_th = ``x'criticalt' * ``x'sd' +`nu'
local `x't_critr = ``x'be_th'/``x'sd'
local `x'r_crit = ``x't_critr'/sqrt((``x't_critr')^2 + `NN'-`dfm'-2)
local `x'r_obs = (``x'coef'/``x'sd')/sqrt((``x'coef'/``x'sd')^2 +`NN'-`dfm'-2)
}

else {
local `x'criticalt =sign(``x'coef' - `nu') *  invttail(`NN'-`dfm'-`Ng'+1,`sig')
local `x'be_th = ``x'criticalt' * ``x'sd' +`nu'
local `x't_critr = ``x'be_th'/``x'sd'
local `x'r_crit = ``x't_critr'/sqrt((``x't_critr')^2 + `NN'-`dfm'-`Ng'-2)
local `x'r_obs = (``x'coef'/``x'sd')/sqrt((``x'coef'/``x'sd')^2 +`NN'-`dfm'-`Ng'-2)
}

local `x'RsqYZ = ((``x'r_obs')^2 - `Rsq')/((``x'r_obs')^2 - 1)
quietly sum `x' if samused_==1
local `x'VarX= r(Var)
local `x'RsqXZ = max(0, 1 - ((`VarY'*(1-`Rsq'))/(``x'VarX'*(`NN'-`dfm'-2)*((``x'sd')^2)))) 

  if (``x'r_obs' - ``x'r_crit') >= 0 {
  local `x'itcv = (``x'r_obs' - ``x'r_crit')/(1-``x'r_crit')
  }
  else {
  local `x'itcv = (``x'r_obs' - ``x'r_crit')/(1 +``x'r_crit')
  }
  
  local `x'impact =  string(``x'itcv',"%6.4f")
 local `x'r_con = string(sqrt(abs(``x'itcv')),"%6.3f")
 local `x'nr_con = -1 * ``x'r_con' 
 local `x'r_ycv = string(``x'r_con' * sqrt(1-(``x'RsqYZ')),"%6.3f")
 local `x'r_xcv = string(``x'r_con' * sqrt(1-(``x'RsqXZ')),"%6.3f")
 local `x'nr_xcv = -1 * ``x'r_xcv'
 local `x'un_impact = string(``x'itcv'*sqrt(1-(``x'RsqYZ'))*sqrt(1-(``x'RsqXZ')),"%6.4f")
 

 
  if abs((``x'coef' - `nu')/``x'sd') >= abs(``x'criticalt')  {
  
  if ``x'itcv'>0 {
  dis "------------------"
  dis "Impact Threshold for Omitted Variable" 
dis ""
  dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_con' with the outcome and at ``x'r_con' with the predictor" _newline "of interest (conditioning on observed covariates) to invalidate an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'r_con' x ``x'r_con'=``x'impact' to invalidate an inference."
}
else {
dis "------------------"
dis "Impact Threshold for Omitted Variable" 
dis ""
  dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_con' with the outcome and at ``x'nr_con' with the predictor" _newline "of interest (conditioning on observed covariates. signs are interchangeable) to invalidate an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'nr_con' x ``x'r_con'=``x'impact' to invalidate an inference."

}

  }
  else {

  if ``x'itcv'>0 {
  dis "------------------"
  dis "Impact Threshold for Omitted Variable" 
dis ""
  dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_con' with the outcome and at ``x'r_con' with the predictor" _newline "of interest (conditioning on observed covariates) to sustain an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'r_con' x ``x'r_con'=``x'impact' to sustain an inference."
}
else {
dis "------------------"
dis "Impact Threshold for Omitted Variable" 
dis ""
  dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_con' with the outcome and at ``x'nr_con' with the predictor" _newline "of interest (conditioning on observed covariates. signs are interchangeable) to sustain an inference. " _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'nr_con' x ``x'r_con'=``x'impact' to sustain an inference."

}  
}

if `uncond' ==1 {

  if abs((``x'coef' - `nu')/``x'sd') >= abs(``x'criticalt')  {
  
  if ``x'itcv'>0 {
  dis ""
  dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_ycv' with the outcome and at ``x'r_xcv' with the predictor" _newline "of interest (before conditioning on observed covariates) to invalidate an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'r_ycv'  x ``x'r_xcv'=``x'un_impact' to invalidate an inference."
}
else {
dis ""
  dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_ycv' with the outcome and at ``x'nr_xcv' with the predictor" _newline "of interest (before conditioning on observed covariates. signs are interchangeable) to invalidate an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'r_ycv'  x ``x'nr_xcv'=``x'un_impact' to invalidate an inference."

}

  }
  else {

  if ``x'itcv'>0 {
  dis ""
  dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_ycv' with the outcome and at ``x'r_xcv' with the predictor" _newline "of interest (before conditioning on observed covariates) to sustain an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'r_ycv'  x ``x'r_xcv'=``x'un_impact' to sustain an inference."
}
else {
dis ""
  dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_ycv' with the outcome and at ``x'nr_xcv' with the predictor" _newline "of interest (before conditioning on observed covariates. signs are interchangeable) to sustain an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'r_ycv' x ``x'nr_xcv'=``x'un_impact' to sustain an inference."

}  
}

}
if `Nz'>0 {
dis ""
dis "These thresholds can be compared with the impacts of observed covariates below."
}
} 
  
else {

  if `Ng'==. {
local `x'criticalt =sign(``x'coef' - `nu') *  invttail(`NN'-`dfm',`sig'/2)
local `x'be_th = ``x'criticalt' * ``x'sd' +`nu'
local `x't_critr = ``x'be_th'/``x'sd'
local `x'r_crit = ``x't_critr'/sqrt((``x't_critr')^2 + `NN'-`dfm'-2)
local `x'r_obs = (``x'coef'/``x'sd')/sqrt((``x'coef'/``x'sd')^2 +`NN'-`dfm'-2)
}
else {
local `x'criticalt =sign(``x'coef' - `nu') *  invttail(`NN'-`dfm'-`Ng'+1,`sig'/2)
local `x'be_th = ``x'criticalt' * ``x'sd' +`nu'
local `x't_critr = ``x'be_th'/``x'sd'
local `x'r_crit = ``x't_critr'/sqrt((``x't_critr')^2 + `NN'-`dfm'-`Ng'-2)
local `x'r_obs = (``x'coef'/``x'sd')/sqrt((``x'coef'/``x'sd')^2 +`NN'-`dfm'-2)
}

local `x'RsqYZ = ((``x'r_obs')^2 - `Rsq')/((``x'r_obs')^2 - 1)
quietly sum `x' if samused_==1
local `x'VarX= r(Var)
local `x'RsqXZ = max(0, 1 - ((`VarY'*(1-`Rsq'))/(``x'VarX'*(`NN'-`dfm'-2)*((``x'sd')^2)))) 

  if (``x'r_obs' - ``x'r_crit') >= 0 {
  local `x'itcv = (``x'r_obs' - ``x'r_crit')/(1-``x'r_crit')
  }
  else {
  local `x'itcv = (``x'r_obs' - ``x'r_crit')/(1 +``x'r_crit')
  }
 local `x'impact =  string(``x'itcv',"%6.4f")
 local `x'r_con = string(sqrt(abs(``x'itcv')),"%6.3f")
 local `x'nr_con = -1 * ``x'r_con'
 local `x'r_ycv = string(``x'r_con' * sqrt(1-(``x'RsqYZ')),"%6.3f")
 local `x'r_xcv = string(``x'r_con' * sqrt(1-(``x'RsqXZ')),"%6.3f")
 local `x'nr_xcv = -1 * ``x'r_xcv'
 local `x'un_impact = string(``x'itcv'*sqrt(1-(``x'RsqYZ'))*sqrt(1-(``x'RsqXZ')),"%6.4f")
 

 
  if abs((``x'coef' - `nu')/``x'sd') >= abs(``x'criticalt')  {
  
  if ``x'itcv'>0 {
  dis "------------------"
   dis "Impact Threshold for Omitted Variable" 
dis ""
  dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_con' with the outcome and at ``x'r_con' with the predictor" _newline "of interest (conditioning on observed covariates) to invalidate an inference. " _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'r_con' x ``x'r_con'=``x'impact' to invalidate an inference."
}
else {
dis "------------------"
 dis "Impact Threshold for Omitted Variable" 
dis ""
  dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_con' with the outcome and at ``x'nr_con' with the predictor" _newline "of interest (conditioning on observed covariates. signs are interchangeable) to invalidate an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'nr_con' x ``x'r_con'=``x'impact' to invalidate an inference."

}
  }
  else {

  if ``x'itcv'>0 {
  dis "------------------"
   dis "Impact Threshold for Omitted Variable" 
dis ""
  dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_con' with the outcome and at ``x'r_con' with the predictor" _newline "of interest (conditioning on observed covariates) to sustain an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'r_con' x ``x'r_con'=``x'impact' to sustain an inference."
}
else {
dis "------------------"
 dis "Impact Threshold for Omitted Variable" 
dis ""
 dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_con' with the outcome and at ``x'nr_con' with the predictor" _newline "of interest (conditioning on observed covariates. signs are interchangeable) to sustain an inference." _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'nr_con' x ``x'r_con'=``x'impact' to sustain an inference."

}
   
}
 
 if `uncond' ==1 {

  if abs((``x'coef' - `nu')/``x'sd') >= abs(``x'criticalt')  {
  
  if ``x'itcv'>0 {
  dis ""
  dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_ycv' with the outcome and at ``x'r_xcv' with the predictor" _newline "of interest (before conditioning on observed covariates) to invalidate an inference. " _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'r_ycv'  x ``x'r_xcv'=``x'un_impact' to invalidate an inference."
}
else {
dis ""
  dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_ycv' with the outcome and at ``x'nr_xcv' with the predictor" _newline "of interest (before conditioning on observed covariates. signs are interchangeable) to invalidate an inference. " _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'r_ycv'  x ``x'nr_xcv'=``x'un_impact' to invalidate an inference."

}

  }
  else {

  if ``x'itcv'>0 {
  dis ""
  dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_ycv' with the outcome and at ``x'r_xcv' with the predictor" _newline "of interest (before conditioning on observed covariates) to sustain an inference. " _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'r_ycv'  x ``x'r_xcv'=``x'un_impact' to sustain an inference."
}
else {
dis ""
  dis "For `x':" _newline "An omitted variable would have to be correlated at ``x'r_ycv' with the outcome and at ``x'nr_xcv' with the predictor" _newline "of interest (before conditioning on observed covariates. signs are interchangeable) to sustain an inference. " _newline "Correspondingly the impact of an omitted variable (as defined in Frank 2000) must be" _newline "``x'r_ycv' x ``x'nr_xcv'=``x'un_impact' to sustain an inference."

}  
}

} 


if `Nz'>0 {
 dis ""
dis "These thresholds can be compared with the impacts of observed covariates below." 
}
}  

if `Nz'>0 {

     
local `x'namelist1=trim(subinword("`namelist'","`x'","",.))

quietly corr `x' ``x'namelist1' if samused_ ==1
matrix rvx1 = r(C)
mat rvx = rvx1[2..`Nz'+1, 1]

quietly corr `Dep' ``x'namelist1' if samused_ ==1
matrix rvy1 = r(C)
mat rvy = rvy1[2..`Nz'+1, 1]

matrix imp_raw = J(`Nz',1,0)
forvalues i = 1/`Nz' {
		 matrix imp_raw[`i',1]= rvx[`i',1] * rvy[`i',1]	
}

}
if `Nz'>1 {
quietly pcorr `x' ``x'namelist1' if samused_ ==1
matrix prvx1 = r(p_corr) 
mat prvx = prvx1[1..`Nz', 1]

quietly pcorr `Dep' ``x'namelist1' if samused_ ==1
matrix prvy1 = r(p_corr) 
mat prvy = prvy1[1..`Nz', 1]

matrix imp_par = J(`Nz',1,0)
forvalues i = 1/`Nz' {
		 matrix imp_par[`i',1]= prvx[`i',1] * prvy[`i',1]	
}


}



if `Nz'>0 {

mat Impact_Table=J(`Nz',3,0)
mat Impact_Table2=J(`Nz',3,0)
forvalues i = 1/`Nz' {
		 matrix Impact_Table[`i',1]= round(rvx[`i',1],.0001)
		 matrix Impact_Table[`i',2]= round(rvy[`i',1],.0001)
		 matrix Impact_Table[`i',3]= round(imp_raw[`i',1],.0001)
		 if `Nz'>1 {
		 matrix Impact_Table2[`i',1]= round(prvx[`i',1],.0001)
		 matrix Impact_Table2[`i',2]= round(prvy[`i',1],.0001)
		 matrix Impact_Table2[`i',3]= round(imp_par[`i',1],.0001)
		 }
}


matrix rownames Impact_Table = ``x'namelist1'
matrix colnames Impact_Table = "Cor(v,X)" "Cor(v,Y)" "Impact" 
matrix rownames Impact_Table2 = ``x'namelist1'
matrix colnames Impact_Table2 = "Cor(v,X)" "Cor(v,Y)" "Impact"

matsort Impact_Table 3 down
matsort Impact_Table2 3 down

matlist Impact_Table,title("Observed Impact Table for `x'") lines(co) border(all)  rowtitle(Raw) 
if `Nz'>1 {
matlist Impact_Table2,lines(co) border(all)  rowtitle(Partial) 
}
if `Nz'>1 {
dis ""
dis "X represents `x', Y represents `Dep', v represents each covariate." _newline "First table is based on unconditional correlations, second table is based on partial correlations."
}
if `Nz'==1 {
dis ""
dis "X represents `x', Y represents `Dep', v represents each covariate." _newline "Table is based on unconditional correlations."
}
 matrix drop Impact_Table Impact_Table2  imp_raw  rvy1 rvy  rvx rvx1
if `Nz'>1 {
 matrix drop imp_par prvy1 prvy prvx1 prvx
 }
 }
}

drop samused_ _namelis _count

  if `prsq'!=. {
dis ""
 dis "Warnings:"
 dis "For a non-linear model impact threshold should not be used."
 dis ""
 }

end




