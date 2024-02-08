library(evaluate)
library(tidyverse)
library(githubinstall)
library(gridExtra)

################################################################################
########################## RIR and ITCV for linear ########################

# loop to create testdata based on the different scenarios
### estimated effect from -100, -2.5, -0.5, 0, 0.5, 2.5, 100
### standard error is always 1
### n_obs 20 and 10000
### n_covariates 0 10 20 
### tails 1 and 2
### nu -1, 0, and 1
### Later: alternative threshold estimated effect +0.25, -0.25, +0, *(-1) - this one is not ready

# Step 1: first is to define the testdata
##library(githubinstall)
##gh_install_packages("konfound", ref = "newitcv")
columns = c("est_eff", "std_err", "n_obs", "n_coviariates",
            "tail", "nu", "sdx","sdy","rs","ITCV", "unconITCV","RIR", "error_message")

df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df) = columns

est_eff_list = c(-100, -2.5, -0.5, 0, 0.5, 2.5, 100)
std_err = 1
n_obs_list = c(20, 10000)
n_covariates_list = c(0, 10, 20)
tail_list = c(1, 2)
nu_list = c(-1, 0, 1)
sdx_list = c(0.5, 0.75, 1, 1.25)
sdy_list = c(0.5, 0.75, 1, 1.25)
rsquare_list = c(0.1, 0.2, 0.5)
nrow = 0

for (i in est_eff_list) {
  for (j in std_err) {
    for (k in n_obs_list) {
      for (l in n_covariates_list) {
        for (m in tail_list) {
          for (n in nu_list) {
            for(o in sdx_list){
              for(p in sdy_list){
                for(q in rsquare_list){
                  nrow = nrow + 1
                  df[nrow,] = c(i, j, k, l, m, n, o, p, q, NA, NA, NA,NA)
                }
              }
            }
          }
        }
      }
    }
  }
}

df
testdata_R <- df
testdata_Stata <- df

## Step 2: use the pkonfond method(new_itcv) in R to test
### Note that if the function generates any error message and stop running then save the error message
#### output$replace_null_cases >> RIR
#### output$itcv >> itcvGz

library(konfound)

for(i in 1:nrow(testdata_R)){
  e <- suppressWarnings(
    try_capture_stack(
      pkonfound(est_eff = testdata_R$est_eff[i], 
                std_err = testdata_R$std_err[i], 
                n_obs = testdata_R$n_obs[i], 
                n_covariates = testdata_R$n_coviariates[i],
                tails = testdata_R$tail[i],
                nu = testdata_R$nu[i],
                sdx = testdata_R$sdx[i],
                sdy = testdata_R$sdy[i],
                R2 = testdata_R$rs[i],
                to_return = "raw_output"), 
      env = parent.frame()))
  if(is.error(e)) {
    testdata_R$RIR[i] <- testdata_R$ITCV[i] <- NA
    testdata_R$error_message[i] <- e$message
  } else {
    output <- pkonfound(est_eff = testdata_R$est_eff[i], 
                        std_err = testdata_R$std_err[i], 
                        n_obs = testdata_R$n_obs[i], 
                        n_covariates = testdata_R$n_coviariates[i],
                        tails = testdata_R$tail[i],
                        nu = testdata_R$nu[i],
                        sdx = testdata_R$sdx[i],
                        sdy = testdata_R$sdy[i],
                        R2 = testdata_R$rs[i],
                        to_return = "raw_output")
    testdata_R$error_message[i] <- NA
    testdata_R$RIR[i] <- output$RIR
    testdata_R$ITCV[i] <- output$itcvGz
    testdata_R$unconITCV[i] <- output$rxcv
  }
}

View(testdata_R)

## Step 3: use the pkonfond method in STATA to test
library("RStata")
chooseStataBin()
options("RStata.StataPath"='/Applications/Stata/StataSE.app/Contents/MacOS/stata-se')
options("RStata.StataVersion"= 17)

#### Note that one needs to update once error message is added into Stata
for (i in 1:nrow(testdata_Stata)){
  m <- testdata_Stata[i,]
  n <- stata("local nu = nu
              local tail = 2 - tail
              local sdx = sdx
              local sdy = sdy
              local rs = rs
              pkonfound_v2 est_eff std_err n_obs n_coviariates, nu(`nu') onetail(`tail') sdx(`sdx') sdy(`sdy') rs(`rs')
              replace ITCV = r(itcv)
              replace RIR = r(rir)
              replace unconITCV = r(uncond_rxcv)", data.in = m, data.out=TRUE)
  testdata_Stata[i,] <- n
}
View(testdata_Stata)


## Step 4: Compare the test outputs in R and STATA

df$ITCV_R <-testdata_R$ITCV
df$ITCV_Stata <- testdata_Stata$ITCV
df$unconITCV_R <- testdata_R$unconITCV
df$unconITCV_Stata <- testdata_Stata$unconITCV

df$ITCV_R <- round(testdata_R$ITCV+1e-10, digits = 4)
df$ITCV_Stata <- round(testdata_Stata$ITCV+1e-10, digits = 4)
df$unconITCV_R <- round(testdata_R$unconITCV+1e-10, digits = 4)
df$unconITCV_Stata <- round(testdata_Stata$unconITCV+1e-10, digits = 4)


df$RIR_R <- testdata_R$RIR
df$RIR_Stata <- testdata_Stata$RIR
df$error_message_R <- testdata_R$error_message
df$error_message_Stata <- testdata_Stata$error_message

df <- df %>% 
  select(-unconITCV, -ITCV, -RIR, -error_message)

df$compare_ITCV <- df$ITCV_R == df$ITCV_Stata
df$compare_unconITCV <- df$unconITCV_R == df$unconITCV_Stata
df$compare_RIR <- df$RIR_R == df$RIR_Stata

View(df)