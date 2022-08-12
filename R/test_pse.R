test_pse <- function(est_eff,
                     std_err,
                     n_obs,
                     n_covariates,
                     eff_thr,
                     sdx,
                     sdy,
                     R2,
                     to_return = "short"){
    
    ## test exampple
    # est_eff = .5
    # std_err = .050
    # n_obs = 6174
    # n_covariates = 7
    # sdx = .217
    # sdy = .991 
    # R2 = .251
    # eff_thr = 0.1
    
    ## prepare input
    df = n_obs - n_covariates - 3
    var_x = sdx^2
    var_y = sdy^2
    var_z = sdz = 1
    
    ## now standardize 
    beta_thr = eff_thr * sdx / sdy
    beta = est_eff * sdx / sdy
    SE = std_err * sdx / sdy
    
    ## observed regression, reg y on x Given z
    tyxGz = beta / SE  ### this should be equal to est_eff / std_err
    ryxGz = tyxGz / sqrt(df + tyxGz^2)
    
    ## make sure R2 due to x alone is not larger than overall or observed R2
    if (ryxGz^2 > R2) {stop("Error! ryxGz^2 > R2")}
    
    ## calculate ryz, rxz, rxy
    ryz = rzy = cal_ryz(ryxGz, R2)
    rxz = rzx = cal_rxz(var_x, var_y, R2, df, std_err)
    rxy = ryx = cal_rxy(ryxGz, rxz, ryz)
    
    thr = eff_thr * sdx / sdy
    sdz = sdcv = 1
    rcvz = rzcv = 0
    
    Gz_pse <- cal_pse(thr, ryxGz)
    rxcvGz = as.numeric(Gz_pse[1])
    rycvGz = as.numeric(Gz_pse[2])
    
    # convert conditional correlations to unconditional correlations to be used in new regression
    rxcv = rxcvGz * sqrt((1 - rcvz^2) * (1 - rxz^2)) + rxz * rcvz
    rycv = rycvGz * sqrt((1 - rcvz^2) * (1 - rzy^2)) + rzy * rcvz
    
    verify_pse_reg = verify_reg_Gzcv(n_obs, sdx, sdy, sdz, sdcv, rxy, rxz, rzy, rycv, rxcv, rcvz)
    verfiy_pse_manual_thr = verify_manual(rxy, rxz, rxcv, ryz, rycv, rzcv, sdy, sdx)
    cov_pse = verify_pse_reg[[7]]
    
    # fTable <- matrix(c(rxy^2, R2, FR2max, # R2 for three reg models
    #                       rxy*sdy/sdx, est_eff, eff_thr,  # unstd reg coef for X in three reg  models 
    #                       se_XX, std_err, , # unstd reg se for X in three reg models
    #                       rxy, rxyGz, thr, # std reg coef for X in three reg models
    #                       rxy*sdy/sdx/se_XX, tyxGz, tyxGzcv_XX, # t values for X in three reg models
    #                       NA, , , # std reg coef for Z in three reg models
    #                       NA, , , # se for Z in three reg models
    #                       NA, , , # t for Z in three reg models,
    #                       NA, NA, , # std reg coef for CV in three reg models
    #                       NA, NA, , # se for CV in three reg models
    #                       NA, NA, , # t for CV in three reg models), 11, 3)
    # rownames(fTable) <- c("R2", "coef_X", "SE_X", "beta_X", "t_X",
    #                       "beta_Z", "SE_Z", "t_Z",
    #                       "beta_CV", "SE_CV", "t_CV")
    # colnames(fTable) <- c("reg Y on X", "reg Y on X, Z", "reg Y on X, Z, and CV")

    if (to_return == "short") {
        output <- list(rxcvGz, rycvGz, rxcv, rycv)
        return(output)
    } 
    
    if (to_return == "full") {
        output <- list(rxcvGz, rycvGz, rxcv, rycv,
                       cov_pse, fTable)
    }
    
}
    