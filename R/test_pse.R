test_pse <- function(est_eff,
                     std_err,
                     n_obs,
                     n_covariates,
                     eff_thr,
                     sdx,
                     sdy,
                     R2,
                     to_return){
    
    ## test exampple
    est_eff = .125
    std_err = .050
    n_obs = 6174
    n_covariates = 7
    sdx = .217
    sdy = .991 
    R2 = .251
    eff_thr = 0.1
    
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
    
    kryx = (rxy - rxz * ryz)/(1 - rxz^2) ## kryx should be equal to ryxGz 
    thr = eff_thr * sdx / sdy
    sdz = sdcv = 1
    
    Gz_pse <- cal_pse(thr, kryx)
    rxcvGz = as.numeric(Gz_pse[1])
    rycvGz = as.numeric(Gz_pse[2])
    
    # convert conditional correlations to unconditional correlations to be used in new regression
    rxcv = rxcvGz * sqrt((1 - rcvz^2) * (1 - rxz^2)) + rxz * rcvz
    rycv = rycvGz * sqrt((1 - rcvz^2)*(1 - rzy^2)) + rzy * rcvz
    
    verify_pse = verify_reg_Gzcv(99999, sdx, sdy, sdz, sdcv, 
                                   rxy, rxz, rzy, rycv, rxcv, rcvz)
}
    