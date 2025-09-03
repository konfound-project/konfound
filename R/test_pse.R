test_pse <- function(est_eff,
                     std_err,
                     n_obs,
                     n_covariates, # the number of z  
                     eff_thr,
                     sdx,
                     sdy,
                     R2,
                     to_return){
    
    ## test_pse(est_eff = .5, std_err = .056, n_obs = 6174, 
    ##         eff_thr = .1, sdx = 0.22, sdy = 1, R2 = .3,to_return = "full")
    ## prepare input
    var_x <- sdx^2
    var_y <- sdy^2
    var_z <- sdz <- 1
    df <- n_obs - n_covariates - 3
    
    ## error message if input is inappropriate
    if (!(std_err > 0)) {stop("Did not run! Standard error needs
                              to be greater than zero.")}
    if (!(sdx > 0)) {stop("Did not run! Standard deviation of
                          x needs to be greater than zero.")}
    if (!(sdy > 0)) {stop("Did not run! Standard deviation of
                          y needs to be greater than zero.")}
    if (!(n_obs > n_covariates + 3)) {
      stop("Did not run! There are too few observations relative
           to the number of observations and covariates.
           Please specify a less complex model to use KonFound-It.")}
    if (!(0 < R2)) {stop("Did not run! R2 needs to be greater than zero.")}
    if (!(R2 < 1)) {stop("Did not run! R2 needs to be less than one.")}
    if (!(1-((sdy^2/sdx^2)*(1-R2)/((df+1) * std_err^2))>0)) {
      stop("Did not run! Entered values produced Rxz^2 <=0,
           consider adding more significant digits to your entered values.")}
    
    ## test boundary for PSE RIR
    alpha = 0.05; tails = 2 # default for PSE
    if (est_eff < 0) {
        thr_t <- qt(1 - alpha / tails, df) * -1
    } else {
        thr_t <- qt(1 - alpha / tails, df)
    }
    
    ## now standardize 
    beta_thr <- eff_thr * sdx / sdy
    beta <- est_eff * sdx / sdy
    SE <- std_err * sdx / sdy
    
    ## observed regression, reg y on x Given z
    tyxGz <- beta / SE  ### this should be equal to est_eff / std_err
    ryxGz <- tyxGz / sqrt(df + tyxGz^2)
    
    ## make sure R2 due to x alone is not larger than overall or observed R2
    if (ryxGz^2 > R2) {stop("Error! ryxGz^2 > R2")}
    
    ## calculate ryz, rxz, rxy
    ryz <- rzy <- cal_ryz(ryxGz, R2)
    rxz <- rzx <- cal_rxz(var_x, var_y, R2, df+1, std_err)
    rxy <- ryx <- cal_rxy(ryxGz, rxz, ryz)
    
    thr <- eff_thr * sdx / sdy
    sdz <- sdcv <- 1
    rcvz <- rzcv <- 0
    
    Gz_pse <- cal_pse(thr, ryxGz)
    rxcvGz <- as.numeric(Gz_pse[1])
    rycvGz <- as.numeric(Gz_pse[2])
    
    # convert conditional correlations to unconditional correlations
    # to be used in new regression
    rxcv <- rxcvGz * sqrt((1 - rcvz^2) * (1 - rxz^2)) + rxz * rcvz
    rycv <- rycvGz * sqrt((1 - rcvz^2) * (1 - rzy^2)) + rzy * rcvz
    
    verify_pse_reg_M3 <- verify_reg_Gzcv(n_obs, sdx, sdy,
                                         sdz, sdcv, rxy,
                                         rxz, rzy, rycv,
                                         rxcv, rcvz)
    verfiy_pse_manual_thr <- verify_manual(rxy, rxz, rxcv,
                                           ryz, rycv, rzcv,
                                           sdy, sdx)
    cov_pse <- verify_pse_reg_M3[[11]]
    
    # prepare some other values in the final Table (long output)
    R2_M3 <- as.numeric(verify_pse_reg_M3[1])
    eff_x_M3 <- as.numeric(verify_pse_reg_M3[2])
    # should be equivalent or very close to eff_thr
    se_x_M3 <- as.numeric(verify_pse_reg_M3[3])
    beta_x_M3 <- as.numeric(verify_pse_reg_M3[9])
    # should be equivalent or very close to thr
    t_x_M3 <- eff_x_M3 / se_x_M3
    eff_z_M3 <- as.numeric(verify_pse_reg_M3[4])
    se_z_M3 <- as.numeric(verify_pse_reg_M3[5])
    eff_cv_M3 <- as.numeric(verify_pse_reg_M3[6])
    se_cv_M3 <- as.numeric(verify_pse_reg_M3[7])
    
    verify_pse_reg_M2 <- verify_reg_Gz(n_obs, sdx, sdy, sdz, rxy, rxz, rzy)
    R2_M2 <- as.numeric(verify_pse_reg_M2[1])
    eff_x_M2 <- as.numeric(verify_pse_reg_M2[2])
    # should be equivalent or very close to est_eff
    se_x_M2 <- as.numeric(verify_pse_reg_M2[3])
    # should be equivalent or very close to std_err
    eff_z_M2 <- as.numeric(verify_pse_reg_M2[4])
    se_z_M2 <- as.numeric(verify_pse_reg_M2[5])
    t_x_M2 <- eff_x_M2 / se_x_M2
    
    verify_pse_reg_M1 <- verify_reg_uncond(n_obs, sdx, sdy, rxy)
    R2_M1 <- as.numeric(verify_pse_reg_M1[1])
    # should be equivalent or very close to rxy^2
    eff_x_M1 <- as.numeric(verify_pse_reg_M1[2])
    # should be equivalent or very close to rxy*sdy/sdx
    se_x_M1 <- as.numeric(verify_pse_reg_M1[3])
    t_x_M1 <- eff_x_M1 / se_x_M1
    
    fTable <- matrix(c(R2_M1, R2_M2, R2_M3, # R2 for three reg models
                       eff_x_M1, eff_x_M2, eff_x_M3,
                       # unstd reg coef for X in three reg  models
                       se_x_M1, se_x_M2, se_x_M3,
                       # unstd reg se for X in three reg models
                       rxy, ryxGz, beta_x_M3,
                       # std reg coef for X in three reg models
                       t_x_M1, t_x_M2, t_x_M3,
                       # t values for X in three reg models
                       NA, eff_z_M2, eff_z_M3,
                       # reg coef for Z in three reg models
                       NA, se_z_M2, se_z_M3,
                       # se for Z in three reg models
                       NA, eff_z_M2 / se_z_M2, eff_z_M3 / se_z_M3,
                       # t for Z in three reg models,
                       NA, NA, eff_cv_M3,
                       # reg coef for CV in three reg models
                       NA, NA, se_cv_M3,
                       # se for CV in three reg models
                       NA, NA, eff_cv_M3 / se_cv_M3),
                     # t for CV in three reg models
                       nrow = 11, ncol = 3, byrow = TRUE)
    
    rownames(fTable) <- c("R2", "coef_X", "SE_X", "std_coef_X", "t_X",
                          "coef_Z", "SE_Z", "t_Z",
                          "coef_CV", "SE_CV", "t_CV")
    
    colnames(fTable) <- c("M1:X", "M2:X,Z", "M3:X,Z,CV")
    
    # compute PSE-RIR results
    pse_rir_result <- se_preserve_replacement(
        est_eff = est_eff,
        std_err = std_err,       
        n_obs = n_obs,
        n_covariates = n_covariates,
        sd_x = sdx,
        sd_y_obs = sdy
    )
    
    invalidate_ob <- isinvalidate(thr_t, tyxGz)
    
    if (to_return == "raw_output") {
        output <- list("correlation between X and CV conditional on Z" = rxcvGz, 
                       "correlation between Y and CV conditional on Z" = rycvGz, 
                       "correlation between X and CV" = rxcv, 
                       "correlation between Y and CV" = rycv,
                       "covariance matrix" = cov_pse, 
                       "eff_M3" = eff_x_M3,
                       "se_M3" = se_x_M3,
                       "Table" = fTable,
                       "RIR_perc" = pse_rir_result$pi,
                       "standard deviation of unobserved Y" = pse_rir_result$sd_y_unobs
)
        return(output)
    }
    
    if (to_return == "print") {
        cat(crayon::bold("Component Correlations (fixed standard error):\n"))
        cat("This function calculates the correlations associated with an omitted confounding \nvariable (CV) that generate an estimated effect that is approximately equal to \nthe threshold while preserving the originally reported standard error.\n\n")
        cat(sprintf("The correlation between X (focal predictor) and CV is %.3f, and the correlation between\nY (outcome) and CV is %.3f.\n\n", rxcv, rycv))
        cat(sprintf("Conditional on the covariates, the correlation between X and CV is %.3f,\nand the correlation between Y and CV is %.3f.\n\n", rxcvGz, rycvGz))
        cat(sprintf("Including such a CV, the coefficient would change to %.3f, with standard error\nof %.3f.\n\n", eff_x_M3, se_x_M3))
        
        cat(crayon::bold("Robustness of Inference to Replacement (fixed standard error)\n"))
        
        change <- if (invalidate_ob) "nullify the" else "sustain an"
        
        cat(sprintf(
            "To %s inference while preserving the reported standard error,\napproximately pi = %.3f (%.1f%% of cases) of the data points would need to be replaced.\n",
            change, pse_rir_result$pi, 100 * pse_rir_result$pi
        ))
        cat(sprintf(
            "This corresponds to replacing about %.0f of %d observations.\n\n",
            n_obs * pse_rir_result$pi, n_obs
        ))
        cat(sprintf(
            "The replacement cases would need to have a standard deviation of Y equal to %.2f\nin order to maintain the same standard error of beta.\n\n",
            pse_rir_result$sd_y_unobs
        ))
        cat(sprintf(
            paste0(
                "Therefore, if %.0f%% of the cases are replaced with cases for which the effect \n",
                "is modified as above, the estimated effect will be %.2f with standard error \n",
                "of %.2f associated with p = %.2f.\n\n"
            ),
            100 * pse_rir_result$pi, pse_rir_result$est_eff_new, std_err, alpha
        ))
        
        cat("Use to_return = \"raw_output\" to see more specific results.\n")
    }
        
    
}
