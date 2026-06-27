program define robust_attrition, rclass
    version 15.1

    * --- parsing block (KEEP THIS) ---
    gettoken args 0 : 0, parse(",")
    local args : list retokenize args

    if substr(`"`0'"', 1, 1) == "," {
        local 0 = substr(`"`0'"', 2, .)
    }
    local opts : list retokenize 0

    local alpha 0.05
    local r2xz
    local r2yz
    local verbose 0
    local pad_frac 0.15
    local graph 0
    local alpha 0.05
    local r2xz
    local r2yz
    local verbose 0
	
    while `"`opts'"' != "" {
        gettoken tok opts : opts, bind
        if regexm(`"`tok'"', "^[ ]*alpha\(([^()]*)\)[ ]*$") {
            local alpha = regexs(1)
        }
        else if regexm(`"`tok'"', "^[ ]*r2xz\(([^()]*)\)[ ]*$") {
            local r2xz = regexs(1)
        }
        else if regexm(`"`tok'"', "^[ ]*r2yz\(([^()]*)\)[ ]*$") {
            local r2yz = regexs(1)
        }
		else if regexm(`"`tok'"', "^[ ]*verbose[ ]*$") {
            local verbose 1
        }
        else if regexm(`"`tok'"', "^[ ]*pad_frac\(([^()]*)\)[ ]*$") {
            local pad_frac = regexs(1)
        }
        else if regexm(`"`tok'"', "^[ ]*graph[ ]*$") {
            local graph 1
        }
        else {
            di as err "option `tok' not allowed"
            exit 198
        }
    }

    if `: word count `args'' != 10 {
        di as err "syntax: robust_attrition se ntreatob ncontrolob ntreattot ncontroltot yobt yobc syob ncovar r2 [, r2xz(#) r2yz(#) alpha(#) pad_frac(#) graph verbose]"
        exit 198
    }

    local se          : word 1  of `args'
    local ntreatob    : word 2  of `args'
    local ncontrolob  : word 3  of `args'
    local ntreattot   : word 4  of `args'
    local ncontroltot : word 5  of `args'
    local yobt        : word 6  of `args'
    local yobc        : word 7  of `args'
    local syob        : word 8  of `args'
    local ncovar      : word 9  of `args'
    local r2          : word 10 of `args'

    capture confirm number `se'
    if _rc {
        di as err "se must be numeric"
        exit 198
    }
    capture confirm number `ntreatob'
    if _rc {
        di as err "ntreatob must be numeric"
        exit 198
    }
    capture confirm number `ncontrolob'
    if _rc {
        di as err "ncontrolob must be numeric"
        exit 198
    }
    capture confirm number `ntreattot'
    if _rc {
        di as err "ntreattot must be numeric"
        exit 198
    }
    capture confirm number `ncontroltot'
    if _rc {
        di as err "ncontroltot must be numeric"
        exit 198
    }
    capture confirm number `yobt'
    if _rc {
        di as err "yobt must be numeric"
        exit 198
    }
    capture confirm number `yobc'
    if _rc {
        di as err "yobc must be numeric"
        exit 198
    }
    capture confirm number `syob'
    if _rc {
        di as err "syob must be numeric"
        exit 198
    }
    capture confirm number `ncovar'
    if _rc {
        di as err "ncovar must be numeric"
        exit 198
    }
    capture confirm number `r2'
    if _rc {
        di as err "r2 must be numeric"
        exit 198
    }

    if (`se' <= 0) {
        di as err "se must be positive"
        exit 198
    }
    if (`syob' <= 0) {
        di as err "syob must be positive"
        exit 198
    }
	if (`ntreatob' != round(`ntreatob') | `ncontrolob' != round(`ncontrolob') | ///
        `ntreattot' != round(`ntreattot') | `ncontroltot' != round(`ncontroltot')) {
        di as err "ntreatob, ncontrolob, ntreattot, and ncontroltot must be whole numbers"
        exit 198
    }
    if (`ntreatob' < 0 | `ncontrolob' < 0) {
        di as err "ntreatob and ncontrolob must be nonnegative"
        exit 198
    }
    if (`ntreattot' <= 0 | `ncontroltot' <= 0) {
        di as err "ntreattot and ncontroltot must be positive"
        exit 198
    }
    if (`ntreatob' > `ntreattot') {
        di as err "ntreatob cannot exceed ntreattot"
        exit 198
    }
    if (`ncontrolob' > `ncontroltot') {
        di as err "ncontrolob cannot exceed ncontroltot"
        exit 198
    }
    if (`ncovar' < 0) {
        di as err "ncovar must be nonnegative"
        exit 198
    }
	if (`ncovar' != round(`ncovar')) {
        di as err "ncovar must be a whole number"
        exit 198
    }
    if (`ncovar' >= `ntreatob' + `ncontrolob' - 2) {
        di as err "ncovar is too large: it must be less than ntreatob + ncontrolob - 2 so that the observed-data degrees of freedom remain positive"
        exit 198
    }
    if (`r2' < 0 | `r2' >= 1) {
        di as err "r2 must satisfy 0 <= r2 < 1"
        exit 198
    }
    if (`alpha' <= 0 | `alpha' >= 1) {
        di as err "alpha must satisfy 0 < alpha < 1"
        exit 198
    }
    if (`pad_frac' <= 0) {
        di as err "pad_frac must be positive"
        exit 198
    }

    if ("`r2xz'" != "") {
        if (`r2xz' < 0 | `r2xz' >= 1) {
            di as err "r2xz must satisfy 0 <= r2xz < 1 when supplied"
            exit 198
        }
    }

    if ("`r2yz'" != "") {
        if (`r2yz' < 0 | `r2yz' >= 1) {
            di as err "r2yz must satisfy 0 <= r2yz < 1 when supplied"
            exit 198
        }
    }

    tempname symi esteffect xbar tob ntreatmi ncontrolmi nmiss alphat alphac deltaob nobs df rob
    tempname fulln kdf tcrit rthresh pi xob yob deltasig ythresh nintended
    tempname np_ycontrolmi np_ycontrolmib np_ytreatmi np_deltami np_ycombc np_ycombt np_deltacomb
    tempname np_beta0 np_beta1 np_beta2 np_beta3 np_pyobc np_pymic np_pyobt np_pyfull np_slope
    tempname np_lineslope np_intercept
    tempname sxob xmi sxmi ymi a_corr b_corr c_corr corr_rmi
    tempname R2yz_default R2xz_default R2yz_used R2xz_used
    tempname corr_sdyGzmi corr_sdyGzmit corr_sdyGzmic corr_sdxGzmi corr_rbetami corr_Ycontrolmi corr_ytreatmi corr_rbeta3
    tempname corr_num corr_denom1 corr_denom2 corr_rcomb
    tempname sdyGxob ssdyGxob sdyGxmi ssdyGxmi varxob sx2ob varxmi sx2mi corr_tdenom1 corr_tdenom2 corr_sebeta corr_betadiff corr_tbeta corr_p_value
    tempname b_root c_root j_root k_root d_root e_root f_root g_root q_root m_root w_root z_root u_root t_root p_root a_root
    tempname corr_rlimnbak corr_rlimpbak corr_expr1 corr_rlimp corr_rlimn corr_BMIp corr_BMIn l_root corr_checkn corr_tvalidatep corr_tvalidaten

    scalar `symi' = `syob'
    scalar `esteffect' = `yobt' - `yobc'
    scalar `xbar' = `ntreattot' / (`ntreattot' + `ncontroltot')
    scalar `tob' = `esteffect' / `se'

    scalar `ntreatmi' = `ntreattot' - `ntreatob'
    scalar `ncontrolmi' = `ncontroltot' - `ncontrolob'
    scalar `nmiss' = `ntreatmi' + `ncontrolmi'

	if (`nmiss' == 0) {
        di as err "No attrition detected: ntreatob equals ntreattot and ncontrolob equals ncontroltot; the attrition analysis is not defined when there is no missing data"
        exit 198
    }
	
    scalar `alphat' = `ntreatmi' / `ntreattot'
    scalar `alphac' = `ncontrolmi' / `ncontroltot'
    scalar `deltaob' = `yobt' - `yobc'
    scalar `nobs' = `ntreatob' + `ncontrolob'
    scalar `df' = `nobs' - `ncovar' - 2
    scalar `rob' = `tob' / sqrt(`df' + `tob'^2)

    scalar `fulln' = `nobs' + `ntreatmi' + `ncontrolmi'
    scalar `kdf' = `fulln' - `ncovar' - 2
    scalar `tcrit' = invttail(`kdf', `alpha'/2)
    scalar `rthresh' = `tcrit' / sqrt(`kdf' - `tcrit'^2)

    scalar `pi' = (`ntreatmi' + `ncontrolmi') / `fulln'
    scalar `xob' = `ntreatob' / (`ntreatob' + `ncontrolob')
    scalar `yob' = `xob' * `yobt' + (1-`xob') * `yobc'
    scalar `deltasig' = `tcrit' * `se' * sqrt((`nobs'-3)/(`fulln'-3))
    scalar `ythresh' = `yob'
    scalar `nintended' = `ntreattot' + `ncontroltot'

    scalar `np_ycontrolmi' = (`ythresh' * (`alphat' * `xob' + `alphac' * (1-`xob')) - ///
        `xob' * ((`alphat' - 1) * `yobt' - `alphac' * `yobc' + `yobc' + `deltasig')) / `alphac'
    scalar `np_ycontrolmib' = (`ythresh' * (`alphat' * `xbar' + `alphac' * (1-`xbar')) - ///
        `xbar' * ((`alphat' - 1) * `yobt' - `alphac' * `yobc' + `yobc' + `deltasig')) / `alphac'
    scalar `np_ytreatmi' = (`ythresh' * (`alphat' * `xob' + `alphac' * (1-`xob')) - ///
        `alphac' * (1-`xob') * `np_ycontrolmi') / (`alphat' * `xob')
    scalar `np_deltami' = `np_ytreatmi' - `np_ycontrolmi'
    scalar `np_ycombc' = (1-`alphac') * `yobc' + `alphac' * `np_ycontrolmi'
    scalar `np_ycombt' = (1-`alphat') * `yobt' + `alphat' * `np_ytreatmi'
    scalar `np_deltacomb' = `np_ycombt' - `np_ycombc'
    scalar `np_beta0' = `yobc'
    scalar `np_beta1' = `np_ycontrolmi' - `yobc'
    scalar `np_beta2' = `yobt' - `yobc'
    scalar `np_beta3' = (`np_ytreatmi' - `np_ycontrolmi') - `np_beta2'
    scalar `np_pyobc' = `np_beta0'
    scalar `np_pymic' = `np_beta0' + `np_beta1'
    scalar `np_pyobt' = `np_beta0' + `np_beta2'
    scalar `np_pyfull' = `np_beta0' + `np_beta1' + `np_beta2' + `np_beta3'
    scalar `np_slope' = `alphac' - `xob' * (`alphac' - `alphat')

    * delta_mi as a linear function of the specified missing-data mean Y#:
    * delta_mi(Y#) = np_intercept + np_lineslope * Y#
    * The line passes through the original-calculation point
    * (Y# = yob, delta_mi = np_deltami).
    scalar `np_lineslope' = (`alphac' - `alphat') * ///
        (`alphac' - `xob' * (`alphac' - `alphat')) / (`alphat' * `alphac')
    scalar `np_intercept' = (`alphac' - `xob' * (`alphac' - `alphat')) * ///
        ((1 - `alphac') * `yobc' - (1 - `alphat') * `yobt' + `deltasig') / ///
        (`alphat' * `alphac')

    scalar `sxob' = sqrt(`xob' * (1-`xob'))
    scalar `xmi' = `ntreatmi' / (`ntreatmi' + `ncontrolmi')
    scalar `sxmi' = sqrt(`xmi' * (1-`xmi'))
    scalar `ymi' = `yob'

    scalar `a_corr' = (1-`pi')*`sxob'^2 + `pi'*`sxmi'^2 + (1-`pi')*`pi'*(`xob'-`xmi')^2
    scalar `b_corr' = (1-`pi')*`syob'^2 + `pi'*`symi'^2 + (1-`pi')*`pi'*(`xob'-`xmi')^2
    scalar `c_corr' = (1-`pi')*`rob'*`sxob'*`syob' + (1-`pi')*`pi'*(`xob'-`xmi')*(`yob'-`ymi')
    scalar `corr_rmi' = (`rthresh'*sqrt(`a_corr'*`b_corr') - `c_corr') / (`pi'*`symi'*`sxmi')

    scalar `R2yz_default' = (`r2' - `rob'^2) / (1 - `rob'^2)
    scalar `R2xz_default' = 1 - (`syob'^2) * (1 - `r2') / ((`sxob'^2) * `df' * `se'^2)

    if ("`r2yz'" == "") scalar `R2yz_used' = `R2yz_default'
    else scalar `R2yz_used' = `r2yz'
    if ("`r2xz'" == "") scalar `R2xz_used' = `R2xz_default'
    else scalar `R2xz_used' = `r2xz'

    if (`R2yz_used' < 0 | `R2yz_used' >= 1) {
        di as err "The resulting R2yz value is invalid. It must satisfy 0 <= R2yz < 1."
        exit 198
    }
    if (`R2xz_used' < 0 | `R2xz_used' >= 1) {
        di as err "The resulting R2xz value is invalid. It must satisfy 0 <= R2xz < 1."
        exit 198
    }

    scalar `corr_sdyGzmi' = sqrt(1-`R2yz_used') * `symi'
    scalar `corr_sdyGzmit' = `corr_sdyGzmi'
    scalar `corr_sdyGzmic' = `corr_sdyGzmi'
    scalar `corr_sdxGzmi' = sqrt(1-`R2xz_used') * `sxmi'
    scalar `corr_rbetami' = `corr_rmi' * `corr_sdyGzmi' / `corr_sdxGzmi'

    scalar `corr_Ycontrolmi' = `yob' - `xmi' * `corr_rbetami'
    scalar `corr_ytreatmi' = `corr_rbetami' + `corr_Ycontrolmi'
    scalar `corr_rbeta3' = `corr_rbetami' - `esteffect'

    scalar `corr_num' = (1-`pi')*`rob'*`sxob'*`syob' + `pi'*`corr_rmi'*`sxmi'*`symi' + (1-`pi')*`pi'*(`xob'-`xmi')*(`yob'-`ymi')
    scalar `corr_denom1' = (1-`pi')*`sxob'^2 + `pi'*`sxmi'^2 + (1-`pi')*`pi'*(`xob'-`xmi')^2
    scalar `corr_denom2' = (1-`pi')*`syob'^2 + `pi'*`symi'^2 + (1-`pi')*`pi'*(`yob'-`ymi')^2
    scalar `corr_rcomb' = `corr_num' / sqrt(`corr_denom1' * `corr_denom2')

    scalar `sdyGxob' = `syob' * sqrt(1-`rob'^2)
    scalar `ssdyGxob' = (`sdyGxob' * sqrt(`nobs'-1))^2
    scalar `sdyGxmi' = `symi' * sqrt(1-`corr_rmi'^2)
    scalar `ssdyGxmi' = (`sdyGxmi' * sqrt(`nmiss'-1))^2
    scalar `varxob' = `xob'*(1-`xob')
    scalar `sx2ob' = `varxob' * `nobs'
    scalar `varxmi' = `xmi'*(1-`xmi')
    scalar `sx2mi' = `varxmi' * `nmiss'
    scalar `corr_tdenom1' = (`ssdyGxob' + `ssdyGxmi') / (`nobs' + `nmiss' - 4)
    scalar `corr_tdenom2' = (`sx2ob' + `sx2mi') / (`sx2ob' * `sx2mi')
    scalar `corr_sebeta' = sqrt(`corr_tdenom1' * `corr_tdenom2')
    scalar `corr_betadiff' = `corr_rbetami' - `esteffect'
    scalar `corr_tbeta' = `corr_betadiff' / `corr_sebeta'
    scalar `corr_p_value' = 2 * ttail(`kdf', abs(`corr_tbeta'))

    scalar `b_root' = `corr_sdyGzmi'^2
    scalar `c_root' = `varxmi' * (1-`R2xz_used')
    scalar `j_root' = sqrt(`b_root'/`c_root')
    scalar `k_root' = `varxob'
    scalar `d_root' = `nmiss'
    scalar `e_root' = `r2'
    scalar `f_root' = `nobs'
    scalar `g_root' = `rob'^2
    scalar `q_root' = `d_root' + `f_root' - 4
    scalar `m_root' = (`d_root'*`varxmi' + `f_root'*`k_root') / (`d_root'*`varxmi'*`f_root'*`k_root')
    scalar `w_root' = (`d_root'*`syob'*`syob'/`q_root') * `m_root'
    scalar `z_root' = ((`d_root'*`symi'*`symi'*(1-`e_root'+`g_root') + `f_root'*(1-`e_root')*`syob'*`syob')/`q_root')*`m_root'
    scalar `u_root' = `m_root' * `w_root'
    scalar `t_root' = `tcrit'
    scalar `p_root' = `m_root' * `z_root'
    scalar `a_root' = `esteffect'
    scalar `corr_rlimnbak' = ((`a_root'*`j_root')-`t_root'*sqrt(-`a_root'^2*`u_root'+`j_root'^2*`p_root'+`p_root'*`t_root'^2*`u_root')) / (`j_root'^2+`t_root'^2*`u_root')
    scalar `corr_rlimpbak' = ((`a_root'*`j_root')+`t_root'*sqrt(-`a_root'^2*`u_root'+`j_root'^2*`p_root'+`p_root'*`t_root'^2*`u_root')) / (`j_root'^2+`t_root'^2*`u_root')
    scalar `corr_expr1' = -`a_root'^2*`w_root' + `j_root'^2*`z_root' + `z_root'*`t_root'^2*`w_root'
    scalar `corr_rlimp' = ((`a_root'*`j_root')+`t_root'*sqrt(`corr_expr1')) / (`j_root'^2+`t_root'^2*`w_root')
    scalar `corr_rlimn' = ((`a_root'*`j_root')-`t_root'*sqrt(`corr_expr1')) / (`j_root'^2+`t_root'^2*`w_root')
    scalar `corr_BMIp' = `corr_rlimp' * `j_root'
    scalar `corr_BMIn' = `corr_rlimn' * `j_root'
    scalar `l_root' = -`w_root'
    scalar `corr_checkn' = ((`a_root'*`j_root')-`t_root'*sqrt((-`a_root'^2*`l_root'+`j_root'^2*`z_root'+`z_root'*`t_root'^2*`l_root'))) / (`j_root'^2+`t_root'^2*`l_root')
    scalar `corr_tvalidatep' = (`a_root' - `j_root'*`corr_rlimp') / sqrt(`z_root' - `w_root'*`corr_rlimp'^2)
    scalar `corr_tvalidaten' = (`a_root' - `j_root'*`corr_rlimn') / sqrt(`z_root' - `w_root'*`corr_rlimn'^2)

* ===================================================================
    * Output
    * ===================================================================
    di as txt _newline "{bf:[BETA] Robustness to Differential Attrition}"
    di ""

    * Verbose opening (context)
    if (`verbose') {
        di as txt "This analysis quantifies what would have to be true in the"
        di as txt "attritted (missing) data to nullify an inference based on"
        di as txt "the observed data. Two complementary blocks are reported:"
        di as txt "  (1) Nonparametric: the treatment effect in the missing"
        di as txt "      data required to bring the combined-sample effect"
        di as txt "      to the significance threshold."
        di as txt "  (2) Correlation-based: the correlation (and implied"
        di as txt "      regression coefficient) in the missing data required"
        di as txt "      to nullify the inference, plus two root-finding"
        di as txt "      bounds from the attrition x treatment interaction."
        di ""
    }

    * Observed inputs
    di as txt "Observed treatment mean: " %5.4f `yobt'
    di as txt "Observed control mean: " %5.4f `yobc'
    di as txt "Estimated effect (observed): " %5.4f `esteffect'
    di as txt "Observed sample: " %4.0f `ntreatob' " treatment, " ///
              %4.0f `ncontrolob' " control"
    di as txt "Intended sample: " %4.0f `ntreattot' " treatment, " ///
              %4.0f `ncontroltot' " control"
    di as txt "Missing: " %4.0f `ntreatmi' " treatment (" ///
              %3.1f 100*`alphat' "%), " ///
              %4.0f `ncontrolmi' " control (" ///
              %3.1f 100*`alphac' "%)"

    * Verbose notes on internally-computed defaults
    if (`verbose') {
        if ("`r2xz'" == "") {
            di as txt "  Note: r2xz not supplied; using default " ///
                      %5.4f `R2xz_default'
        }
        if ("`r2yz'" == "") {
            di as txt "  Note: r2yz not supplied; using default " ///
                      %5.4f `R2yz_default'
        }
    }

    di ""

    * Nonparametric block
    di as txt "{bf:Nonparametric:}"
    if (`verbose') {
        di as txt "  (effect in missing data required to pull the"
        di as txt "   combined-sample effect to the inference threshold)"
    }
    di as txt "  Effect in missing data to nullify: " %5.4f `np_deltami'
    di as txt "  Effect in combined data: " %5.4f `np_deltacomb'
    di ""

    * Correlation-based block
    di as txt "{bf:Correlation-based:}"
    if (`verbose') {
        di as txt "  (correlation in missing data required to nullify,"
        di as txt "   with the implied regression coefficient and the"
        di as txt "   attrition x treatment interaction)"
    }
    di as txt "  Effect in missing data to nullify: " %5.4f `corr_rbetami'
    di ""
    di as txt "  For the model Y = B0 + B1*X + B2*Missing + B3*X*Missing:"
    di as txt "    B3: " %5.4f `corr_rbeta3'
    di as txt "    p-value for B3: " %5.4f `corr_p_value'
    di ""
    di as txt "  Min missing effect to nullify:"
    di as txt "    +root: " %5.4f `corr_BMIp'
    di as txt "    -root: " %5.4f `corr_BMIn' ///
              "  [more threatening boundary:"
    di as txt "            smaller deviation from observed effect]"
    di ""

    * Footer
    if (`verbose') {
        di as txt "All computed quantities are returned in r()."
        di as txt "Type 'help robust_attrition' for documentation."
    }
    else {
        di as txt "For interpretation guidance, run with the verbose option."
        di as txt "All computed quantities are returned in r()."
    }
    di ""

    if (`graph') {
        di as txt "See the figure: effect in the missing data required to nullify"
        di as txt "the inference, across assumed missing-data means. The red point"
        di as txt "marks the original calculation (Y# = observed pooled mean)."
        di ""
    }
    else {
        di as txt "To plot the effect in the missing data required to nullify the"
        di as txt "inference across assumed missing-data means, add the graph option."
        di ""
    }

    return scalar nonpar_deltami = `np_deltami'
    return scalar nonpar_deltacomb = `np_deltacomb'
    return scalar nonpar_beta0 = `np_beta0'
    return scalar nonpar_beta1 = `np_beta1'
    return scalar nonpar_beta2 = `np_beta2'
    return scalar nonpar_beta3 = `np_beta3'
    return scalar nonpar_pyobc = `np_pyobc'
    return scalar nonpar_pymic = `np_pymic'
    return scalar nonpar_pyobt = `np_pyobt'
    return scalar nonpar_pyfull = `np_pyfull'
    return scalar nonpar_slope = `np_slope'
    return scalar nonpar_line_slope = `np_lineslope'
    return scalar nonpar_line_intercept = `np_intercept'
    return scalar corr_rmi = `corr_rmi'
    return scalar corr_rbetami = `corr_rbetami'
    return scalar corr_rbeta3 = `corr_rbeta3'
    return scalar corr_p_value = `corr_p_value'
    return scalar corr_positive_root_min_effect = `corr_BMIp'
    return scalar corr_negative_root_min_effect = `corr_BMIn'
    return scalar R2xz_used = `R2xz_used'
    return scalar R2xz_default = `R2xz_default'
	return scalar R2yz_used = `R2yz_used'
    return scalar R2yz_default = `R2yz_default'

    * ===================================================================
    * Plot (option: graph)
    * ===================================================================
    if (`graph') {

        * x-axis: centered on the observed pooled mean (yob), the mean
        * assumed for the missing data in the original calculation. Half-width
        * defaults to pad_frac (15%) of |yob|; raise pad_frac for a longer line.
        local center = `yob'
        local half = `pad_frac' * abs(`yob')
        if (`half' >= . | `half' <= 0) local half = max(abs(`esteffect'), 1)
        local x_lo = `center' - `half'
        local x_hi = `center' + `half'

        * y-axis: cover the line across the x-window, the original point,
        * and the delta_mi = 0 reference, with a small margin.
        local b_int = `np_intercept'
        local b_slp = `np_lineslope'
        local dmi   = `np_deltami'
        local y_lo_line = `b_int' + `b_slp' * `x_lo'
        local y_hi_line = `b_int' + `b_slp' * `x_hi'
        local y_min = min(`y_lo_line', `y_hi_line', `dmi', 0)
        local y_max = max(`y_lo_line', `y_hi_line', `dmi', 0)
        local y_margin = 0.08 * (`y_max' - `y_min')
        if (`y_margin' >= . | `y_margin' <= 0) local y_margin = 1
        local y_winlo = `y_min' - `y_margin'
        local y_winhi = `y_max' + `y_margin'
        local y_floor = floor(`y_winlo')

        * in-panel key (upper-left), slightly inset from the corner
        local lgd_x = `x_lo' + 0.02*(`x_hi' - `x_lo')
        local lgd_y = `y_winhi' - 0.02*(`y_winhi' - `y_winlo')
        local s_center = strtrim(string(`center', "%12.2f"))
        local s_delta  = strtrim(string(`dmi', "%12.2f"))
        local s_slope  = strtrim(string(`b_slp', "%12.3f"))
        local lgd3 = "Y# = `s_center',  delta_mi = `s_delta',  slope = `s_slope'"

        quietly {
            preserve
            clear
            set obs 100
            gen double xline_x = `x_lo' + (_n-1)*(`x_hi' - `x_lo')/99
            gen double yline_y = `b_int' + `b_slp'*xline_x

            twoway ///
                (line yline_y xline_x, lcolor(navy) lwidth(medthick)) ///
                (pci `dmi' `x_lo' `dmi' `center', lpattern(dash) lcolor(red) lwidth(thin)) ///
                (pci `y_winlo' `center' `dmi' `center', lpattern(dash) lcolor(red) lwidth(thin)) ///
                (scatteri `dmi' `center', mcolor(red) msymbol(O) msize(medium)) ///
                , ///
                yline(0, lcolor(gs11) lwidth(thin)) ///
                yscale(range(`y_winlo' `y_winhi') noextend) ///
                xscale(range(`x_lo' `x_hi') noextend) ///
                ylabel(, angle(horizontal)) ///
                plotregion(margin(zero)) ///
                title("Robustness of inference to differential attrition", size(medlarge)) ///
                subtitle("Effect the missing data would need to nullify the observed inference," "by assumed mean outcome in the missing data", size(medsmall)) ///
                xtitle("Assumed mean outcome in the missing data (Y#)") ///
                ytitle("Required effect in the missing data (delta_mi)") ///
                text(`dmi' `center' "Original calculation", placement(e) color(red) size(small)) ///
                text(`lgd_y' `lgd_x' "Red point: original calculation (Y# = pooled mean)" "Line slope: rise in delta_mi per unit of Y#" "`lgd3'", placement(se) box fcolor(white) lcolor(black) margin(small) size(small) just(left)) ///
                legend(off) ///
                graphregion(color(white)) ///
                name(attrition_plot, replace)
            restore
        }

        graph display attrition_plot
    }
end
