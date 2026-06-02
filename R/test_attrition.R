# robust_attrition.R
# Robustness of inferences to differential attrition (beta).
# Standalone exported function.

#' Robustness of inferences to differential attrition (beta)
#'
#' Quantifies what would need to be true in the attritted (missing)
#' data to nullify an inference based on the observed data, using
#' two complementary approaches: a nonparametric decomposition of
#' the combined-sample effect, and a correlation-based framework
#' adapted from Frank and Min (2007).
#'
#' @param std_err standard error of the observed treatment effect.
#' @param ntreatob number of observed treatment cases.
#' @param ncontrolob number of observed control cases.
#' @param ntreattot intended total treatment cases.
#' @param ncontroltot intended total control cases.
#' @param yobt observed treatment-group mean.
#' @param yobc observed control-group mean.
#' @param syob observed outcome standard deviation.
#' @param n_covariates number of covariates in the observed-data model.
#' @param R2 unadjusted R-squared of the observed-data model
#'   (0 <= R2 < 1).
#' @param R2xz optional predictor-covariate fit; computed
#'   internally from other inputs when NULL (default).
#' @param R2yz optional outcome-covariate fit; computed
#'   internally when NULL (default).
#' @param alpha significance level (default 0.05).
#' @param verbose if TRUE, the printed output includes a short
#'   description of what each block computes and inline notes
#'   about internally-computed defaults. Defaults to FALSE.
#' @param to_return either "print" (default) to display output
#'   and return the result list invisibly, or "raw_output" to
#'   return the full list.
#'
#' @return A list with components \code{inputs}, \code{nonpar},
#'   \code{correlation_based}, and \code{derived}. Returned
#'   invisibly when \code{to_return = "print"}.
#'
#' @details
#' This is a beta (development) version. Calculations and output
#' are under review.
#'
#' @examples
#' \dontrun{
#' robust_attrition(
#'   std_err = 2.19,
#'   ntreatob = 1817, ncontrolob = 1981,
#'   ntreattot = 2028, ncontroltot = 2311,
#'   yobt = 54.72, yobc = 49.90, syob = 29,
#'   n_covariates = 1, R2 = 0.01, R2xz = 0.479^2
#' )
#' }
#'
#' @export
robust_attrition <- function(
        std_err,
        ntreatob,
        ncontrolob,
        ntreattot,
        ncontroltot,
        yobt,
        yobc,
        syob,
        n_covariates,
        R2,
        R2xz = NULL,
        R2yz = NULL,
        alpha = 0.05,
        verbose = FALSE,
        to_return = "print"
) {
   
  # Internal names used throughout the engine
  se <- std_err
  ncovar <- n_covariates

  # ===================================================================
  # Validation
  # ===================================================================
  if (!is.numeric(se) || length(se) != 1 || is.na(se) || se <= 0) {
    stop("std_err must be a single positive numeric value.")
  }
  if (!is.numeric(ntreatob) || length(ntreatob) != 1 ||
      is.na(ntreatob) || ntreatob < 0) {
    stop("ntreatob must be a single nonnegative numeric value.")
  }
  if (ntreatob != round(ntreatob)) {
      stop("ntreatob must be a whole number.")
  }
  if (!is.numeric(ncontrolob) || length(ncontrolob) != 1 ||
      is.na(ncontrolob) || ncontrolob < 0) {
    stop("ncontrolob must be a single nonnegative numeric value.")
  }
  if (ncontrolob != round(ncontrolob)) {
      stop("ncontrolob must be a whole number.")
  }
  if (!is.numeric(ntreattot) || length(ntreattot) != 1 ||
      is.na(ntreattot) || ntreattot <= 0) {
    stop("ntreattot must be a single positive numeric value.")
  }
  if (ntreattot != round(ntreattot)) {
      stop("ntreattot must be a whole number.")
  }
  if (!is.numeric(ncontroltot) || length(ncontroltot) != 1 ||
      is.na(ncontroltot) || ncontroltot <= 0) {
    stop("ncontroltot must be a single positive numeric value.")
  }
  if (ncontroltot != round(ncontroltot)) {
      stop("ncontroltot must be a whole number.")
  }
  
  if (ntreatob > ntreattot) stop("ntreatob cannot exceed ntreattot.")
  if (ncontrolob > ncontroltot) stop("ncontrolob cannot exceed ncontroltot.")
  
  if (!is.numeric(yobt) || length(yobt) != 1 || is.na(yobt)) {
    stop("yobt must be a single numeric value.")
  }
  if (!is.numeric(yobc) || length(yobc) != 1 || is.na(yobc)) {
    stop("yobc must be a single numeric value.")
  }
  if (!is.numeric(syob) || length(syob) != 1 ||
      is.na(syob) || syob <= 0) {
    stop("syob must be a single positive numeric value.")
  }
  if (!is.numeric(ncovar) || length(ncovar) != 1 ||
      is.na(ncovar) || ncovar < 0) {
    stop("n_covariates must be a single nonnegative numeric value.")
  }
  if (ncovar != round(ncovar)) {
      stop("n_covariates must be a whole number.")
  }
  if (ncovar >= ntreatob + ncontrolob - 2) {
      stop("n_covariates is too large: it must be less than ntreatob + ncontrolob - 2 ",
           "so that the observed-data degrees of freedom remain positive.")
  }
  if (!is.numeric(R2) || length(R2) != 1 ||
      is.na(R2) || R2 < 0 || R2 >= 1) {
    stop("R2 must satisfy 0 <= R2 < 1.")
  }
  if (!is.null(R2xz) &&
      (!is.numeric(R2xz) || length(R2xz) != 1 ||
       is.na(R2xz) || R2xz < 0 || R2xz >= 1)) {
    stop("R2xz must satisfy 0 <= R2xz < 1 when supplied.")
  }
  if (!is.null(R2yz) &&
      (!is.numeric(R2yz) || length(R2yz) != 1 ||
       is.na(R2yz) || R2yz < 0 || R2yz >= 1)) {
    stop("R2yz must satisfy 0 <= R2yz < 1 when supplied.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 ||
      is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must satisfy 0 < alpha < 1.")
  }

  # ===================================================================
  # Shared preliminaries
  # ===================================================================
  symi <- syob
  esteffect <- yobt - yobc

  xbar <- ntreattot / (ntreattot + ncontroltot)
  tob <- esteffect / se

  ntreatmi <- ntreattot - ntreatob
  ncontrolmi <- ncontroltot - ncontrolob
  nmiss <- ntreatmi + ncontrolmi

  if (nmiss == 0) {
      stop("No attrition detected: ntreatob equals ntreattot AND ncontrolob equals ncontroltot. ",
           "The attrition analysis is not defined when there is no missing data.")
  }
  
  alphat <- ntreatmi / ntreattot
  alphac <- ncontrolmi / ncontroltot
  deltaob <- yobt - yobc
  nobs <- ntreatob + ncontrolob
  df <- nobs - ncovar - 2
  rob <- tob / sqrt(df + tob^2)

  fulln <- nobs + ntreatmi + ncontrolmi
  kdf <- fulln - ncovar - 2
  tcrit <- stats::qt(p = alpha / 2, df = kdf, lower.tail = FALSE)
  rthresh <- tcrit / sqrt(kdf - tcrit^2)

  pi_miss <- (ntreatmi + ncontrolmi) / fulln
  xob <- ntreatob / (ntreatob + ncontrolob)
  yob <- xob * yobt + (1 - xob) * yobc
  deltasig <- tcrit * se * sqrt((nobs - 3) / (fulln - 3))
  ythresh <- yob
  nintended <- ntreattot + ncontroltot

  # ===================================================================
  # Nonparametric block
  # ===================================================================
  nonpar_ycontrolmi <- (
    ythresh * (alphat * xob + alphac * (1 - xob)) -
    xob * ((alphat - 1) * yobt - alphac * yobc + yobc + deltasig)
  ) / alphac

  nonpar_ycontrolmib <- (
    ythresh * (alphat * xbar + alphac * (1 - xbar)) -
    xbar * ((alphat - 1) * yobt - alphac * yobc + yobc + deltasig)
  ) / alphac

  nonpar_ytreatmi <- (
    ythresh * (alphat * xob + alphac * (1 - xob)) -
    alphac * (1 - xob) * nonpar_ycontrolmi
  ) / (alphat * xob)

  nonpar_deltami <- nonpar_ytreatmi - nonpar_ycontrolmi

  nonpar_ycombc <- (1 - alphac) * yobc + alphac * nonpar_ycontrolmi
  nonpar_ycombt <- (1 - alphat) * yobt + alphat * nonpar_ytreatmi
  nonpar_deltacomb <- nonpar_ycombt - nonpar_ycombc

  nonpar_beta0 <- yobc
  nonpar_beta1 <- nonpar_ycontrolmi - yobc
  nonpar_beta2 <- yobt - yobc
  nonpar_beta3 <- (nonpar_ytreatmi - nonpar_ycontrolmi) - nonpar_beta2

  nonpar_pyobc <- nonpar_beta0
  nonpar_pymic <- nonpar_beta0 + nonpar_beta1
  nonpar_pyobt <- nonpar_beta0 + nonpar_beta2
  nonpar_pyfull <- nonpar_beta0 + nonpar_beta1 +
    nonpar_beta2 + nonpar_beta3

  nonpar_slope <- alphac - xob * (alphac - alphat)

  # ===================================================================
  # Correlation-based block
  # ===================================================================
  sxob <- sqrt(xob * (1 - xob))
  xmi <- ntreatmi / (ntreatmi + ncontrolmi)
  sxmi <- sqrt(xmi * (1 - xmi))
  ymi <- yob

  a_corr <- (1 - pi_miss) * sxob^2 + pi_miss * sxmi^2 +
    (1 - pi_miss) * pi_miss * (xob - xmi)^2
  b_corr <- (1 - pi_miss) * syob^2 + pi_miss * symi^2 +
    (1 - pi_miss) * pi_miss * (xob - xmi)^2
  c_corr <- (1 - pi_miss) * rob * sxob * syob +
    (1 - pi_miss) * pi_miss * (xob - xmi) * (yob - ymi)
  corr_rmi <- (rthresh * sqrt(a_corr * b_corr) - c_corr) /
    (pi_miss * symi * sxmi)

  if (rob^2 > R2) {
      stop("Inputs are inconsistent: the implied bivariate correlation ",
           sprintf("(rob^2 = %.4f) ", rob^2),
           sprintf("exceeds the supplied R2 = %.4f. ", R2),
           "R2 must be at least as large as rob^2, since adding covariates ",
           "cannot reduce R-squared. Check that R2 is consistent with yobt, yobc, ",
           "std_err, and the observed sample sizes.")
  }
  
  R2yz_default <- (R2 - rob^2) / (1 - rob^2)
  R2xz_default <- 1 - (syob^2) * (1 - R2) /
    ((sxob^2) * df * se^2)

  R2yz_used <- if (is.null(R2yz)) R2yz_default else R2yz
  R2xz_used <- if (is.null(R2xz)) R2xz_default else R2xz

  if (R2yz_used < 0 || R2yz_used >= 1 || is.na(R2yz_used)) {
    stop("The resulting R2yz value is invalid. ",
         "It must satisfy 0 <= R2yz < 1.")
  }
  if (R2xz_used < 0 || R2xz_used >= 1 || is.na(R2xz_used)) {
    stop("The resulting R2xz value is invalid. ",
         "It must satisfy 0 <= R2xz < 1.")
  }

  corr_sdyGzmi <- sqrt(1 - R2yz_used) * symi
  corr_sdyGzmit <- corr_sdyGzmi
  corr_sdyGzmic <- corr_sdyGzmi
  corr_sdxGzmi <- sqrt(1 - R2xz_used) * sxmi
  corr_rbetami <- corr_rmi * corr_sdyGzmi / corr_sdxGzmi

  corr_Ycontrolmi <- yob - xmi * corr_rbetami
  corr_ytreatmi <- corr_rbetami + corr_Ycontrolmi
  corr_rbeta3 <- corr_rbetami - esteffect

  corr_num <- (1 - pi_miss) * rob * sxob * syob +
    pi_miss * corr_rmi * sxmi * symi +
    (1 - pi_miss) * pi_miss * (xob - xmi) * (yob - ymi)
  corr_denom1 <- (1 - pi_miss) * sxob^2 + pi_miss * sxmi^2 +
    (1 - pi_miss) * pi_miss * (xob - xmi)^2
  corr_denom2 <- (1 - pi_miss) * syob^2 + pi_miss * symi^2 +
    (1 - pi_miss) * pi_miss * (yob - ymi)^2
  corr_rcomb <- corr_num / sqrt(corr_denom1 * corr_denom2)

  sdyGxob <- syob * sqrt(1 - rob^2)
  ssdyGxob <- (sdyGxob * sqrt(nobs - 1))^2
  sdyGxmi <- symi * sqrt(1 - corr_rmi^2)
  ssdyGxmi <- (sdyGxmi * sqrt(nmiss - 1))^2

  varxob <- xob * (1 - xob)
  sx2ob <- varxob * nobs
  varxmi <- xmi * (1 - xmi)
  sx2mi <- varxmi * nmiss

  corr_tdenom1 <- (ssdyGxob + ssdyGxmi) / (nobs + nmiss - 4)
  corr_tdenom2 <- (sx2ob + sx2mi) / (sx2ob * sx2mi)
  corr_sebeta <- sqrt(corr_tdenom1 * corr_tdenom2)

  corr_betadiff <- corr_rbetami - esteffect
  corr_tbeta <- corr_betadiff / corr_sebeta
  corr_p_value <- 2 * stats::pt(
    q = abs(corr_tbeta), df = kdf, lower.tail = FALSE
  )

  # ===================================================================
  # Minimum effect different from observed effect (root-finding)
  # ===================================================================
  b_root <- corr_sdyGzmi^2
  c_root <- varxmi * (1 - R2xz_used)
  j_root <- sqrt(b_root / c_root)
  k_root <- varxob
  d_root <- nmiss
  e_root <- R2
  f_root <- nobs
  g_root <- rob^2
  q_root <- d_root + f_root - 4

  m_root <- (d_root * varxmi + f_root * k_root) /
    (d_root * varxmi * f_root * k_root)
  w_root <- (d_root * syob * syob / q_root) * m_root
  z_root <- ((d_root * symi * symi * (1 - e_root + g_root) +
    f_root * (1 - e_root) * syob * syob) / q_root) * m_root

  u_root <- m_root * w_root
  t_root <- tcrit
  p_root <- m_root * z_root
  a_root <- esteffect

  corr_expr1 <- -a_root * a_root * w_root +
    j_root * j_root * z_root +
    z_root * t_root * t_root * w_root

  corr_rlimp <- ((a_root * j_root) +
    t_root * sqrt(corr_expr1)) /
    (j_root * j_root + t_root * t_root * w_root)

  corr_rlimn <- ((a_root * j_root) -
    t_root * sqrt(corr_expr1)) /
    (j_root * j_root + t_root * t_root * w_root)

  corr_BMIp <- corr_rlimp * j_root
  corr_BMIn <- corr_rlimn * j_root

  corr_tvalidatep <- (a_root - j_root * corr_rlimp) /
    sqrt(z_root - w_root * corr_rlimp^2)
  corr_tvalidaten <- (a_root - j_root * corr_rlimn) /
    sqrt(z_root - w_root * corr_rlimn^2)

  # ===================================================================
  # Output
  # ===================================================================
  if (to_return == "print") {
      
      # -----------------------------------------------------------
      # Header
      # -----------------------------------------------------------
      cat(crayon::bold("[BETA] Robustness to Differential Attrition\n\n"))
      
      # -----------------------------------------------------------
      # Verbose opening (context)
      # -----------------------------------------------------------
      if (isTRUE(verbose)) {
          cat("This analysis quantifies what would have to be true in the\n")
          cat("attritted (missing) data to nullify an inference based on the\n")
          cat("observed data. Two complementary blocks are reported:\n")
          cat("  (1) Nonparametric: the treatment effect in the missing data\n")
          cat("      required to bring the combined-sample effect to the\n")
          cat("      significance threshold.\n")
          cat("  (2) Correlation-based: the correlation (and implied\n")
          cat("      regression coefficient) in the missing data required to\n")
          cat("      nullify the inference, plus two root-finding bounds\n")
          cat("      from the attrition x treatment interaction.\n\n")
      }
      
      # -----------------------------------------------------------
      # Observed inputs
      # -----------------------------------------------------------
      cat(sprintf("Observed treatment mean: %.4f\n", yobt))
      cat(sprintf("Observed control mean: %.4f\n", yobc))
      cat(sprintf("Estimated effect (observed): %.4f\n", esteffect))
      cat(sprintf("Observed sample: %d treatment, %d control\n",
                  ntreatob, ncontrolob))
      cat(sprintf("Intended sample: %d treatment, %d control\n",
                  ntreattot, ncontroltot))
      cat(sprintf("Missing: %d treatment (%.1f%%), %d control (%.1f%%)\n",
                  ntreatmi, 100 * alphat,
                  ncontrolmi, 100 * alphac))
      
      # -----------------------------------------------------------
      # Verbose notes on internally-computed defaults
      # -----------------------------------------------------------
      if (isTRUE(verbose)) {
          if (is.null(R2xz)) {
              cat(sprintf(
                  "  Note: R2xz not supplied; using default %.4f\n",
                  R2xz_default
              ))
          }
          if (is.null(R2yz)) {
              cat(sprintf(
                  "  Note: R2yz not supplied; using default %.4f\n",
                  R2yz_default
              ))
          }
      }
      
      cat("\n")
      
      # -----------------------------------------------------------
      # Nonparametric block
      # -----------------------------------------------------------
      cat("Nonparametric:\n")
      if (isTRUE(verbose)) {
          cat("  (effect in missing data required to pull the\n")
          cat("   combined-sample effect to the inference threshold)\n")
      }
      cat(sprintf("  Effect in missing data to nullify: %.4f\n",
                  nonpar_deltami))
      cat(sprintf("  Effect in combined data: %.4f\n",
                  nonpar_deltacomb))
      cat("\n")
      
      # -----------------------------------------------------------
      # Correlation-based block
      # -----------------------------------------------------------
      cat("Correlation-based:\n")
      if (isTRUE(verbose)) {
          cat("  (correlation in missing data required to nullify,\n")
          cat("   with the implied regression coefficient and the\n")
          cat("   attrition x treatment interaction)\n")
      }
      cat(sprintf("  Effect in missing data to nullify: %.4f\n",
                  corr_rbetami))
      cat("\n  For the model Y = B0 + B1*X + B2*Missing + B3*X*Missing:\n")
      cat(sprintf("    B3: %.4f\n", corr_rbeta3))
      cat(sprintf("    p-value for B3: %.4f\n", corr_p_value))
      cat("\n  Min missing effect to nullify:\n")
      cat(sprintf("    +root: %.4f\n", corr_BMIp))
      cat(sprintf(
          "    -root: %.4f  [more threatening boundary: smaller deviation from observed effect]\n",
          corr_BMIn
      ))
      cat("\n")
      
      # -----------------------------------------------------------
      # Footer
      # -----------------------------------------------------------
      if (isTRUE(verbose)) {
          cat("For all computed quantities, run with to_return = \"raw_output\".\n")
          cat("See ?robust_attrition for help.\n")
      } else {
          cat("For interpretation guidance, run with verbose = TRUE.\n")
          cat("For all computed quantities, run with to_return = \"raw_output\".\n")
      }
      cat("\n")
      
      # -----------------------------------------------------------
      # Invisible return (same structure as raw_output)
      # -----------------------------------------------------------
      output <- list(
          inputs = list(
              std_err = std_err, ntreatob = ntreatob, ncontrolob = ncontrolob,
              ntreattot = ntreattot, ncontroltot = ncontroltot,
              yobt = yobt, yobc = yobc, syob = syob,
              n_covariates = n_covariates, R2 = R2,
              R2xz = R2xz_used, R2xz_default = R2xz_default,
              R2yz = R2yz_used, R2yz_default = R2yz_default,
              alpha = alpha
          ),
          nonpar = list(
              ycontrolmi = nonpar_ycontrolmi,
              ytreatmi = nonpar_ytreatmi,
              deltami = nonpar_deltami,
              ycombc = nonpar_ycombc,
              ycombt = nonpar_ycombt,
              deltacomb = nonpar_deltacomb,
              beta0 = nonpar_beta0,
              beta1 = nonpar_beta1,
              beta2 = nonpar_beta2,
              beta3 = nonpar_beta3
          ),
          correlation_based = list(
              rmi = corr_rmi,
              rbetami = corr_rbetami,
              rbeta3 = corr_rbeta3,
              p_value = corr_p_value,
              positive_root_min_effect = corr_BMIp,
              negative_root_min_effect = corr_BMIn,
              rcomb = corr_rcomb,
              sebeta = corr_sebeta
          ),
          derived = list(
              esteffect = esteffect,
              nobs = nobs,
              nmiss = nmiss,
              fulln = fulln,
              alphat = alphat,
              alphac = alphac,
              rob = rob,
              tcrit = tcrit,
              rthresh = rthresh
          )
      )
      return(invisible(output))

  } else if (to_return == "raw_output") {

    output <- list(
      inputs = list(
        std_err = std_err, ntreatob = ntreatob, ncontrolob = ncontrolob,
        ntreattot = ntreattot, ncontroltot = ncontroltot,
        yobt = yobt, yobc = yobc, syob = syob,
        n_covariates = n_covariates, R2 = R2,
        R2xz = R2xz_used, R2xz_default = R2xz_default,
        R2yz = R2yz_used, R2yz_default = R2yz_default,
        alpha = alpha
        ),
      nonpar = list(
        ycontrolmi = nonpar_ycontrolmi,
        ycontrolmib = nonpar_ycontrolmib,
        ytreatmi = nonpar_ytreatmi,
        deltami = nonpar_deltami,
        ycombc = nonpar_ycombc,
        ycombt = nonpar_ycombt,
        deltacomb = nonpar_deltacomb,
        beta0 = nonpar_beta0,
        beta1 = nonpar_beta1,
        beta2 = nonpar_beta2,
        beta3 = nonpar_beta3,
        pyobc = nonpar_pyobc,
        pymic = nonpar_pymic,
        pyobt = nonpar_pyobt,
        pyfull = nonpar_pyfull,
        slope = nonpar_slope
      ),
      correlation_based = list(
        rmi = corr_rmi,
        sdyGzmi = corr_sdyGzmi,
        sdxGzmi = corr_sdxGzmi,
        rbetami = corr_rbetami,
        Ycontrolmi = corr_Ycontrolmi,
        ytreatmi = corr_ytreatmi,
        rbeta3 = corr_rbeta3,
        rcomb = corr_rcomb,
        sebeta = corr_sebeta,
        betadiff = corr_betadiff,
        tbeta = corr_tbeta,
        p_value = corr_p_value,
        rlimp = corr_rlimp,
        rlimn = corr_rlimn,
        positive_root_min_effect = corr_BMIp,
        negative_root_min_effect = corr_BMIn,
        positive_root_t = corr_tvalidatep,
        negative_root_t = corr_tvalidaten
      ),
      derived = list(
        symi = symi,
        esteffect = esteffect,
        xbar = xbar,
        tob = tob,
        ntreatmi = ntreatmi,
        ncontrolmi = ncontrolmi,
        nmiss = nmiss,
        alphat = alphat,
        alphac = alphac,
        deltaob = deltaob,
        nobs = nobs,
        df = df,
        rob = rob,
        fulln = fulln,
        kdf = kdf,
        tcrit = tcrit,
        rthresh = rthresh,
        pi_miss = pi_miss,
        xob = xob,
        yob = yob,
        deltasig = deltasig,
        ythresh = ythresh,
        nintended = nintended
      )
    )
    return(output)
  }
}
