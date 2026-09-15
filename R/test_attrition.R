# robust_attrition.R
# Robustness of inferences to differential attrition (beta).
# Standalone exported function.

#' Robustness of inferences to differential attrition (beta)
#'
#' Quantifies the conditions necessary in the attritted (missing)
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
#' @param ycmi optional. The assumed mean outcome among the missing
#'   control cases. This is the anchor for the mean-difference framework
#'   in equation (7). Defaults to \code{yobc}, the observed control mean,
#'   which assumes no outcome selection among controls.
#' @param ypooled optional. The assumed mean outcome across all missing
#'   cases, treatment and control combined. This is the earlier
#'   parameterization. It describes the same relationship as \code{ycmi}
#'   and is converted internally, so supply one or the other, not both.
#' @param alpha significance level (default 0.05).
#' @param pad_frac half-width of the plotting window for
#'   \code{to_return = "plot"}, as a fraction of the observed
#'   pooled mean. Defaults to 0.15 (a +/- 15% band). Larger
#'   values draw a longer line.
#' @param verbose if TRUE, the printed output includes a short
#'   description of what each block computes and inline notes
#'   about internally-computed defaults. Defaults to FALSE.
#' @param to_return one of "print" (default) to display output
#'   and return the result list invisibly, "raw_output" to
#'   return the full list, or "plot" to draw and return the
#'   figures. The "plot" option produces three figures: the
#'   effect required in the missing data to nullify the
#'   inference across assumed missing-data means under the
#'   mean-difference framework, the same relationship under the
#'   correlation-based framework, and the four cell means implied
#'   by each framework.
#'
#' @return A list with components \code{inputs}, \code{nonpar},
#'   \code{correlation_based}, and \code{derived}. Returned
#'   invisibly when \code{to_return = "print"}. When
#'   \code{to_return = "plot"}, a named list of ggplot objects
#'   (\code{robustness}, \code{robustness_corr}, \code{means}) is
#'   returned invisibly instead.
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
#' @importFrom ggplot2 ggplot geom_hline geom_abline annotate
#'   coord_cartesian labs theme_bw theme
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
        ycmi = NULL, # assumed mean outcome among the missing control cases
        ypooled = NULL, # assumed mean outcome across all missing cases (treatment + control)
        alpha = 0.05,
        pad_frac = 0.15,
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
  if (!is.numeric(pad_frac) || length(pad_frac) != 1 ||
      is.na(pad_frac) || pad_frac <= 0) {
      stop("pad_frac must be a single positive numeric value.")
  }
  
  # ===================================================================
  # Shared preliminaries
  # ===================================================================
  symi <- syob
  esteffect <- yobt - yobc

  # guard for zero estimated effect
  if (esteffect == 0) {
      stop("The observed treatment effect is exactly zero. ",
           "There is no inference to nullify.")
  }
  
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
  rthresh <- sign(esteffect) * tcrit / sqrt(kdf + tcrit^2)

  pi_miss <- (ntreatmi + ncontrolmi) / fulln
  xob <- ntreatob / (ntreatob + ncontrolob)
  yob <- xob * yobt + (1 - xob) * yobc
  deltasig <- sign(esteffect) * tcrit * se * sqrt((nobs - 3) / (fulln - 3))
  #ythresh <- yob
  nintended <- ntreattot + ncontroltot

  # guard: no inference to nullify (observed effect does not exceed delta#)
  if (abs(esteffect) <= abs(deltasig)) {
      stop("The observed effect (", sprintf("%.4f", esteffect), ") does not exceed ",
           "the inference threshold delta# (", sprintf("%.4f", abs(deltasig)), " in magnitude). ",
           "No inference is supported at this threshold, so there is nothing to nullify.")
  }
  
  # ===================================================================
  # Mean-difference anchor
  # ===================================================================
  # From equation (6), the combined-sample effect decomposes as
  #   delta_combined = delta_ob + alphat*(delta_mi - delta_ob) + (alphac - alphat)*(yobc - ycmi).
  # Setting delta_combined = delta# and solving for delta_mi (equation 7) gives a relationship 
  # that is linear in ycmi, the control mean among the missing cases:
  #   delta_mi(ycmi) = nonpar_intercept + nonpar_slope * ycmi
  
  nonpar_slope <- (alphac - alphat) / alphat
  nonpar_intercept <- (deltasig + (alphat - alphac) * yobc -
                       (1 - alphat) * esteffect) / alphat
  
  # Share of the missing cases that are in the treatment group. Used to
  # convert between ycmi and the pooled missing-data mean.
  
  # w_ for weight
  w_treat_mi <- ntreatmi / (ntreatmi + ncontrolmi)
  
  # The anchor may be given as ycmi (equation 7, the default) or as the
  # pooled mean across all missing cases (the earlier parameterization).
  # The two describe the same relationship, so ypooled is converted to
  # ycmi and a single computation follows.
  if (!is.null(ycmi) && !is.null(ypooled)) {
      stop("Supply ycmi or ypooled, not both. They are two parameterizations ",
           "of the same relationship.")
  }
  
  if (!is.null(ypooled)) {
      if (!is.numeric(ypooled) || length(ypooled) != 1 || is.na(ypooled)) {
          stop("ypooled must be a single numeric value.")
      }
      # Y# = ycmi + w_treat_mi * delta_mi(ycmi); invert for ycmi
      ycmi <- (ypooled - w_treat_mi * nonpar_intercept) /
          (1 + w_treat_mi * nonpar_slope)
      nonpar_anchor <- "ypooled"
  } else if (!is.null(ycmi)) {
      if (!is.numeric(ycmi) || length(ycmi) != 1 || is.na(ycmi)) {
          stop("ycmi must be a single numeric value.")
      }
      nonpar_anchor <- "ycmi"
  } else {
      ycmi <- yobc
      nonpar_anchor <- "default"
  }
  
  # ===================================================================
  # Mean-difference block
  # ===================================================================
  # retired and collapsed with ycmi
  # nonpar_ycontrolmi <- (
  #     ythresh * (alphat * xbar + alphac * (1 - xbar)) -
  #         xbar * ((alphat - 1) * yobt - alphac * yobc + yobc + deltasig)
  # ) / alphac
  # 
  # nonpar_ytreatmi <- (
  #     ythresh * (alphat * xbar + alphac * (1 - xbar)) -
  #         alphac * (1 - xbar) * nonpar_ycontrolmi
  # ) / (alphat * xbar)
  # 
  # nonpar_deltami <- nonpar_ytreatmi - nonpar_ycontrolmi

  nonpar_ycontrolmi <- ycmi
  nonpar_deltami <- nonpar_intercept + nonpar_slope * ycmi
  nonpar_ytreatmi <- ycmi + nonpar_deltami
  
  # Pooled mean across all missing cases implied by this anchor. 
  # Reported so the result can be read under either parameterization.
  nonpar_ypooled <- ycmi + w_treat_mi * nonpar_deltami
  
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

  # nonpar_slope <- alphac - xbar * (alphac - alphat)
  
  # nonpar_slope and nonpar_intercept is defined above
  # nonpar_lineslope <- (alphac - alphat) *
  #     (alphac - xbar * (alphac - alphat)) / (alphat * alphac)
  # nonpar_intercept <- (alphac - xbar * (alphac - alphat)) *
  #     ((1 - alphac) * yobc - (1 - alphat) * yobt + deltasig) /
  #     (alphat * alphac)
  
  # ===================================================================
  # Correlation-based block
  # ===================================================================
  sxob <- sqrt(xob * (1 - xob))
  xmi <- ntreatmi / (ntreatmi + ncontrolmi)
  sxmi <- sqrt(xmi * (1 - xmi))
  ymi <- yob

  a_corr <- (1 - pi_miss) * sxob^2 + pi_miss * sxmi^2 +
      (1 - pi_miss) * pi_miss * (xob - xmi)^2
  
  # Single source of truth for the correlation-based block. Returns the
  # correlation in the missing data required to nullify the inference for a
  # specified mean outcome in the missing data (Y#). Used both for the
  # reported value (at Y# = ymi) and for the Y# sensitivity plot, so the
  # two can never be different.
  #
  # NOTE: from equation (12) in the main text page 17 the third term of
  # b_h has been update to (yob - y_hash)^2, not (xob - xmi)^2. 
  corr_r_at <- function(y_hash) {
      b_h <- (1 - pi_miss) * syob^2 + pi_miss * symi^2 +
          (1 - pi_miss) * pi_miss * (yob - y_hash)^2
      c_h <- (1 - pi_miss) * rob * sxob * syob +
          (1 - pi_miss) * pi_miss * (xob - xmi) * (yob - y_hash)
      (rthresh * sqrt(a_corr * b_h) - c_h) / (pi_miss * symi * sxmi)
  }
 
  corr_rmi <- corr_r_at(ymi)

  # A correlation required to nullify cannot exceed [-1, 1].
  # When it does, no correlation in the attritted data could nullify the
  # inference, so the implied coefficient and its test are not defined.
  corr_valid <- abs(corr_rmi) <= 1
  
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

  # Implied regression coefficient in the missing data required to nullify
  # the inference, as a function of the specified missing-data mean Y#.
  corr_delta_at <- function(y_hash) {
      corr_r_at(y_hash) * corr_sdyGzmi / corr_sdxGzmi
  }
  
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

  if (corr_valid) {
      sdyGxmi  <- symi * sqrt(1 - corr_rmi^2)
      ssdyGxmi <- (sdyGxmi * sqrt(nmiss - 1))^2
  } else {
      sdyGxmi  <- NA_real_
      ssdyGxmi <- NA_real_
  }

  varxob <- xob * (1 - xob)
  sx2ob <- varxob * nobs
  varxmi <- xmi * (1 - xmi)
  sx2mi <- varxmi * nmiss

  if (corr_valid) {
      corr_tdenom1  <- (ssdyGxob + ssdyGxmi) / (nobs + nmiss - 4)
      corr_tdenom2  <- (sx2ob + sx2mi) / (sx2ob * sx2mi)
      corr_sebeta   <- sqrt(corr_tdenom1 * corr_tdenom2)
      
      corr_betadiff <- corr_rbetami - esteffect
      corr_tbeta    <- corr_betadiff / corr_sebeta
      corr_p_value  <- 2 * stats::pt(
          q = abs(corr_tbeta), df = kdf, lower.tail = FALSE
      )
  } else {
      corr_tdenom1  <- NA_real_
      corr_tdenom2  <- NA_real_
      corr_sebeta   <- NA_real_
      corr_betadiff <- corr_rbetami - esteffect
      corr_tbeta    <- NA_real_
      corr_p_value  <- NA_real_
  }

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
  # Printed report (shared by to_return = "print" and "plot")
  # ===================================================================
  print_report <- function() {
      cat(crayon::bold("[BETA] Robustness to Differential Attrition\n\n"))
      
      if (isTRUE(verbose)) {
          cat("This analysis quantifies the conditions necessary in the\n")
          cat("attritted (missing) data to nullify an inference based on the\n")
          cat("observed data. Two complementary blocks are reported:\n")
          cat("  (1) Mean-difference: the treatment estimate in the missing data\n")
          cat("      required to bring the combined-sample effect to the\n")
          cat("      significance threshold.\n")
          cat("  (2) Correlation-based: the correlation (and implied\n")
          cat("      regression coefficient) in the missing data required to\n")
          cat("      nullify the inference, plus two root-finding bounds\n")
          cat("      from the attrition x treatment interaction.\n\n")
      }
      
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
      
      cat("Mean-difference framework:\n")
      if (isTRUE(verbose)) {
          cat("  (effect in missing data required to pull the\n")
          cat("   combined-sample effect to the inference threshold)\n")
      }
      # added block based on YCmi or ypooled usage
      cat(sprintf("  Assumed control mean in missing data (YCmi): %.4f%s\n",
                  ycmi,
                  switch(nonpar_anchor,
                         default = "  [default: observed control mean]",
                         ypooled = "  [converted from ypooled]",
                         "")))
      cat(sprintf("  Implied pooled mean in missing data (Y#): %.4f\n",
                  nonpar_ypooled))
      
      cat(sprintf("  Effect in missing data to nullify: %.4f\n",
                  nonpar_deltami))
      cat(sprintf("  Effect in combined data: %.4f\n",
                  nonpar_deltacomb))
      cat("\n  For the model Y = B0 + B1*Missing + B2*X + B3*X*Missing:\n")
      cat(sprintf("    B1: %.4f\n", nonpar_beta1))
      cat(sprintf("    B3: %.4f\n", nonpar_beta3))
      cat("\n")
      
      cat("Correlation-based framework:\n")
      if (isTRUE(verbose)) {
          cat("  (correlation in missing data required to nullify,\n")
          cat("   with the implied regression coefficient and the\n")
          cat("   attrition x treatment interaction)\n")
      }
      
      if (corr_valid) {
          cat(sprintf("  Effect in missing data to nullify: %.4f\n",
                      corr_rbetami))
          cat("\n  For the model Y = B0 + B1*Missing + B2*X + B3*X*Missing:\n")
          cat(sprintf("    B3: %.4f\n", corr_rbeta3))
          cat(sprintf("    p-value for B3: %.4f\n", corr_p_value))
      } else {
          cat(sprintf("\n  Required correlation in missing data: %.4f\n", corr_rmi))
          cat("\n  This exceeds the attainable range for a correlation.\n")
          cat("  No correlation in the attritted data could nullify this\n")
          cat("  inference, so the implied coefficient and its test are\n")
          cat("  not defined.\n")
      }
      
      cat("\n  Min missing effect to nullify:\n")
      cat(sprintf("    +root: %.4f\n", corr_BMIp))
      cat(sprintf(
          "    -root: %.4f  [more threatening boundary: smaller deviation from observed effect]\n",
          corr_BMIn
      ))
      cat("\n")
      
      if (isTRUE(verbose)) {
          cat("For all computed quantities, run with to_return = \"raw_output\".\n")
          cat("See ?robust_attrition for help.\n")
      } else {
          cat("For interpretation guidance, run with verbose = TRUE.\n")
          cat("For all computed quantities, run with to_return = \"raw_output\".\n")
      }
      cat("\n")
  }
  
  # ===================================================================
  # Output
  # ===================================================================
  if (to_return == "print") {
      
      print_report()
      
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
              ycmi = ycmi,
              ypooled = nonpar_ypooled,
              anchor = nonpar_anchor,
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
        line_slope = nonpar_slope,
        line_intercept = nonpar_intercept
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
        corr_valid = corr_valid,
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
        #ythresh = ythresh,
        nintended = nintended
      )
    )
    return(output)
    
  } else if (to_return == "plot") {
      
      # Print the same report shown by to_return = "print", honoring verbose,
      # then build and return the two plots.
      print_report()
      
      # x-window: centered on the assumed mean outcome among missing control cases 
      # (now, ycmi; previously, yob), Half-width defaults to pad_frac (15%) of |ycmi|; 
      # raise pad_frac for a longer line.
      center <- ycmi
      half <- pad_frac * abs(center)
      if (!is.finite(half) || half <= 0) {
          half <- max(abs(esteffect), 1)
      }
      x_lo <- center - half
      x_hi <- center + half
      
      # y-window: cover the line across the x-window, the original point,
      # and the delta_mi = 0 reference, with a small margin.
      y_lo_line <- nonpar_intercept + nonpar_slope * x_lo
      y_hi_line <- nonpar_intercept + nonpar_slope * x_hi
      y_pts <- c(y_lo_line, y_hi_line, nonpar_deltami, 0)
      y_min <- min(y_pts)
      y_max <- max(y_pts)
      y_margin <- 0.08 * (y_max - y_min)
      if (!is.finite(y_margin) || y_margin <= 0) y_margin <- 1
      
      # in-panel key (upper-left, away from the rising line)
      legend_x <- x_lo + 0.005 * (x_hi - x_lo)
      legend_y <- (y_max + y_margin) -
          0.01 * ((y_max + y_margin) - (y_min - y_margin))
      legend_label <- paste0(
          switch(nonpar_anchor,
                 default = "Red point: YCmi = observed control mean\n",
                 ypooled = "Red point: YCmi converted from ypooled\n",
                 "Red point: supplied YCmi\n"),
          "Line slope: rise in delta_mi per unit of YCmi\n",
          sprintf("YCmi = %.2f,  delta_mi = %.2f,  slope = %.3f",
                  center, nonpar_deltami, nonpar_slope)
      )
      
      p <- ggplot2::ggplot() +
          # delta_mi = 0 reference (no effect in the missing data)
          ggplot2::geom_hline(yintercept = 0, color = "grey70",
                              linewidth = 0.5) +
          # required missing-data effect across assumed missing-data means
          ggplot2::geom_abline(intercept = nonpar_intercept,
                               slope = nonpar_slope,
                               color = "#1F78B4", linewidth = 1.1) +
          # guide lines to the original-calculation point
          ggplot2::annotate("segment", x = center, xend = center,
                            y = -Inf, yend = nonpar_deltami,
                            color = "#E31A1C", linetype = "dashed",
                            linewidth = 0.6) +
          ggplot2::annotate("segment", x = -Inf, xend = center,
                            y = nonpar_deltami, yend = nonpar_deltami,
                            color = "#E31A1C", linetype = "dashed",
                            linewidth = 0.6) +
          ggplot2::annotate("point", x = center, y = nonpar_deltami,
                            color = "#E31A1C", size = 2.6) +
          ggplot2::annotate("text", x = center, y = nonpar_deltami,
                            label = "Original calculation",
                            color = "#E31A1C", hjust = -0.05, vjust = 1.6,
                            size = 3.4) +
          ggplot2::annotate("label", x = legend_x, y = legend_y,
                            hjust = 0, vjust = 1, label = legend_label,
                            size = 11 / ggplot2::.pt, color = "grey20",
                            fill = "white", lineheight = 1.1) +
          ggplot2::coord_cartesian(xlim = c(x_lo, x_hi),
                                   ylim = c(y_min - y_margin,
                                            y_max + y_margin)) +
          ggplot2::labs(
              title = "Robustness of inference to differential attrition",
              subtitle = paste0(
                  "Effect in the missing data necessary to nullify the observed ",
                  "inference,\nby assumed control mean in the missing data ",
                  "(mean-difference framework)"
              ),
              x = "Assumed control mean in the missing data  (YCmi)",
              y = "Required effect in the missing data  (delta_mi)"
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
              plot.title.position = "plot",
              plot.title = ggplot2::element_text(size = 16),
              plot.subtitle = ggplot2::element_text(size = 14),
              axis.title = ggplot2::element_text(size = 14),
              axis.text = ggplot2::element_text(size = 13),
              strip.text = ggplot2::element_text(size = 13),
              legend.text = ggplot2::element_text(size = 13),
              legend.position = "bottom"
          )

      # ---- Correlational framework version of Figure A1 (Figure A2) ----
      # The correlation-based framework is anchored at the pooled observed
      # mean, not at ycmi, so it gets its own x-window. The curve is
      # evaluated on a grid rather than drawn with geom_abline, because the
      # relationship is exactly linear only under the b_h term as currently
      # coded (see the note at corr_r_at above).
      
      # The correlation-based framework is anchored at the pooled observed
      # mean, not at ycmi. Use its own window.
      corr_center <- yob
      corr_half   <- pad_frac * abs(corr_center)
      if (!is.finite(corr_half) || corr_half <= 0) corr_half <- max(abs(esteffect), 1)
      corr_x_lo <- corr_center - corr_half
      corr_x_hi <- corr_center + corr_half
      corr_legend_x <- corr_x_lo + 0.005 * (corr_x_hi - corr_x_lo)
     
      corr_pad <- 0.06 * (corr_x_hi - corr_x_lo)    
      
      corr_grid <- seq(corr_x_lo - corr_pad, corr_x_hi + corr_pad, length.out = 201)
      corr_df <- data.frame(
          y_hash = corr_grid,
          delta  = vapply(corr_grid, corr_delta_at, numeric(1))
      )
      
      # local slope at the original-calculation point, by central difference
      corr_h <- max(1e-4, 1e-3 * abs(corr_center))
      corr_slope <- (corr_delta_at(corr_center + corr_h) -
                         corr_delta_at(corr_center - corr_h)) / (2 * corr_h)
      
      cy_pts <- c(corr_delta_at(corr_x_lo), corr_delta_at(corr_x_hi), corr_rbetami, 0)
      cy_min <- min(cy_pts)
      cy_max <- max(cy_pts)
      cy_margin <- 0.08 * (cy_max - cy_min)
      if (!is.finite(cy_margin) || cy_margin <= 0) cy_margin <- 1
      
      corr_legend_y <- (cy_max + cy_margin) -
          0.01 * ((cy_max + cy_margin) - (cy_min - cy_margin))
      corr_legend_label <- paste0(
          "Red point: original calculation (Y# = pooled mean)\n",
          "Line slope: rise in delta|Z_mi per unit of Y#\n",
          sprintf("Y# = %.2f,  delta|Z_mi = %.2f,  slope = %.3f",
                  corr_center, corr_rbetami, corr_slope)
      )
      
      p_corr <- ggplot2::ggplot() +
          ggplot2::geom_hline(yintercept = 0, color = "grey70",
                              linewidth = 0.5) +
          ggplot2::geom_line(
              data = corr_df, inherit.aes = FALSE,
              ggplot2::aes(x = y_hash, y = delta),
              color = "#6A3D9A", linewidth = 1.1
          ) +
          ggplot2::annotate("segment", x = corr_center, xend = corr_center,
                            y = -Inf, yend = corr_rbetami,
                            color = "#E31A1C", linetype = "dashed",
                            linewidth = 0.6) +
          ggplot2::annotate("segment", x = -Inf, xend = corr_center,
                            y = corr_rbetami, yend = corr_rbetami,
                            color = "#E31A1C", linetype = "dashed",
                            linewidth = 0.6) +
          ggplot2::annotate("point", x = corr_center, y = corr_rbetami,
                            color = "#E31A1C", size = 2.6) +
          ggplot2::annotate("text", x = corr_center, y = corr_rbetami,
                            label = "Original calculation",
                            color = "#E31A1C", hjust = -0.05, vjust = 1.6,
                            size = 3.4) +
          ggplot2::annotate("label", x = corr_legend_x, y = corr_legend_y,
                            hjust = 0, vjust = 1, label = corr_legend_label,
                            size = 11 / ggplot2::.pt, color = "grey20",
                            fill = "white", lineheight = 1.1) +
          ggplot2::coord_cartesian(xlim = c(corr_x_lo, corr_x_hi),
                                   ylim = c(cy_min - cy_margin,
                                            cy_max + cy_margin)) +
          ggplot2::labs(
              title = "Robustness of inference to differential attrition",
              subtitle = paste0(
                  "Effect in the missing data necessary to nullify the ",
                  "observed inference,\nby assumed mean outcome in the ",
                  "missing data (correlation-based framework)"
              ),
              x = "Assumed mean outcome in the missing data  (Y#)",
              y = "Required effect in the missing data  (delta|Z_mi)"
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
              plot.title.position = "plot",
              plot.title = ggplot2::element_text(size = 16),
              plot.subtitle = ggplot2::element_text(size = 14),
              axis.title = ggplot2::element_text(size = 14),
              axis.text = ggplot2::element_text(size = 13),
              strip.text = ggplot2::element_text(size = 13),
              legend.text = ggplot2::element_text(size = 13),
              legend.position = "bottom"
          )      
            
      # ---- Second plot: cell means by group and attrition status ----
      # Four cell means from Y = b0 + b1*M + b2*T + b3*M*T. The observed
      # (M = 0) cells are shared; the attritted (M = 1) cells differ between
      # the mean-difference and correlation-based (parametric) frameworks.
      means_df <- data.frame(
          framework = factor(
              rep(c("Mean-difference", "Correlation-based"), each = 4),
              levels = c("Mean-difference", "Correlation-based")
          ),
          group = factor(
              rep(c("Treatment", "Treatment", "Control", "Control"), times = 2),
              levels = c("Treatment", "Control")
          ),
          M = rep(c(0, 1, 0, 1), times = 2),
          Y = c(
              yobt, nonpar_ytreatmi, yobc, nonpar_ycontrolmi,
              yobt, corr_ytreatmi,   yobc, corr_Ycontrolmi
          )
      )
      # push each value label vertically away from the lines: the upper
      # point in a column goes up, the lower point goes down. Within each
      # (framework, M) column, compare the two group means.
      means_df$vj_lab <- ave(
          means_df$Y, means_df$framework, means_df$M,
          FUN = function(v) ifelse(v == max(v), -0.8, 1.8)
      )
      
      # delta indicators: observed effect (shared) and the framework-specific
      # attritted effect, drawn just outside the M = 0 and M = 1 columns.
      gap_df <- data.frame(
          framework = factor(
              rep(c("Mean-difference", "Correlation-based"), each = 2),
              levels = c("Mean-difference", "Correlation-based")
          ),
          x   = rep(c(-0.08, 1.08), times = 2),
          ylo = c(pmin(yobt, yobc), pmin(nonpar_ytreatmi, nonpar_ycontrolmi),
                  pmin(yobt, yobc), pmin(corr_ytreatmi,  corr_Ycontrolmi)),
          yhi = c(pmax(yobt, yobc), pmax(nonpar_ytreatmi, nonpar_ycontrolmi),
                  pmax(yobt, yobc), pmax(corr_ytreatmi,  corr_Ycontrolmi)),
          val = c(esteffect, nonpar_deltami, esteffect, corr_rbetami),
          sym = c("ob", "mi", "ob", "mi")
      )
      gap_df$hj  <- ifelse(gap_df$x < 0, 1.15, -0.15)
      gap_df$txt <- sprintf("hat(delta)^{%s} == %.4f", gap_df$sym, gap_df$val)
      
      p_means <- ggplot2::ggplot(
          means_df,
          ggplot2::aes(x = M, y = Y, color = group, group = group)
      ) +
          # treatment-control gap (double-headed) behind the lines
          ggplot2::geom_segment(
              data = gap_df, inherit.aes = FALSE,
              ggplot2::aes(x = x, xend = x, y = ylo, yend = yhi),
              color = "grey45", linewidth = 0.5,
              arrow = grid::arrow(ends = "both",
                                  length = grid::unit(0.05, "in"))
          ) +
          ggplot2::geom_text(
              data = gap_df, inherit.aes = FALSE, parse = TRUE,
              ggplot2::aes(x = x, y = (ylo + yhi) / 2, label = txt, hjust = hj),
              color = "grey20", size = 4.2
          ) +
          ggplot2::geom_line(linewidth = 1.1) +
          ggplot2::geom_point(size = 2.6) +
          ggplot2::geom_text(
              ggplot2::aes(label = sprintf("%.3f", Y), vjust = vj_lab),
              color = "grey20", size = 4.2, show.legend = FALSE
          ) +
          ggplot2::facet_wrap(~ framework) +
          ggplot2::scale_color_manual(
              values = c("Treatment" = "#1F78B4", "Control" = "#33A02C")
          ) +
          ggplot2::scale_x_continuous(
              breaks = c(0, 1),
              labels = c("Observed\n(M = 0)", "Attritted\n(M = 1)"),
              limits = c(-0.55, 1.55)
          ) +
          ggplot2::scale_y_continuous(
              expand = ggplot2::expansion(mult = 0.09)
          ) +
          ggplot2::labs(
              title = expression(
                  paste("Treatment estimates (", hat(delta)^{mi},
                        ") in missing (attritted) data necessary to nullify the inference")
              ),
              subtitle = paste0(
                  "Observed estimate on the left side (M = 0) and missing right side (M = 1).\n",
                  "The left side is the observed treatment estimate; the right side is the treatment estimate \n",
                  "in the missing data necessary to nullify the inference."
              ),
              x = "Missing status, M",
              y = "Outcome mean, Y",
              color = NULL
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
              plot.title.position = "plot",
              plot.title = ggplot2::element_text(size = 16),
              plot.subtitle = ggplot2::element_text(size = 14),
              legend.position = "bottom"
          )
      
      print(p)
      print(p_corr)
      print(p_means)
      return(invisible(list(robustness = p,
                            robustness_corr = p_corr,
                            means = p_means)))
      
  } else {
      stop("to_return must be one of \"print\", \"raw_output\", or \"plot\".")
  }
}
