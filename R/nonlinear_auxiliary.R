#' @importFrom stats qt


# to evaluate whether we are moving cases to invalidate or sustain the inference
isinvalidate <- function(thr_t, ob_t) {
  if ((0 < thr_t && thr_t < ob_t) || (ob_t < thr_t && thr_t < 0)) {
    x <- TRUE
  } else {
    x <- FALSE
  }
  return(x)
}

#to evaluate what kind of switches we need-increase or decrease the odds ratio
isdcroddsratio <- function(thr_t, ob_t) {
  if (thr_t < ob_t) {
    x <- T
  } else {
    x <- F
  }
  return(x)
}

# get the a cell (control failure) for given odds_ratio, 
# se and treatment cases - first solution
get_a1_kfnl <- function(odds_ratio, std_err, n_obs, n_trm) {
  a1 <- -(1 / (2 * (1 + odds_ratio^2 + odds_ratio * (-2 + n_trm * std_err^2)))) *
    (2 * n_trm * (-1 + odds_ratio) * odds_ratio + n_trm^2 * odds_ratio * std_err^2 -
      n_obs * odds_ratio * (-2 + 2 * odds_ratio + n_trm * std_err^2) +
      sqrt(n_trm * (-n_obs + n_trm) * odds_ratio * (4 + 4 * odds_ratio^2 +
        odds_ratio * (-8 + 4 * n_obs * std_err^2 - n_obs * n_trm * std_err^4 + n_trm^2 * std_err^4))))
  return(a1)
}

# get the c cell (treatment failure) for given odds_ratio, 
# se and treatment cases - first solution
get_c1_kfnl <- function(odds_ratio, std_err, n_obs, n_trm) {
  c1 <- -((-2 * n_trm + 2 * n_trm * odds_ratio - n_obs * n_trm * odds_ratio * std_err^2 +
    n_trm^2 * odds_ratio * std_err^2 +
    sqrt(n_trm * (-n_obs + n_trm) * odds_ratio * (4 + 4 * odds_ratio^2 +
      odds_ratio * (-8 + 4 * n_obs * std_err^2 - n_obs * n_trm * std_err^4 + n_trm^2 * std_err^4)))) /
    (2 * (1 + odds_ratio^2 + odds_ratio * (-2 + n_obs * std_err^2 - n_trm * std_err^2))))
  return(c1)
}

# get the a cell (control failure) for given odds_ratio, 
# se and treatment cases - second solution
get_a2_kfnl <- function(odds_ratio, std_err, n_obs, n_trm) {
  a2 <- (1 / (2 * (1 + odds_ratio^2 + odds_ratio * (-2 + n_trm * std_err^2)))) *
    (-2 * n_trm * (-1 + odds_ratio) * odds_ratio - n_trm^2 * odds_ratio * std_err^2 +
      n_obs * odds_ratio * (-2 + 2 * odds_ratio + n_trm * std_err^2) +
      sqrt(n_trm * (-n_obs + n_trm) * odds_ratio * (4 + 4 * odds_ratio^2 +
        odds_ratio * (-8 + 4 * n_obs * std_err^2 - n_obs * n_trm * std_err^4 + n_trm^2 * std_err^4))))
  return(a2)
}

# get the c cell (treatment failure) for given odds_ratio, 
# se and treatment cases - second solution
get_c2_kfnl <- function(odds_ratio, std_err, n_obs, n_trm) {
  c2 <- (2 * n_trm - 2 * n_trm * odds_ratio + n_obs * n_trm * odds_ratio * std_err^2 -
    n_trm^2 * odds_ratio * std_err^2 +
    sqrt(n_trm * (-n_obs + n_trm) * odds_ratio * (4 + 4 * odds_ratio^2 +
      odds_ratio * (-8 + 4 * n_obs * std_err^2 - n_obs * n_trm * std_err^4 + n_trm^2 * std_err^4)))) /
    (2 * (1 + odds_ratio^2 + odds_ratio * (-2 + n_obs * std_err^2 - n_trm * std_err^2)))
  return(c2)
}

# taylor expansion to get the linear approximation around q
# move d to c
taylorexp <- function(a, b, c, d, q, thr) {
  if (q > 0 && d - q < 0) {
    q <- d - 1
  }
  if (q < 0 && c + q < 0) {
    q <- 1 - c
  }
  Num1 <- 2 * b * (c + q) * (-a * (d - q) / (b * (c + q)^2) - a / (b * (c + q))) * log(a * (d - q) / (b * (c + q)))
  Den1 <- a * (d - q) * (1 / a + 1 / b + 1 / (d - q) + 1 / (c + q))
  Num2 <- (1 / (d - q)^2 - 1 / (c + q)^2) * (log(a * (d - q) / (b * (c + q))))^2
  Den2 <- (1 / a + 1 / b + 1 / (d - q) + 1 / (c + q))^2
  # d1square is the first derivative of the squared term
  d1square <- Num1 / Den1 - Num2 / Den2
  # d1unsquare is the first derivative of the unsquared term
  d1unsquare <- d1square / (2 * log(a * (d - q) / (b * (c + q))) / sqrt(1 / a + 1 / b + 1 / (c + q) + 1 / (d - q)))
  # x is the number of cases need to be replaced 
  # solved based on the taylor expansion
  # this is the (linear approximation) of the 
  # original/unsquared term around the value of q
  x <- (thr - log(a * (d - q) / (b * (c + q))) / sqrt((1 / a + 1 / b + 1 / (c + q) + 1 / (d - q)))) / d1unsquare + q
  return(x)
}

# get t value for a contingency table
get_t_kfnl <- function(a, b, c, d) {
  if (a == 0) {a <- a + 0.5}
  if (b == 0) {b <- b + 0.5}
  if (c == 0) {c <- c + 0.5}
  if (d == 0) {d <- d + 0.5}
  est <- log(a * d / b / c)
  se <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
  t <- est / se
  return(t)
}

# round a, b, c, d
get_abcd_kfnl <- function(a1, b1, c1, d1) {
  x <- c(round(a1), round(b1), round(c1), round(d1))
  return(x)
}

# get the number of switches


getswitch <- function(table_bstart, thr_t, switch_trm, n_obs) {
  ### calculate the est and se after rounding (before any switches)
  a <- table_bstart[1]
  b <- table_bstart[2]
  c <- table_bstart[3]
  d <- table_bstart[4]
  table_start <- matrix(c(a, b, c, d), byrow = TRUE, 2, 2)
  est_eff_start <- log(a * d / b / c)
  std_err_start <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
  t_start <- get_t_kfnl(a, b, c, d)

  invalidate_start <- isinvalidate(thr_t, t_start)
  dcroddsratio_start <- isdcroddsratio(thr_t, t_start)

  if (dcroddsratio_start) {
    step <- 1 # transfer cases from D to C or A to B
  } else {
    step <- -1 # transfer cases from B to A or C to D
  }

  ### check whether it is enough to transfer all cases in one row
  if (t_start < thr_t) {
    # transfer cases from B to A or C to D to increase odds ratio
    c_tryall <- c - (c - 1) * as.numeric(switch_trm)
    d_tryall <- d + (c - 1) * as.numeric(switch_trm)
    a_tryall <- a + (b - 1) * (1 - as.numeric(switch_trm))
    b_tryall <- b - (b - 1) * (1 - as.numeric(switch_trm))
    tryall_t <- get_t_kfnl(a_tryall, b_tryall, c_tryall, d_tryall)
    tryall_est <- log(a_tryall*d_tryall/c_tryall/b_tryall)
    allnotenough <- isTRUE(
      thr_t - tryall_t > 0 & tryall_est*est_eff_start > 0
    )
  }
  if (t_start > thr_t) {
    # transfer cases from A to B or D to C to decrease odds ratio
    c_tryall <- c + (d - 1) * as.numeric(switch_trm)
    d_tryall <- d - (d - 1) * as.numeric(switch_trm)
    a_tryall <- a - (a - 1) * (1 - as.numeric(switch_trm))
    b_tryall <- b + (a - 1) * (1 - as.numeric(switch_trm))
    tryall_t <- get_t_kfnl(a_tryall, b_tryall, c_tryall, d_tryall)
    tryall_est <- log(a_tryall*d_tryall/c_tryall/b_tryall)
    allnotenough <- isTRUE(
      tryall_t - thr_t > 0 & tryall_est*est_eff_start > 0)
  }

  ### run following if transfering one row is enough
  if (!allnotenough) {
    ### calculate percent of bias and predicted switches
    if (invalidate_start) {
      perc_bias <- 1 - thr_t / t_start
    } else {
      perc_bias <- abs(thr_t - t_start) / abs(t_start)
    }
    if (switch_trm && dcroddsratio_start) {
      perc_bias_pred <- perc_bias * d * (a + c) / n_obs
    }
    if (switch_trm && !dcroddsratio_start) {
      perc_bias_pred <- perc_bias * c * (b + d) / n_obs
    }
    if (!switch_trm && dcroddsratio_start) {
      perc_bias_pred <- perc_bias * a * (b + d) / n_obs
    }
    if (!switch_trm && !dcroddsratio_start) {
      perc_bias_pred <- perc_bias * b * (a + c) / n_obs
    }

    ### calculate predicted switches based on Taylor expansion
    if (switch_trm) {
      taylor_pred <- abs(
        taylorexp(a, b, c, d, step * perc_bias_pred, thr_t))
      a_taylor <- round(a)
      b_taylor <- round(b)
      c_taylor <- round(c + taylor_pred * step)
      d_taylor <- round(d - taylor_pred * step)
    } else {
      taylor_pred <- abs(
        taylorexp(d, c, b, a, step * perc_bias_pred, thr_t))
      a_taylor <- round(a - taylor_pred * step)
      b_taylor <- round(b + taylor_pred * step)
      c_taylor <- round(c)
      d_taylor <- round(d)
    }

    ### check whether taylor_pred move too many and 
    ### causes non-positive odds ratio
    if (a_taylor <= 0) {
      b_taylor <- a_taylor + b_taylor - 1
      a_taylor <- 1
    }
    if (b_taylor <= 0) {
      a_taylor <- a_taylor + b_taylor - 1
      b_taylor <- 1
    }
    if (c_taylor <= 0) {
      d_taylor <- c_taylor + d_taylor - 1
      c_taylor <- 1
    }
    if (d_taylor <= 0) {
      c_taylor <- c_taylor + d_taylor - 1
      d_taylor <- 1
    }

    ### set brute force starting point from the taylor expansion result
    t_taylor <- get_t_kfnl(a_taylor, b_taylor, c_taylor, d_taylor)
    a_loop <- a_taylor
    b_loop <- b_taylor
    c_loop <- c_taylor
    d_loop <- d_taylor
    t_loop <- t_taylor
  }

  ### when we need to transfer two rows the previously defined 
  ### tryall are the starting point for brute force
  if (allnotenough) {
    ### Later: set tryall as the starting point and 
    ### call this getswitch function again
    a_loop <- a_tryall
    b_loop <- b_tryall
    c_loop <- c_tryall
    d_loop <- d_tryall
    t_loop <- get_t_kfnl(a_loop, b_loop, c_loop, d_loop)
  }

  ### start brute force
  if (t_loop < thr_t) {
    while (t_loop < thr_t) {
      c_loop <- c_loop - 1 * (1 - as.numeric(switch_trm == allnotenough))
      d_loop <- d_loop + 1 * (1 - as.numeric(switch_trm == allnotenough))
      a_loop <- a_loop + 1 * as.numeric(switch_trm == allnotenough)
      b_loop <- b_loop - 1 * as.numeric(switch_trm == allnotenough)
      t_loop <- get_t_kfnl(a_loop, b_loop, c_loop, d_loop)
    }
    ### make a small adjustment to make it just below/above the thresold
    if (t_start > thr_t) {
      c_final <- c_loop + 1 * (1 - as.numeric(switch_trm == allnotenough))
      d_final <- d_loop - 1 * (1 - as.numeric(switch_trm == allnotenough))
      a_final <- a_loop - 1 * as.numeric(switch_trm == allnotenough)
      b_final <- b_loop + 1 * as.numeric(switch_trm == allnotenough)
    } else if (t_start < thr_t) {
      c_final <- c_loop
      d_final <- d_loop
      a_final <- a_loop
      b_final <- b_loop
    }
  }

  if (t_loop > thr_t) {
    while (t_loop > thr_t) {
      c_loop <- c_loop + 1 * (1 - as.numeric(switch_trm == allnotenough))
      d_loop <- d_loop - 1 * (1 - as.numeric(switch_trm == allnotenough))
      a_loop <- a_loop - 1 * as.numeric(switch_trm == allnotenough)
      b_loop <- b_loop + 1 * as.numeric(switch_trm == allnotenough)
      t_loop <- get_t_kfnl(a_loop, b_loop, c_loop, d_loop)
    }
    if (t_start < thr_t) {
      c_final <- c_loop - 1 * (1 - as.numeric(switch_trm == allnotenough))
      d_final <- d_loop + 1 * (1 - as.numeric(switch_trm == allnotenough))
      a_final <- a_loop + 1 * as.numeric(switch_trm == allnotenough)
      b_final <- b_loop - 1 * as.numeric(switch_trm == allnotenough)
    } else if (t_start > thr_t) {
      c_final <- c_loop
      d_final <- d_loop
      a_final <- a_loop
      b_final <- b_loop
    }
  }

  ### so the final results (after switching) is as follows:
  est_eff_final <- log(a_final * d_final / (b_final * c_final))
  std_err_final <- sqrt(1 / a_final + 1 / b_final + 1 / c_final + 1 / d_final)
  t_final <- est_eff_final / std_err_final
  table_final <- matrix(
    c(a_final, b_final, c_final, d_final), byrow = TRUE, 2, 2)
  if (switch_trm == allnotenough) {
    final <- abs(a - a_final) + as.numeric(allnotenough) * abs(c - c_final)
  } else {
    final <- abs(c - c_final) + as.numeric(allnotenough) * abs(a - a_final)
  }

  if (allnotenough) {
    taylor_pred <- NA
    perc_bias_pred <- NA
    if (switch_trm) {
      final_extra <- abs(a - a_final)
    }
    else {
      final_extra <- abs(c - c_final)
    }
  } else {
    final_extra <- 0
  }

  ### add column and row names to contingency tables
  rownames(table_start) <- rownames(table_final) <- c("Control", "Treatment")
  colnames(table_start) <- colnames(table_final) <- c("Fail", "Success")

  ### return result
  result <- list(
    final_switch = final, table_start = table_start, 
    table_final = table_final, est_eff_start = est_eff_start,
    est_eff_final = est_eff_final, std_err_start = std_err_start,
    std_err_final = std_err_final,
    t_start = t_start, t_final = t_final, taylor_pred = taylor_pred, 
    perc_bias_pred = perc_bias_pred,
    step = step, needtworows = allnotenough, final_extra = final_extra
  )

  return(result)
}

# to calculate the new pi when there is imaginary numbers in the implied table
get_pi <- function(odds_ratio, std_err, n_obs, n_trm) {
  a <- odds_ratio * n_obs^2 * std_err^4
  b <- -a
  c <- 4 + 4 * odds_ratio^2 + odds_ratio * (-8 + 4 * n_obs * std_err^2)
  x1 <- (-b - sqrt(b^2 - 4 * a * c)) / (a * 2)
  x2 <- (-b + sqrt(b^2 - 4 * a * c)) / (a * 2)
  if (n_trm / n_obs <= 0.5) {
    x <- x1
  }
  else {
    x <- x2
  }
  return(x)
}

#' Perform a Chi-Square Test
#'
#' @description
#' `chisq_p` calculates the p-value for a chi-square test given a contingency table.
#'
#' @param a Frequency count for row 1, column 1.
#' @param b Frequency count for row 1, column 2.
#' @param c Frequency count for row 2, column 1.
#' @param d Frequency count for row 2, column 2.
#'
#' @return P-value from the chi-square test.
#' @importFrom stats chisq.test


# get p value for chi-square test 
chisq_p <- function(a, b, c, d){
  table <- matrix(c(a,b,c,d), byrow = TRUE, 2, 2)
  p <- chisq.test(table,correct = FALSE)$p.value
  return(p)
}

# get p value for exact fisher p test
fisher_p <- function(a, b, c, d){
  table <- matrix(c(a,b,c,d), byrow = TRUE, 2, 2)
  p <- suppressWarnings(fisher.test(table)$p.value)
  return(p)
}

# get chi-square value for chi-square test 
chisq_value <- function(a, b, c, d){
  table <- matrix(c(a,b,c,d), byrow = TRUE, 2, 2)
  value <- chisq.test(table,correct = FALSE)$statistic
  return(value)
}

# get odds ratio for exact fisher p test 
fisher_oddsratio <- function(a, b, c, d){
  table <- matrix(c(a,b,c,d), byrow = TRUE, 2, 2)
  value <- suppressWarnings(fisher.test(table)$estimate)
  return(value)
}

getswitch_chisq <- function(a, b, c, d, thr_p = 0.05, switch_trm = T){
odds_ratio <- a*d/(b*c)
n_cnt <- a+b
n_trm <- c+d
n_obs <- n_cnt + n_trm
est <- log(odds_ratio)

# this is the 2 by 2 table we start with
table_ob <- matrix(c(a, b, c, d), byrow = TRUE, 2, 2)
p_ob <- chisq_p(a, b, c, d)
chisq_ob <- chisq_value(a, b, c, d)

# to evaluate whether we are moving cases to 
# invalidate or sustain the inference
if (p_ob < thr_p){isinvalidate_ob <- TRUE}
if (p_ob > thr_p){isinvalidate_ob <- FALSE}

# to evaluate what kind of switches we need-increase 
#or decrease the odds ratio
if (odds_ratio >= 1) {dcroddsratio_ob <- isinvalidate_ob}
if (odds_ratio < 1) {dcroddsratio_ob <- !isinvalidate_ob}

isinvalidate_start <- isinvalidate_ob
dcroddsratio_start <- dcroddsratio_ob
p_start <- p_ob
table_start <- table_ob
t_ob <- t_start <- get_t_kfnl(a, b, c, d)
if (est < 0){
thr_t <- stats::qt(1 - thr_p/2, n_obs - 1)*(-1)
} else {
thr_t <- stats::qt(1 - thr_p/2, n_obs - 1)
}

if (dcroddsratio_start) {
  step <- 1 # transfer cases from D to C or A to B
} else {
  step <- -1 # transfer cases from B to A or C to D
}

### check whether it is enough to transfer all cases in one row
if (!dcroddsratio_start) {
  # transfer cases from B to A or C to D to increase odds ratio
  c_tryall <- c - (c - 1) * as.numeric(switch_trm)
  d_tryall <- d + (c - 1) * as.numeric(switch_trm)
  a_tryall <- a + (b - 1) * (1 - as.numeric(switch_trm))
  b_tryall <- b - (b - 1) * (1 - as.numeric(switch_trm))
  tryall_p <- chisq_p(a_tryall, b_tryall, c_tryall, d_tryall)
  tryall_est <- log(a_tryall*d_tryall/c_tryall/b_tryall)
  allnotenough <- isTRUE(
    (thr_p-tryall_p)*tryall_est< 0 & tryall_est*est > 0)
}
if (dcroddsratio_start ) {
  # transfer cases from A to B or D to C to decrease odds ratio
  c_tryall <- c + (d - 1) * as.numeric(switch_trm)
  d_tryall <- d - (d - 1) * as.numeric(switch_trm)
  a_tryall <- a - (a - 1) * (1 - as.numeric(switch_trm))
  b_tryall <- b + (a - 1) * (1 - as.numeric(switch_trm))
  tryall_p <- chisq_p(a_tryall, b_tryall, c_tryall, d_tryall)
  tryall_est <- log(a_tryall*d_tryall/c_tryall/b_tryall)
  allnotenough <- isTRUE(
    (thr_p-tryall_p)*tryall_est> 0  & tryall_est*est > 0)
}

### run following if transfering one row is enough
if (!allnotenough) {
  ### calculate percent of bias and predicted switches
  if (isinvalidate_start) {
    perc_bias <- 1 - thr_t / t_start
  } else {
    perc_bias <- abs(thr_t - t_start) / abs(t_start)
  }
  if (switch_trm && dcroddsratio_start) {
    perc_bias_pred <- perc_bias * d * (a + c) / n_obs
  }
  if (switch_trm && !dcroddsratio_start) {
    perc_bias_pred <- perc_bias * c * (b + d) / n_obs
  }
  if (!switch_trm && dcroddsratio_start) {
    perc_bias_pred <- perc_bias * a * (b + d) / n_obs
  }
  if (!switch_trm && !dcroddsratio_start) {
    perc_bias_pred <- perc_bias * b * (a + c) / n_obs
  }
  
  ### calculate predicted switches based on Taylor expansion
  if (switch_trm) {
    taylor_pred <- abs(
      taylorexp(a, b, c, d, step * perc_bias_pred, thr_t))
    a_taylor <- round(a)
    b_taylor <- round(b)
    c_taylor <- round(c + taylor_pred * step)
    d_taylor <- round(d - taylor_pred * step)
  } else {
    taylor_pred <- abs(
      taylorexp(d, c, b, a, step * perc_bias_pred, thr_t))
    a_taylor <- round(a - taylor_pred * step)
    b_taylor <- round(b + taylor_pred * step)
    c_taylor <- round(c)
    d_taylor <- round(d)
  }
  
  ### check whether taylor_pred move too many 
  ### and causes non-positive odds ratio
  if (a_taylor <= 0) {
    b_taylor <- a_taylor + b_taylor - 1
    a_taylor <- 1
  }
  if (b_taylor <= 0) {
    a_taylor <- a_taylor + b_taylor - 1
    b_taylor <- 1
  }
  if (c_taylor <= 0) {
    d_taylor <- c_taylor + d_taylor - 1
    c_taylor <- 1
  }
  if (d_taylor <= 0) {
    c_taylor <- c_taylor + d_taylor - 1
    d_taylor <- 1
  }
  
  ### set brute force starting point from the taylor expansion result
  p_taylor <- chisq_p(a_taylor, b_taylor, c_taylor, d_taylor)
  a_loop <- a_taylor
  b_loop <- b_taylor
  c_loop <- c_taylor
  d_loop <- d_taylor
  p_loop <- p_taylor
  t_loop <- get_t_kfnl(a_loop, b_loop, c_loop, d_loop)
}

### when we need to transfer two rows the previously defined 
### tryall are the starting point for brute force
if (allnotenough) {
  ### Later: set tryall as the starting point 
  ### and call this getswitch function again
  a_loop <- a_tryall
  b_loop <- b_tryall
  c_loop <- c_tryall
  d_loop <- d_tryall
  p_loop <- chisq_p(a_loop, b_loop, c_loop, d_loop)
  t_loop <- get_t_kfnl(a_loop, b_loop, c_loop, d_loop)
}

### start brute force
if (t_loop < thr_t) {
  while (t_loop < thr_t) {
    c_loop <- c_loop - 1 * (1 - as.numeric(switch_trm == allnotenough))
    d_loop <- d_loop + 1 * (1 - as.numeric(switch_trm == allnotenough))
    a_loop <- a_loop + 1 * as.numeric(switch_trm == allnotenough)
    b_loop <- b_loop - 1 * as.numeric(switch_trm == allnotenough)
    t_loop <- get_t_kfnl(a_loop, b_loop, c_loop, d_loop)
  }
  ### make a small adjustment to make it just below/above the thresold
  if (t_start > thr_t) {
    c_loopsec <- c_loop +
  1 * (1 - as.numeric(switch_trm == allnotenough))
    d_loopsec <- d_loop -
  1 * (1 - as.numeric(switch_trm == allnotenough))
    a_loopsec <- a_loop - 1 * as.numeric(switch_trm == allnotenough)
    b_loopsec <- b_loop + 1 * as.numeric(switch_trm == allnotenough)
  } else if (t_start < thr_t) {
    c_loopsec <- c_loop
    d_loopsec <- d_loop
    a_loopsec <- a_loop
    b_loopsec <- b_loop
  }
}

if (t_loop > thr_t) {
  while (t_loop > thr_t) {
    c_loop <- c_loop + 1 * (1 - as.numeric(switch_trm == allnotenough))
    d_loop <- d_loop - 1 * (1 - as.numeric(switch_trm == allnotenough))
    a_loop <- a_loop - 1 * as.numeric(switch_trm == allnotenough)
    b_loop <- b_loop + 1 * as.numeric(switch_trm == allnotenough)
    t_loop <- get_t_kfnl(a_loop, b_loop, c_loop, d_loop)
  }
  if (t_start < thr_t) {
    c_loopsec <- c_loop - 
  1 * (1 - as.numeric(switch_trm == allnotenough))
    d_loopsec <- d_loop +
  1 * (1 - as.numeric(switch_trm == allnotenough))
    a_loopsec <- a_loop + 1 * as.numeric(switch_trm == allnotenough)
    b_loopsec <- b_loop - 1 * as.numeric(switch_trm == allnotenough)
  } else if (t_start > thr_t) {
    c_loopsec <- c_loop
    d_loopsec <- d_loop
    a_loopsec <- a_loop
    b_loopsec <- b_loop
  }
}

p_loopsec <- chisq_p(a_loopsec, b_loopsec, c_loopsec, d_loopsec)

### start 2nd round brute force - use fisher 
###test p value as evaluation criterion
#### scenario 1 need to reduce odds ratio to 
###invalidate the inference-need to increase p
if (isinvalidate_start & dcroddsratio_start){
  if (p_loopsec < thr_p) {
    while (p_loopsec < thr_p) {
      c_loopsec <- c_loopsec +
    1 * (1 - as.numeric(switch_trm == allnotenough))
      d_loopsec <- 
    d_loopsec - 1 * (1 - as.numeric(switch_trm == allnotenough))
      a_loopsec <- a_loopsec - 
    1 * as.numeric(switch_trm == allnotenough)
      b_loopsec <- b_loopsec + 
    1 * as.numeric(switch_trm == allnotenough)
      p_loopsec <- chisq_p(a_loopsec, b_loopsec, c_loopsec, d_loopsec)
    }
    c_final <- c_loopsec
    d_final <- d_loopsec
    a_final <- a_loopsec
    b_final <- b_loopsec
  }
  if (p_loopsec > thr_p){ #taylor too much, return some odds ratio
    while (p_loopsec > thr_p) {
      c_loopsec <- c_loopsec - 
    1 * (1 - as.numeric(switch_trm == allnotenough))
      d_loopsec <- d_loopsec + 
    1 * (1 - as.numeric(switch_trm == allnotenough))
      a_loopsec <- a_loopsec + 
    1 * as.numeric(switch_trm == allnotenough)
      b_loopsec <- b_loopsec - 
    1 * as.numeric(switch_trm == allnotenough)
      p_loopsec <- chisq_p(a_loopsec, b_loopsec, c_loopsec, d_loopsec)
    }
    c_final <- c_loopsec + 
  1 * (1 - as.numeric(switch_trm == allnotenough))
    d_final <- d_loopsec - 
  1 * (1 - as.numeric(switch_trm == allnotenough))
    a_final <- a_loopsec - 1 * as.numeric(switch_trm == allnotenough)
    b_final <- b_loopsec + 1 * as.numeric(switch_trm == allnotenough)
  }
}

#### scenario 2 need to reduce odds ratio to 
#### sustain the inference-need to reduce p
if (!isinvalidate_start & dcroddsratio_start) {
  if (p_loopsec < thr_p) { # taylor too  much, return some odds ratio
    while (p_loopsec < thr_p) {
      c_loopsec <- c_loopsec -
    1 * (1 - as.numeric(switch_trm == allnotenough))
      d_loopsec <- d_loopsec +
    1 * (1 - as.numeric(switch_trm == allnotenough))
      a_loopsec <- a_loopsec +
    1 * as.numeric(switch_trm == allnotenough)
      b_loopsec <- b_loopsec -
    1 * as.numeric(switch_trm == allnotenough)
      p_loopsec <- chisq_p(a_loopsec, b_loopsec, c_loopsec, d_loopsec)
    }
    c_final <- c_loopsec +
  1 * (1 - as.numeric(switch_trm == allnotenough))
    d_final <- d_loopsec -
  1 * (1 - as.numeric(switch_trm == allnotenough))
    a_final <- a_loopsec -
  1 * as.numeric(switch_trm == allnotenough)
    b_final <- b_loopsec +
  1 * as.numeric(switch_trm == allnotenough)
  }
  # taylor not enough, continue to reduce odds ratio
  if (p_loopsec > thr_p){ 
    while (p_loopsec > thr_p) {
      c_loopsec <- c_loopsec +
    1 * (1 - as.numeric(switch_trm == allnotenough))
      d_loopsec <- d_loopsec -
    1 * (1 - as.numeric(switch_trm == allnotenough))
      a_loopsec <- a_loopsec -
    1 * as.numeric(switch_trm == allnotenough)
      b_loopsec <- b_loopsec +
    1 * as.numeric(switch_trm == allnotenough)
      p_loopsec <- chisq_p(
        a_loopsec, b_loopsec, c_loopsec, d_loopsec
      )
    }
    c_final <- c_loopsec
    d_final <- d_loopsec
    a_final <- a_loopsec
    b_final <- b_loopsec
  }
}

#### scenario 3 need to increase odds ratio to 
#### invalidate the inference-need to increase p
if (isinvalidate_start & !dcroddsratio_start){
  #taylor not enough, continue to increase odds ratio 
  if (p_loopsec < thr_p){ 
    while (p_loopsec < thr_p) {
      c_loopsec <- c_loopsec -
    1 * (1 - as.numeric(switch_trm == allnotenough))
      d_loopsec <- d_loopsec +
    1 * (1 - as.numeric(switch_trm == allnotenough))
      a_loopsec <- a_loopsec +
    1 * as.numeric(switch_trm == allnotenough)
      b_loopsec <- b_loopsec -
    1 * as.numeric(switch_trm == allnotenough)
      p_loopsec <- chisq_p(a_loopsec, b_loopsec, c_loopsec, d_loopsec)
    }
    c_final <- c_loopsec
    d_final <- d_loopsec
    a_final <- a_loopsec
    b_final <- b_loopsec
  }
  #taylor too much, returns some odds ratio - decrease
  if (p_loopsec > thr_p){
    while(p_loopsec > thr_p) {
      c_loopsec <- c_loopsec +
    1 * (1 - as.numeric(switch_trm == allnotenough))
      d_loopsec <- d_loopsec -
    1 * (1 - as.numeric(switch_trm == allnotenough))
      a_loopsec <- a_loopsec -
    1 * as.numeric(switch_trm == allnotenough)
      b_loopsec <- b_loopsec +
    1 * as.numeric(switch_trm == allnotenough)
      p_loopsec <- chisq_p(a_loopsec, b_loopsec, c_loopsec, d_loopsec)
    }
    c_final <- c_loopsec -
  1 * (1 - as.numeric(switch_trm == allnotenough))
    d_final <- d_loopsec +
  1 * (1 - as.numeric(switch_trm == allnotenough))
    a_final <- a_loopsec + 1 * as.numeric(switch_trm == allnotenough)
    b_final <- b_loopsec - 1 * as.numeric(switch_trm == allnotenough)
  }
}

#### scenario 4 need to increase odds ratio 
#### to sustain the inference-need to decrease p
if (!isinvalidate_start & !dcroddsratio_start){
  #taylor not enough, continue to increase odds ratio
  if (p_loopsec > thr_p){
    while (p_loopsec > thr_p){
      c_loopsec <- c_loopsec -
    1 * (1 - as.numeric(switch_trm == allnotenough))
      d_loopsec <- d_loopsec +
    1 * (1 - as.numeric(switch_trm == allnotenough))
      a_loopsec <- a_loopsec +
    1 * as.numeric(switch_trm == allnotenough)
      b_loopsec <- b_loopsec -
    1 * as.numeric(switch_trm == allnotenough)
      p_loopsec <- chisq_p(a_loopsec, b_loopsec, c_loopsec, d_loopsec)  
    }
    c_final <- c_loopsec
    d_final <- d_loopsec
    a_final <- a_loopsec
    b_final <- b_loopsec
  }
  #taylor too much, return some odds ratio - decrease
  if (p_loopsec < thr_p){
    while (p_loopsec < thr_p){
      c_loopsec <- c_loopsec +
    1 * (1 - as.numeric(switch_trm == allnotenough))
      d_loopsec <- d_loopsec -
    1 * (1 - as.numeric(switch_trm == allnotenough))
      a_loopsec <- a_loopsec -
    1 * as.numeric(switch_trm == allnotenough)
      b_loopsec <- b_loopsec +
    1 * as.numeric(switch_trm == allnotenough)
      p_loopsec <- chisq_p(a_loopsec, b_loopsec, c_loopsec, d_loopsec)
    }
    c_final <- c_loopsec -
  1 * (1 - as.numeric(switch_trm == allnotenough))
    d_final <- d_loopsec +
  1 * (1 - as.numeric(switch_trm == allnotenough))
    a_final <- a_loopsec +
  1 * as.numeric(switch_trm == allnotenough)
    b_final <- b_loopsec -
  1 * as.numeric(switch_trm == allnotenough)
  }
}

### so the final results (after switching) is as follows:
table_final <- matrix(
  c(a_final, b_final, c_final, d_final), byrow = TRUE, 2, 2
)
p_final <- chisq_p(a_final, b_final, c_final, d_final)
chisq_final <- chisq_value(a_final, b_final, c_final, d_final)

if (switch_trm == allnotenough) {
  final <- abs(a - a_final) + as.numeric(allnotenough) * abs(c - c_final)
} else {
  final <- abs(c - c_final) + as.numeric(allnotenough) * abs(a - a_final)
}

if (allnotenough) {
  taylor_pred <- NA
  perc_bias_pred <- NA
  if (switch_trm) {
    final_extra <- abs(a - a_final)
  }
  else {
    final_extra <- abs(c - c_final)
  }
} else {
  final_extra <- 0
}

total_switch <- final + allnotenough*final_extra

result <- list(final_switch = final, 
               User_enter_value = table_start, 
               Transfer_Table = table_final, 
               p_final = p_final, 
               chisq_final = chisq_final,
               needtworows=allnotenough, 
               taylor_pred = taylor_pred,
               perc_bias_pred = perc_bias_pred, 
               final_extra = final_extra, 
               dcroddsratio_ob = dcroddsratio_ob, 
               total_switch = total_switch, 
               isinvalidate_ob = isinvalidate_ob)

return(result)
}

getswitch_fisher <- function(a, b, c, d, thr_p = 0.05, switch_trm = T){
  if (a > 0 & b > 0 & c > 0 & d > 0){
    odds_ratio <- fisher_oddsratio(a, b, c, d)
  } else {
    odds_ratio <- 1 + 0.1*(a*d-b*c)
  }
  est <- log(odds_ratio)
  n_cnt <- a+b
  n_trm <- c+d
  n_obs <- n_cnt + n_trm
  
  # this is the 2 by 2 table we start with
  table_ob <- matrix(c(a, b, c, d), byrow = TRUE, 2, 2)
  p_ob <- fisher_p(a, b, c, d)
  
  # to evaluate whther we are moving cases 
  # to invalidate or sustain the inference
  if (p_ob < thr_p){isinvalidate_ob <- TRUE}
  if (p_ob > thr_p){isinvalidate_ob <- FALSE}
  
  # to evaluate what kind of switches 
  # we need-increase or decrease the odds ratio
  if (odds_ratio >= 1) {dcroddsratio_ob <- isinvalidate_ob}
  if (odds_ratio < 1) {dcroddsratio_ob <- !isinvalidate_ob}
  
  isinvalidate_start <- isinvalidate_ob
  dcroddsratio_start <- dcroddsratio_ob
  p_start <- p_ob
  table_start <- table_ob
  t_ob <- t_start <- get_t_kfnl(a, b, c, d)
  
  if (est < 0){
    thr_t <- stats::qt(1 - thr_p/2, n_obs - 1)*(-1)
  } else {
    thr_t <- stats::qt(1 - thr_p/2, n_obs - 1)
  }
  
  if (dcroddsratio_start) {
    step <- 1 # transfer cases from D to C or A to B
  } else {
    step <- -1 # transfer cases from B to A or C to D
  }
  
  ### check whether it is enough to transfer all cases in one row
  if (!dcroddsratio_start) {
    # transfer cases from B to A or C to D to increase odds ratio
    c_tryall <- c - c * as.numeric(switch_trm)
    d_tryall <- d + c * as.numeric(switch_trm)
    a_tryall <- a + b * (1 - as.numeric(switch_trm))
    b_tryall <- b - b * (1 - as.numeric(switch_trm))
    if (a_tryall == 0) {a_tryall <- a_tryall + 0.5}
    if (b_tryall == 0) {b_tryall <- b_tryall + 0.5}
    if (c_tryall == 0) {c_tryall <- c_tryall + 0.5}
    if (d_tryall == 0) {d_tryall <- d_tryall + 0.5}
    tryall_p <- fisher_p(a_tryall, b_tryall, c_tryall, d_tryall)
    tryall_est <- log(a_tryall*d_tryall/c_tryall/b_tryall)
    allnotenough <- isTRUE(
      (thr_p-tryall_p)*tryall_est< 0 & tryall_est*est > 0
    )
  }
  if (dcroddsratio_start ) {
    # transfer cases from A to B or D to C to decrease odds ratio
    c_tryall <- c + d * as.numeric(switch_trm)
    d_tryall <- d - d * as.numeric(switch_trm)
    a_tryall <- a - a * (1 - as.numeric(switch_trm))
    b_tryall <- b + a * (1 - as.numeric(switch_trm))
    if (a_tryall == 0) {a_tryall <- a_tryall + 0.5}
    if (b_tryall == 0) {b_tryall <- b_tryall + 0.5}
    if (c_tryall == 0) {c_tryall <- c_tryall + 0.5}
    if (d_tryall == 0) {d_tryall <- d_tryall + 0.5}
    tryall_p <- fisher_p(a_tryall, b_tryall, c_tryall, d_tryall)
    tryall_est <- log(a_tryall*d_tryall/c_tryall/b_tryall)
    allnotenough <- isTRUE(
      (thr_p-tryall_p)*tryall_est> 0  & tryall_est*est > 0
    )
  }
  
  ### run following if transfering one row is enough
  if (!allnotenough) {
    ### calculate percent of bias and predicted switches
    if (isinvalidate_start) {
      perc_bias <- 1 - thr_t / t_start
    } else {
      perc_bias <- abs(thr_t - t_start) / abs(t_start)
    }
    if (switch_trm && dcroddsratio_start) {
      perc_bias_pred <- perc_bias * d * (a + c) / n_obs
    }
    if (switch_trm && !dcroddsratio_start) {
      perc_bias_pred <- perc_bias * c * (b + d) / n_obs
    }
    if (!switch_trm && dcroddsratio_start) {
      perc_bias_pred <- perc_bias * a * (b + d) / n_obs
    }
    if (!switch_trm && !dcroddsratio_start) {
      perc_bias_pred <- perc_bias * b * (a + c) / n_obs
    }
    
    ### calculate predicted switches based on Taylor expansion
    if (switch_trm) {
      taylor_pred <- abs(
        taylorexp(a, b, c, d, step * perc_bias_pred, thr_t)
      )
      a_taylor <- round(a)
      b_taylor <- round(b)
      c_taylor <- round(c + taylor_pred * step)
      d_taylor <- round(d - taylor_pred * step)
    } else {
      taylor_pred <- abs(
        taylorexp(d, c, b, a, step * perc_bias_pred, thr_t)
      )
      a_taylor <- round(a - taylor_pred * step)
      b_taylor <- round(b + taylor_pred * step)
      c_taylor <- round(c)
      d_taylor <- round(d)
    }
    
    ### check whether taylor_pred move too many and 
    ### causes non-positive odds ratio
    if (a_taylor < 0) {
      b_taylor <- a_taylor + b_taylor
      a_taylor <- 0
    }
    if (b_taylor < 0) {
      a_taylor <- a_taylor + b_taylor
      b_taylor <- 0
    }
    if (c_taylor < 0) {
      d_taylor <- c_taylor + d_taylor
      c_taylor <- 0
    }
    if (d_taylor < 0) {
      c_taylor <- c_taylor + d_taylor
      d_taylor <- 0
    }
    
    ### set brute force starting point from the taylor expansion result
    p_taylor <- fisher_p(a_taylor, b_taylor, c_taylor, d_taylor)
    a_loop <- a_taylor
    b_loop <- b_taylor
    c_loop <- c_taylor
    d_loop <- d_taylor
    p_loop <- p_taylor
    t_loop <- get_t_kfnl(a_loop, b_loop, c_loop, d_loop)
  }
  
  ### when we need to transfer two rows the previously defined 
  ### tryall are the starting point for brute force
  if (allnotenough) {
    ### Later: set tryall as the starting point and 
    ### call this getswitch function again
    a_loop <- a_tryall
    b_loop <- b_tryall
    c_loop <- c_tryall
    d_loop <- d_tryall
    p_loop <- fisher_p(a_loop, b_loop, c_loop, d_loop)
    t_loop <- get_t_kfnl(a_loop, b_loop, c_loop, d_loop)
  }
  
  ### use t as evaluation criterion to start first round brute force
  if (t_loop < thr_t) {
    while (t_loop < thr_t & 
           a_loop + 1 * as.numeric(switch_trm == allnotenough)>0 & 
           b_loop - 1 * as.numeric(switch_trm == allnotenough)>0 & 
           c_loop - 1 * (1 - as.numeric(switch_trm == allnotenough))>0 & 
           d_loop + 1 * (1 - as.numeric(switch_trm == allnotenough))>0) {
      c_loop <- c_loop - 1 * (1 - as.numeric(switch_trm == allnotenough))
      d_loop <- d_loop + 1 * (1 - as.numeric(switch_trm == allnotenough))
      a_loop <- a_loop + 1 * as.numeric(switch_trm == allnotenough)
      b_loop <- b_loop - 1 * as.numeric(switch_trm == allnotenough)
      t_loop <- get_t_kfnl(a_loop, b_loop, c_loop, d_loop)
    }
    ### make a small adjustment to make it just below/above the thresold
    if (t_start > thr_t) {
      c_loopsec <- c_loop +
    1 * (1 - as.numeric(switch_trm == allnotenough))
      d_loopsec <- d_loop -
    1 * (1 - as.numeric(switch_trm == allnotenough))
      a_loopsec <- a_loop - 1 * as.numeric(switch_trm == allnotenough)
      b_loopsec <- b_loop + 1 * as.numeric(switch_trm == allnotenough)
    } else if (t_start < thr_t) {
      c_loopsec <- c_loop
      d_loopsec <- d_loop
      a_loopsec <- a_loop
      b_loopsec <- b_loop
    }
  }
  
  if (t_loop > thr_t) {
    while (t_loop > thr_t &
           c_loop + 1 * (1 - as.numeric(switch_trm == allnotenough)) > 0 &
           d_loop - 1 * (1 - as.numeric(switch_trm == allnotenough)) > 0 &
           a_loop - 1 * as.numeric(switch_trm == allnotenough) > 0 &
           b_loop + 1 * as.numeric(switch_trm == allnotenough) > 0) {
      c_loop <- c_loop + 1 * (1 - as.numeric(switch_trm == allnotenough))
      d_loop <- d_loop - 1 * (1 - as.numeric(switch_trm == allnotenough))
      a_loop <- a_loop - 1 * as.numeric(switch_trm == allnotenough)
      b_loop <- b_loop + 1 * as.numeric(switch_trm == allnotenough)
      t_loop <- get_t_kfnl(a_loop, b_loop, c_loop, d_loop)
    }
    if (t_start < thr_t) {
      c_loopsec <- c_loop -
    1 * (1 - as.numeric(switch_trm == allnotenough))
      d_loopsec <- d_loop +
    1 * (1 - as.numeric(switch_trm == allnotenough))
      a_loopsec <- a_loop + 1 * as.numeric(switch_trm == allnotenough)
      b_loopsec <- b_loop - 1 * as.numeric(switch_trm == allnotenough)
    } else if (t_start > thr_t) {
      c_loopsec <- c_loop
      d_loopsec <- d_loop
      a_loopsec <- a_loop
      b_loopsec <- b_loop
    }
  }
  
  p_loopsec <- fisher_p(a_loopsec, b_loopsec, c_loopsec, d_loopsec)
  
  ### start 2nd round brute force - use fisher test 
  ### p value as evaluation criterion
  #### scenario 1 need to reduce odds ratio to 
  ### invalidate the inference-need to increase p
  if (isinvalidate_start & dcroddsratio_start){
    if (p_loopsec < thr_p) {
      while (p_loopsec < thr_p) {
        c_loopsec <- c_loopsec +
      1 * (1 - as.numeric(switch_trm == allnotenough))
        d_loopsec <- d_loopsec -
      1 * (1 - as.numeric(switch_trm == allnotenough))
        a_loopsec <- a_loopsec -
      1 * as.numeric(switch_trm == allnotenough)
        b_loopsec <- b_loopsec +
      1 * as.numeric(switch_trm == allnotenough)
        p_loopsec <- fisher_p(
          a_loopsec, b_loopsec, c_loopsec, d_loopsec
        )
      }
      c_final <- c_loopsec
      d_final <- d_loopsec
      a_final <- a_loopsec
      b_final <- b_loopsec
    }
    #taylor too much, return some odds ratio
    if (p_loopsec > thr_p){ 
      while (p_loopsec > thr_p) {
        c_loopsec <- c_loopsec -
      1 * (1 - as.numeric(switch_trm == allnotenough))
        d_loopsec <- d_loopsec +
      1 * (1 - as.numeric(switch_trm == allnotenough))
        a_loopsec <- a_loopsec +
      1 * as.numeric(switch_trm == allnotenough)
        b_loopsec <- b_loopsec -
      1 * as.numeric(switch_trm == allnotenough)
        p_loopsec <- fisher_p(
          a_loopsec, b_loopsec, c_loopsec, d_loopsec
        )
      }
      c_final <- c_loopsec +
    1 * (1 - as.numeric(switch_trm == allnotenough))
      d_final <- d_loopsec -
    1 * (1 - as.numeric(switch_trm == allnotenough))
      a_final <- a_loopsec - 1 * as.numeric(switch_trm == allnotenough)
      b_final <- b_loopsec + 1 * as.numeric(switch_trm == allnotenough)
    }
  }
  
  #### scenario 2 need to reduce odds ratio 
  ###  to sustain the inference-need to reduce p
  if (!isinvalidate_start & dcroddsratio_start) {
    if (p_loopsec < thr_p) { # taylor too  much, return some odds ratio
      while (p_loopsec < thr_p) {
        c_loopsec <- c_loopsec -
      1 * (1 - as.numeric(switch_trm == allnotenough))
        d_loopsec <- d_loopsec +
      1 * (1 - as.numeric(switch_trm == allnotenough))
        a_loopsec <- a_loopsec +
      1 * as.numeric(switch_trm == allnotenough)
        b_loopsec <- b_loopsec -
      1 * as.numeric(switch_trm == allnotenough)
        p_loopsec <- fisher_p(
          a_loopsec, b_loopsec, c_loopsec, d_loopsec
        )
      }
      c_final <- c_loopsec +
    1 * (1 - as.numeric(switch_trm == allnotenough))
      d_final <- d_loopsec -
    1 * (1 - as.numeric(switch_trm == allnotenough))
      a_final <- a_loopsec -
    1 * as.numeric(switch_trm == allnotenough)
      b_final <- b_loopsec +
    1 * as.numeric(switch_trm == allnotenough)
    }
    # taylor not enough, continue to reduce odds ratio
    if (p_loopsec > thr_p){ 
      while (p_loopsec > thr_p) {
        c_loopsec <- c_loopsec +
      1 * (1 - as.numeric(switch_trm == allnotenough))
        d_loopsec <- d_loopsec -
      1 * (1 - as.numeric(switch_trm == allnotenough))
        a_loopsec <- a_loopsec -
      1 * as.numeric(switch_trm == allnotenough)
        b_loopsec <- b_loopsec +
      1 * as.numeric(switch_trm == allnotenough)
        p_loopsec <- fisher_p(
          a_loopsec, b_loopsec, c_loopsec, d_loopsec
        )
      }
      c_final <- c_loopsec
      d_final <- d_loopsec
      a_final <- a_loopsec
      b_final <- b_loopsec
    }
  }
  
  #### scenario 3 need to increase odds 
  ### ratio to invalidate the inference-need to increase p
  if (isinvalidate_start & !dcroddsratio_start){
    #taylor not enough, continue to increase odds ratio 
    if (p_loopsec < thr_p){ 
      while (p_loopsec < thr_p) {
        c_loopsec <- c_loopsec -
      1 * (1 - as.numeric(switch_trm == allnotenough))
        d_loopsec <- d_loopsec +
      1 * (1 - as.numeric(switch_trm == allnotenough))
        a_loopsec <- a_loopsec +
      1 * as.numeric(switch_trm == allnotenough)
        b_loopsec <- b_loopsec -
      1 * as.numeric(switch_trm == allnotenough)
        p_loopsec <- fisher_p(
          a_loopsec, b_loopsec, c_loopsec, d_loopsec
        )
      }
      c_final <- c_loopsec
      d_final <- d_loopsec
      a_final <- a_loopsec
      b_final <- b_loopsec
    }
    #taylor too much, returns some odds ratio - decrease
    if (p_loopsec > thr_p){
      while(p_loopsec > thr_p) {
        c_loopsec <- c_loopsec +
      1 * (1 - as.numeric(switch_trm == allnotenough))
        d_loopsec <- d_loopsec -
      1 * (1 - as.numeric(switch_trm == allnotenough))
        a_loopsec <- a_loopsec -
      1 * as.numeric(switch_trm == allnotenough)
        b_loopsec <- b_loopsec +
      1 * as.numeric(switch_trm == allnotenough)
        p_loopsec <- fisher_p(
          a_loopsec, b_loopsec, c_loopsec, d_loopsec
        )
      }
      c_final <- c_loopsec -
    1 * (1 - as.numeric(switch_trm == allnotenough))
      d_final <- d_loopsec +
    1 * (1 - as.numeric(switch_trm == allnotenough))
      a_final <- a_loopsec +
    1 * as.numeric(switch_trm == allnotenough)
      b_final <- b_loopsec -
    1 * as.numeric(switch_trm == allnotenough)
    }
  }
  
  #### scenario 4 need to increase odds ratio 
  #### to sustain the inference-need to decrease p
  if (!isinvalidate_start & !dcroddsratio_start){
    #taylor not enough, continue to increase odds ratio
    if (p_loopsec > thr_p){
      while (p_loopsec > thr_p){
        c_loopsec <- c_loopsec -
      1 * (1 - as.numeric(switch_trm == allnotenough))
        d_loopsec <- d_loopsec +
      1 * (1 - as.numeric(switch_trm == allnotenough))
        a_loopsec <- a_loopsec +
      1 * as.numeric(switch_trm == allnotenough)
        b_loopsec <- b_loopsec -
      1 * as.numeric(switch_trm == allnotenough)
        p_loopsec <- fisher_p(
          a_loopsec, b_loopsec, c_loopsec, d_loopsec
        )  
      }
      c_final <- c_loopsec
      d_final <- d_loopsec
      a_final <- a_loopsec
      b_final <- b_loopsec
    }
    #taylor too much, return some odds ratio - decrease
    if (p_loopsec < thr_p){
      while (p_loopsec < thr_p){
        c_loopsec <- c_loopsec +
      1 * (1 - as.numeric(switch_trm == allnotenough))
        d_loopsec <- d_loopsec -
      1 * (1 - as.numeric(switch_trm == allnotenough))
        a_loopsec <- a_loopsec -
      1 * as.numeric(switch_trm == allnotenough)
        b_loopsec <- b_loopsec +
      1 * as.numeric(switch_trm == allnotenough)
        p_loopsec <- fisher_p(
          a_loopsec, b_loopsec, c_loopsec, d_loopsec
        )
      }
      c_final <- c_loopsec -
    1 * (1 - as.numeric(switch_trm == allnotenough))
      d_final <- d_loopsec +
    1 * (1 - as.numeric(switch_trm == allnotenough))
      a_final <- a_loopsec +
    1 * as.numeric(switch_trm == allnotenough)
      b_final <- b_loopsec -
    1 * as.numeric(switch_trm == allnotenough)
    }
  }
  
  ### so the final results (after switching) is as follows:
  table_final <- matrix(
    c(a_final, b_final, c_final, d_final), byrow = TRUE, 2, 2
  )
  p_final <- fisher_p(a_final, b_final, c_final, d_final)
  fisher_final <- fisher_oddsratio(a_final, b_final, c_final, d_final)
  
  if (switch_trm == allnotenough) {
    final <- abs(a - a_final) + as.numeric(allnotenough) * abs(c - c_final)
  } else {
    final <- abs(c - c_final) + as.numeric(allnotenough) * abs(a - a_final)
  }
  
  if (allnotenough) {
    taylor_pred <- NA
    perc_bias_pred <- NA
    if (switch_trm) {
      final_extra <- abs(a - a_final)
    }
    else {
      final_extra <- abs(c - c_final)
    }
  } else {
    final_extra <- 0
  }
  
  total_switch <- final + allnotenough*final_extra
  
  result <- list(final_switch = final, User_enter_value = table_start, 
                 Transfer_Table = table_final, 
                 p_final = p_final, 
                 fisher_final = fisher_final,
                 needtworows=allnotenough, 
                 taylor_pred = taylor_pred,
                 perc_bias_pred = perc_bias_pred, 
                 final_extra = final_extra, 
                 dcroddsratio_ob = dcroddsratio_ob, 
                 total_switch = total_switch, 
                 isinvalidate_ob = isinvalidate_ob)
  
  return(result)
}
