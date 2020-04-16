# #' Perform tkonfound analysis
# #' @param a s
# #' @param b d
# #' @param c d
# #' @param d s
# #' @param thr_p s
# #' @param switch_trm
# #' @inheritParams pkonfound
# #' @return dd
# #' @examples
# #' d <- read.csv("https://msu.edu/~kenfrank/example%20dataset%20for%20mkonfound.csv")
# #' @export

tkonfound <- function(a, b, c, d, thr_p = 0.05, switch_trm = T){
# a <- 35
# b <- 17
# c <- 17
# d <- 38
# thr_p <- 0.05
# switch_trm <- T

# stop message
if (a <= 0 || b <= 0 || c <= 0 || d <= 0) {
  stop("Please enter positive integers for each cell.")
}

if (a != as.integer(a) || b != as.integer(b) || c != as.integer(c) || d != as.integer(d)) {
stop("Please enter positive integers for each cell.")
}

odds_ratio <- a*d/(b*c)
n_cnt <- a+b
n_trm <- c+d
n_obs <- n_cnt + n_trm
est <- log(odds_ratio)

# this is the 2 by 2 table we start with
table_ob <- matrix(c(a, b, c, d), byrow = TRUE, 2, 2)
p_ob <- chisq_p(a, b, c, d)
chisq_ob <- chisq_value(a, b, c, d)


# dcroddsratio_ob is true - our goal is to decrease the odds ratio
if (p_ob < thr_p){dcroddsratio_ob <- TRUE}
if (p_ob > thr_p){dcroddsratio_ob <- FALSE}

dcroddsratio_start <- dcroddsratio_ob
p_start <- p_ob
table_start <- table_ob
t_ob <- t_start <- get_t_kfnl(a, b, c, d)
thr_t <- stats::qt(1 - thr_p/2, n_obs - 1) 

if (dcroddsratio_start) {
  step <- 1 # transfer cases from D to C or A to B
} else {
  step <- -1 # transfer cases from B to A or C to D
}

### check whether it is enough to transfer all cases in one row
if (p_start > thr_p) {
  # transfer cases from B to A or C to D to increase odds ratio
  c_tryall <- c - (c - 1) * as.numeric(switch_trm)
  d_tryall <- d + (c - 1) * as.numeric(switch_trm)
  a_tryall <- a + (b - 1) * (1 - as.numeric(switch_trm))
  b_tryall <- b - (b - 1) * (1 - as.numeric(switch_trm))
  tryall_p <- chisq_p(a_tryall, b_tryall, c_tryall, d_tryall)
  tryall_est <- log(a_tryall*d_tryall/c_tryall/b_tryall)
  allnotenough <- isTRUE(thr_p - tryall_p < 0 & tryall_est*est > 0)
}
if (p_start < thr_p) {
  # transfer cases from A to B or D to C to decrease odds ratio
  c_tryall <- c + (d - 1) * as.numeric(switch_trm)
  d_tryall <- d - (d - 1) * as.numeric(switch_trm)
  a_tryall <- a - (a - 1) * (1 - as.numeric(switch_trm))
  b_tryall <- b + (a - 1) * (1 - as.numeric(switch_trm))
  tryall_p <- chisq_p(a_tryall, b_tryall, c_tryall, d_tryall)
  tryall_est <- log(a_tryall*d_tryall/c_tryall/b_tryall)
  allnotenough <- isTRUE(tryall_p - thr_p < 0 & tryall_est*est > 0)
}

### run following if transfering one row is enough
if (!allnotenough) {
  ### calculate percent of bias and predicted switches
  if (p_ob < thr_p) {
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
    taylor_pred <- abs(taylorexp(a, b, c, d, step * perc_bias_pred, thr_t))
    a_taylor <- round(a)
    b_taylor <- round(b)
    c_taylor <- round(c + taylor_pred * step)
    d_taylor <- round(d - taylor_pred * step)
  } else {
    taylor_pred <- abs(taylorexp(d, c, b, a, step * perc_bias_pred, thr_t))
    a_taylor <- round(a - taylor_pred * step)
    b_taylor <- round(b + taylor_pred * step)
    c_taylor <- round(c)
    d_taylor <- round(d)
  }
  
  ### check whether taylor_pred move too many and causes non-positive odds ratio
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
}

### when we need to transfer two rows the previously defined tryall are the starting point for brute force
if (allnotenough) {
  ### Later: set tryall as the starting point and call this getswitch function again
  a_loop <- a_tryall
  b_loop <- b_tryall
  c_loop <- c_tryall
  d_loop <- d_tryall
  p_loop <- chisq_p(a_loop, b_loop, c_loop, d_loop)
}

### start brute force
if (p_loop > thr_p) {
  while (p_loop > thr_p) {
    c_loop <- c_loop - 1 * (1 - as.numeric(switch_trm == allnotenough))
    d_loop <- d_loop + 1 * (1 - as.numeric(switch_trm == allnotenough))
    a_loop <- a_loop + 1 * as.numeric(switch_trm == allnotenough)
    b_loop <- b_loop - 1 * as.numeric(switch_trm == allnotenough)
    p_loop <- chisq_p(a_loop, b_loop, c_loop, d_loop)
  }
  ### make a small adjustment to make it just below/above the thresold
  if (p_start < thr_p) {
    c_final <- c_loop + 1 * (1 - as.numeric(switch_trm == allnotenough))
    d_final <- d_loop - 1 * (1 - as.numeric(switch_trm == allnotenough))
    a_final <- a_loop - 1 * as.numeric(switch_trm == allnotenough)
    b_final <- b_loop + 1 * as.numeric(switch_trm == allnotenough)
  } else if (p_start > thr_t) {
    c_final <- c_loop
    d_final <- d_loop
    a_final <- a_loop
    b_final <- b_loop
  }
}

if (p_loop < thr_p) {
  while (p_loop < thr_p) {
    c_loop <- c_loop + 1 * (1 - as.numeric(switch_trm == allnotenough))
    d_loop <- d_loop - 1 * (1 - as.numeric(switch_trm == allnotenough))
    a_loop <- a_loop - 1 * as.numeric(switch_trm == allnotenough)
    b_loop <- b_loop + 1 * as.numeric(switch_trm == allnotenough)
    p_loop <- chisq_p(a_loop, b_loop, c_loop, d_loop)
  }
  if (p_start > thr_p) {
    c_final <- c_loop - 1 * (1 - as.numeric(switch_trm == allnotenough))
    d_final <- d_loop + 1 * (1 - as.numeric(switch_trm == allnotenough))
    a_final <- a_loop + 1 * as.numeric(switch_trm == allnotenough)
    b_final <- b_loop - 1 * as.numeric(switch_trm == allnotenough)
  } else if (p_start < thr_p) {
    c_final <- c_loop
    d_final <- d_loop
    a_final <- a_loop
    b_final <- b_loop
  }
}

### so the final results (after switching) is as follows:
table_final <- matrix(c(a_final, b_final, c_final, d_final), byrow = TRUE, 2, 2)
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

### add column and row names to contingency tables
rownames(table_start) <- rownames(table_final) <- c("Control", "Treatment")
colnames(table_start) <- colnames(table_final) <- c("Fail", "Success")

if (switch_trm && dcroddsratio_ob) {
  transferway <- "treatment success to treatment failure,"
}
if (switch_trm && !dcroddsratio_ob) {
  transferway <- "treatment failure to treatment success,"
}
if (!switch_trm && dcroddsratio_ob) {
  transferway <- "control failure to control success,"
}
if (!switch_trm && !dcroddsratio_ob) {
  transferway <- "control success to control failure,"
}

if (allnotenough) {
  if (switch_trm && dcroddsratio_ob) {
    transferway_extra <- "control failure to control success,"
  }
  if (switch_trm && !dcroddsratio_ob) {
    transferway_extra <- "control success to control failure,"
  }
  if (!switch_trm && dcroddsratio_ob) {
    transferway_extra <- "treatment success to treatment failure,"
  }
  if (!switch_trm && !dcroddsratio_ob) {
    transferway_extra <- "treatment failure to treatment success,"
  }
}

if (p_ob < thr_p) {
  change <- "To invalidate the inference,"
} else {
    change <- "To sustain an inference,"
} 

if (!allnotenough & final > 1) {
  conclusion1 <- paste(
    change, sprintf("%d cases need to be transferred from", final),
    transferway, c("as shown, from the Observed Table to the Transfer Table.")
  )
} 

if (!allnotenough & final == 1) {
  conclusion1 <- paste(
    change, sprintf("%d case needs to be transferred from", final),
    transferway, c("as shown, from the Observed Table to the Transfer Table.")
  )
}

if (allnotenough){
  conclusion1 <- paste(
    change, c("only transferring cases from"), transferway,
    sprintf("is not enough. We also need to transfer %d cases from", final_extra),
    transferway_extra, c("as shown, from the Observed Table to the Transfer Table.")
  )
}

conclusion2 <- sprintf(
  "For the Observed Table (based on user enter values), we have a Pearson's chi square of %.3f, with p value of %.3f.", chisq_ob, p_ob)
conclusion3 <- sprintf(
  "For the Transfer Table, we have a Pearson's chi square of %.3f, with p value of %.3f.", chisq_final, p_final)

info <- "The tkonfound function calculates the number of cases that would have to be switched from one cell to another of a 2x2 table to invalidate an inference made about the association between the rows and columns. This can be applied to treatment vs control with successful vs unsuccessful outcomes."

result <- list(info, conclusion1,
               User_enter_value = table_start, Transfer_Table = table_final,
               conclusion2, conclusion3)

return(result)
}



