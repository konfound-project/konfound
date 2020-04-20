#' Perform sensitivity analysis for a 2x2 table
#' @description This function calculates the number of cases (RIS) that would have to be switched from one cell to another of a 2x2 table to invalidate an inference made about the association between the rows and columns. This can be applied to treatment vs control with successful vs unsuccessful outcomes.
#' @param a cell is the number of cases in the control group showing unsuccessful results
#' @param b cell is the number of cases in the control group showing successful results
#' @param c cell is the number of cases in the treatment group showing unsuccessful results
#' @param d cell is the number of cases in the treatment group showing successful results
#' @param thr_p the p-value threshold used to evaluate statistical significance, with the default of 0.05
#' @param switch_trm whether switching the two cells in the treatment row or the two cells in the control row, with the default of the treatment row
#' @param plt whether return a plot shows how RIS% (the percent of number of cases need to change out of total sample size) changes as sample sizes increases with default of True 
#' @inheritParams pkonfound
#' @return prints a 2x2 table after switching cases from one cell to another so that the inference is invalidated about the association between the rows and columns 
#' @examples
#' # using tkonfound for a 2x2 table
#' tkonfound(35, 17, 17, 38)
#' tkonfound(35, 17, 17, 38, thr_p = 0.01)
#' tkonfound(35, 17, 17, 38, thr_p = 0.01, switch_trm = F)
#' tkonfound(35, 17, 17, 38, thr_p = 0.01, switch_trm = F, plt = F)
#' @export

tkonfound <- function(a, b, c, d, thr_p = 0.05, switch_trm = T){
# a <- 35
# b <- 17
# c <- 17
# d <- 38
# thr_p <- 0.05
# switch_trm <- T
# plt <- T

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

# get solution
solution <- getswitch_chisq(a, b, c, d, thr_p, switch_trm)
  
table_final <- solution$Transfer_Table
table_start <- table_ob
dcroddsratio_ob <- solution$dcroddsratio_ob
allnotenough <- solution$needtworows
final <- solution$final_switch
p_final <- solution$p_final
chisq_final <- solution$chisq_final
taylor_pred <- solution$taylor_pred
perc_bias_pred <- solution$perc_bias_pred
final_extra <- solution$final_extra
total_switch <- solution$total_switch

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
    transferway, c("as shown, from the User-entered Table to the Transfer Table.")
  )
} 

if (!allnotenough & final == 1) {
  conclusion1 <- paste(
    change, sprintf("%d case needs to be transferred from", final),
    transferway, c("as shown, from the User-entered Table to the Transfer Table.")
  )
}

if (allnotenough){
  conclusion1 <- paste(
    change, c("only transferring cases from"), transferway,
    sprintf("is not enough. We also need to transfer %d cases from", final_extra),
    transferway_extra, c("as shown, from the User-entered Table to the Transfer Table.")
  )
}

conclusion2 <- sprintf(
  "For the User-entered Table, we have a Pearson's chi square of %.3f, with p value of %.3f.", chisq_ob, p_ob)
conclusion3 <- sprintf(
  "For the Transfer Table, we have a Pearson's chi square of %.3f, with p value of %.3f.", chisq_final, p_final)

info <- "The tkonfound function calculates the number of cases that would have to be switched from one cell to another of a 2x2 table to invalidate an inference made about the association between the rows and columns. This can be applied to treatment vs control with successful vs unsuccessful outcomes."


###plot figure 1
###***generate the log odds for each step of switch
meta1 <- data.frame(matrix(ncol = 7, nrow = n_obs-3))
colnames(meta1) <- c("a", "b", "c", "d", "nobs", "switch", "logodds")
if (switch_trm == T) {
  for (i in 1:(n_obs-3)){ 
    if (i <= a){
    #from table(1, a+b-1, c+d-1, 1) to table(a, b, c+d-1, 1)
    meta1$a[i] <- i 
    meta1$b[i] <- a+b-i
    meta1$c[i] <- c+d-1
    meta1$d[i] <- 1
    meta1$nobs[i] <- meta1$a[i] + meta1$b[i] + meta1$c[i] + meta1$d[i]
    meta1$switch[i] <- -(d-1+a-i)
    meta1$logodds[i] <- log(meta1$a[i]*meta1$d[i]/meta1$c[i]/meta1$b[i])
    }
    if (i>a & i<=a+d-1){
    # from table(a, b, c+d-1, 1) to table(a, b, c, d)
    meta1$a[i] <- a
    meta1$b[i] <- b
    meta1$c[i] <- c+d-1-i+a
    meta1$d[i] <- 1+i-a
    meta1$nobs[i] <- meta1$a[i] + meta1$b[i] + meta1$c[i] + meta1$d[i]
    meta1$switch[i] <- -(a+d-1-i)
    meta1$logodds[i] <- log(meta1$a[i]*meta1$d[i]/meta1$c[i]/meta1$b[i])
    }
    if (i>a+d-1 & i<=a+d+c-2){
    # from table(a, b, c, d) to table(a, b, 1, d+c-1)
    meta1$a[i] <- a
    meta1$b[i] <- b
    meta1$c[i] <- c-(i-a-d+1)
    meta1$d[i] <- d+(i-a-d+1)
    meta1$nobs[i] <- meta1$a[i] + meta1$b[i] + meta1$c[i] + meta1$d[i]
    meta1$switch[i] <- i-(a+d-1)
    meta1$logodds[i] <- log(meta1$a[i]*meta1$d[i]/meta1$c[i]/meta1$b[i])
    }
    if (i>a+d+c-2 & i<=a+b+c+d-3){
    # from table(a, b, 1, d+c-1) to table(a+b-1, 1, 1, d+c-1)
    meta1$a[i] <- a+(i-a-d-c+2)
    meta1$b[i] <- b-(i-a-d-c+2)
    meta1$c[i] <- 1
    meta1$d[i] <- c+d-1
    meta1$nobs[i] <- meta1$a[i] + meta1$b[i] + meta1$c[i] + meta1$d[i]
    meta1$switch[i] <- i-(a+d-1)
    meta1$logodds[i] <- log(meta1$a[i]*meta1$d[i]/meta1$c[i]/meta1$b[i])
  }  
  }
}
if (switch_trm == F) {
  for (i in 1:(n_obs-3)){ 
    if (i <= d){
      #from table(1, a+b-1, c+d-1, 1) to table(1, a+b-1, c, d)
      meta1$d[i] <- i 
      meta1$c[i] <- d+c-i
      meta1$b[i] <- b+a-1
      meta1$a[i] <- 1
      meta1$nobs[i] <- meta1$a[i] + meta1$b[i] + meta1$c[i] + meta1$d[i]
      meta1$switch[i] <- -(a-1+d-i)
      meta1$logodds[i] <- log(meta1$a[i]*meta1$d[i]/meta1$c[i]/meta1$b[i])
    }
    if (i>d & i<=a+d-1){
      # from table(1, a+b-1, c, d) to table(a, b, c, d)
      meta1$d[i] <- d
      meta1$c[i] <- c
      meta1$b[i] <- b+a-1-i+d
      meta1$a[i] <- 1+i-d
      meta1$nobs[i] <- meta1$a[i] + meta1$b[i] + meta1$c[i] + meta1$d[i]
      meta1$switch[i] <- -(a+d-1-i)
      meta1$logodds[i] <- log(meta1$a[i]*meta1$d[i]/meta1$c[i]/meta1$b[i])
    }
    if (i>a+d-1 & i<=a+d+b-2){
      # from table(a, b, c, d) to table(a+b-1, 1, c, d)
      meta1$d[i] <- d
      meta1$c[i] <- c
      meta1$b[i] <- b-(i-a-d+1)
      meta1$a[i] <- a+(i-a-d+1)
      meta1$nobs[i] <- meta1$a[i] + meta1$b[i] + meta1$c[i] + meta1$d[i]
      meta1$switch[i] <- i-(a+d-1)
      meta1$logodds[i] <- log(meta1$a[i]*meta1$d[i]/meta1$c[i]/meta1$b[i])
    }
    if (i>a+d+b-2 & i<=a+b+c+d-3){
      # from table(a+b-1, 1, c, d) to table(a+b-1, 1, 1, d+c-1)
      meta1$d[i] <- d+(i-d-a-b+2)
      meta1$c[i] <- c-(i-a-d-b+2)
      meta1$b[i] <- 1
      meta1$a[i] <- b+a-1
      meta1$nobs[i] <- meta1$a[i] + meta1$b[i] + meta1$c[i] + meta1$d[i]
      meta1$switch[i] <- i-(a+d-1)
      meta1$logodds[i] <- log(meta1$a[i]*meta1$d[i]/meta1$c[i]/meta1$b[i])
    }  
  }
}

###***find out significant thresholds 

meta1$sig <- ifelse(meta1$a==table_final[1,1] & meta1$b==table_final[1,2] & meta1$c==table_final[2,1] & meta1$d==table_final[2,2],
                       1,0)
if (meta1[meta1$sig==1,]$logodds > 0){
  if (dcroddsratio_ob){#from sig to not sig by decreasing odds ratio 
    posinsig <- meta1[meta1$sig==1,]$switch
    pos_thr <- (meta1[meta1$switch==posinsig,]$logodds+meta1[meta1$switch==(posinsig+1),]$logodds)/2
  } else {#from not sig to sig by increasing positive effect
    possig <- meta1[meta1$sig==1,]$switch
    pos_thr <- (meta1[meta1$switch==possig,]$logodds+meta1[meta1$switch==(possig-1),]$logodds)/2
  }
  # find out the row that is cloest to logodds of 0 but negative
  temp1 <- meta1[meta1$logodds<0,]
  temp1 <- temp1[order(abs(temp1$logodds)),]
  j <- 1 
  while (chisq_p(temp1$a[j], temp1$b[j], temp1$c[j], temp1$d[j])>thr_p){
    j <- j+1
  }
  neg_thr <- (temp1$logodds[j-1]+temp1$logodds[j])/2 
}

if (meta1[meta1$sig==1,]$logodds < 0){
  if (dcroddsratio_ob){ # from not sig to sig by decreasing odds ratio
    negsig <- meta1[meta1$sig==1,]$switch
    neg_thr <- (meta1[meta1$switch==negsig,]$logodds+meta1[meta1$switch==(negsig+1),]$logodds)/2
  } else { # from sig to not sig by increasing odds ratio that is smaller than 1 
    neginsig <- meta1[meta1$sig==1,]$switch
    neg_thr <- (meta1[meta1$switch==neginsig,]$logodds+meta1[meta1$switch==(neginsig-1),]$logodds)/2 
  }
  # find out the row that is cloest to logodds of 0 but positive
  temp1 <- meta1[meta1$logodds>0,]
  temp1 <- temp1[order(abs(temp1$logodds)),]
  j <- 1 
  while (chisq_p(temp1$a[j], temp1$b[j], temp1$c[j], temp1$d[j])>thr_p){
    j <- j+1
  }
  pos_thr <- (temp1$logodds[j-1]+temp1$logodds[j])/2 
}

# meta1$oddsratio <- exp(meta1$logodds)
#ggplot2::ggplot(meta1[meta1$switch>=-8 & meta1$switch<=6,], ggplot2::aes(x=switch, y=oddsratio))+
#  ggplot2::geom_line(size = 1)+
#  ggplot2::geom_point(size = 2.5)+
#  ggplot2::geom_vline(xintercept = pos_thr, linetype = "dashed", color="blue", size = 1.2)+
#  ggplot2::geom_vline(xintercept = neg_thr, linetype = "dashed", color="red", size = 1.2)+
#  ggplot2::geom_vline(xintercept = 0, color="black", size = 1.2)

meta1$current <- ifelse(meta1$switch==0, 1, 0)
fig1 <- ggplot2::ggplot(meta1, ggplot2::aes(x=switch, y=logodds))+
  ggplot2::geom_line(size = 1)+
  ggplot2::geom_point(size = 2.5+meta1$current*2, color = meta1$current+1)+
  ggplot2::geom_hline(yintercept = pos_thr, linetype = "dashed", color="blue", size = 1)+
  ggplot2::geom_hline(yintercept = neg_thr, linetype = "dashed", color="green4", size = 1)+
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color="black", size = 1)+
  ggplot2::labs(title = "Log-odds Changes as Switching Cases")+
  ggplot2::scale_x_continuous(name="Switch cases")+
  ggplot2::scale_y_continuous(name="Log-odds") +
  #scale_colour_manual(name="Thresholds", values=cols) +
  #scale_linetype_manual(name="Thresholds", values=typs)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
                                  axis.text = element_text(size = 12))


###plot figure 3 RIS% as sample size gets larger, using t statistic as the criterion
# if (plt3 == TRUE) {
#  meta3 <- data.frame(matrix(ncol = 7, nrow = 11))
#  colnames(meta3) <- c("a", "b", "c", "d", "nobs", "RIS", "RISperc_total", "RISperc_D") 
#  for (i in 1:11){
#    meta3$nobs[i] <- size <- 2^(i+5)
#    meta3$a[i] <- a_i <- round(size/n_obs*a)
#    meta3$b[i] <- b_i <- round(size/n_obs*b)
#    meta3$c[i] <- c_i <- round(size/n_obs*c)
#    meta3$d[i] <- d_i <- round(size/n_obs*d)
#    table_i <- get_abcd_kfnl(a_i, b_i, c_i, d_i)
#    thr_i_t <- stats::qt(1 - thr_p/2, size - 1) 
#    meta3$RIS[i] <- RIS_i <- getswitch(table_i, thr_t_i, switch_trm, size)$final_switch + getswitch(table_i, thr_t_i, switch_trm, size)$final_extra
#    meta3$RISperc[i] <- RISperc_i <- RIS_i/size*100
#  } 
#  fig3 <- ggplot2::ggplot(meta3, aes(x=nobs, y=RISperc))+
#    geom_line(size = 1)+
#    geom_point(size = 2.5)+
#    labs(title = "RIS as % of Sample Size")+
#    scale_x_continuous(name="Sample Size", labels=scales::comma)+
#    scale_y_continuous(name="RIS%") +
#    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
#          axis.text = element_text(size = 12))
#} 


result <- list(info, conclusion1,
                 User_enter_value = table_start, Transfer_Table = table_final,
                 conclusion2, conclusion3, fig1)

  return(result)
}



