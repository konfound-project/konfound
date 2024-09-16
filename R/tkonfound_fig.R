#' Draw Figures for Change in Effect Size in 2x2 Tables
#'
#' This function generates plots illustrating how the change in effect size 
#' is influenced by switching or replacing outcomes in a 2x2 table. It produces
#' two plots: one showing all possibilities (switching) and another zoomed in 
#' the area for positive RIR (Relative Impact Ratio).
#'
#' @param a Number of cases in the control group with unsuccessful outcomes.
#' @param b Number of cases in the control group with successful outcomes.
#' @param c Number of cases in the treatment group with unsuccessful outcomes.
#' @param d Number of cases in the treatment group with successful outcomes.
#' @param thr_p P-value threshold for statistical significance, default is 0.05.
#' @param switch_trm Whether to switch the two cells in the treatment or 
#'     control row, default is TRUE (treatment row).
#' @param test Type of statistical test used, either "Fisher's Exact Test"
#'  (default) or "Chi-square test".
#' @param replace Indicates whether to use the entire sample or just the control
#'  group for calculating the base rate, default is "control".
#'
#' @importFrom ggplot2 ggplot aes_string geom_line geom_point scale_fill_manual
#'            scale_shape_manual geom_hline scale_y_continuous
#'            scale_x_continuous theme element_blank element_line
#' @importFrom ggrepel geom_label_repel
#'
#' @return Returns two plots showing the effect of hypothetical case switches 
#'         on the effect size in a 2x2 table.
#' @examples
#' tkonfound_fig(14, 17, 6, 25, test = "chisq")
#' 
#' @export 

tkonfound_fig <- function(a, b, c, d, thr_p = 0.05, switch_trm = TRUE, 
                          test = "fisher", replace = "control"){

n_obs <- a + b + c + d
###***generate the log odds for each step of switch
meta <- data.frame(matrix(ncol = 10, nrow = n_obs-3))
colnames(meta) <- c("a", "b", "c", "d", "nobs", "switch", 
                    "logodds","cntrl_p","tr_p","pdif")
if (switch_trm == TRUE) {
  for (i in 1:(n_obs-3)){ 
    if (i <= a){
      #from table(1, a+b-1, c+d-1, 1) to table(a, b, c+d-1, 1)
      meta$a[i] <- i 
      meta$b[i] <- a+b-i
      meta$c[i] <- c+d-1
      meta$d[i] <- 1
      meta$nobs[i] <- meta$a[i] + meta$b[i] + meta$c[i] + meta$d[i]
      meta$switch[i] <- -(d-1+a-i)
    }
    if (i>a & i<=a+d-1){
      # from table(a, b, c+d-1, 1) to table(a, b, c, d)
      meta$a[i] <- a
      meta$b[i] <- b
      meta$c[i] <- c+d-1-i+a
      meta$d[i] <- 1+i-a
      meta$nobs[i] <- meta$a[i] + meta$b[i] + meta$c[i] + meta$d[i]
      meta$switch[i] <- -(a+d-1-i)
    }
    if (i>a+d-1 & i<=a+d+c-2){
      # from table(a, b, c, d) to table(a, b, 1, d+c-1)
      meta$a[i] <- a
      meta$b[i] <- b
      meta$c[i] <- c-(i-a-d+1)
      meta$d[i] <- d+(i-a-d+1)
      meta$nobs[i] <- meta$a[i] + meta$b[i] + meta$c[i] + meta$d[i]
      meta$switch[i] <- i-(a+d-1)
    }
    if (i>a+d+c-2 & i<=a+b+c+d-3){
      # from table(a, b, 1, d+c-1) to table(a+b-1, 1, 1, d+c-1)
      meta$a[i] <- a+(i-a-d-c+2)
      meta$b[i] <- b-(i-a-d-c+2)
      meta$c[i] <- 1
      meta$d[i] <- c+d-1
      meta$nobs[i] <- meta$a[i] + meta$b[i] + meta$c[i] + meta$d[i]
      meta$switch[i] <- i-(a+d-1)
    }  
  }
}
if (switch_trm == FALSE) {
  for (i in 1:(n_obs-3)){ 
    if (i <= d){
      #from table(1, a+b-1, c+d-1, 1) to table(1, a+b-1, c, d)
      meta$d[i] <- i 
      meta$c[i] <- d+c-i
      meta$b[i] <- b+a-1
      meta$a[i] <- 1
      meta$nobs[i] <- meta$a[i] + meta$b[i] + meta$c[i] + meta$d[i]
      meta$switch[i] <- -(a-1+d-i)
    }
    if (i>d & i<=a+d-1){
      # from table(1, a+b-1, c, d) to table(a, b, c, d)
      meta$d[i] <- d
      meta$c[i] <- c
      meta$b[i] <- b+a-1-i+d
      meta$a[i] <- 1+i-d
      meta$nobs[i] <- meta$a[i] + meta$b[i] + meta$c[i] + meta$d[i]
      meta$switch[i] <- -(a+d-1-i)
    }
    if (i>a+d-1 & i<=a+d+b-2){
      # from table(a, b, c, d) to table(a+b-1, 1, c, d)
      meta$d[i] <- d
      meta$c[i] <- c
      meta$b[i] <- b-(i-a-d+1)
      meta$a[i] <- a+(i-a-d+1)
      meta$nobs[i] <- meta$a[i] + meta$b[i] + meta$c[i] + meta$d[i]
      meta$switch[i] <- i-(a+d-1)
    }
    if (i>a+d+b-2 & i<=a+b+c+d-3){
      # from table(a+b-1, 1, c, d) to table(a+b-1, 1, 1, d+c-1)
      meta$d[i] <- d+(i-d-a-b+2)
      meta$c[i] <- c-(i-a-d-b+2)
      meta$b[i] <- 1
      meta$a[i] <- b+a-1
      meta$nobs[i] <- meta$a[i] + meta$b[i] + meta$c[i] + meta$d[i]
      meta$switch[i] <- i-(a+d-1)
    }  
  }
}
meta$cntrl_p <- meta$b/(meta$a + meta$b)
meta$tr_p <- meta$d/(meta$c + meta$d)
meta$pdif <- meta$tr_p - meta$cntrl_p

## if any of a, b, c, d is exactly zero, then we add 0.5 to all of them
## note that this is only for computing odds ratio and log OR
if (a == 0 || b == 0 || c == 0 || d == 0) {
    a_OR <- a + 0.5
    b_OR <- b + 0.5
    c_OR <- c + 0.5
    d_OR <- d + 0.5
} else {
    a_OR <- a
    b_OR <- b
    c_OR <- c 
    d_OR <- d
}  

odds_ratio <- a_OR * d_OR / (b_OR * c_OR)

###***find out significant thresholds 
if (test == "chisq"){
  solution <- getswitch_chisq(a, b, c, d, odds_ratio, thr_p, switch_trm)
  meta$logodds <- log(meta$a*meta$d/meta$c/meta$b)
}

if (test == "fisher"){
  solution <- getswitch_fisher(a, b, c, d, odds_ratio, thr_p, switch_trm)
  for (i in 1:(n_obs-3)){
  meta$logodds[i] <- log(fisher_oddsratio(meta$a[i], meta$b[i], 
                                          meta$c[i], meta$d[i]))
  }
}

table_final <- solution$Transfer_Table
dcroddsratio_ob <- solution$dcroddsratio_ob

meta$sig <- ifelse(meta$a==table_final[1,1] & meta$b==table_final[1,2] & meta$c==table_final[2,1] & meta$d==table_final[2,2],
                    1,0)

if (meta[meta$sig==1,]$logodds > 0){
  if (dcroddsratio_ob){#from sig to not sig by decreasing odds ratio 
    posinsig <- meta[meta$sig==1,]$switch
    pos_thr <- (meta[meta$switch==posinsig,]$logodds+meta[meta$switch==(posinsig+1),]$logodds)/2
    pos_thr_pdif <- (meta[meta$switch==posinsig,]$pdif+meta[meta$switch==(posinsig+1),]$pdif)/2
    zoom_lower <- -(posinsig + 2)
    meta$sigpoint <- ifelse(meta$switch==(posinsig+1),"positive","other")
  } else {#from not sig to sig by increasing positive effect
    possig <- meta[meta$sig==1,]$switch
    pos_thr <- (meta[meta$switch==possig,]$logodds+meta[meta$switch==(possig-1),]$logodds)/2
    pos_thr_pdif <- (meta[meta$switch==possig,]$pdif+meta[meta$switch==(possig-1),]$pdif)/2
    zoom_lower <- -(possig + 2)
    meta$sigpoint <- ifelse(meta$switch==possig,"positive","other")
    }
  # find out the row that is cloest to logodds of 0 but negative
  temp1 <- meta[meta$logodds<0,]
  temp1 <- temp1[order(abs(temp1$logodds)),]
  j <- 1
  if (test == "chisq"){
    while (chisq_p(temp1$a[j], temp1$b[j], temp1$c[j], temp1$d[j])>thr_p){
      j <- j+1
    }
  }
  if (test == "fisher"){
    while (fisher_p(temp1$a[j], temp1$b[j], temp1$c[j], temp1$d[j])>thr_p){
      j <- j+1
    }
  }
  neg_thr <- (temp1$logodds[j-1]+temp1$logodds[j])/2 
  neg_thr_pdif <- (temp1$pdif[j-1]+temp1$pdif[j])/2
  zoom_upper <- -(temp1$switch[j]-2)
  meta$sigpoint <- ifelse(meta$switch==temp1$switch[j],"negative",meta$sigpoint)
}

if (meta[meta$sig==1,]$logodds < 0){
  if (dcroddsratio_ob){ # from not sig to sig by decreasing odds ratio
    negsig <- meta[meta$sig==1,]$switch
    neg_thr <- (meta[meta$switch==negsig,]$logodds+meta[meta$switch==(negsig+1),]$logodds)/2
    neg_thr_pdif <- (meta[meta$switch==negsig,]$pdif+meta[meta$switch==(negsig+1),]$pdif)/2
    zoom_upper <- -(negsig - 4)
    meta$sigpoint <- ifelse(meta$switch==negsig,"negative","other")
    } else { # from sig to not sig by increasing odds ratio that is smaller than 1 
    neginsig <- meta[meta$sig==1,]$switch
    neg_thr <- (meta[meta$switch==neginsig,]$logodds+meta[meta$switch==(neginsig-1),]$logodds)/2
    neg_thr_pdif <- (meta[meta$switch==neginsig,]$pdif+meta[meta$switch==(neginsig-1),]$pdif)/2 
    zoom_upper <- -(neginsig - 2)
    meta$sigpoint <- ifelse(meta$switch==neginsig-1,"negative","other")
  }
  # find out the row that is cloest to logodds of 0 but positive
  temp1 <- meta[meta$logodds>0,]
  temp1 <- temp1[order(abs(temp1$logodds)),]
  j <- 1 
  if (test == "chisq"){
    while (chisq_p(temp1$a[j], temp1$b[j], temp1$c[j], temp1$d[j])>thr_p){
      j <- j+1
    }
  }
  if (test == "fisher"){
    while (fisher_p(temp1$a[j], temp1$b[j], temp1$c[j], temp1$d[j])>thr_p){
      j <- j+1
    }
  }
  pos_thr <- (temp1$logodds[j-1]+temp1$logodds[j])/2 
  pos_thr_pdif <- (temp1$pdif[j-1]+temp1$pdif[j])/2
  zoom_lower <- -(temp1$switch[j]+2)
  meta$sigpoint <- ifelse(meta$switch==temp1$switch[j],"positive",meta$sigpoint)
}

meta$current <- ifelse(meta$switch==0, "current", "other")
meta$currentlabel <- ifelse(meta$switch==0, "current", NA)
meta$sigpoint <- ifelse(meta$switch==0, "current",meta$sigpoint)

if (switch_trm && dcroddsratio_ob) {
  # transferway <- "treatment success to treatment failure,"
  meta$switch <- meta$switch*(-1)
}

if (!switch_trm && dcroddsratio_ob) {
 #transferway <- "control failure to control success,"
  meta$switch <- meta$switch*(-1)
}

fillcol <-c("current"="white","positive"="green4","negative"="red","other"="white") 
pointshape <- c("current"=15,"other"=21)

if (switch_trm && dcroddsratio_ob) {
  meta$RIR <- ceiling(meta$switch/((a+c)/n_obs))*(replace=="entire") + ceiling(meta$switch/(a/(a+b)))*(1-(replace=="entire"))
}
if (switch_trm && !dcroddsratio_ob) {
  meta$RIR <- ceiling(meta$switch/((b+d)/n_obs))*(replace=="entire") + ceiling(meta$switch/(b/(a+b)))*(1-(replace=="entire"))
}
if (!switch_trm && dcroddsratio_ob) {
  meta$RIR <- ceiling(meta$switch/((b+d)/n_obs))*(replace=="entire") + ceiling(meta$switch/(b/(a+b)))*(1-(replace=="entire"))
}
if (!switch_trm && !dcroddsratio_ob) {
  meta$RIR <- ceiling(meta$switch/((a+c)/n_obs))*(replace=="entire") + ceiling(meta$switch/(a/(a+b)))*(1-(replace=="entire"))
}

meta$xaxis <- paste(meta$RIR,"\n","(", meta$switch, ")", sep = "")

fig1 <- ggplot2::ggplot(meta, ggplot2::aes(x=meta$RIR, y=meta$pdif))+
    ggplot2::geom_line(ggplot2::aes(y=meta$pdif), linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(y=meta$pdif, shape = meta$current, fill = meta$sigpoint)) +
  ggplot2::scale_fill_manual(values=fillcol)+
  ggplot2::scale_shape_manual(values=pointshape)+
  ggrepel::geom_label_repel(ggplot2::aes(label=meta$currentlabel))+
  ggplot2::geom_hline(yintercept = pos_thr_pdif, linetype = "dashed", color="green4", size = 1)+
  ggplot2::geom_hline(yintercept = neg_thr_pdif, linetype = "dashed", color="red", size = 1)+
  ggplot2::scale_y_continuous(name="Difference in probability of successful outcome (treatment - control)")+
  ggplot2::scale_x_continuous(name="RIR (Fragility)", 
                              breaks= c(meta[meta$switch==0,]$RIR, meta[meta$sigpoint=="negative",]$RIR,
                                        meta[meta$sigpoint=="positive",]$RIR),
                              labels= c(meta[meta$switch==0,]$xaxis, meta[meta$sigpoint=="negative",]$xaxis,
                                        meta[meta$sigpoint=="positive",]$xaxis)) +
  ggplot2::theme(#axis.title = ggplot2::element_text(size = 15),
        #axis.text= ggplot2::element_text(size = 14),
        panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(), 
        axis.line = ggplot2::element_line(colour = "black"),
        legend.position = "none")

zoom <- meta[meta$switch<=zoom_upper & meta$switch>=zoom_lower,]
zoom <- zoom[zoom$switch>=0,]

if (switch_trm && dcroddsratio_ob) {
  zoom <- zoom[zoom$RIR<=d,]
}
if (switch_trm && !dcroddsratio_ob) {
  zoom <- zoom[zoom$RIR<=c,]
}
if (!switch_trm && dcroddsratio_ob) {
  zoom <- zoom[zoom$RIR<=a,]
}
if (!switch_trm && !dcroddsratio_ob) {
  zoom <- zoom[zoom$RIR<=b,]
}

zoom$label <- ifelse(zoom$sigpoint=="positive", 
                     paste("sig pos:RIR=", 
                           zoom[zoom$sigpoint=="positive",]$RIR),
                     NA)
zoom$label <- ifelse(zoom$sigpoint=="negative", 
                     paste("sig neg:RIR=", 
                           zoom[zoom$sigpoint=="negative",]$RIR),
                     zoom$label)
zoom$label <- ifelse(zoom$sigpoint=="current", 
                     paste("current"),zoom$label)

fig2 <- ggplot2::ggplot(zoom, ggplot2::aes_string(x="RIR",y="pdif"))+
  ggplot2::geom_line(ggplot2::aes_string(y="pdif"), size = 1) +
  ggplot2::geom_point(ggplot2::aes_string(y="pdif", 
                                          shape = "current",
                                          fill = "sigpoint"), 
                      size = 1)+
  ggrepel::geom_label_repel(ggplot2::aes_string(label="label"))+
  ggplot2::scale_fill_manual(values=fillcol)+
  ggplot2::scale_shape_manual(values=pointshape)+
  ggplot2::scale_y_continuous(name="Difference in probability of successful outcome (treatment - control)")+
  ggplot2::scale_x_continuous(name="RIR (Fragility)", 
                              breaks= c(zoom$RIR[1], 
                                        zoom$RIR[as.integer(length(zoom$RIR)/2)], 
                                        zoom$RIR[as.integer(length(zoom$RIR))]),
                              labels= c(zoom$xaxis[1], 
                                        zoom$xaxis[as.integer(length(zoom$RIR)/2)],
                                        zoom$xaxis[as.integer(length(zoom$RIR))])) +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(), 
                 axis.line = ggplot2::element_line(colour = "black"),
                 legend.position = "none")

if (pos_thr_pdif <= max(zoom$pdif) && pos_thr_pdif >= min(zoom$pdif)) {
  fig2 <- fig2 + ggplot2::geom_hline(yintercept = pos_thr_pdif, 
                                     linetype = "dashed", color="green4", size = 1)
}

if (neg_thr_pdif <= max(zoom$pdif) && neg_thr_pdif >= min(zoom$pdif)) {
  fig2 <- fig2 + ggplot2::geom_hline(yintercept = neg_thr_pdif, 
                                     linetype = "dashed", color="red", size = 1)
}

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

if (switch_trm == TRUE) {
  note <- "A bend in line indicates switches from the control 
  row because the treatment row was exhausted."
  } else {
  note <- "A bend in line indicates switches from the treatment row 
  because the control row was exhausted."
  }

result <- list(fig1, note, fig2)

return(result)
}
