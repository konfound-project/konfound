RIRvP <- function(ao, bo, co, do){
  A <- a <- ao
  B <- b <- bo
  C <- c_start <- c <- co
  D <- d <- do
  P <- fisher_p(a, b, c, d)
  RIR <- tkonfound(a, b, c, d)$RIR
  while (c>0){
    c <- c-1
    d <- d+1
    A <- c(A,a)
    B <- c(B,b)
    C <- c(C,c)
    D <- c(D,d)
    P <- c(P, fisher_p(a, b, c, d))
    RIR <- c(RIR, tkonfound(a, b, c, d)$RIR)
  }
  data <- data.frame("a"=A, "b"=B, "c"=C, "d"=D, "p"=P, "RIR" = RIR)
  return(data)
}

library(ggplot2)
exp_HCQ <- RIRvP(14, 17, 6, 25)
ggplot(data = exp_HCQ, aes(x = p, y = RIR)) + 
  geom_line(aes(y = RIR), size = 1) 

exp2 <- RIRvP(70, 85, 75, 100)
ggplot(data = exp2[exp2$p<0.06,], aes(x = p, y = RIR)) + 
  geom_line(aes(y = RIR), size = 1) 

# maybe ths
exp3 <- RIRvP(35, 42, 40, 50)
ggplot(data = exp3[exp3$p<0.06,], aes(x = p, y = RIR)) + 
  geom_line(aes(y = RIR), size = 1)

exp3 <- RIRvP(40, 30, 40, 50)
ggplot(data = exp3[exp3$p<0.06,], aes(x = p, y = RIR)) + 
  geom_line(aes(y = RIR), size = 1)


stroke <- RIRvP(20, 174, 5, 181)
ggplot(data = stroke[stroke$p<0.06,], aes(x = p, y = RIR)) + 
  geom_line(aes(y = RIR), size = 1) 

walter <- RIRvP(5, 90, 1, 95)
ggplot(data = walter[walter$p<0.06,], aes(x = p, y = RIR)) + 
  geom_line(aes(y = RIR), size = 1) 

stroke12 <- RIRvP(21, 215, 17, 214)
ggplot(data = stroke12[stroke12$p<0.08,], aes(x = p, y = RIR)) + 
  geom_line(aes(y = RIR), size = 1) + ylim(0,250)

### This is the one that is used in the paper now
stroke2 <- RIRvP(20, 174, 16, 170)
ggplot(data = stroke2[stroke2$p<0.08,], aes(x = p, y = RIR)) + 
  geom_line(aes(y = RIR), size = 1) + ylim(0,175) + 
  #geom_point(data = stroke2[stroke2$RIR==39,], aes(x = p, y = RIR), colour="blue", size = 3)+
  #annotate("text", x= 0.0055, y = 39, label = "VA II", size = 7) + 
  scale_x_continuous(breaks=seq(0,0.05,0.01))+
  scale_y_continuous(breaks=seq(0,175,25))+
  geom_vline(xintercept = 0.01, linetype="dotted")+
  annotate("text", x=0.011, y=25.1, label = "**", size = 11)+
  geom_vline(xintercept = 0.05, linetype="dotted")+
  annotate("text", x=0.0505, y=2.7, label = "*", size = 11)+
  ylab("Robustness of Inference to Replacement") + 
  xlab("p-value")+
  theme(axis.title = ggplot2::element_text(size = 20),
               axis.text= ggplot2::element_text(size = 18),
               panel.grid.major = ggplot2::element_blank(), 
               panel.grid.minor = ggplot2::element_blank(),
               panel.background = ggplot2::element_blank(), 
               axis.line = ggplot2::element_line(colour = "black"),
               legend.position = "none")


# too many bump
exp4 <- RIRvP(40, 40, 40, 100)
ggplot(data = exp4[exp4$p<0.06,], aes(x = p, y = RIR)) + 
  geom_line(aes(y = RIR), size = 1)


exp3 <- RIRvP(700, 850, 400, 1000)
exp3_fig <- ggplot(data = exp3[exp3$p<0.06,], aes(x = p, y = RIR)) + 
  geom_line(aes(y = RIR), size = 1) 

exp1 <- RIRvP(70, 85, 40, 100)
exp1_fig <- ggplot(data = exp1[exp1$p<0.06,], aes(x = p, y = RIR)) + 
  geom_line(aes(y = RIR), size = 1) 

exp2_fig_2 <- ggplot(data = exp2[exp2$p<0.1,], aes(x = p, y = RIR)) + 
  geom_line(aes(y = RIR), size = 1) 

exp3_fig_2 <- ggplot(data = exp3[exp3$p<0.1,], aes(x = p, y = RIR)) + 
  geom_line(aes(y = RIR), size = 1) 

exp1_fig_2 <- ggplot(data = exp1[exp1$p<0.1,], aes(x = p, y = RIR)) + 
  geom_line(aes(y = RIR), size = 1) 

ggarrange(exp1_fig, exp2_fig, exp3_fig,
          labels = c("Small", "Medium", "Large"),
          ncol = 2, nrow = 2)

ggarrange(exp1_fig_2, exp2_fig_2, exp3_fig_2,
          labels = c("Small", "Medium", "Large"),
          ncol = 2, nrow = 2)
