RIRvP <- function(a, b, c, d){
  A <- a
  B <- b
  C <- c_start <- c
  D <- d
  P <- fisher_p(a, b, c, d)
  rate <- (a+c)/(a+b+c+d)
  RIR <- 0
  while (fisher_p(a, b, c, d) < 0.1){
    c <- c+1
    d <- d-1
    A <- c(A,a)
    B <- c(B,b)
    C <- c(C,c)
    D <- c(D,d)
    P <- c(P, fisher_p(a, b, c, d))
    RIR <- c(RIR, round((c-c_start)/rate))
  }
  data <- data.frame("a"=A, "b"=B, "c"=C, "d"=D, "p"=P, "RIR" = RIR)
  return(data)
}

library(ggplot2)
exp_HCQ <- RIRvP(14, 17, 6, 25)
ggplot(data = exp_HCQ, aes(x = p, y = RIR)) + 
  geom_line(aes(y = RIR), size = 1) 

exp2 <- RIRvP(140, 170, 80, 200)
exp2_fig <- ggplot(data = exp2[exp2$p<0.06,], aes(x = p, y = RIR)) + 
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
