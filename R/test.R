# test

#m1 <- lm(mpg ~ wt + hp, data = mtcars)
# # 
# m2 <-glm(am ~ cyl + hp * wt, data = mtcars, family = binomial)
# m2
# x <- margins::margins(m2)
# konfound(m2, cyl)
# 
# library("margins")
# x <- lm(mpg ~ cyl * hp + wt, data = mtcars)
# (m <- margins(x))
# 
# library(nlme)
# fm1 <- lme(distance ~ age, data = Orthodont) # random is ~ age
# konfound(fm1, age)
# 
# library(mgcv)
# set.seed(2) ## simulate some data...
# dat <- gamSim(1,n=400,dist="normal",scale=2)
# b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
# konfound(b, x0)
# 
# xx <- KRmodcomp(fm1, sm)
# ?getKR(xx, "ddf") # get denominator degrees of freedom.
