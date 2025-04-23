# Henry Sun 
# HW 12 
library(tidyverse)
library(VGAM)
################################################################################
# Problem 1 
# part A 
mu0 <- 0
alpha <- 0.05
sample.sizeA <- 20
(hA.support20 = qt(p = 1-alpha, df = sample.size-1))

# part B
sample.sizeB <- 30
(hA.support30 = qt(p = 1-alpha, df = sample.size-1))

# part C 
# simulation study 
num.sims <- 10000
a <- 0
b <- 4.0
type1error.n20.peek <- 0
type1error.n30.peek <- 0
type1error.n30.final <- 0
for(i in 1:num.sims){
  curr.sim30 <- rlaplace(n=sample.sizeB, location = a, scale = b)
  curr.sim20 <- curr.sim30[1:20]
  
  n20.test <- t.test(x = curr.sim20, mu = mu0, alternative = "greater")
  n20.pval <- n20.test$p.value
  
  n30.test <- t.test(x = curr.sim30, mu = mu0, alternative = "greater")
  n30.pval <- n30.test$p.value
  # p <= alpha is the probably
  if (n20.pval < alpha){
    type1error.n20.peek <- type1error.n20.peek + 1
  }
  else if(n30.pval < alpha){
    type1error.n30.peek <- type1error.n30.peek + 1
  }
  
  if (n30.pval < alpha){
    type1error.n30.final <- type1error.n30.final + 1
  }
}


(e.rate.final <- type1error.n30.final/num.sims)
(e.rate.peek <- (type1error.n20.peek + type1error.n30.peek)
                /num.sims)

