# Henry Sun 
# HW 12 
# Q: do i need to set.seed()? type 1 errors will change every time
library(tidyverse)
library(VGAM)
library(xtable)
################################################################################
# Problem 1 
# part A 
alpha <- 0.05
sample.sizeA <- 20
(hA.support20 = qt(p = 1-alpha, df = sample.sizeA-1))

# part B
alpha <- 0.05
sample.sizeB <- 30
(hA.support30 = qt(p = 1-alpha, df = sample.sizeB-1))

# part C 
# simulation study 
mu0 <- 0
num.sims <- 10000
a <- 0
b <- 4.0
# errors
type1error.n20.peek <- 0
type1error.n30.peek <- 0
type1error.n30.final <- 0

for(i in 1:num.sims){
  # sims for n = 20 peek, n = 30
  curr.sim30 <- rlaplace(n=sample.sizeB, location = a, scale = b)
  curr.sim20 <- curr.sim30[1:20]
  
  # t test for n = 20 peek
  n20.test <- t.test(x = curr.sim20, mu = mu0, alternative = "greater")
  n20.t <- n20.test$statistic
  
  # t test for n = 30
  n30.test <- t.test(x = curr.sim30, mu = mu0, alternative = "greater")
  n30.t <- n30.test$statistic
  
  # if hA is supported with n = 20
  # if not continue on and see if hA is supported with n = 30
  if (n20.t >= hA.support20){
    type1error.n20.peek <- type1error.n20.peek + 1
  }
  
  # situation where researchers do not peek the data
  else if(n30.t >= hA.support30){
    type1error.n30.peek <- type1error.n30.peek + 1
  }

  if (n30.t >= hA.support30){
    type1error.n30.final <- type1error.n30.final + 1
  }
}


(e.rate.final <- type1error.n30.final/num.sims)
(e.rate.peek <- (type1error.n20.peek + type1error.n30.peek)
                /num.sims)

################################################################################
# Problem 2
betatype1.errors <- function(a, b){
  num.sims <- 1000
  alpha <- a
  beta <- b
  pop.mean <- alpha/(alpha + beta)
  dist.skew <- (2*(beta-alpha)*sqrt(alpha+beta+1))/
               ((alpha + beta + 2)*sqrt(alpha*beta))
  sample.size2 <- 15
  
  # errors
  righttail.errors <- 0 
  lefttail.errors <- 0
  twotail.errors <- 0
  
  for(i in 1:num.sims){
    # sims using rbeta, n = 15
    curr.sim <- rbeta(n=sample.size2, shape1 = alpha, shape2 = beta)
    
    # t test for right tail 
    t.right <- t.test(x=curr.sim, mu = pop.mean, alternative = "greater")
    right.pval <- t.right$p.value
    # t test for left tail 
    t.left <- t.test(x=curr.sim, mu = pop.mean, alternative = "less")
    left.pval <- t.left$p.value
    # t test for two tailed
    t.twotail <- t.test(x=curr.sim, mu = pop.mean, alternative = "two.sided")
    twotail.pval <- t.twotail$p.value
    
    if (right.pval < 0.05){
      righttail.errors <- righttail.errors + 1
    }
    if (left.pval < 0.05){
      lefttail.errors <- lefttail.errors + 1
    }
    if(twotail.pval < 0.05){
      twotail.errors <- twotail.errors + 1
    }
  }
  
  error.left <- lefttail.errors/num.sims
  error.right <- righttail.errors/num.sims
  error.twotail <- twotail.errors/num.sims
  
  tibble(type = c("left tailed", "right tailed", "two tailed", "skewness"),
         errors = c(error.left, error.right, error.twotail, dist.skew)) |>
    pivot_wider(names_from = type, values_from = errors)
}

errors.data <- tibble(bind_rows(betatype1.errors(a = 10, b = 2),
betatype1.errors(a = 2, b = 10),
betatype1.errors(a = 10, b = 10)))

errors.table <- tibble(distribution = c("Beta(10,2)", "Beta(2,10)", 
                                        "Beta(10,10)"), errors.data)

xtable(errors.table)