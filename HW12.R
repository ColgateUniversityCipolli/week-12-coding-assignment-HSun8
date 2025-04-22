# Henry Sun 
# HW 12 
library(tidyverse)
library(VGAM)
################################################################################
# Problem 1 
# part A 
sample.size <- 20
alpha <- 0.05
(hA.support = qt(p = 1-alpha, df = sample.size-1))

# part B
sample.size <- 30
alpha <- 0.05
(hA.support = qt(p = 1-alpha, df = sample.size-1))

# part C 
# simulation study 
num.sims <- 10000
a <- 0
b <- 4.0

