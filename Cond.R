#Local version of VTSims code

#Packages for local version of Cond.R:
library(rpart)
library(rpart.plot)
library(glmnet)
library(dplyr)
library(randomForest)
library(randomForestSRC) 
library(purrr)
library(MASS)
library(earth)
library(SuperLearner)
library(caret)
library(party)
library(stringr)
set.seed(1234)

#Helper functions:
source('C:/Users/Chuyu/Desktop/Research/StatInMed/Funcs.R')

####################################
# dg specifies the data generation scheme of the simulation set 
# options: regDG, corDG, sbDG, treeregDG, treecorDG, treesbDG
dg <- regDG

# vt1 specifies the method used for step 1 of VT
# options: i.las, i.rf, i.mars, i.super, i.svm
vt1 <- i.rf

# sims specifies the number of total simulation iterations
sims <- 200  
####################################

seeds <- round(runif(n = sims, min = 1, max = 10000))

#Create simulation wrapper  
Cond <- function(N, seed){
  set.seed(seed)
  #generate data based on script input for dg
  dat <- dg(N)
  #VT1: getting individual treatment effects based on script input for vt1
  est <- vt1(dat$train, dat$test)
  #VT2: getting results from all 3 variations
  b.none <- c.none(dat, est)
  b.tree <- c.tree(dat, est)
  b.lin <- c.lin(dat, est, b.tree$nvars)
  b.ctree <- c.ctree(dat, est)
  #putting all the results together
  nwgs <- data.frame(none=b.none$nwg, lin=b.lin$nwg, tree=b.tree$nwg, ctree=b.ctree$nwg)
  mses <- data.frame(none=b.none$mse, lin=b.lin$mse, tree=b.tree$mse, ctree=b.ctree$mse)
  vars <- list(lin=b.lin$vars, tree=b.tree$vars, ctree=b.ctree$vars)
  return(list(nwgs=nwgs, mses=mses, vars=vars))
}

#Error handeling
Cond <- possibly(eval(Cond), otherwise = list("error", "error", "error"))

#Sample size of 1000:
list1 <- mapply(Cond, 1000, seeds)
#Sample size of 200:
list2 <- mapply(Cond, 200, seeds)
#Sample size of 80:
list3 <- mapply(Cond, 80, seeds)

#Storing all the simulations:
res <- list(list1, list2, list3)

