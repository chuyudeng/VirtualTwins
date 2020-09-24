#Functions for Data Simulations

####################################### Linear Data Generation #######################################


regDG <- function(N){
  n <- 2*N #N is 1000, 200 or 80
  p <- 100; c <- 10
  m <- rnorm(100,0,3) #mean for each column
  #continuous variables
  V <- matrix(rnorm(n*p, m, 1),ncol=p, byrow = TRUE)
  #categorical variables
  C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
  #outcome variable
  temp0 <- cbind(V,C) %*% c(0, rep(1,15), rep(0,84), rep(1,2), rep(0,8) )
  temp1 <- cbind(V,C) %*% c(rep(1,20), rep(0,80), rep(1,5), rep(0,5) )
  offset <- mean(temp1) - mean(temp0) ##
  temp1 <- temp1-offset ##
  Y0 <- rnorm(n, mean = temp0, sd=3) #adjust sd here to make R^2 reasonable
  Y1 <- rnorm(n, mean = temp1, sd=3)
  Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
  #making the data frame (110 covars)
  reg <- as.data.frame(cbind(Y,V,C))
  names(reg) <- c("Trt", "Y", paste("V", 1:110, sep=""))
  #Creating training and test samples
  test.i <- sample(nrow(reg), (nrow(reg)/2))
  train <- reg[-test.i,]
  test <- reg[test.i,]
  #Benefit in test group (benefit=T if trt raised outcome)
  benefit <- (temp1-temp0)>0 ##
  return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
}


corDG <- function(N){
  n <- 2*N
  p <- 96; c <- 10
  m <- rnorm(100,0,3) #mean for each column
  #correlated variables
  r <- 0.70; a <- 4
  mat <- matrix(r,a,a); diag(mat) <- 1
  corV <- mvrnorm(n=n, mu=m[1:4], Sigma=mat, empirical=TRUE)
  #continuous variables
  V <- cbind(corV,matrix(rnorm(n*p, m[5:100], 1),ncol=p, byrow = TRUE))
  #categorical variables
  C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
  #outcome variable
  temp0 <- cbind(V,C) %*% c(0, rep(1,15), rep(0,84), rep(1,2), rep(0,8) )
  temp1 <- cbind(V,C) %*% c(rep(1,20), rep(0,80), rep(1,5), rep(0,5) )
  offset <- mean(temp1) - mean(temp0) ##
  temp1 <- temp1-offset ##
  Y0 <- rnorm(n, mean = temp0, sd=3) #adjust sd here to make R^2 reasonable
  Y1 <- rnorm(n, mean = temp1, sd=3)
  Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
  #making the data frame (110 covars)
  cordat <- as.data.frame(cbind(Y,V,C))
  names(cordat) <- c("Trt", "Y", paste("V", 1:110, sep=""))
  #Creating training and test samples
  test.i <- sample(nrow(cordat), (nrow(cordat)/2))
  train <- cordat[-test.i,]
  test <- cordat[test.i,]
  #Benefit in test group (benefit=T if trt raised outcome)
  benefit <- (temp1-temp0)>0 ##
  return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
}

sbDG <- function(N){
  n <- 3*N
  p <- 100; c <- 10
  m <- rnorm(100,0,3) #mean for each column
  #continuous variables
  V <- matrix(rnorm(n*p, m, 1),ncol=p, byrow = TRUE)
  #categorical variables
  C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
  #outcome variable
  temp0 <- cbind(V,C) %*% c(0, rep(1,15), rep(0,84), rep(1,2), rep(0,8) )
  temp1 <- cbind(V,C) %*% c(rep(1,20), rep(0,80), rep(1,5), rep(0,5) )
  offset <- mean(temp1) - mean(temp0) ##
  temp1 <- temp1-offset ##
  Y0 <- rnorm(n, mean = temp0, sd=3) #adjust sd here to make R^2 reasonable
  Y1 <- rnorm(n, mean = temp1, sd=3)
  Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
  #making the data frame (110 covars)
  sbdat <- as.data.frame(cbind(Y,V,C))
  names(sbdat) <- c("Trt", "Y", paste("V", 1:110, sep=""))
  #Creating nonrandom training and random test samples
  test.i <- sample(nrow(sbdat), N)
  test <- sbdat[test.i,]
  Rsum <- rowSums(sbdat[-test.i ,c("V15", "V16", "V17","V18")]) ######both predictive and prog covariates
  tophalf <- sbdat[-test.i,][Rsum>sort(Rsum)[N],]
  bothalf <- sbdat[-test.i,][Rsum<=sort(Rsum)[N],]
  train <- rbind(sample_n(tbl = tophalf, size = N*0.75), sample_n(tbl = bothalf, size = N/4))
  #Benefit in test group (benefit=T if trt raised outcome)
  benefit <- (temp1-temp0)>0 ##
  return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
}


####################################### Nonlinear Data Generation #######################################

treeregDG <- function(N){
  n <- 2*N #N is 1000, 200 or 80
  p <- 100; c <- 10
  m <- rnorm(100,0,3) #mean for each column
  #continuous variables
  V <- matrix(rnorm(n*p, m, 1),ncol=p, byrow = TRUE)
  #categorical variables
  C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
  #outcome variable
  temp0 <- temp1 <- c()
  for(i in 1:n){
    if(V[i,1]>m[1]){
      if(V[i,5]>m[5]){temp0[i] <- 20; temp1[i] <- 22
      }else{temp0[i] <- 23; temp1[i] <- 20}
    }else{
      if(V[i,6]>m[6]){temp0[i] <- 25; temp1[i] <- 25
      }else{temp0[i] <- 22; temp1[i] <- 23}
    }
  }
  Y0 <- rnorm(n, mean = temp0, sd=1)
  Y1 <- rnorm(n, mean = temp1, sd=1)
  Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
  #making the data frame (110 covars)
  reg <- as.data.frame(cbind(Y,V,C))
  names(reg) <- c("Trt", "Y", paste("V", 1:110, sep=""))
  #Creating training and test samples
  test.i <- sample(nrow(reg), (nrow(reg)/2))
  train <- reg[-test.i,]
  test <- reg[test.i,]
  #Benefit in test group (benefit=T if trt raised outcome)
  benefit <- (temp1-temp0)>0 ##
  return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
}

treecorDG <- function(N){
  n <- 2*N
  p <- 96; c <- 10
  m <- rnorm(100,0,3) #mean for each column
  #correlated variables
  r <- 0.70; a <- 4
  mat <- matrix(r,a,a); diag(mat) <- 1
  corV <- mvrnorm(n=n, mu=m[1:4], Sigma=mat, empirical=TRUE)
  #continuous variables
  V <- cbind(corV,matrix(rnorm(n*p, m[5:100], 1),ncol=p, byrow = TRUE))
  #categorical variables
  C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
  #outcome variable
  temp0 <- temp1 <- c()
  for(i in 1:n){
    if(V[i,1]>m[1]){
      if(V[i,5]>m[5]){temp0[i] <- 20; temp1[i] <- 22
      }else{temp0[i] <- 23; temp1[i] <- 20}
    }else{
      if(V[i,6]>m[6]){temp0[i] <- 25; temp1[i] <- 25
      }else{temp0[i] <- 22; temp1[i] <- 23}
    }
  }
  Y0 <- rnorm(n, mean = temp0, sd=1)
  Y1 <- rnorm(n, mean = temp1, sd=1)
  Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
  #making the data frame (110 covars)
  cordat <- as.data.frame(cbind(Y,V,C))
  names(cordat) <- c("Trt", "Y", paste("V", 1:110, sep=""))
  #Creating training and test samples
  test.i <- sample(nrow(cordat), (nrow(cordat)/2))
  train <- cordat[-test.i,]
  test <- cordat[test.i,]
  #Benefit in test group (benefit=T if trt raised outcome)
  benefit <- (temp1-temp0)>0 ##
  return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
}

treesbDG <- function(N){
  n <- 3*N
  p <- 100; c <- 10
  m <- rnorm(100,0,3) #mean for each column
  #continuous variables
  V <- matrix(rnorm(n*p, m, 1),ncol=p, byrow = TRUE)
  #categorical variables
  C <- matrix(rbinom(c*n, size=1, prob=0.7), ncol = c, byrow = TRUE)
  #outcome variable
  temp0 <- temp1 <- c()
  for(i in 1:n){
    if(V[i,1]>m[1]){
      if(V[i,5]>m[5]){temp0[i] <- 20; temp1[i] <- 22
      }else{temp0[i] <- 23; temp1[i] <- 20}
    }else{
      if(V[i,6]>m[6]){temp0[i] <- 25; temp1[i] <- 25
      }else{temp0[i] <- 22; temp1[i] <- 23}
    }
  }
  Y0 <- rnorm(n, mean = temp0, sd=1)
  Y1 <- rnorm(n, mean = temp1, sd=1)
  Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
  #making the data frame (110 covars)
  sbdat <- as.data.frame(cbind(Y, V,C))
  names(sbdat) <- c("Trt", "Y", paste("V", 1:110, sep=""))
  #Creating nonrandom training and random test samples
  test.i <- sample(nrow(sbdat), N)
  test <- sbdat[test.i,]
  Rsum <- rowSums(sbdat[-test.i ,c("V1", "V5")]) ###
  tophalf <- sbdat[-test.i,][Rsum>sort(Rsum)[N],]
  bothalf <- sbdat[-test.i,][Rsum<=sort(Rsum)[N],]
  train <- rbind(sample_n(tbl = tophalf, size = N*0.75), sample_n(tbl = bothalf, size = N/4))
  #Benefit in test group (benefit=T if trt raised outcome)
  benefit <- (temp1-temp0)>0 ##
  return(list(train=train, test=test, bene=benefit[test.i], temp0=temp0[test.i], temp1=temp1[test.i]))
}

####################################### VT Step 1 #######################################
#Get individual treatment effects 

#LASSO models for trt/ctrl arms, errors "nope" when lasso model is too sparse
i.las <- function(train, test){
  dat0 <- subset(train, train$Trt==0)
  dat1 <- subset(train, train$Trt==1)
  m0 <- cv.glmnet(x = data.matrix(subset(dat0, select = -c(Y, Trt))), 
                  y = dat0$Y, 
                  family = "gaussian", alpha = 1, standardize = TRUE)
  m1 <- cv.glmnet(x = data.matrix(subset(dat1, select = -c(Y, Trt))), 
                  y = dat1$Y, 
                  family = "gaussian", alpha = 1, standardize = TRUE)
  p0 <- coef(m0, s="lambda.1se")
  p1 <- coef(m1, s="lambda.1se")
  ifelse(length(p0@i)<2 | length(p1@i)<2, stop("nope"), x <- 1)
  pred0 <- predict(m0, newx = data.matrix(subset(test, select = -c(Y, Trt))), s = "lambda.1se")
  pred1 <- predict(m1, newx = data.matrix(subset(test, select = -c(Y, Trt))), s = "lambda.1se")
  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

#RF with RandomforestSRC
i.rf <- function(train, test){
  dat0 <- subset(train, train$Trt==0)
  dat1 <- subset(train, train$Trt==1)
  m0 <- tune(Y~. , data = subset(dat0, select = -Trt), doBest = TRUE) #automatically tunes forest
  m1 <- tune(Y~. , data = subset(dat1, select = -Trt), doBest = TRUE)
  pred0 <- predict(m0$rf, subset(test, select = -c(Y, Trt)))$predicted
  pred1 <- predict(m1$rf, subset(test, select = -c(Y, Trt)))$predicted
  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

#Piecewise model with MARS
i.mars <- function(train, test){
  dat0 <- subset(train, train$Trt==0)
  dat1 <- subset(train, train$Trt==1)
  m0 <- earth(Y~., data = subset(dat0, select = -Trt))
  m1 <- earth(Y~., data = subset(dat1, select = -Trt))
  pred0 <- predict(m0, newdata = subset(test, select = -c(Y, Trt)), type = "response")
  pred1 <- predict(m1, newdata = subset(test, select = -c(Y, Trt)), type = "response")
  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

#SVM using kernlab inside of caret
i.svm <- function(train, test){
  dat0 <- subset(train, train$Trt==0)
  dat1 <- subset(train, train$Trt==1)
  m0 <- caret::train(Y~., data = subset(dat0, select = -Trt),
                     method = "svmRadial",
                     tuneLength = 3, 
                     trControl=trainControl(method = "repeatedcv", number = 10, repeats = 3))
  m1 <- caret::train(Y~., data = subset(dat1, select = -Trt),
                     method = "svmRadial",
                     tuneLength = 3, 
                     trControl=trainControl(method = "repeatedcv", number = 10, repeats = 3)) 
  pred0 <- predict(m0$finalModel, newdata=subset(test, select = -c(Y, Trt)))
  pred1 <- predict(m1$finalModel, newdata=subset(test, select = -c(Y, Trt)))  
  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

#Piecewise model with Superlearner
i.super <- function(train, test){
  dat0 <- subset(train, train$Trt==0)
  dat1 <- subset(train, train$Trt==1)
  SL.earth.def = function(...) { #changing to earth package defaults
    SL.earth(..., degree = 1, penalty = 2)
  }
  slmethods <- c("SL.glmnet", "SL.randomForest","SL.earth.def") 
  m0 <- SuperLearner(Y = dat0$Y, 
                     X = as.data.frame(subset(dat0, select = -c(Y, Trt))), 
                     family = gaussian(), 
                     SL.library = slmethods)
  m1 <- SuperLearner(Y = dat1$Y, 
                     X = as.data.frame(subset(dat1, select = -c(Y, Trt))), 
                     family = gaussian(), 
                     SL.library = slmethods)
  pred0 <- predict.SuperLearner(m0, as.data.frame(subset(test, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
  pred1 <- predict.SuperLearner(m1, as.data.frame(subset(test, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
  Z <- pred1$pred-pred0$pred
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

####################################### VT Step 2 #######################################
#make trees using test set, run the estimated test set treatment effects through to get classification 

#not doing a step 2, just using est from step 1 to get trt assignment
c.none <- function(dat, est){
  preds <- est$Z
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
  mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  return(list(nwg=sum(wg), mse=mse))
}

#using a tree to get number of misclassified, and grabbing the predictors
c.tree <- function(dat, est){
  test <- dat$test
  test$Z <- as.double(est$Z)
  tfit <- caret::train(Z~., data = subset(test, select = -c(Y, Trt)),
                       method = "rpart2",
                       tuneLength = 3, 
                       trControl=trainControl(method = "repeatedcv", number = 10, repeats = 3))
  preds <- predict(tfit$finalModel, newdata = subset(test, select = -c(Y, Trt)))
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
  mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  #getting predictors from tree: 
  vars <- levels(tfit$finalModel$frame$var)
  tvars <- vars[!vars=="<leaf>"]
  return(list(nwg=sum(wg), mse=mse, vars=tvars, nvars = length(tvars)))
}


#linear model for step 2, and using top predictors (same # as tree)
c.lin <- function(dat, est, top){
  test <- dat$test
  test$Z <- est$Z
  m <- cv.glmnet(x = data.matrix(subset(dat$test, select = -c(Y, Trt))), 
                    y = data.matrix(est$Z), 
                    family = "gaussian", alpha = 1, standardize = TRUE)
  #getting top predictors: 
  p <- coef(m, s="lambda.min") %>% as.matrix()
  pv <- p[-1]
  vars <- rownames(p)[abs(p)>=sort(abs(pv), decreasing = T)[top] & p!=0]
  lvars <- vars[!vars=="(Intercept)"]
  #fitting the linear model with top predictors
  fit <- lm(paste("Z~", paste(lvars, collapse = "+")), data = test)
  preds <- predict(fit, newdata = subset(test, select = -c(Z, Y, Trt)))
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
  mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  return(list(nwg=sum(wg), mse=mse, vars=lvars))
}

#using a conditional inference tree
c.ctree <- function(dat, est){
  test <- dat$test
  test$Z <- as.double(est$Z)
  tfit <- caret::train(Z~., data = subset(test, select = -c(Y, Trt)), 
                      method = 'ctree2', 
                      trControl = trainControl(method = "repeatedcv", number=10, repeats = 3),
                      tuneGrid = expand.grid(maxdepth = c(1:3), mincriterion=0.95),
                      metric='RMSE')
  preds <- predict(tfit$finalModel, newdata = subset(test, select = -c(Y, Trt)))
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
  mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  #getting predictors from tree: 
  raw <- capture.output(tfit$finalModel@tree)
  vars <- unique(str_trim(str_match(raw, "\\)(.+?)>")[,2]))
  vars <- vars[!is.na(vars)]
  return(list(nwg=sum(wg), mse=mse, vars=vars))
}


