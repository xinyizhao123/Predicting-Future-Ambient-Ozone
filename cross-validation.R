##################################################
# This program is to fit mixed/individual models
# Programmer: Xinyi Zhao
# Date: 03/11/2016
##################################################

library(lme4)
library(Metrics)
library(merTools)
library(lattice)

st <- read.csv("C:/Users/zhaohexu/Dropbox/Ozone project/project.csv", stringsAsFactors = FALSE)
st <- st[-c(1)]

st <- subset(st, st$state != "Rhode Island") # drop Providence

# standardize variables

st$sNOx <- st$NOx/sd(st$NOx)
st$sVOC <- st$VOC/sd(st$VOC)
st$stemp.max <- st$temp.max/sd(st$temp.max)
st$shumid <- st$humid/sd(st$humid)
st$sprcp <- st$prcp/sd(st$prcp)
st$ssolar <- st$solar/sd(st$solar)

st$y <- sqrt(st$ozone)
st$N2 <- st$sNOx^2
st$Vl <- log(st$sVOC+1)

st <- subset(st, st$siteID == "13-247-1"|st$siteID == "13-89-2"|st$siteID == "24-5-3001"|st$siteID == "18-89-22"|
               st$siteID == "48-121-34"|st$siteID == "6-37-2"|st$siteID == "6-71-1004"|st$siteID == "48-113-69"|
               st$siteID == "6-37-1002"|st$siteID == "6-37-6012")

##### cross-validation

# (1) overall mixed model

N <- length(st$date)
random <- sample(1:10, N, replace=T)
st3 <- cbind(st,random)
num <- unique(random)

rm <- rep(NA,10)
rsq <- rep(NA,10)
cvg <- rep(NA,10)
beta <- matrix(NA,10,17)
lb <- matrix(NA,10,17)
ub <- matrix(NA,10,17)

for (i in 1:length(num)){
  train <- subset(st3,random != num[i]) # change the number from 1 to 10
  test <- subset(st3,random == num[i]) 
  
  fit4 <- lmer(y~(sprcp+stemp.max+shumid)*sNOx+
                 (ssolar+stemp.max+shumid)*N2+
                 (ssolar+stemp.max+shumid)*(sNOx:Vl)+(sNOx+N2+(sNOx:Vl)|site), data=train)
  
  beta[i,] <- summary(fit4)$coefficients[,1]
  se <- summary(fit4)$coefficients[,2]
  lb[i,] <- summary(fit4)$coefficients[,1]-1.96*se
  ub[i,] <- summary(fit4)$coefficients[,1]+1.96*se
  
  test$pred <- predict(fit4, newdata=test, allow.new.levels=T, type="response")
  rm[i] <- rmse(test$ozone, test$pred^2) # RMSE
  rsq[i] <- cor(test$ozone, test$pred^2)^2 # R square out-of-sample
  
  predi <- predictInterval(fit4, newdata=test, level=0.95, n.sims=1000) # 95% PI 
  test2 <- cbind(test, predi)
  test2$included <- as.numeric((test2$ozone-test2$lwr^2)*(test2$ozone-test2$upr^2) < 0)
  tb <- prop.table(table(test2$included))
  cvg[i] <- tb[2] # 95% PI coverage
}

mean(rm) # overall RMSE
mean(rsq) # overall R square
mean(cvg) # overall PI coverage

colMeans(beta) # overall beta estimates
colMeans(lb) # lower bound for beta
colMeans(ub) # upper bound for beta

# (2) individual model (12-variable)

st0 <- st
tmp.id <- unique(st$siteID)
n <- 0
rm12 <- 0
rsq12 <- 0
cvg12 <- 0
rm15 <- 0
rsq15 <- 0
cvg15 <- 0
beta12 <- matrix(NA,length(unique(st$siteID)),17)
lb12 <- matrix(NA,length(unique(st$siteID)),17)
ub12 <- matrix(NA,length(unique(st$siteID)),17)

for (iid in tmp.id){
  dat <- st0[st0$siteID == iid, ]
  n[iid] <- length(dat$date)
  random <- sample(1:10, length(dat$date), replace=T)
  st3 <- cbind(dat,random)
  num <- unique(random)
  rm.12 <- rep(NA,10)
  rsq.12 <- rep(NA,10)
  cvg.12 <- rep(NA,10)
  rm.15 <- rep(NA,10)
  rsq.15 <- rep(NA,10)
  cvg.15 <- rep(NA,10)
  beta.12 <- matrix(NA,10,17)
  lb.12 <- matrix(NA,10,17)
  ub.12 <- matrix(NA,10,17)
  
  for (i in 1:length(num)){
    train <- subset(st3,random != num[i]) 
    test <- subset(st3,random == num[i]) 
    
    # 12-variable model
    fit.12 <- lm(y~(sprcp+stemp.max+shumid)*sNOx+
                   (ssolar+stemp.max+shumid)*N2+
                   (ssolar+stemp.max+shumid)*(sNOx:Vl), data=train)
    
    beta.12[i,] <- summary(fit.12)$coefficients[,1]
    se.12 <- summary(fit.12)$coefficients[,2]
    ci.12 <- confint(fit.12)
    lb.12[i,] <- ci.12[,1]
    ub.12[i,] <- ci.12[,2]
    
    predi.12 <- predict(fit.12, newdata=test, allow.new.levels=T, interval="predict") # 95% PI
    test.12 <- cbind(test, predi.12)
    rm.12[i] <- rmse(test.12$ozone, test.12$fit^2) # RMSE
    rsq.12[i] <- cor(test.12$ozone, test.12$fit^2)^2 # R square out-of-sample
    test.12$included <- as.numeric((test.12$ozone-test.12$lwr^2)*(test.12$ozone-test.12$upr^2) < 0)
    tb.12 <- prop.table(table(test.12$included))
    cvg.12[i] <- tb.12[2] # 95% PI coverage
    if(is.na(cvg.12[i])) {
      cvg.12[i] <- 1
    }
    
    # 15-variable model
    fit.15 <- lm(y~(sprcp+ssolar+stemp.max+shumid)*sNOx+
                   (sprcp+ssolar+stemp.max+shumid)*N2+
                   (sprcp+ssolar+stemp.max+shumid)*(sNOx:Vl), data=train)
    
    predi.15 <- predict(fit.15, newdata=test, allow.new.levels=T, interval="predict") # 95% PI
    test.15 <- cbind(test, predi.15)
    rm.15[i] <- rmse(test.15$ozone, test.15$fit^2) # RMSE
    rsq.15[i] <- cor(test.15$ozone, test.15$fit^2)^2 # R square out-of-sample
    test.15$included <- as.numeric((test.15$ozone-test.15$lwr^2)*(test.15$ozone-test.15$upr^2) < 0)
    tb.15 <- prop.table(table(test.15$included))
    cvg.15[i] <- tb.15[2] # 95% PI coverage
    if(is.na(cvg.15[i])) {
      cvg.15[i] <- 1
    }
  }
  
  rm12[iid] <- mean(rm.12)
  rsq12[iid] <- mean(rsq.12)
  cvg12[iid] <- mean(cvg.12)
  
  rm15[iid] <- mean(rm.15)
  rsq15[iid] <- mean(rsq.15)
  cvg15[iid] <- mean(cvg.15)
  
  beta12[iid] <- colMeans(beta.12) # overall beta estimates
  lb12[iid] <- colMeans(lb.12) # lower bound for beta
  ub12[iid] <- colMeans(ub.12) # upper bound for beta
}

# results for 12-variable model

n # number of observations
rm12 # RMSE
rsq12 # R square out-of-sample
cvg12 # 95% PI coverage
mean(rm12[2:length(rm12)]) # overall RMSE
mean(rsq12[2:length(rsq12)]) # overall R square
mean(cvg12[2:length(cvg12)]) # overall PI coverage

# results for 15-variable model

rm15 # RMSE
rsq15 # R square out-of-sample
cvg15 # 95% PI coverage
mean(rm15[2:length(rm15)]) # overall RMSE
mean(rsq15[2:length(rsq15)]) # overall R square
mean(cvg15[2:length(cvg15)]) # overall PI coverage
