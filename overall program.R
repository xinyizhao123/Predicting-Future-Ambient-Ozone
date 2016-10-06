library(lme4)
library(Metrics)
library(merTools)
library(lattice)

st <- read.csv("C:/Users/Hitomi/Dropbox/Ozone project/project.csv", stringsAsFactors = FALSE)
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

################################
#      summary statistics      #
################################

### overall ###

summary(st$ozone)
hist(st$ozone)
mean(st$ozone)
sd(st$ozone)
prop.table(table(st$ozone.q))*100

summary(st$VOC)
hist(st$VOC) # skewed
mean(st$VOC)
sd(st$VOC)

summary(st$NOx)
hist(st$NOx) # skewed
mean(st$NOx)
sd(st$NOx)
prop.table(table(st$NOx.q))*100

summary(st$temp.max)
hist(st$temp.max)
mean(st$temp.max)
sd(st$temp.max)
prop.table(table(st$temp.q))*100

summary(st$humid)
hist(st$humid)
mean(st$humid)
sd(st$humid)
prop.table(table(st$humid.q))*100

summary(st$prcp)
hist(st$prcp) # skewed
mean(st$prcp)
sd(st$prcp)

summary(st$solar)
hist(st$solar)
mean(st$solar)
sd(st$solar)

### by site ###

tapply(st$date, st$siteID, length)

tapply(st$ozone, st$siteID, summary)
tapply(st$ozone, st$siteID, sd)

tapply(st$VOC, st$siteID, summary)
tapply(st$VOC, st$siteID, sd)

tapply(st$NOx, st$siteID, summary)
tapply(st$NOx, st$siteID, sd)

#########################
#      correlation      #
#########################

#### correlation within sites ###

abc <- st
ro1 <- 0
ro2 <- 0
ro3 <- 0
tmp.id=unique(st$siteID)
for (iid in tmp.id){
  ro1[iid] <- cor(abc[which(st$siteID==iid),"ozone"],st[which(st$siteID==iid),"VOC"])
  ro2[iid] <- cor(abc[which(st$siteID==iid),"ozone"],st[which(st$siteID==iid),"NOx"])
  ro3[iid] <- cor(abc[which(st$siteID==iid),"NOx"],st[which(st$siteID==iid),"VOC"])
}

# ozone and VOC
ro1
# ozone and NOx
ro2
# NOx and VOC
ro3

#### correlation between sites ###

st0 <- st[c(1,2,3,5,6)]

f.corr <- function(site1, site2){
  dat1 <- st0[st0$siteID == site1, ]
  dat2 <- st0[st0$siteID == site2, ]
  
  colnames(dat1)[3] <- "ozone1"
  colnames(dat2)[3] <- "ozone2"
  colnames(dat1)[4] <- "VOC1"
  colnames(dat2)[4] <- "VOC2"
  colnames(dat1)[5] <- "NOx1"
  colnames(dat2)[5] <- "NOx2"
  
  dat1 <- dat1[order(dat1$date),]
  dat2 <- dat2[order(dat2$date),]
  
  dat1 <- dat1[-c(1)]
  dat2 <- dat2[-c(1)]
  
  compare <- merge(dat1, dat2, by=c("date"))
  
  r.ozone <- cor(compare$ozone1, compare$ozone2)
  r.voc <- cor(compare$VOC1, compare$VOC2)
  r.nox <- cor(compare$NOx1, compare$NOx2)
  
  return(c(r.ozone, r.voc, r.nox))
}

# return order: ozone, VOC, NOx
f.corr("13-247-1", "13-89-2")
f.corr("48-113-69", "48-121-34")
f.corr("6-37-1002", "6-37-2")
f.corr("6-37-1002", "6-37-6012")
f.corr("6-37-1002", "6-71-1004")
f.corr("6-37-2", "6-37-6012")
f.corr("6-37-2", "6-71-1004")
f.corr("6-37-6012", "6-71-1004")

##############################
#      cross-validation      #
##############################

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

# individual model (12-variable)

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
  }
  
  rm12[iid] <- mean(rm.12)
  rsq12[iid] <- mean(rsq.12)
  cvg12[iid] <- mean(cvg.12)
  
  beta12[which(tmp.id==iid),] <- colMeans(beta.12) # overall beta estimates
  lb12[which(tmp.id==iid),] <- colMeans(lb.12) # lower bound for beta
  ub12[which(tmp.id==iid),] <- colMeans(ub.12) # upper bound for beta
}

rownames(beta12) <- tmp.id
rownames(lb12) <- tmp.id
rownames(ub12) <- tmp.id

# results for 12-variable model

n # number of observations
rm12 # RMSE
rsq12 # R square out-of-sample
cvg12 # 95% PI coverage

beta12 # beta estimates
lb12
lb12.r <- round(lb12, digits = 3)# lower CL for beta
ub12
ub12.r <- round(ub12, digits = 3)# upper CL for beta

mean(rm12[2:length(rm12)]) # overall RMSE
mean(rsq12[2:length(rsq12)]) # overall R square
mean(cvg12[2:length(cvg12)]) # overall PI coverage
