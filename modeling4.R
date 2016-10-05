#######################################################
# This program is to fit mixed models for the monitors
# Programmer: Xinyi Zhao
# Date: 02/11/2016
#######################################################

#install.packages("Metrics")
#install.packages("merTools")
library(lme4)
library(Metrics)
library(merTools)
library(lattice)
library(ggplot2)
#library(lmerTest)

st <- read.csv("C:/Users/zhaohexu/Dropbox/Ozone project/project.csv", stringsAsFactors = FALSE)
#st <- read.csv("C:/Users/Hitomi/Dropbox/Ozone project/project.csv", stringsAsFactors = FALSE)
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

###### Model for each monitor (standard model)

st0 <- st

f.model <- function(site){
  dat <- st0[st0$siteID == site, ]
  
  fit <- lm(y~(sprcp+ssolar+stemp.max+shumid)*sNOx+
              (sprcp+ssolar+stemp.max+shumid)*N2+
              (sprcp+ssolar+stemp.max+shumid)*(sNOx:Vl), data=dat)
  b0 <- fit$coef[1]
  b1 <- fit$coef[6]
  b2 <- fit$coef[7]
  b3 <- fit$coef[16]
  
  return(c(b0,b1,b2,b3))
}

f.model("13-247-1")
f.model("13-89-2")
f.model("17-31-4201")
f.model("18-89-22")
f.model("22-33-13")
f.model("22-33-9")
f.model("22-47-9")
f.model("24-5-3001")
f.model("25-25-42")
f.model("25-9-2006")
f.model("48-113-69")
f.model("48-121-34")
f.model("48-141-44")
f.model("48-201-1039")
f.model("48-201-24")
f.model("48-201-29")
f.model("6-37-1002")
f.model("6-37-2")
f.model("6-37-6012")
f.model("6-71-1004")

##### monitor-specfic mixed model (random intercept model)

## all

fit1 <- lmer(y~(sprcp+ssolar+stemp.max+shumid)*sNOx+
               (sprcp+ssolar+stemp.max+shumid)*N2+
               (sprcp+ssolar+stemp.max+shumid)*(sNOx:Vl)+(1|siteID), data=st)
summary(fit1)
plot(fit1)

## only urban/suburban monitors

st1 <- subset(st, st$siteID != "13-247-1")
st1 <- subset(st1, st1$siteID != "22-33-13")
st1 <- subset(st1, st1$siteID != "22-47-9")
st1 <- subset(st1, st1$siteID != "48-121-34")
st1 <- subset(st1, st1$siteID != "48-201-29")
unique(st1$siteID)

fit2 <- lmer(y~(sprcp+ssolar+stemp.max+shumid)*sNOx+
               (sprcp+ssolar+stemp.max+shumid)*N2+
               (sprcp+ssolar+stemp.max+shumid)*(sNOx:Vl)+(1|siteID), data=st1)
summary(fit2)
plot(fit2)

## only the best monitor of each city and monitors whose R square >= 0.5

st2 <- subset(st, st$siteID != "17-31-4201")
st2 <- subset(st2, st2$siteID != "22-33-13")
st2 <- subset(st2, st2$siteID != "22-47-9")
st2 <- subset(st2, st2$siteID != "25-25-42")
unique(st2$siteID)

fit3 <- lmer(y~(sprcp+ssolar+stemp.max+shumid)*sNOx+
               (sprcp+ssolar+stemp.max+shumid)*N2+
               (sprcp+ssolar+stemp.max+shumid)*(sNOx:Vl)+(1|siteID), data=st2)
summary(fit3)
plot(fit3)

######## Cross-validation

random <- sample(1:5435, 4892)
st3 <- cbind(st,1:5435)
head(st3)
test <- subset(st3,is.element(st3[,34],random)==T) # tester dataset
vali <- subset(st3,is.element(st3[,34],random)==F) # validation dataset
head(test,20)

fit4 <- lmer(y~(sprcp+ssolar+stemp.max+shumid)*sNOx+
               (sprcp+ssolar+stemp.max+shumid)*N2+
               (sprcp+ssolar+stemp.max+shumid)*(sNOx:Vl)+(1|siteID), data=test)
summary(fit4)
dotplot(ranef(fit4, condVar = T))

vali$pred <- predict(fit4, newdata=vali, allow.new.levels=T)
rmse(vali$y, vali$pred) # RMSE
cor(vali$y, vali$pred)^2 # R square out-of-sample
predi <- predictInterval(fit4, newdata=vali) # prediction interval
ggplot(aes(x=vali$siteID, y=fit, ymin=lwr, ymax=upr), data=predi) +
  geom_point() + 
  geom_linerange() +
  labs(x="Monitor", y="Prediction w/ 95% PI") + 
  ggtitle("Prediction with 95% Prediction Interval") + 
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 13)) +
  theme(plot.title = element_text(size = 22))


######## Random slope model

fit5 <- lmer(y~(sprcp+ssolar+stemp.max+shumid)*sNOx+
               (sprcp+ssolar+stemp.max+shumid)*N2+
               (sprcp+ssolar+stemp.max+shumid)*(sNOx:Vl)+(sNOx+N2+(sNOx:Vl)|siteID), data=st)
summary(fit5)
plot(fit5)
anova(fit5)
dotplot(ranef(fit5, condVar = T))

# drop some interactions
fit6 <- lmer(y~(sprcp+ssolar+stemp.max+shumid)*sNOx+
               (ssolar+stemp.max+shumid)*N2+
               (ssolar+stemp.max+shumid)*(sNOx:Vl)+(sNOx+N2+(sNOx:Vl)|siteID), data=st)
summary(fit6)
plot(fit6)
anova(fit5,fit6)

fit7 <- lmer(y~(sprcp+stemp.max+shumid)*sNOx+
               (ssolar+stemp.max+shumid)*N2+
               (ssolar+stemp.max+shumid)*(sNOx:Vl)+(sNOx+N2+(sNOx:Vl)|siteID), data=st)
summary(fit7)
plot(fit7)
anova(fit5,fit7)
dotplot(ranef(fit7, condVar = T))

######## Cross-validation

#### overall mixed model

random <- sample(1:10, 5435, replace=T)
st3 <- cbind(st,random)
write.csv(st3, "crossvali.csv")

train <- subset(st3,random != 10) # change the number from 1 to 10
test <- subset(st3,random == 10) 

fit4 <- lmer(y~(sprcp+stemp.max+shumid)*sNOx+
               (ssolar+stemp.max+shumid)*N2+
               (ssolar+stemp.max+shumid)*(sNOx:Vl)+(sNOx+N2+(sNOx:Vl)|siteID), data=train)
summary(fit4)

test$pred <- predict(fit4, newdata=test, allow.new.levels=T, type="response")
rm.test <- rmse(test$y, test$pred); rm.test # RMSE
rm.test/(max(test$y)-min(test$y)) # NRMSE
cor(test$ozone, test$pred^2)^2 # R square out-of-sample

predi <- predictInterval(fit4, newdata=test, level=0.95, n.sims=1000) # 95% PI 
test2 <- cbind(test,predi)
test2$included <- as.numeric((test2$ozone-test2$lwr^2)*(test2$ozone-test2$upr^2) < 0)
tb <- prop.table(table(test2$included)) # 95% PI coverage

## 95% Prediction Interval using bootstrap

mySumm <- function(.) {
  predict(., newdata=test, re.form=NULL)
}

sumBoot <- function(merBoot) {
  return(
    data.frame(fit = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
               lwr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))),
               upr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
    )
  )
}

boot1 <- lme4::bootMer(fit4, mySumm, nsim=1000, use.u=FALSE, type="parametric")
PI.boot1 <- sumBoot(boot1)

test3 <- cbind(test,PI.boot1)
test3$included <- as.numeric((test3$y-test3$lwr)*(test3$y-test3$upr) < 0)
prop.table(table(test3$included))*100 # 95% PI coverage

#write.csv(test3, "time1bootPI.csv")