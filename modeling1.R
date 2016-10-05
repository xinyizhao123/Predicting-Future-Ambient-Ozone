#######################################################
# This program is to perform modeling for the dataset
# Programmer: Xinyi Zhao
# Date: 01/23/2016
#######################################################

#setwd("C:/Users/zhaohexu/Dropbox/Ozone project")
#setwd("C:/Users/Hitomi/Dropbox/Ozone project")
#install.packages("leaps")
library(ggplot2)
library(leaps)

st <- read.csv("study1.csv", stringsAsFactors = FALSE)
st <- st[-c(1)]
st <- st[st$county == "Harris", ]
unique(st$siteID)

# transformation of response variable
st$so <- sqrt(st$ozone) # square root transformed
st$lo <- log(st$ozone) # log-transformed
st$co <- (st$ozone)^(1/3) # cube root transformed

# create higher order terms of predictors
st$N2 <- st$NOx^2
st$V2 <- st$VOC^2
st$N3 <- st$NOx^3
st$V3 <- st$VOC^3
st$Nl <- log(st$NOx) # zero values
st$Vl <- log(st$VOC)
st$Ns=sqrt(st$NOx)
st$Vs=sqrt(st$VOC)
st$Nc=st$NOx^(1/3)
st$Vc=st$VOC^(1/3)

##### plot (unadjusted association)

ggplot(st, aes(x=VOC, y=ozone)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Ozone and VOC") + theme(axis.title = element_text(size = 15.5)) + 
  theme(plot.title = element_text(size = 19)) + theme(axis.text = element_text(size = 13))

ggplot(st, aes(x=NOx, y=ozone)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Ozone and NOx") + theme(axis.title = element_text(size = 15.5)) + 
  theme(plot.title = element_text(size = 19)) + theme(axis.text = element_text(size = 13))

ggplot(st, aes(x=temp, y=ozone)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Ozone and Temperature") + theme(axis.title = element_text(size = 15.5)) + 
  theme(plot.title = element_text(size = 19)) + theme(axis.text = element_text(size = 13))

ggplot(st, aes(x=humid, y=ozone)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Ozone and Relative Humidity") + theme(axis.title = element_text(size = 15.5)) + 
  theme(plot.title = element_text(size = 19)) + theme(axis.text = element_text(size = 13))

ggplot(st, aes(x=prcp, y=ozone)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Ozone and Precipitation") + theme(axis.title = element_text(size = 15.5)) + 
  theme(plot.title = element_text(size = 19)) + theme(axis.text = element_text(size = 13))

ggplot(st, aes(x=solar, y=ozone)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Ozone and Solar Radiation") + theme(axis.title = element_text(size = 15.5)) + 
  theme(plot.title = element_text(size = 19)) + theme(axis.text = element_text(size = 13))

##### transformation of response variable

st$so <- sqrt(st$ozone) # square root transformed
st$lo <- log(st$ozone) # log-transformed
st$co <- (st$ozone)^(1/3) # cube root transformed

### histogram

qplot(st$ozone, geom="histogram", main = "Histogram for Non-transformed Ozone", 
      fill=I("grey50"), col=I("black"), xlab = "Ozone")
qplot(st$so, geom="histogram", main = "Histogram for Square-root Transformed Ozone", 
      fill=I("grey50"), col=I("black"), xlab = "Square-root Ozone")
qplot(st$co, geom="histogram", main = "Histogram for Cube-root Transformed Ozone", 
      fill=I("grey50"), col=I("black"), xlab = "Cube-root Ozone")
qplot(st$lo, geom="histogram", main = "Histogram for Log Transformed Ozone", 
      fill=I("grey50"), col=I("black"), xlab = "Log Ozone")

### scattering plot

# square root
ggplot(st, aes(x=VOC, y=so)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Square-root Ozone and VOC") + theme(axis.title = element_text(size = 15.5)) + 
  ylab("Square-root Ozone") + theme(plot.title = element_text(size = 19)) + 
  theme(axis.text = element_text(size = 13))

ggplot(st, aes(x=NOx, y=so)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Square-root Ozone and NOx") + theme(axis.title = element_text(size = 15.5)) + 
  ylab("Square-root Ozone") + theme(plot.title = element_text(size = 19)) + 
  theme(axis.text = element_text(size = 13))

# cube root
ggplot(st, aes(x=VOC, y=co)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Cube-root Ozone and VOC") + theme(axis.title = element_text(size = 15.5)) + 
  ylab("Cube-root Ozone") + theme(plot.title = element_text(size = 19)) + 
  theme(axis.text = element_text(size = 13))

ggplot(st, aes(x=NOx, y=co)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Cube-root Ozone and NOx") + theme(axis.title = element_text(size = 15.5)) + 
  ylab("Cube-root Ozone") + theme(plot.title = element_text(size = 19)) + 
  theme(axis.text = element_text(size = 13))

ggplot(st, aes(x=temp, y=co)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Cube-root Ozone and Temperature") + theme(axis.title = element_text(size = 15.5)) + 
  ylab("Cube-root Ozone") + theme(plot.title = element_text(size = 19)) + 
  theme(axis.text = element_text(size = 13))

ggplot(st, aes(x=humid, y=co)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Cube-root Ozone and Relative Humidity") + theme(axis.title = element_text(size = 15.5)) + 
  ylab("Cube-root Ozone") + theme(plot.title = element_text(size = 19)) + 
  theme(axis.text = element_text(size = 13))

ggplot(st, aes(x=prcp, y=co)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Cube-root Ozone and Precipitation") + theme(axis.title = element_text(size = 15.5)) + 
  ylab("Cube-root Ozone") + theme(plot.title = element_text(size = 19)) + 
  theme(axis.text = element_text(size = 13))

ggplot(st, aes(x=solar, y=co)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Cube-root Ozone and Solar Radiation") + theme(axis.title = element_text(size = 15.5)) + 
  ylab("Cube-root Ozone") + theme(plot.title = element_text(size = 19)) + 
  theme(axis.text = element_text(size = 13))

# log
ggplot(st, aes(x=VOC, y=lo)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Log Ozone and VOC") + theme(axis.title = element_text(size = 15.5)) + 
  ylab("Log Ozone") + theme(plot.title = element_text(size = 19)) + 
  theme(axis.text = element_text(size = 13))

ggplot(st, aes(x=NOx, y=lo)) + geom_point(size=3.5, shape=20) + stat_smooth() + 
  ggtitle("Log Ozone and NOx") + theme(axis.title = element_text(size = 15.5)) + 
  ylab("Log Ozone") + theme(plot.title = element_text(size = 19)) + 
  theme(axis.text = element_text(size = 13))

# model
fit1 = lm(ozone~VOC, data=st)
summary(fit1)
plot(fit1)

# square root transformed
fit2 = lm(so~VOC, data=st)
summary(fit2)
plot(fit2)

# log-transformed
fit3 = lm(lo~VOC, data=st)
summary(fit3)
plot(fit3)

#### choose cube-root

################################
#    Univariate association    #
################################

fit1.1 = lm(co~VOC, data=st)
summary(fit1.1)
confint(fit1.1)

fit1.2 = lm(co~NOx, data=st)
summary(fit1.2)
confint(fit1.2)

fit1.3 = lm(co~temp, data=st)
summary(fit1.3)
confint(fit1.3)

fit1.4 = lm(co~humid, data=st)
summary(fit1.4)
confint(fit1.4)

fit1.5 = lm(co~prcp, data=st)
summary(fit1.5)
confint(fit1.5)

fit1.6 = lm(co~solar, data=st)
summary(fit1.6)
confint(fit1.6)

##################################
#    multivariate association    #
##################################

##### interaction

fit0.00 = lm(co~VOC*NOx, data=st)
summary(fit0) 

fit0.01 = lm(co~VOC+NOx, data=st)
anova(fit0.00, fit0.01)# not significant

fit0.0 = lm(co~VOC+NOx+V2+V3+N3+N2+Vl+Ns+Nc+Vs+Vc, data=st)

fit0.1 = lm(co~VOC*NOx+V2*NOx+V3*NOx+Vl*NOx+Vs*NOx+Vc*NOx+
            VOC*N2+V2*N2+V3*N2+Vl*N2+Vs*N2+Vc*N2+
            VOC*N3+V2*N3+V3*N3+Vl*N3+Vs*N3+Vc*N3+
            VOC*Ns+V2*Ns+V3*Ns+Vl*Ns+Vs*Ns+Vc*Ns+
            VOC*Nc+V2*Nc+V3*Nc+Vl*Nc+Vs*Nc+Vc*Nc, data=st)
summary(fit0.1)

anova(fit0.0, fit0.1) # not significant

# interaction not significant

##### only main exposures included

# 1
fit2 = lm(co~VOC+NOx, data=st)
summary(fit2)$adj.r.squared; AIC(fit2); BIC(fit2)

# 2
fit2 = lm(co~VOC+NOx+N2, data=st)
summary(fit2)$adj.r.squared; AIC(fit2); BIC(fit2)

# 3
fit2 = lm(co~VOC+NOx+V2, data=st)
summary(fit2)$adj.r.squared; AIC(fit2); BIC(fit2)

# 4
fit2 = lm(co~VOC+NOx+V2+N2, data=st)
summary(fit2)$adj.r.squared; AIC(fit2); BIC(fit2)

# 5
fit2 = lm(co~VOC+NOx+V3, data=st)
summary(fit2)$adj.r.squared; AIC(fit2); BIC(fit2)

# 6
fit2 = lm(co~VOC+NOx+N3, data=st)
summary(fit2)$adj.r.squared; AIC(fit2); BIC(fit2)

# 7
fit2 = lm(co~VOC+NOx+N2+V2+N3+Vl, data=st)
summary(fit2)$adj.r.squared; AIC(fit2); BIC(fit2)

### All possible model selection
leaps=regsubsets(co~VOC+NOx+V2+V3+N3+N2+Vl+Ns+Nc+Vs+Vc, data=st, nbest=5)
plot(leaps, scale="adjr2")
plot(leaps, scale="bic")

# write.csv(st, "s1.csv")
# (go to SAS....)

### model selected: NOx, NOx^3 and NOx^1/3

fit3 = lm(co~NOx+N3+Nc, data=st)
summary(fit3)
plot(fit3)

###### interaction test

## chunk test

fit4 = lm(co~NOx+N3+Nc
          +NOx:VOC+N3:VOC+Nc:VOC
          +NOx:V2+N3:V2+Nc:V2
          +NOx:V3+N3:V3+Nc:V3
          +NOx:Vs+N3:Vs+Nc:Vs
          +NOx:Vc+N3:Vc+Nc:Vc
          +NOx:Vl+N3:Vl+Nc:Vl, data=st)
summary(fit4)
anova(fit3, fit4) # not significant jointly

## VOC as effect modifier

# 1 interaction
fit4 = lm(co~NOx+N3+Nc+NOx:VOC, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+N3:VOC, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+Nc:VOC, data=st)
summary(fit4); anova(fit3, fit4) # not significant

# 2 interactions
fit4 = lm(co~NOx+N3+Nc+NOx:VOC+N3:VOC, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+NOx:VOC+Nc:VOC, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+N3:VOC+Nc:VOC, data=st)
summary(fit4); anova(fit3, fit4) # not significant

# 3 interactions
fit4 = lm(co~NOx+N3+Nc+NOx:VOC+Nc:VOC+N3:VOC, data=st)
summary(fit4); anova(fit3, fit4) # not significant

## VOC^2 as effect modifier

# 1 interaction
fit4 = lm(co~NOx+N3+Nc+NOx:V2, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+N3:V2, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+Nc:V2, data=st)
summary(fit4); anova(fit3, fit4) # not significant

# 2 interactions
fit4 = lm(co~NOx+N3+Nc+NOx:V2+N3:V2, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+NOx:V2+Nc:V2, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+N3:V2+Nc:V2, data=st)
summary(fit4); anova(fit3, fit4) # not significant

# 3 interactions
fit4 = lm(co~NOx+N3+Nc+NOx:V2+Nc:V2+N3:V2, data=st)
summary(fit4); anova(fit3, fit4) # not significant

## VOC^3 as effect modifier

# 1 interaction
fit4 = lm(co~NOx+N3+Nc+NOx:V3, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+N3:V3, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+Nc:V3, data=st)
summary(fit4); anova(fit3, fit4) # not significant

# 2 interactions
fit4 = lm(co~NOx+N3+Nc+NOx:V3+N3:V3, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+NOx:V3+Nc:V3, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+N3:V3+Nc:V3, data=st)
summary(fit4); anova(fit3, fit4) # not significant

# 3 interactions
fit4 = lm(co~NOx+N3+Nc+NOx:V3+Nc:V3+N3:V3, data=st)
summary(fit4); anova(fit3, fit4) # not significant

## sqrt(VOC) as effect modifier

# 1 interaction
fit4 = lm(co~NOx+N3+Nc+NOx:Vs, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+N3:Vs, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+Nc:Vs, data=st)
summary(fit4); anova(fit3, fit4) # not significant

# 2 interactions
fit4 = lm(co~NOx+N3+Nc+NOx:Vs+N3:Vs, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+NOx:Vs+Nc:Vs, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+N3:Vs+Nc:Vs, data=st)
summary(fit4); anova(fit3, fit4) # not significant

# 3 interactions
fit4 = lm(co~NOx+N3+Nc+NOx:Vs+Nc:Vs+N3:Vs, data=st)
summary(fit4); anova(fit3, fit4) # not significant

## VOC^(1/3) as effect modifier

# 1 interaction
fit4 = lm(co~NOx+N3+Nc+NOx:Vc, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+N3:Vc, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+Nc:Vc, data=st)
summary(fit4); anova(fit3, fit4) # not significant

# 2 interactions
fit4 = lm(co~NOx+N3+Nc+NOx:Vc+N3:Vc, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+NOx:Vc+Nc:Vc, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+N3:Vc+Nc:Vc, data=st)
summary(fit4); anova(fit3, fit4) # not significant

# 3 interactions
fit4 = lm(co~NOx+N3+Nc+NOx:Vc+Nc:Vc+N3:Vc, data=st)
summary(fit4); anova(fit3, fit4) # not significant

## log(VOC) as effect modifier

# 1 interaction
fit4 = lm(co~NOx+N3+Nc+NOx:Vl, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+N3:Vl, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+Nc:Vl, data=st)
summary(fit4); anova(fit3, fit4) # not significant

# 2 interactions
fit4 = lm(co~NOx+N3+Nc+NOx:Vl+N3:Vl, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+NOx:Vl+Nc:Vl, data=st)
summary(fit4); anova(fit3, fit4) # not significant

fit4 = lm(co~NOx+N3+Nc+N3:Vl+Nc:Vl, data=st)
summary(fit4); anova(fit3, fit4) # not significant

# 3 interactions
fit4 = lm(co~NOx+N3+Nc+NOx:Vl+Nc:Vl+N3:Vl, data=st)
summary(fit4); anova(fit3, fit4) # not significant

### other possible models
