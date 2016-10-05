#######################################################
# This program is to perform modeling for the dataset
# Programmer: Xinyi Zhao
# Date: 02/03/2016
#######################################################

#setwd("C:/Users/zhaohexu/Dropbox/Ozone project")
#setwd("C:/Users/Hitomi/Dropbox/Ozone project")
library(lme4)

st <- read.csv("project.csv", stringsAsFactors = FALSE)
st$date <- as.Date(as.character(st$date), format("%Y-%m-%d"))
st <- st[-c(1)]
str(st)
unique(st$siteID)

#st <- st[st$month > 5 & st$month < 9, ]
#unique(st$month)

# transformation of response variable
st$y <- sqrt(st$ozone) # square root transformed

# create higher order terms of predictors
st$N2 <- st$NOx^2
st$V2 <- st$VOC^2
st$N3 <- st$NOx^3
st$V3 <- st$VOC^3
st$Vl <- log(st$VOC+1)

##### basic model

fit1 <- lm(y~NOx+N2+NOx:Vl+factor(siteID), data=st)
summary(fit1) 

##### add meteorology

### all meteorology

# standard model
fit2 <- lm(y~(prcp+solar+temp+humid)*NOx+
             (prcp+solar+temp+humid)*N2+
             (prcp+solar+temp+humid)*(NOx:Vl)+
              factor(siteID), data=st)
summary(fit2)$r.squared
AIC(fit2)
BIC(fit2)

### only solar radiation, temperature and precipitation

# standard model
fit3 <- lm(y~(prcp+solar+temp)*NOx+
             (prcp+solar+temp)*N2+
             (prcp+solar+temp)*(NOx:Vl)+
             factor(siteID), data=st)
summary(fit3)
AIC(fit3)
BIC(fit3)

### only solar radiation and temperature

# standard model
fit4 <- lm(y~(solar+temp)*NOx+
             (solar+temp)*N2+
             (solar+temp)*(NOx:Vl)+
             factor(siteID), data=st)
summary(fit4)
AIC(fit4)
BIC(fit4)


##### try other high order terms

### all meteorology

# add NOx^3
fit2 <- lm(y~(prcp+solar+temp+humid)*NOx+
             (prcp+solar+temp+humid)*N2+
             (prcp+solar+temp+humid)*N3+
             (prcp+solar+temp+humid)*(NOx:Vl)+
             factor(siteID), data=st)
summary(fit2)
AIC(fit2)
BIC(fit2)

# add NOx^2*log(VOC)
fit2 <- lm(y~(prcp+solar+temp+humid)*NOx+
             (prcp+solar+temp+humid)*N2+
             (prcp+solar+temp+humid)*(NOx:Vl)+
             (prcp+solar+temp+humid)*(N2:Vl)+
             factor(siteID), data=st)
summary(fit2)
AIC(fit2)
BIC(fit2)

# add NOx^3 and NOx^3*log(VOC)
fit2 <- lm(y~(prcp+solar+temp+humid)*NOx+
             (prcp+solar+temp+humid)*N2+
             (prcp+solar+temp+humid)*N3+
             (prcp+solar+temp+humid)*(NOx:Vl)+
             (prcp+solar+temp+humid)*(N3:Vl)+
             factor(siteID), data=st)
summary(fit2)
AIC(fit2)
BIC(fit2)

# add NOx^3, NOx^2*log(VOC) and NOx^3*log(VOC)
fit2 <- lm(y~(prcp+solar+temp+humid)*NOx+
             (prcp+solar+temp+humid)*N2+
             (prcp+solar+temp+humid)*N3+
             (prcp+solar+temp+humid)*(NOx:Vl)+
             (prcp+solar+temp+humid)*(N2:Vl)+
             (prcp+solar+temp+humid)*(N3:Vl)+
             factor(siteID), data=st)
summary(fit2)
AIC(fit2)
BIC(fit2)

### only solar radiation, temperature and precipitation

# standard model
fit3 <- lm(y~(prcp+solar+temp)*NOx+
             (prcp+solar+temp)*N2+
             (prcp+solar+temp)*(NOx:Vl)+
             factor(siteID), data=st)
summary(fit3)

### only solar radiation and temperature

# standard model
fit4 <- lm(y~(solar+temp)*NOx+
             (solar+temp)*N2+
             (solar+temp)*(NOx:Vl)+
             factor(siteID), data=st)
summary(fit4)