#######################################################
# This program is to perform modeling for the dataset
# Programmer: Xinyi Zhao
# Date: 02/07/2016
#######################################################

#setwd("C:/Users/zhaohexu/Dropbox/Ozone project")
#setwd("C:/Users/Hitomi/Dropbox/Ozone project")

st <- read.csv("C:/Users/zhaohexu/Dropbox/Ozone project/project.csv", stringsAsFactors = FALSE)
st <- st[-c(1)]
unique(st$siteID)

#st <- st[st$month > 5 & st$month < 9, ]
unique(st$month)

st$y <- sqrt(st$ozone)
st$N2 <- st$NOx^2
st$V2 <- st$VOC^2
st$N3 <- st$NOx^3
st$V3 <- st$VOC^3
st$Vl <- log(st$VOC+1)

# use a function to get modeling result for each monitor
st0 <- st

f.model <- function(site){
  dat <- st0[st0$siteID == site, ]
  N <- length(dat$date)
  
  fit1 <- lm(y~(prcp+solar+temp.max+humid)*NOx+
               (prcp+solar+temp.max+humid)*N2+
               (prcp+solar+temp.max+humid)*(NOx:Vl), data=dat)
  
  R1 <- summary(fit1)$r.squared
  AIC1 <- AIC(fit1)
  BIC1 <- BIC(fit1)
  
  fit2 <- lm(y~(prcp+solar+temp.max)*NOx+
               (prcp+solar+temp.max)*N2+
               (prcp+solar+temp.max)*(NOx:Vl), data=dat)
  
  R2 <- summary(fit2)$r.squared
  AIC2 <- AIC(fit2)
  BIC2 <- BIC(fit2)
  
  fit3 <- lm(y~(solar+temp.max)*NOx+
               (solar+temp.max)*N2+
               (solar+temp.max)*(NOx:Vl), data=dat)
  
  R3 <- summary(fit3)$r.squared
  AIC3 <- AIC(fit3)
  BIC3 <- BIC(fit3)
  
  return(c(N, R1, AIC1, BIC1, R2, AIC2, BIC2, R3, AIC3, BIC3))
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
f.model("44-3-2")
f.model("44-7-1010")
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


st0 <- st

f.model <- function(site){
  dat <- st0[st0$siteID == site, ]
  N <- length(dat$date)
  
  fit1 <- lm(y~(prcp+solar+temp.max+humid)*NOx+
               (prcp+solar+temp.max+humid)*N2+
               (prcp+solar+temp.max+humid)*(NOx:Vl), data=dat)
  R1 <- summary(fit1)$r.squared
  
  return(c(N, R1))
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

tapply(st$year,st$siteID,table)


#### average temp vs. max temp

st0 <- st

f.model <- function(site){
  dat <- st0[st0$siteID == site, ]
  
  fit1.1 <- lm(y~(prcp+solar+temp.avg+humid)*NOx+
                 (prcp+solar+temp.avg+humid)*N2+
                 (prcp+solar+temp.avg+humid)*(NOx:Vl), data=dat)
  R1 <- summary(fit1.1)$r.squared
  
  fit1.2 <- lm(y~(prcp+solar+temp.max+humid)*NOx+
                 (prcp+solar+temp.max+humid)*N2+
                 (prcp+solar+temp.max+humid)*(NOx:Vl), data=dat)
  R2 <- summary(fit1.2)$r.squared
  
  return(c(R1, R2))
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