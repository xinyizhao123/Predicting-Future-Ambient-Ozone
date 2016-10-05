####################################################################
# This program is to obtain descriptive statistics for the dataset
# Programmer: Xinyi Zhao
# Date: 11/16/2015
####################################################################

#setwd("C:/Users/zhaohexu/Dropbox/Ozone project")
#setwd("C:/Users/Hitomi/Dropbox/Ozone project")

st <- read.csv("study.csv", stringsAsFactors = FALSE)
st <- st[-c(1)]
st$date <- as.Date(as.character(st$date), format("%Y-%m-%d"))
str(st)

#############################
#      frequency table      #
#############################

st$year <- format(st$date, "%Y")
st$month <- format(st$date, "%m")
table(st$year, st$month, st$siteID)

#install.packages("xtable")
library(xtable)

### overall ###
tb0 <- xtable(table(st$year, st$month))

### by site ###
tb1 <- xtable(table(st[which(st$siteID=="6-71-1004"),"year"],st[which(st$siteID=="6-71-1004"),"month"]))

################################
#      summary statistics      #
################################

### overall ###

summary(st$ozone)
mean(st$ozone)
sd(st$ozone)
prop.table(table(st$ozone.q))*100

summary(st$VOC)
mean(st$VOC)
sd(st$VOC)

summary(st$NOx)
mean(st$NOx)
sd(st$NOx)
prop.table(table(st$NOx.q))*100

summary(st$temp)
mean(st$temp)
sd(st$temp)
prop.table(table(st$temp.q))*100

summary(st$humid)
mean(st$humid)
sd(st$humid)
prop.table(table(st$humid.q))*100

summary(st$prcp)
mean(st$prcp)
sd(st$prcp)

summary(st$solar)
mean(st$solar)
sd(st$solar)

### by site ###

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

st0 <- st[c(1,2,3,4,6)]

f.corr <- function(site1, site2){
  dat1 <- st0[st0$siteID == site1, ]
  dat2 <- st0[st0$siteID == site2, ]
  
  colnames(dat1)[3] <- "VOC1"
  colnames(dat2)[3] <- "VOC2"
  colnames(dat1)[4] <- "NOx1"
  colnames(dat2)[4] <- "NOx2"
  colnames(dat1)[5] <- "ozone1"
  colnames(dat2)[5] <- "ozone2"
  
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
f.corr("17-31-4201", "18-89-22")
f.corr("22-33-13", "22-33-9")
f.corr("22-33-13", "22-47-9")
f.corr("22-33-9", "22-47-9")
f.corr("25-25-42", "25-9-2006")
f.corr("44-3-2", "44-7-1010")
f.corr("48-113-69", "48-121-34")
f.corr("48-201-1039", "48-201-24")
f.corr("48-201-1039", "48-201-29")
f.corr("48-201-24", "48-201-29")
f.corr("6-37-1002", "6-37-2")
f.corr("6-37-1002", "6-37-6012")
f.corr("6-37-1002", "6-71-1004")
f.corr("6-37-2", "6-37-6012")
f.corr("6-37-2", "6-71-1004")
f.corr("6-37-6012", "6-71-1004")

################################
#   Subset Data for Modeling   #
################################

# Select only Mar to Oct
st <- subset(st, st$month != "01")
st <- subset(st, st$month != "02")
st <- subset(st, st$month != "11")
st <- subset(st, st$month != "12")
unique(st$month)

# Drop Chattanooga
st <- subset(st, st$siteID != "13-223-3")

write.csv(st, "study1.csv")