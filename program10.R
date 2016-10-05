setwd("H:/AQproject/Data Collection/Data Files/Level 2 - Subset Data/Complete Subset Data")

nox <- read.csv("NOx_new.csv", stringsAsFactors = FALSE)
ozone <- read.csv("ozone_new.csv", stringsAsFactors = FALSE)
voc <- read.csv("VOC_new.csv", stringsAsFactors = FALSE)
temp <- read.csv("temp_new.csv", stringsAsFactors = FALSE)
hmdt  <- read.csv("hmdt_new.csv", stringsAsFactors = FALSE)
prcp <- read.csv("prcp_new.csv", stringsAsFactors = FALSE)
SR <- read.csv("SR_new.csv", stringsAsFactors = FALSE)

nox <- nox[-c(1)]
ozone <- ozone[-c(1)]
voc <- voc[-c(1)]
temp <- temp[-c(1)]
hmdt <- hmdt[-c(1)]
prcp <- prcp[-c(1)]
SR <- SR[-c(1)]

nox <- nox[order(nox$siteID, nox$date),]
ozone <- ozone[order(ozone$siteID, ozone$date),]
voc <- voc[order(voc$siteID, voc$date),]
temp <- temp[order(temp$siteID, temp$date),]
hmdt <- hmdt[order(hmdt$siteID, hmdt$date),]
prcp <- prcp[order(prcp$siteID, prcp$date),]
SR <- SR[order(SR$siteID, SR$date),]

a <- merge(nox,ozone, by=c("siteID", "date"), all=TRUE) 
b <- merge(a,voc, by=c("siteID", "date"), all=TRUE) 
x <- merge(b,temp, by=c("siteID", "date"), all=TRUE) 
y <- merge(x, hmdt, by=c("siteID", "date"), all=TRUE) 
z <- merge(y, prcp, by=c("siteID", "date"), all=TRUE) 
study <- merge(z, SR, by=c("siteID", "date"), all=TRUE)

study <- study[-c(5:7,9:14,17,18)]
site <- unique(ozone[c(1:3,6:10)])
site <- site[order(site$siteID),]

pro <- merge(study, site, by="siteID", all=TRUE)

test <- pro
t1 <- unique(test[c(1,2)])

pro0 <- pro
pro0$ozone = rep(0,dim(pro0)[1])
pro0$nobs2 = rep(0,dim(pro0)[1])
tmp.id=unique(pro$siteID)
for (iid in tmp.id){
  tmp.date=unique(pro[which(pro$siteID==iid),"date"])
  tmp=tapply(pro[which(pro$siteID==iid),"ozone"],pro[which(pro$siteID==iid),"date"],mean)
  tmp.len=tapply(pro[which(pro$siteID==iid),"date"],pro[which(pro$siteID==iid),"date"],length)
  pro0$ozone[which(pro$siteID==iid)]=rep(tmp,times=tmp.len)
  pro0$nobs2[which(pro$siteID==iid)]=rep(tmp.len,times=tmp.len)
}

table(pro0$nobs2)

pro0 <- unique(pro0)
pro0 <- pro0[-c(22)]

write.csv(pro0, "fulldata.csv")
