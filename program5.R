################################################################################################
# This program is to merge meteorological data into the study dataset and to clean the dataset
# Programmer: Xinyi Zhao
# Date: 11/15/2015
################################################################################################

##############################
#         Merge Data         #
##############################

setwd("H:/AQproject/data/data files/level 2 - subset data/meterological data")

ovn <- read.csv("ovn.csv", stringsAsFactors = FALSE)
temp <- read.csv("temp.csv", stringsAsFactors = FALSE)
hmdt  <- read.csv("hmdt.csv", stringsAsFactors = FALSE)
prcp <- read.csv("prcp.csv", stringsAsFactors = FALSE)
SR <- read.csv("SR_daily.csv", stringsAsFactors = FALSE)

ovn <- ovn[-c(1)]
temp <- temp[-c(1)]
hmdt <- hmdt[-c(1)]
prcp <- prcp[-c(1)]
SR <- SR[-c(1)]

ovn <- ovn[order(ovn$siteID, ovn$date),]
temp <- temp[order(temp$siteID, temp$date),]
hmdt <- hmdt[order(hmdt$siteID, hmdt$date),]
prcp <- prcp[order(prcp$siteID, prcp$date),]
SR <- SR[order(SR$siteID, SR$date),]

x <- merge(ovn, temp, by=c("siteID", "date")) 
y <- merge(x, hmdt, by=c("siteID", "date")) 
z <- merge(y, prcp, by=c("siteID", "date")) 
study <- merge(z, SR, by=c("siteID", "date"))

tapply(x$year, x$siteID, table) 
tapply(y$year, y$siteID, table) 
tapply(z$year, z$siteID, table) 
tapply(study$year, study$siteID, table)

length(unique(study$site)) # 16 sites
length(unique(study$county)) # 14 counties
length(unique(study$state)) # 9 states

write.csv(study, "study.csv")

##############################
#         Clean Data         #
##############################

setwd("H:/AQproject/data/data files/level 3 - combined data")

study <- read.csv("study.csv", stringsAsFactors = FALSE)
study <- study[-c(1)]

### unify the unit for each parameter

table(study$VOC.unit) # Parts per billion vs. Parts per billion carbon (same?)
table(study$NOx.unit) # unified (Parts per billion)
table(study$ozone.unit) # unified (Parts per million)
table(study$temp.unit) # unified (Degrees Fahrenheit)
table(study$hmdt.unit) # unified (Percent relative humidity)

study <- study[-c(12,14,16,17,18,21,22,25,26,28,29,30,32,33,35)]

table(study$NOx.quality) 
table(study$ozone.quality) 
table(study$temp.quality) 
table(study$hmdt.quality)

s1 <- study[study$VOC.unit == 8,]
s2 <- study[study$VOC.unit == 78,]

mean(s1$VOC.value)
mean(s2$VOC.value)

table(s1$siteID) # only 18-89-22 and 25-25-42 have unit ppb
table(s2$siteID) # 18-89-22 only have unit ppb, but no ppbc

tapply(s1$year, s1$siteID, table)
tapply(s2$year, s2$siteID, table)

s11 <- s1[s1$siteID == "25-25-42",]
s22 <- s2[s2$siteID == "25-25-42",]

tapply(s11$date, s11$siteID, table)
tapply(s22$date, s22$siteID, table) # no overlapping dates of different units

mean(s11$VOC.value)
mean(s22$VOC.value)

s222 <- s22[s22$siteID == "25-25-42" & s22$year == 2008,]
mean(s222$VOC.value)

study <- study[-c(12)]
study <- study[-c(8)]

### address multiple daily values in a same day --calculate average for them

s3 <- unique(study)

test <- study
t1 <- unique(test[c(1,2,11)]) # multiple daily values of VOC in a same day
t2 <- unique(test[c(1,2,12)])
t3 <- unique(test[c(1,2,14)]) # multiple daily values of ozone in a same day
t4 <- unique(test[c(1,2,16)])
t5 <- unique(test[c(1,2,18)])
t6 <- unique(test[c(1,2,20)])
t7 <- unique(test[c(1,2,21)])
t0 <- unique(test[c(1,2)])

study1 <- study
study1$VOC = rep(0,dim(study1)[1])
study1$nobs = rep(0,dim(study1)[1])
tmp.id=unique(study$siteID)
for (iid in tmp.id){
  tmp.date=unique(study[which(study$siteID==iid),"date"])
  tmp=tapply(study[which(study$siteID==iid),"VOC.value"],study[which(study$siteID==iid),"date"],mean)
  tmp.len=tapply(study[which(study$siteID==iid),"date"],study[which(study$siteID==iid),"date"],length)
  study1$VOC[which(study$siteID==iid)]=rep(tmp,times=tmp.len)
  study1$nobs[which(study$siteID==iid)]=rep(tmp.len,times=tmp.len)
}

table(study1$nobs)

study2 <- study1
study2$ozone = rep(0,dim(study2)[1])
study2$nobs2 = rep(0,dim(study2)[1])
tmp.id=unique(study$siteID)
for (iid in tmp.id){
  tmp.date=unique(study[which(study$siteID==iid),"date"])
  tmp=tapply(study[which(study$siteID==iid),"ozone.value"],study[which(study$siteID==iid),"date"],mean)
  tmp.len=tapply(study[which(study$siteID==iid),"date"],study[which(study$siteID==iid),"date"],length)
  study2$ozone[which(study$siteID==iid)]=rep(tmp,times=tmp.len)
  study2$nobs2[which(study$siteID==iid)]=rep(tmp.len,times=tmp.len)
}

table(study2$nobs2)

final <- study2[-c(11,14,23,25)]
final <- final[order(final$siteID, final$date),]
final <- unique(final)

### still one duplicated row --find and delete

abc <- final
abc$num = rep(0,dim(abc)[1])
tmp.id=unique(final$siteID)
for (iid in tmp.id){
  tmp.date=unique(final[which(final$siteID==iid),"date"])
  tmp.len=tapply(final[which(final$siteID==iid),"date"],final[which(final$siteID==iid),"date"],length)
  abc$num[which(final$siteID==iid)]=rep(tmp.len,times=tmp.len)
}

table(abc$num)
abc2 <- abc[abc$num == 2,]

final1 <-final[!(final$siteID =="17-31-4201" & final$date == "2001-09-10" & final$ozone.quality == 0),]

tapply(final1$year, final1$siteID, table)

length(unique(final1$site)) # 16 sites
length(unique(final1$county)) # 14 counties
length(unique(final1$state)) # 9 states

final1 <- final1[c(1:3,20,11,12,21,13:19,8:10,4:7)]

names(final1) <- c("siteID", "date", "year", "VOC", "NOx", "NOx.q", "ozone", "ozone.q", 
                   "temp", "temp.q","humid", "humid.q", "prcp", "solar", "latitude", 
                   "longitude", "datum", "state", "county", "city", "site")

final1 <- final1[order(final1$siteID, final1$date),]
final1 <- final1[c(1,2,4:21,3)]
final1 <- na.omit(final1)

write.csv(final1, "final.csv")
