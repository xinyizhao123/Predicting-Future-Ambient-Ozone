#############################################################################
# This program is to create dataset for add additional monitoring sites 
# Programmer: Xinyi Zhao
# Date: 12/09/2015
#############################################################################

setwd("H:/AQproject/Data Collection/Data Files/Level 1 - Raw Data/Main Parameters")

ozone <- read.csv("ozone_all.csv", stringsAsFactors = FALSE)
voc <- read.csv("voc_all.csv", stringsAsFactors = FALSE)
nox <- read.csv("NOx_all.csv", stringsAsFactors = FALSE)

####### VOC #######

voc1 <- subset(voc, voc$siteid == "13-247-1"|
                 voc$siteid =="48-113-69"|
                 voc$siteid =="48-201-29"|
                 voc$siteid == "6-37-1002"|
                 voc$siteid == "6-37-6012"|
                 voc$siteid == "6-71-1004"|
                 voc$siteid == "48-141-44")

head(voc1)
table(voc1$unit) # unified (ppbc)
unique(voc1$siteid)
tapply(voc1$year, voc1$siteid, table)

voc1 <- voc1[c(13,10,12,14,15)]
names(voc1) <- c("siteID","date","VOC", "year","month")
test <- voc1
t1 <- unique(test[c(1,2)]) # one value for each day

write.csv(voc1, "VOC_add.csv")

####### ozone #######

ozone1 <- subset(ozone, ozone$siteid == "13-247-1"|
                   ozone$siteid =="48-113-69"|
                   ozone$siteid =="48-201-29"|
                   ozone$siteid == "6-37-1002"|
                   ozone$siteid == "6-37-6012"|
                   ozone$siteid == "6-71-1004"|
                   ozone$siteid == "48-141-44")

ozone1 <- subset(ozone1, ozone1$Arithmetic.Mean!="NA")
ozone1 <- subset(ozone1, ozone1$Observation.Count>=23)
ozone1$ozone.q <- as.numeric(ozone1$Observation.Count == 24)

head(ozone1)

tapply(ozone1$year, ozone1$siteid, table)

ozone1 <- ozone1[c(7:9,13,18,24,26,27,28,31:34)]
names(ozone1) <- c("latitude","longitude","datum", "date","ozone","site",
                 "state","county","city","siteID", "year", "month","ozone.q")
test <- ozone1
t1 <- unique(test[c(4,10)]) # one value for each day

write.csv(ozone1, "ozone_add.csv")

####### NOx #######

nox1 <- subset(nox, nox$siteid == "13-247-1"|
                 nox$siteid =="48-113-69"|
                 nox$siteid =="48-201-29"|
                 nox$siteid == "6-37-1002"|
                 nox$siteid == "6-37-6012"|
                 nox$siteid == "6-71-1004"|
                 nox$siteid == "48-141-44")

nox1 <- subset(nox1, nox1$Arithmetic.Mean!="NA")
nox1 <- subset(nox1, nox1$Observation.Count>=23)
nox1$NOx.q <- as.numeric(nox1$Observation.Count == 24)

head(nox1)   

tapply(nox1$year, nox1$siteid, table)

nox1 <- nox1[c(13,18,31,34)]
names(nox1) <- c("date","NOx","siteID", "NOx.q")
test <- nox1
t1 <- unique(test[c(1,3)]) # one value for each day

write.csv(nox1, "NOx_add.csv")


####### merge three main parameters #######

voc1 <- voc1[order(voc1$siteID, voc1$date),]
nox1 <- nox1[order(nox1$siteID, nox1$date),]
ozone1 <- ozone1[order(ozone1$siteID, ozone1$date),]

temp <- merge(voc1, nox1, by=c("siteID", "date"))
ovn_add <- merge(temp, ozone1, by=c("siteID", "date"))

head(ovn_add)

tapply(ovn_add$year.x, ovn_add$siteID, table)

ovn_add <- ovn_add[c(1:3,6:15,18)]

write.csv(ovn_add, "ovn_add.csv")

##########################################################
# list for sites
ovn_add <- read.csv("ovn_add.csv", stringsAsFactors = FALSE)
head(ovn_add)
site <- ovn_add[c(2,7:9,12:14)]
site <- unique(site)
head(site)
write.csv(site, "site_add.csv")

####################################################################
#                     meteorological data                          #
####################################################################

setwd("H:/AQproject/Data Collection/Data Files/Level 1 - Raw Data/Meteorological Data")

####### temperature #######

temp2001 <- read.csv("daily_TEMP_2001.csv", stringsAsFactors = FALSE)
temp2002 <- read.csv("daily_TEMP_2002.csv", stringsAsFactors = FALSE)
temp2003 <- read.csv("daily_TEMP_2003.csv", stringsAsFactors = FALSE)
temp2004 <- read.csv("daily_TEMP_2004.csv", stringsAsFactors = FALSE)
temp2005 <- read.csv("daily_TEMP_2005.csv", stringsAsFactors = FALSE)
temp2006 <- read.csv("daily_TEMP_2006.csv", stringsAsFactors = FALSE)
temp2007 <- read.csv("daily_TEMP_2007.csv", stringsAsFactors = FALSE)
temp2008 <- read.csv("daily_TEMP_2008.csv", stringsAsFactors = FALSE)
temp2009 <- read.csv("daily_TEMP_2009.csv", stringsAsFactors = FALSE)
temp2010 <- read.csv("daily_TEMP_2010.csv", stringsAsFactors = FALSE)

temp <- rbind(temp2001, temp2002, temp2003, temp2004, temp2005, 
              temp2006, temp2007, temp2008, temp2009, temp2010)

head(temp)

temp$siteID <- paste(temp$State.Code, temp$County.Code, temp$Site.Num, sep = '-')

temp1 <- subset(temp, temp$siteID == "13-247-1"|
                  temp$siteID =="48-113-69"|
                  temp$siteID =="48-201-29"|
                  temp$siteID == "6-37-1002"|
                  temp$siteID == "6-37-6012"|
                  temp$siteID == "6-71-1004"|
                  temp$siteID == "48-141-44")

temp1 <- subset(temp1, temp1$Arithmetic.Mean!="NA")
temp1 <- subset(temp1, temp1$Observation.Count>=23)
temp1$temp.q <- as.numeric(temp1$Observation.Count == 24)

head(temp1)   

temp1$Date.Local <- as.Date(as.character(temp1$Date.Local), format("%Y-%m-%d"))
temp1$year <- format(temp1$Date.Local, "%Y")

tapply(temp1$year, temp1$siteID, table)
unique(temp1$siteID)

temp1 <- temp1[c(12,17,30,31)]
names(temp1) <- c("date","temp","siteID", "temp.q")

test <- temp1
t1 <- unique(test[c(1,3)]) # one value for each day

write.csv(temp1, "temp_add.csv")

####### humidity #######

hmdt2001 <- read.csv("daily_RH_DP_2001.csv", stringsAsFactors = FALSE)
hmdt2002 <- read.csv("daily_RH_DP_2002.csv", stringsAsFactors = FALSE)
hmdt2003 <- read.csv("daily_RH_DP_2003.csv", stringsAsFactors = FALSE)
hmdt2004 <- read.csv("daily_RH_DP_2004.csv", stringsAsFactors = FALSE)
hmdt2005 <- read.csv("daily_RH_DP_2005.csv", stringsAsFactors = FALSE)
hmdt2006 <- read.csv("daily_RH_DP_2006.csv", stringsAsFactors = FALSE)
hmdt2007 <- read.csv("daily_RH_DP_2007.csv", stringsAsFactors = FALSE)
hmdt2008 <- read.csv("daily_RH_DP_2008.csv", stringsAsFactors = FALSE)
hmdt2009 <- read.csv("daily_RH_DP_2009.csv", stringsAsFactors = FALSE)
hmdt2010 <- read.csv("daily_RH_DP_2010.csv", stringsAsFactors = FALSE)

hmdt <- rbind(hmdt2001, hmdt2002, hmdt2003, hmdt2004, hmdt2005, 
              hmdt2006, hmdt2007, hmdt2008, hmdt2009, hmdt2010)

head(hmdt)

hmdt$siteID <- paste(hmdt$State.Code, hmdt$County.Code, hmdt$Site.Num, sep = '-')

hmdt <- hmdt[hmdt$Parameter.Code == 62201 & hmdt$Sample.Duration == "1 HOUR",]


hmdt1 <- subset(hmdt, hmdt$siteID == "13-247-1"|
                  hmdt$siteID =="48-121-34"|
                  hmdt$siteID =="48-201-24"|
                  hmdt$siteID == "6-37-1002"|
                  hmdt$siteID == "6-37-2"|
                  hmdt$siteID == "6-71-1004"|
                  hmdt$siteID == "48-141-44")

hmdt1 <- subset(hmdt1, hmdt1$Arithmetic.Mean!="NA")
hmdt1 <- subset(hmdt1, hmdt1$Observation.Count>=23)
hmdt1$hmdt.q <- as.numeric(hmdt1$Observation.Count == 24)

head(hmdt1)   

hmdt1$Date.Local <- as.Date(as.character(hmdt1$Date.Local), format("%Y-%m-%d"))
hmdt1$year <- format(hmdt1$Date.Local, "%Y")

tapply(hmdt1$year, hmdt1$siteID, table)
unique(hmdt1$siteID)

hmdt1$siteID[hmdt1$siteID == "48-121-34"] <- "48-113-69"
hmdt1$siteID[hmdt1$siteID == "48-201-24"] <- "48-201-29"
hmdt1$siteID[hmdt1$siteID == "6-37-2"] <- "6-37-6012"

hmdt1 <- hmdt1[c(12,17,30,31)]
names(hmdt1) <- c("date","hmdt","siteID", "hmdt.q")

test <- hmdt1
t1 <- unique(test[c(1,3)]) # one value for each day

write.csv(hmdt1, "hmdt_add.csv")

############################################
# merge

ovn_add <- read.csv("ovn_add.csv", stringsAsFactors = FALSE)
temp1 <- read.csv("temp_add.csv", stringsAsFactors = FALSE)
hmdt1 <- read.csv("hmdt_add.csv", stringsAsFactors = FALSE)

ovn_add <- ovn_add[order(ovn_add$siteID, ovn_add$date),]
temp1 <- temp1[order(temp1$siteID, temp1$date),]
hmdt1 <- hmdt1[order(hmdt1$siteID, hmdt1$date),]

z <- merge(ovn_add, temp1, by=c("siteID", "date"))
ovnth_add <- merge(z, hmdt1, by=c("siteID", "date"))

head(ovnth_add)

ovnth_add <- ovnth_add[c(1,2,4:6,10,15,17,18,20,21,7:9,12:14,11)]
colnames(ovnth_add)[10] <- "humid"
colnames(ovnth_add)[11] <- "humid.q"

write.csv(ovnth_add, "ovnth_add.csv")

ovnth_add$date <- as.Date(as.character(ovnth_add$date), format("%Y-%m-%d"))
ovnth_add$year <- format(ovnth_add$date, "%Y")
tapply(ovnth_add$year, ovnth_add$siteID, table)

####### precipitation #######

prcp <- read.csv("prcp_add.csv", stringsAsFactors = FALSE)

table(prcp$STATION_NAME)
head(prcp)

prcp <- prcp[c(2,6,7)]
prcp <- prcp[c(1,2,3,1)]

colnames(prcp)[4] <- "siteID"
colnames(prcp)[2] <- "date"

prcp <- subset(prcp, prcp$PRCP!="NA")
prcp <- subset(prcp, prcp$PRCP!=-9999)
prcp$date <- as.Date(as.character(prcp$date), format("%Y%m%d"))

prcp$siteID[prcp$siteID == "ATLANTA HARTSFIELD INTERNATIONAL AIRPORT GA US"] <- "13-247-1"
prcp$siteID[prcp$siteID == "CYPRESS TX US"] <- "48-201-29"
prcp$siteID[prcp$siteID == "EL PASO INTERNATIONAL AIRPORT TX US"] <- "48-141-44"
prcp$siteID[prcp$siteID == "SAN GABRIEL CANYON PH CA US"] <- "6-71-1004"
prcp$siteID[prcp$siteID == "BURBANK GLENDALE PASADENA AIRPORT CA US"] <- "6-37-1002"
prcp$siteID[prcp$siteID == "DALLAS FAA AIRPORT TX US"] <- "48-113-69"
prcp$siteID[prcp$siteID == "NORTHRIDGE CAL STATE CA US"] <- "6-37-6012"

table(prcp$siteID,prcp$STATION_NAME)

colnames(prcp)[3] <- "prcp"
prcp <- prcp[-c(1)]

test <- prcp
t1 <- unique(test[c(1,3)]) # one value for each day

prcp$date <- as.Date(as.character(prcp$date), format("%Y-%m-%d"))
prcp$year <- format(prcp$date, "%Y")

tapply(prcp$year, prcp$siteID, table)

write.csv(prcp, "precip_add.csv")

####### solar radiation #######

# Atlanta 

GA2001 <- read.csv("722190_2001_solar.csv", stringsAsFactors = FALSE)
GA2002 <- read.csv("722190_2002_solar.csv", stringsAsFactors = FALSE)
GA2003 <- read.csv("722190_2003_solar.csv", stringsAsFactors = FALSE)
GA2004 <- read.csv("722190_2004_solar.csv", stringsAsFactors = FALSE)
GA2005 <- read.csv("722190_2005_solar.csv", stringsAsFactors = FALSE)
GA2006 <- read.csv("722190_2006_solar.csv", stringsAsFactors = FALSE)
GA2007 <- read.csv("722190_2007_solar.csv", stringsAsFactors = FALSE)
GA2008 <- read.csv("722190_2008_solar.csv", stringsAsFactors = FALSE)
GA2009 <- read.csv("722190_2009_solar.csv", stringsAsFactors = FALSE)
GA2010 <- read.csv("722190_2010_solar.csv", stringsAsFactors = FALSE)

GA1 <- rbind(GA2001, GA2002, GA2003, GA2004, GA2005, GA2006, GA2007, GA2008, GA2009, GA2010)

head(GA1)

GA1 <- GA1[c(1,2,16)]
names(GA1) <- c("date","time","solar")
GA1 <- subset(GA1, GA1$solar!="NA")
GA1 <- subset(GA1, GA1$solar!=-9900)

GA1 <- data.frame(GA1, siteID=rep("13-247-1", nrow(GA1)))

# Dallas

DEN2001 <- read.csv("722590_2001_solar.csv", stringsAsFactors = FALSE)
DEN2002 <- read.csv("722590_2002_solar.csv", stringsAsFactors = FALSE)
DEN2003 <- read.csv("722590_2003_solar.csv", stringsAsFactors = FALSE)
DEN2004 <- read.csv("722590_2004_solar.csv", stringsAsFactors = FALSE)
DEN2005 <- read.csv("722590_2005_solar.csv", stringsAsFactors = FALSE)
DEN2006 <- read.csv("722590_2006_solar.csv", stringsAsFactors = FALSE)
DEN2007 <- read.csv("722590_2007_solar.csv", stringsAsFactors = FALSE)
DEN2008 <- read.csv("722590_2008_solar.csv", stringsAsFactors = FALSE)
DEN2009 <- read.csv("722590_2009_solar.csv", stringsAsFactors = FALSE)
DEN2010 <- read.csv("722590_2010_solar.csv", stringsAsFactors = FALSE)

DEN1 <- rbind(DEN2001, DEN2002, DEN2003, DEN2004, DEN2005, DEN2006, DEN2007, DEN2008, DEN2009, DEN2010)

head(DEN1)
DEN1 <- DEN1[c(1,2,16)]
names(DEN1) <- c("date","time","solar")
DEN1 <- subset(DEN1, DEN1$solar!="NA")
DEN1 <- subset(DEN1, DEN1$solar!=-9900)

DEN1 <- data.frame(DEN1, siteID=rep("48-113-69", nrow(DEN1)))

# Houston

HOU2001 <- read.csv("722430_2001_solar.csv", stringsAsFactors = FALSE)
HOU2002 <- read.csv("722430_2002_solar.csv", stringsAsFactors = FALSE)
HOU2003 <- read.csv("722430_2003_solar.csv", stringsAsFactors = FALSE)
HOU2004 <- read.csv("722430_2004_solar.csv", stringsAsFactors = FALSE)
HOU2005 <- read.csv("722430_2005_solar.csv", stringsAsFactors = FALSE)
HOU2006 <- read.csv("722430_2006_solar.csv", stringsAsFactors = FALSE)
HOU2007 <- read.csv("722430_2007_solar.csv", stringsAsFactors = FALSE)
HOU2008 <- read.csv("722430_2008_solar.csv", stringsAsFactors = FALSE)
HOU2009 <- read.csv("722430_2009_solar.csv", stringsAsFactors = FALSE)
HOU2010 <- read.csv("722430_2010_solar.csv", stringsAsFactors = FALSE)

HOU1 <- rbind(HOU2001, HOU2002, HOU2003, HOU2004, HOU2005, HOU2006, HOU2007, HOU2008, HOU2009, HOU2010)

HOU1 <- HOU1[c(1,2,16)]
names(HOU1) <- c("date","time","solar")
HOU1 <- subset(HOU1, HOU1$solar!="NA")
HOU1 <- subset(HOU1, HOU1$solar!=-9900)

HOU1 <- data.frame(HOU1, siteID=rep("48-201-29", nrow(HOU1)))
head(HOU1)

# Los Angeles 1

CA2001 <- read.csv("722970_2001_solar.csv", stringsAsFactors = FALSE)
CA2002 <- read.csv("722970_2002_solar.csv", stringsAsFactors = FALSE)
CA2003 <- read.csv("722970_2003_solar.csv", stringsAsFactors = FALSE)
CA2004 <- read.csv("722970_2004_solar.csv", stringsAsFactors = FALSE)
CA2005 <- read.csv("722970_2005_solar.csv", stringsAsFactors = FALSE)
CA2006 <- read.csv("722970_2006_solar.csv", stringsAsFactors = FALSE)
CA2007 <- read.csv("722970_2007_solar.csv", stringsAsFactors = FALSE)
CA2008 <- read.csv("722970_2008_solar.csv", stringsAsFactors = FALSE)
CA2009 <- read.csv("722970_2009_solar.csv", stringsAsFactors = FALSE)
CA2010 <- read.csv("722970_2010_solar.csv", stringsAsFactors = FALSE)

CA1 <- rbind(CA2001, CA2002, CA2003, CA2004, CA2005, CA2006, CA2007, CA2008, CA2009, CA2010)

CA1 <- CA1[c(1,2,16)]
names(CA1) <- c("date","time","solar")
CA1 <- subset(CA1, CA1$solar!="NA")
CA1 <- subset(CA1, CA1$solar!=-9900)

CA1 <- data.frame(CA1, siteID=rep("6-71-1004", nrow(CA1)))
head(CA1)

# Los Angeles 2

CA_2001 <- read.csv("722950_2001_solar.csv", stringsAsFactors = FALSE)
CA_2002 <- read.csv("722950_2002_solar.csv", stringsAsFactors = FALSE)
CA_2003 <- read.csv("722950_2003_solar.csv", stringsAsFactors = FALSE)
CA_2004 <- read.csv("722950_2004_solar.csv", stringsAsFactors = FALSE)
CA_2005 <- read.csv("722950_2005_solar.csv", stringsAsFactors = FALSE)
CA_2006 <- read.csv("722950_2006_solar.csv", stringsAsFactors = FALSE)
CA_2007 <- read.csv("722950_2007_solar.csv", stringsAsFactors = FALSE)
CA_2008 <- read.csv("722950_2008_solar.csv", stringsAsFactors = FALSE)
CA_2009 <- read.csv("722950_2009_solar.csv", stringsAsFactors = FALSE)
CA_2010 <- read.csv("722950_2010_solar.csv", stringsAsFactors = FALSE)

CA_1 <- rbind(CA_2001, CA_2002, CA_2003, CA_2004, CA_2005, CA_2006, CA_2007, CA_2008, CA_2009, CA_2010)

CA_1 <- CA_1[c(1,2,16)]
names(CA_1) <- c("date","time","solar")
CA_1 <- subset(CA_1, CA_1$solar!="NA")
CA_1 <- subset(CA_1, CA_1$solar!=-9900)

CA_2 <- CA_1
CA_1 <- data.frame(CA_1, siteID=rep("6-37-1002", nrow(CA_1)))
CA_2 <- data.frame(CA_2, siteID=rep("6-37-6012", nrow(CA_2)))
head(CA_1)
head(CA_2)

# El Paso

EL2001 <- read.csv("722700_2001_solar.csv", stringsAsFactors = FALSE)
EL2002 <- read.csv("722700_2002_solar.csv", stringsAsFactors = FALSE)
EL2003 <- read.csv("722700_2003_solar.csv", stringsAsFactors = FALSE)
EL2004 <- read.csv("722700_2004_solar.csv", stringsAsFactors = FALSE)
EL2005 <- read.csv("722700_2005_solar.csv", stringsAsFactors = FALSE)
EL2006 <- read.csv("722700_2006_solar.csv", stringsAsFactors = FALSE)
EL2007 <- read.csv("722700_2007_solar.csv", stringsAsFactors = FALSE)
EL2008 <- read.csv("722700_2008_solar.csv", stringsAsFactors = FALSE)
EL2009 <- read.csv("722700_2009_solar.csv", stringsAsFactors = FALSE)
EL2010 <- read.csv("722700_2010_solar.csv", stringsAsFactors = FALSE)

EL1 <- rbind(EL2001, EL2002, EL2003, EL2004, EL2005, EL2006, EL2007, EL2008, EL2009, EL2010)

EL1 <- EL1[c(1,2,16)]
names(EL1) <- c("date","time","solar")
EL1 <- subset(EL1, EL1$solar!="NA")
EL1 <- subset(EL1, EL1$solar!=-9900)

EL1 <- data.frame(EL1, siteID=rep("48-141-44", nrow(EL1)))
head(EL1)

SR <- rbind(GA1,DEN1,HOU1,CA1,CA_1,CA_2,EL1)
SR$date <- as.Date(as.character(SR$date), format("%Y-%m-%d"))
class(SR$date)

SR <- SR[order(SR$siteID, SR$date),]

#### calculate daily total SR

SR1 <- SR
SR1$tsolar = rep(0,dim(SR1)[1])
SR1$nobs = rep(0,dim(SR1)[1])
tmp.id=unique(SR$siteID)
for (iid in tmp.id){
  tmp.date=unique(SR[which(SR$siteID==iid),"date"])
  tmp=tapply(SR[which(SR$siteID==iid),"solar"],SR[which(SR$siteID==iid),"date"],sum)
  tmp.len=tapply(SR[which(SR$siteID==iid),"date"],SR[which(SR$siteID==iid),"date"],length)
  SR1$tsolar[which(SR$siteID==iid)]=rep(tmp,times=tmp.len)
  SR1$nobs[which(SR$siteID==iid)]=rep(tmp.len,times=tmp.len)
}

table(SR1$nobs)

SR1 <- subset(SR1, SR1$time=="1:00")
SR1 <- SR1[-c(2,3,6)]
SR1 <- SR1[c(2,1,3)]

write.csv(SR1, "SR_daily_add.csv")

SR1$date <- as.Date(as.character(SR1$date), format("%Y-%m-%d"))
SR1$year <- format(SR1$date, "%Y")

tapply(SR1$year, SR1$siteID, table)

colnames(SR1)[3] <- "solar"

############################################
# merge

head(ovnth_add)
head(prcp)
head(SR1)

prcp <- prcp[-c(4)]
SR1 <- SR1[-c(4)]

ovnth_add <- ovnth_add[order(ovnth_add$siteID, ovnth_add$date),]
prcp <- prcp[order(prcp$siteID, prcp$date),]
SR1 <- SR1[order(SR1$siteID, SR1$date),]

xy <- merge(ovnth_add, prcp, by=c("siteID", "date"))
study_add <- merge(xy, SR1, by=c("siteID", "date"))

head(study_add)

tapply(study_add$year, study_add$siteID, table)

study_add <- study_add[-c(19)]
study_add <- study_add[c(1:11,19,20,12:18)]

write.csv(study_add, "study_add.csv")

test <- study_add
t1 <- unique(test[c(1,2)]) # one value for each day

study_add <- na.omit(study_add)

############################################
# combine to the main study dataset

final <- read.csv("final.csv", stringsAsFactors = FALSE)
head(final)
final <- final[-c(1,22)]
final$date <- as.Date(as.character(final$date), format("%Y-%m-%d"))
final2 <- rbind(final,study_add)

fi <- final2
  
fi$date <- as.Date(as.character(fi$date), format("%Y-%m-%d"))
fi$year <- format(fi$date, "%Y")

tapply(fi$year, fi$siteID, table)

final2 <- final2[order(final2$siteID, final2$date),]
head(final2)
tail(final2)

unique(final2$state)
length(unique(final2$siteID))
length(unique(final2$county))

write.csv(final2, "final2.csv")
