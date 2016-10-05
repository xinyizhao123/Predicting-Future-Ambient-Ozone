
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
                  temp$siteID == "13-89-2"|
                  temp$siteID == "18-89-22"|
                  temp$siteID == "24-5-3001"|
                  temp$siteID =="48-113-69"|
                  temp$siteID =="48-121-34"|
                  temp$siteID == "6-37-1002"|
                  temp$siteID == "6-37-6012"|
                  temp$siteID == "6-71-1004"|
                  temp$siteID == "6-37-2")

temp1 <- subset(temp1, temp1$Arithmetic.Mean!="NA")
temp1 <- subset(temp1, temp1$Observation.Count>=23)
temp1$temp.q <- as.numeric(temp1$Observation.Count == 24)

head(temp1)   

temp1$Date.Local <- as.Date(as.character(temp1$Date.Local), format("%Y-%m-%d"))
temp1$year <- format(temp1$Date.Local, "%Y")

tapply(temp1$year, temp1$siteID, table)
unique(temp1$siteID)

temp1 <- temp1[c(12,17,18,30,31)]
names(temp1) <- c("date","temp.avg","temp.max","siteID", "temp.q")

test <- temp1
t1 <- unique(test[c(1,4)]) 
# single value for each day

write.csv(temp1, "temp_new.csv")

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


hmdt1.1 <- subset(hmdt, hmdt$siteID == "13-247-1"|
                    hmdt$siteID == "13-89-2"|
                    hmdt$siteID == "18-89-22"|
                    hmdt$siteID == "24-5-3001"|
                    hmdt$siteID =="48-121-34"|
                    hmdt$siteID == "6-37-1002"|
                    hmdt$siteID == "6-71-1004"|
                    hmdt$siteID == "6-37-2")


hmdt1.1 <- subset(hmdt1.1, hmdt1.1$Arithmetic.Mean!="NA")
hmdt1.1 <- subset(hmdt1.1, hmdt1.1$Observation.Count>=23)
hmdt1.1$hmdt.q <- as.numeric(hmdt1.1$Observation.Count == 24)

head(hmdt1.1)   

hmdt1.1$Date.Local <- as.Date(as.character(hmdt1.1$Date.Local), format("%Y-%m-%d"))
hmdt1.1$year <- format(hmdt1.1$Date.Local, "%Y")

tapply(hmdt1.1$year, hmdt1.1$siteID, table)
unique(hmdt1.1$siteID)


hmdt1.2 <- subset(hmdt, hmdt$siteID == "48-121-34"| hmdt$siteID == "6-37-2")


hmdt1.2 <- subset(hmdt1.2, hmdt1.2$Arithmetic.Mean!="NA")
hmdt1.2 <- subset(hmdt1.2, hmdt1.2$Observation.Count>=23)
hmdt1.2$hmdt.q <- as.numeric(hmdt1.2$Observation.Count == 24)

head(hmdt1.2)   

hmdt1.2$Date.Local <- as.Date(as.character(hmdt1.2$Date.Local), format("%Y-%m-%d"))
hmdt1.2$year <- format(hmdt1.2$Date.Local, "%Y")

tapply(hmdt1.2$year, hmdt1.2$siteID, table)
unique(hmdt1.2$siteID)

hmdt1.2$siteID[hmdt1.2$siteID == "48-121-34"] <- "48-113-69"
hmdt1.2$siteID[hmdt1.2$siteID == "6-37-2"] <- "6-37-6012"

hmdt1 <- rbind(hmdt1.1, hmdt1.2)

tapply(hmdt1$year, hmdt1$siteID, table)
unique(hmdt1$siteID)

hmdt1 <- hmdt1[c(12,17,30,31)]
names(hmdt1) <- c("date","hmdt","siteID", "hmdt.q")
head(hmdt1)

test <- hmdt1
t1 <- unique(test[c(1,3)]) 
# one value for each day

write.csv(hmdt1, "hmdt_new.csv")



setwd("H:/AQproject/Data Collection/Data Files/Level 2 - Subset Data/Meteorological Data")

prcp1 <- read.csv("prcp.csv", stringsAsFactors = FALSE)
prcp2 <- read.csv("precip_add.csv", stringsAsFactors = FALSE)

colnames(prcp1)[4] <- "prcp"
prcp1 <- prcp1[c(2:4)]
prcp2 <- prcp2[c(4,2,3)]

prcp1.1 <- subset(prcp1, prcp1$siteID == "13-247-1"|
                  prcp1$siteID == "13-89-2"|
                  prcp1$siteID == "18-89-22"|
                  prcp1$siteID == "24-5-3001"|
                  prcp1$siteID =="48-113-69"|
                  prcp1$siteID =="48-121-34"|
                  prcp1$siteID == "6-37-1002"|
                  prcp1$siteID == "6-37-6012"|
                  prcp1$siteID == "6-71-1004"|
                  prcp1$siteID == "6-37-2")

prcp2.1 <- subset(prcp2, prcp2$siteID == "13-247-1"|
                    prcp2$siteID == "13-89-2"|
                    prcp2$siteID == "18-89-22"|
                    prcp2$siteID == "24-5-3001"|
                    prcp2$siteID =="48-113-69"|
                    prcp2$siteID =="48-121-34"|
                    prcp2$siteID == "6-37-1002"|
                    prcp2$siteID == "6-37-6012"|
                    prcp2$siteID == "6-71-1004"|
                    prcp2$siteID == "6-37-2")

prcp <- rbind(prcp1.1,prcp2.1)
unique(prcp$siteID)

test <- prcp
t1 <- unique(test[c(1,2)]) # one value for each day
write.csv(prcp,"prcp_new.csv")

SR1 <- read.csv("SR_daily.csv", stringsAsFactors = FALSE)
SR2 <- read.csv("SR_daily_add.csv", stringsAsFactors = FALSE)

SR1.1 <- subset(SR1, SR1$siteID == "13-247-1"|
                  SR1$siteID == "13-89-2"|
                  SR1$siteID == "18-89-22"|
                  SR1$siteID == "24-5-3001"|
                  SR1$siteID =="48-113-69"|
                  SR1$siteID =="48-121-34"|
                  SR1$siteID == "6-37-1002"|
                  SR1$siteID == "6-37-6012"|
                  SR1$siteID == "6-71-1004"|
                  SR1$siteID == "6-37-2")

SR2.1 <- subset(SR2, SR2$siteID == "13-247-1"|
                  SR2$siteID == "13-89-2"|
                  SR2$siteID == "18-89-22"|
                  SR2$siteID == "24-5-3001"|
                  SR2$siteID =="48-113-69"|
                  SR2$siteID =="48-121-34"|
                  SR2$siteID == "6-37-1002"|
                  SR2$siteID == "6-37-6012"|
                  SR2$siteID == "6-71-1004"|
                  SR2$siteID == "6-37-2")

colnames(SR1.1)[4] <- "solar"
SR <- rbind(SR1.1,SR2.1)
unique(SR$siteID)
SR <- SR[c(-1)]

test <- SR
t1 <- unique(test[c(1,2)]) # one value for each day
write.csv(SR,"SR_new.csv")




setwd("H:/AQproject/Data Collection/Data Files/Level 2 - Subset Data/Main Parameters")

ozone <- read.csv("ozone_all.csv", stringsAsFactors = FALSE)
voc <- read.csv("voc_all.csv", stringsAsFactors = FALSE)
nox <- read.csv("NOx_all.csv", stringsAsFactors = FALSE)

####### ozone #######

colnames(ozone)[31] <- "siteID"
ozone1 <- subset(ozone, ozone$siteID == "13-247-1"|
                   ozone$siteID == "13-89-2"|
                   ozone$siteID == "18-89-22"|
                   ozone$siteID == "24-5-3001"|
                   ozone$siteID =="48-113-69"|
                   ozone$siteID =="48-121-34"|
                   ozone$siteID == "6-37-1002"|
                   ozone$siteID == "6-37-6012"|
                   ozone$siteID == "6-71-1004"|
                   ozone$siteID == "6-37-2")


ozone1 <- subset(ozone1, ozone1$Arithmetic.Mean!="NA")
ozone1 <- subset(ozone1, ozone1$Observation.Count>=23)
ozone1$ozone.q <- as.numeric(ozone1$Observation.Count == 24)

head(ozone1)

tapply(ozone1$year, ozone1$siteID, table)

ozone1 <- ozone1[c(7:9,13,19,24,26,27,28,31:34)]
names(ozone1) <- c("latitude","longitude","datum", "date","ozone","site",
                   "state","county","city","siteID", "year", "month","ozone.q")
test <- ozone1
t1 <- unique(test[c(4,10)]) 
# duplicated values

write.csv(ozone1, "ozone_new.csv")

####### NOx #######

colnames(nox)[31] <- "siteID"
nox1 <- subset(nox, nox$siteID == "13-247-1"|
                 nox$siteID == "13-89-2"|
                 nox$siteID == "18-89-22"|
                 nox$siteID == "24-5-3001"|
                 nox$siteID =="48-113-69"|
                 nox$siteID =="48-121-34"|
                 nox$siteID == "6-37-1002"|
                 nox$siteID == "6-37-6012"|
                 nox$siteID == "6-71-1004"|
                 nox$siteID == "6-37-2")

nox1 <- subset(nox1, nox1$Arithmetic.Mean!="NA")
nox1 <- subset(nox1, nox1$Observation.Count>=23)
nox1$NOx.q <- as.numeric(nox1$Observation.Count == 24)

head(nox1)   

tapply(nox1$year, nox1$siteID, table)

nox1 <- nox1[c(13,18,31,34)]
names(nox1) <- c("date","NOx","siteID", "NOx.q")
test <- nox1
t1 <- unique(test[c(1,3)]) # one value for each day

write.csv(nox1, "NOx_new.csv")

####### VOC #######

colnames(voc)[13] <- "siteID"
voc1 <- subset(voc, voc$siteID == "13-247-1"|
                 voc$siteID == "13-89-2"|
                 voc$siteID == "18-89-22"|
                 voc$siteID == "24-5-3001"|
                 voc$siteID =="48-113-69"|
                 voc$siteID =="48-121-34"|
                 voc$siteID == "6-37-1002"|
                 voc$siteID == "6-37-6012"|
                 voc$siteID == "6-71-1004"|
                 voc$siteID == "6-37-2")

head(voc1)
table(voc1$unit) # unified (ppbc)
unique(voc1$siteID)
tapply(voc1$year, voc1$siteID, table)

voc1 <- voc1[c(13,10,12,14,15)]
names(voc1) <- c("siteID","date","VOC", "year","month")
test <- voc1
t1 <- unique(test[c(1,2)]) # one value for each day

write.csv(voc1, "VOC_new.csv")



setwd("H:/AQproject/Data Collection/Data Files/Hourly CMAQ Simulations-selected")

NO <- read.csv("NOCMAQ_PAMS_CONUS_36km.csv", stringsAsFactors = FALSE)
NO2 <- read.csv("NO2CMAQ_PAMS_CONUS_36km.csv", stringsAsFactors = FALSE)
VOCs <- read.csv("VOCCMAQ_PAMS_CONUS_36km.csv", stringsAsFactors = FALSE)

NO <- NO[c(1:4,6,10,15,16,21:24)]
names(NO) <- c("date","hour","13-247-1","13-89-2","18-89-22","24-5-3001","48-113-69","48-121-34","6-37-1002","6-37-2","6-37-6012","6-71-1004")
head(NO)

NO2 <- NO2[c(1:4,6,10,15,16,21:24)]
names(NO2) <- c("date","hour","13-247-1","13-89-2","18-89-22","24-5-3001","48-113-69","48-121-34","6-37-1002","6-37-2","6-37-6012","6-71-1004")
head(NO2)

VOCs <- VOCs[c(1:4,6,10,15,16,21:24)]
names(VOCs) <- c("date","hour","13-247-1","13-89-2","18-89-22","24-5-3001","48-113-69","48-121-34","6-37-1002","6-37-2","6-37-6012","6-71-1004")
head(VOCs)

write.csv(NO, "NO_simu.csv")
write.csv(NO2, "NO2_simu.csv")
write.csv(VOCs, "VOCs_simu.csv")
