################################################################################
# This program is to combine, check and clean meteorological data (2001-2010)
# Programmer: Xinyi Zhao
# Date: 11/02/2015
################################################################################

setwd("H:/AQproject/data/data files/level 1 - raw data/meterological data")

################################# 
#         Temperature           #
################################# 

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

class(temp$State.Code)
class(temp$County.Code)
class(temp$Site.Num)

table(temp$Parameter.Code)
table(temp$Parameter.Name) 
table(temp$POC)
table(temp$Sample.Duration) 
table(temp$Pollutant.Standard) # can be removed
table(temp$Units.of.Measure)
table(temp$Event.Type) # can be removed
table(temp$Observation.Count) # Need clean
table(temp$Method.Name)

temp$siteID <- paste(temp$State.Code, temp$County.Code, temp$Site.Num, sep = '-')

temp1 <- subset(temp, temp$Arithmetic.Mean!="NA")
temp1 <- subset(temp1, temp1$Observation.Count>=23)
temp1$quality <- as.numeric(temp1$Observation.Count == 24)

temp1 <- subset(temp1, temp1$siteID =="13-223-3" | 
                  temp1$siteID =="13-89-2" |
                  temp1$siteID =="17-31-72" |
                  temp1$siteID =="18-89-22" |
                  temp1$siteID =="22-33-13" |
                  temp1$siteID =="22-33-9" |
                  temp1$siteID =="22-47-9" |
                  temp1$siteID =="24-5-3001" |
                  temp1$siteID =="25-13-8" |
                  temp1$siteID =="25-25-42" |
                  temp1$siteID =="25-9-2006" |
                  temp1$siteID =="44-3-2" |
                  temp1$siteID =="44-7-1010" |
                  temp1$siteID =="48-113-69" |
                  temp1$siteID =="48-121-34" |
                  temp1$siteID =="48-201-1039" |
                  temp1$siteID =="48-201-24" |
                  temp1$siteID =="48-201-29" |
                  temp1$siteID =="6-37-2")

table(temp1$siteID)

temp1$siteID[temp1$siteID == "17-31-72"] <- "17-31-4201"

table(temp1$siteID)

temp1 <- temp1[-c(1:4,6:11,14:16,20,23:29)]
temp1 <- temp1[-c(7)]
prop.table(table(temp1$quality))

names(temp1) <- c("temp.POC", "date", "temp.unit", "temp.avg", "temp.max", 
                  "temp.maxhr", "temp.method", "siteID", "temp.quality")

write.csv(temp1, "temp.csv")

################################## 
#          Wind Speed            #
##################################

wind2001 <- read.csv("daily_WIND_2001.csv", stringsAsFactors = FALSE)
wind2002 <- read.csv("daily_WIND_2002.csv", stringsAsFactors = FALSE)
wind2003 <- read.csv("daily_WIND_2003.csv", stringsAsFactors = FALSE)
wind2004 <- read.csv("daily_WIND_2004.csv", stringsAsFactors = FALSE)
wind2005 <- read.csv("daily_WIND_2005.csv", stringsAsFactors = FALSE)
wind2006 <- read.csv("daily_WIND_2006.csv", stringsAsFactors = FALSE)
wind2007 <- read.csv("daily_WIND_2007.csv", stringsAsFactors = FALSE)
wind2008 <- read.csv("daily_WIND_2008.csv", stringsAsFactors = FALSE)
wind2009 <- read.csv("daily_WIND_2009.csv", stringsAsFactors = FALSE)
wind2010 <- read.csv("daily_WIND_2010.csv", stringsAsFactors = FALSE)

wind <- rbind(wind2001, wind2002, wind2003, wind2004, wind2005, 
              wind2006, wind2007, wind2008, wind2009, wind2010)

head(wind)

class(wind$State.Code)
class(wind$County.Code)
class(wind$Site.Num)

table(wind$Parameter.Code)
table(wind$Parameter.Name) # need subset (61103 = wind speed)
table(wind$POC)
table(wind$Sample.Duration) 
table(wind$Pollutant.Standard) # can be removed
table(wind$Units.of.Measure)
table(wind$Event.Type) # can be removed
table(wind$Observation.Count) # Need clean
table(wind$Method.Name)

wind$siteID <- paste(wind$State.Code, wind$County.Code, wind$Site.Num, sep = '-')

wind <- wind[wind$Parameter.Code == 61103,]
wind1 <- subset(wind, wind$Arithmetic.Mean!="NA")
wind1 <- subset(wind1, wind1$Observation.Count>=23)
wind1$quality <- as.numeric(wind1$Observation.Count == 24)

wind1 <- subset(wind1, wind1$siteID =="13-223-3" | 
                  wind1$siteID =="13-89-2" |
                  wind1$siteID =="17-31-4201" |
                  wind1$siteID =="18-89-22" |
                  wind1$siteID =="22-33-13" |
                  wind1$siteID =="22-33-9" |
                  wind1$siteID =="22-47-9" |
                  wind1$siteID =="24-5-3001" |
                  wind1$siteID =="25-13-8" |
                  wind1$siteID =="25-25-42" |
                  wind1$siteID =="25-9-2006" |
                  wind1$siteID =="44-3-2" |
                  wind1$siteID =="44-7-1010" |
                  wind1$siteID =="48-113-69" |
                  wind1$siteID =="48-121-34" |
                  wind1$siteID =="48-201-1039" |
                  wind1$siteID =="48-201-24" |
                  wind1$siteID =="48-201-29" |
                  wind1$siteID =="6-37-2")

wind1 <- wind1[-c(1:4,6:11,14:16,20,23:29)]
wind1 <- wind1[-c(5,6)]
wind1 <- wind1[-c(5)]

prop.table(table(wind1$quality))

names(wind1) <- c("wind.POC", "date", "wind.unit", "wind.value", "wind.method", "siteID", "wind.quality")

write.csv(wind1, "wind.csv")

#####################################
#       Dew Point Temperature       #
#####################################

dpt2001 <- read.csv("daily_RH_DP_2001.csv", stringsAsFactors = FALSE)
dpt2002 <- read.csv("daily_RH_DP_2002.csv", stringsAsFactors = FALSE)
dpt2003 <- read.csv("daily_RH_DP_2003.csv", stringsAsFactors = FALSE)
dpt2004 <- read.csv("daily_RH_DP_2004.csv", stringsAsFactors = FALSE)
dpt2005 <- read.csv("daily_RH_DP_2005.csv", stringsAsFactors = FALSE)
dpt2006 <- read.csv("daily_RH_DP_2006.csv", stringsAsFactors = FALSE)
dpt2007 <- read.csv("daily_RH_DP_2007.csv", stringsAsFactors = FALSE)
dpt2008 <- read.csv("daily_RH_DP_2008.csv", stringsAsFactors = FALSE)
dpt2009 <- read.csv("daily_RH_DP_2009.csv", stringsAsFactors = FALSE)
dpt2010 <- read.csv("daily_RH_DP_2010.csv", stringsAsFactors = FALSE)

dpt <- rbind(dpt2001, dpt2002, dpt2003, dpt2004, dpt2005, 
             dpt2006, dpt2007, dpt2008, dpt2009, dpt2010)

head(dpt)

class(dpt$State.Code)
class(dpt$County.Code)
class(dpt$Site.Num)

table(dpt$Parameter.Code)
table(dpt$Parameter.Name) # need subset (62103 = dew point temperature)
table(dpt$POC)
table(dpt$Sample.Duration) 
table(dpt$Pollutant.Standard) # can be removed
table(dpt$Units.of.Measure)
table(dpt$Event.Type) # can be removed
table(dpt$Observation.Count) # Need clean
table(dpt$Method.Name)

dpt$siteID <- paste(dpt$State.Code, dpt$County.Code, dpt$Site.Num, sep = '-')

dpt <- dpt[dpt$Parameter.Code == 62103,]
dpt1 <- subset(dpt, dpt$Arithmetic.Mean!="NA")
dpt1 <- subset(dpt1, dpt1$Observation.Count>=23)
dpt1$quality <- as.numeric(dpt1$Observation.Count == 24)

dpt1 <- subset(dpt1, dpt1$siteID =="13-223-3" | 
                 dpt1$siteID =="13-89-2" |
                 dpt1$siteID =="17-31-4201" |
                 dpt1$siteID =="18-89-22" |
                 dpt1$siteID =="22-33-13" |
                 dpt1$siteID =="22-33-9" |
                 dpt1$siteID =="22-47-9" |
                 dpt1$siteID =="24-5-3001" |
                 dpt1$siteID =="25-13-8" |
                 dpt1$siteID =="25-25-42" |
                 dpt1$siteID =="25-9-2006" |
                 dpt1$siteID =="44-3-2" |
                 dpt1$siteID =="44-7-1010" |
                 dpt1$siteID =="48-113-69" |
                 dpt1$siteID =="48-121-34" |
                 dpt1$siteID =="48-201-1039" |
                 dpt1$siteID =="48-201-24" |
                 dpt1$siteID =="48-201-29" |
                 dpt1$siteID =="6-37-2")

dpt1 <- dpt1[-c(1:4,6:11,14:16,20,23:29)]
dpt1 <- dpt1[-c(5,6)]
dpt1 <- dpt1[-c(5)]

prop.table(table(dpt1$quality))

names(dpt1) <- c("dpt.POC", "date", "dpt.unit", "dpt.value", "dpt.method", "siteID", "dpt.quality")

write.csv(dpt1, "dpt.csv")

###################################
#        Relative Humidity        #
###################################

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

class(hmdt$State.Code)
class(hmdt$County.Code)
class(hmdt$Site.Num)

table(hmdt$Parameter.Code)
table(hmdt$Parameter.Name) # need subset (62201 = relative humidity)
table(hmdt$POC)
table(hmdt$Sample.Duration) 
table(hmdt$Pollutant.Standard) # can be removed
table(hmdt$Units.of.Measure)
table(hmdt$Event.Type) # can be removed
table(hmdt$Observation.Count) # Need clean
table(hmdt$Method.Name)

hmdt$siteID <- paste(hmdt$State.Code, hmdt$County.Code, hmdt$Site.Num, sep = '-')

hmdt <- hmdt[hmdt$Parameter.Code == 62201 & hmdt$Sample.Duration == "1 HOUR",]
hmdt1 <- subset(hmdt, hmdt$Arithmetic.Mean!="NA")
hmdt1 <- subset(hmdt1, hmdt1$Observation.Count>=23)
hmdt1$quality <- as.numeric(hmdt1$Observation.Count == 24)

hmdt1 <- subset(hmdt1, hmdt1$siteID =="13-223-3" | 
                  hmdt1$siteID =="13-89-2" |
                  hmdt1$siteID =="17-31-72" |
                  hmdt1$siteID =="18-89-22" |
                  hmdt1$siteID =="22-33-13" |
                  hmdt1$siteID =="22-33-9" |
                  hmdt1$siteID =="22-47-9" |
                  hmdt1$siteID =="24-5-3001" |
                  hmdt1$siteID =="25-13-8" |
                  hmdt1$siteID =="25-25-42" |
                  hmdt1$siteID =="25-9-2006" |
                  hmdt1$siteID =="44-3-2" |
                  hmdt1$siteID =="44-7-1010" |
                  hmdt1$siteID =="48-113-69" |
                  hmdt1$siteID =="48-121-34" |
                  hmdt1$siteID =="48-201-1039" |
                  hmdt1$siteID =="48-201-24" |
                  hmdt1$siteID =="48-201-29" |
                  hmdt1$siteID =="6-37-2")

table(hmdt1$siteID)

hmdt1$siteID[hmdt1$siteID == "17-31-72"] <- "17-31-4201"

table(hmdt1$siteID)

hmdt1 <- hmdt1[-c(1:4,6:11,14:16,20,23:29)]
hmdt1 <- hmdt1[-c(5,6)]
hmdt1 <- hmdt1[-c(5)]

prop.table(table(hmdt1$quality))

names(hmdt1) <- c("hmdt.POC", "date", "hmdt.unit", "hmdt.value", "hmdt.method", "siteID", "hmdt.quality")

write.csv(hmdt1, "hmdt.csv")

### The availability of relative humidity data is much better than dew point data. So choose relative humidity data.
####################################################################################################################

###################################
#          Precipitation          #
###################################

prcp <- read.csv("precip.csv", stringsAsFactors = FALSE)

table(prcp$STATION_NAME)

prcp <- prcp[-c(1,3,4,5, 8:11)]
prcp <- prcp[c(1,2,3,1)]

colnames(prcp)[4] <- "siteID"
colnames(prcp)[2] <- "date"

prcp <- subset(prcp, prcp$PRCP!=-9999)
prcp$date <- as.Date(as.character(prcp$date), format("%Y%m%d"))

prcp$siteID[prcp$siteID == "ATLANTA DEKALB PEACHTREE AIRPORT GA US"] <- "13-89-2"
prcp$siteID[prcp$siteID == "BAYOU SORREL LOCK LA US"] <- "22-47-9"
prcp$siteID[prcp$siteID == "DENTON MUNICPAL AIRPORT TX US"] <- "48-121-34"
prcp$siteID[prcp$siteID == "HOUSTON INTERCONTINENTAL AIRPORT TX US"] <- "48-201-24"
prcp$siteID[prcp$siteID == "NORTH FOSTER 1 E RI US"] <- "44-3-2"
prcp$siteID[prcp$siteID == "BATON ROUGE CONCORD LA US"] <- "22-33-9"
prcp$siteID[prcp$siteID == "BOSTON LOGAN INTERNATIONAL AIRPORT MA US"] <- "25-25-42"
prcp$siteID[prcp$siteID == "EMBRY GA US"] <- "13-223-3"
prcp$siteID[prcp$siteID == "MARBLEHEAD MA US"] <- "25-9-2006"
prcp$siteID[prcp$siteID == "PROVIDENCE T F GREEN STATE AIRPORT RI US"] <- "44-7-1010"
prcp$siteID[prcp$siteID == "BATON ROUGE RYAN AIRPORT LA US"] <- "22-33-13"
prcp$siteID[prcp$siteID == "CROWN POINT 1 N IN US"] <- "18-89-22"
prcp$siteID[prcp$siteID == "HOUSTON DEER PARK TX US"] <- "48-201-1039"
prcp$siteID[prcp$siteID == "MARYLAND SCIENCE CENTER BALTIMORE MD US"] <- "24-5-3001"
prcp$siteID[prcp$siteID == "SAN GABRIEL CANYON PH CA US"] <- "6-37-2"
prcp$siteID[prcp$siteID == "CHICAGO WAUKEGAN REGIONAL AIRPORT IL US"] <- "17-31-4201"

table(prcp$siteID,prcp$STATION_NAME)

precip <- prcp[c(4, 2, 3)]

write.csv(precip, "prcp.csv")

######################################################################
# IMPORTANT: Study period is now changed to 2001-2010 (10-yr period)
#############

####################################
#         Solar Radiation          #
####################################

# for the two sites located in GA

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

GA1 <- GA1[c(1,2,16)]
names(GA1) <- c("date","time","solar")
GA1 <- subset(GA1, GA1$solar!="NA")
GA1 <- subset(GA1, GA1$solar!=-9900)

GA2 <- GA1
GA1 <- data.frame(GA1, siteID=rep("13-223-3", nrow(GA1)))
GA2 <- data.frame(GA2, siteID=rep("13-89-2", nrow(GA2)))

# for the two sites near Chicago

CHI2001 <- read.csv("725300_2001_solar.csv", stringsAsFactors = FALSE)
CHI2002 <- read.csv("725300_2002_solar.csv", stringsAsFactors = FALSE)
CHI2003 <- read.csv("725300_2003_solar.csv", stringsAsFactors = FALSE)
CHI2004 <- read.csv("725300_2004_solar.csv", stringsAsFactors = FALSE)
CHI2005 <- read.csv("725300_2005_solar.csv", stringsAsFactors = FALSE)
CHI2006 <- read.csv("725300_2006_solar.csv", stringsAsFactors = FALSE)
CHI2007 <- read.csv("725300_2007_solar.csv", stringsAsFactors = FALSE)
CHI2008 <- read.csv("725300_2008_solar.csv", stringsAsFactors = FALSE)
CHI2009 <- read.csv("725300_2009_solar.csv", stringsAsFactors = FALSE)
CHI2010 <- read.csv("725300_2010_solar.csv", stringsAsFactors = FALSE)

CHI1 <- rbind(CHI2001, CHI2002, CHI2003, CHI2004, CHI2005, CHI2006, CHI2007, CHI2008, CHI2009, CHI2010)

CHI1 <- CHI1[c(1,2,16)]
names(CHI1) <- c("date","time","solar")
CHI1 <- subset(CHI1, CHI1$solar!="NA")
CHI1 <- subset(CHI1, CHI1$solar!=-9900)

CHI2 <- CHI1
CHI1 <- data.frame(CHI1, siteID=rep("17-31-4201", nrow(CHI1)))
CHI2 <- data.frame(CHI2, siteID=rep("18-89-22", nrow(CHI2)))

# for the two sites located in LA

LA2001 <- read.csv("722317_2001_solar.csv", stringsAsFactors = FALSE)
LA2002 <- read.csv("722317_2002_solar.csv", stringsAsFactors = FALSE)
LA2003 <- read.csv("722317_2003_solar.csv", stringsAsFactors = FALSE)
LA2004 <- read.csv("722317_2004_solar.csv", stringsAsFactors = FALSE)
LA2005 <- read.csv("722317_2005_solar.csv", stringsAsFactors = FALSE)
LA2006 <- read.csv("722317_2006_solar.csv", stringsAsFactors = FALSE)
LA2007 <- read.csv("722317_2007_solar.csv", stringsAsFactors = FALSE)
LA2008 <- read.csv("722317_2008_solar.csv", stringsAsFactors = FALSE)
LA2009 <- read.csv("722317_2009_solar.csv", stringsAsFactors = FALSE)
LA2010 <- read.csv("722317_2010_solar.csv", stringsAsFactors = FALSE)

LA1 <- rbind(LA2001, LA2002, LA2003, LA2004, LA2005, LA2006, LA2007, LA2008, LA2009, LA2010)

LA1 <- LA1[c(1,2,16)]
names(LA1) <- c("date","time","solar")
LA1 <- subset(LA1, LA1$solar!="NA")
LA1 <- subset(LA1, LA1$solar!=-9900)

LA2 <- LA1
LA3 <- LA1
LA1 <- data.frame(LA1, siteID=rep("22-33-13", nrow(LA1)))
LA2 <- data.frame(LA2, siteID=rep("22-33-9", nrow(LA2)))
LA3 <- data.frame(LA3, siteID=rep("22-47-9", nrow(LA3)))

# for the one site located in MD

MD2001 <- read.csv("724060_2001_solar.csv", stringsAsFactors = FALSE)
MD2002 <- read.csv("724060_2002_solar.csv", stringsAsFactors = FALSE)
MD2003 <- read.csv("724060_2003_solar.csv", stringsAsFactors = FALSE)
MD2004 <- read.csv("724060_2004_solar.csv", stringsAsFactors = FALSE)
MD2005 <- read.csv("724060_2005_solar.csv", stringsAsFactors = FALSE)
MD2006 <- read.csv("724060_2006_solar.csv", stringsAsFactors = FALSE)
MD2007 <- read.csv("724060_2007_solar.csv", stringsAsFactors = FALSE)
MD2008 <- read.csv("724060_2008_solar.csv", stringsAsFactors = FALSE)
MD2009 <- read.csv("724060_2009_solar.csv", stringsAsFactors = FALSE)
MD2010 <- read.csv("724060_2010_solar.csv", stringsAsFactors = FALSE)

MD1 <- rbind(MD2001, MD2002, MD2003, MD2004, MD2005, MD2006, MD2007, MD2008, MD2009, MD2010)

MD1 <- MD1[c(1,2,16)]
names(MD1) <- c("date","time","solar")
MD1 <- subset(MD1, MD1$solar!="NA")
MD1 <- subset(MD1, MD1$solar!=-9900)

MD1 <- data.frame(MD1, siteID=rep("24-5-3001", nrow(MD1)))

# for the two sites located in MA

MA2001 <- read.csv("725090_2001_solar.csv", stringsAsFactors = FALSE)
MA2002 <- read.csv("725090_2002_solar.csv", stringsAsFactors = FALSE)
MA2003 <- read.csv("725090_2003_solar.csv", stringsAsFactors = FALSE)
MA2004 <- read.csv("725090_2004_solar.csv", stringsAsFactors = FALSE)
MA2005 <- read.csv("725090_2005_solar.csv", stringsAsFactors = FALSE)
MA2006 <- read.csv("725090_2006_solar.csv", stringsAsFactors = FALSE)
MA2007 <- read.csv("725090_2007_solar.csv", stringsAsFactors = FALSE)
MA2008 <- read.csv("725090_2008_solar.csv", stringsAsFactors = FALSE)
MA2009 <- read.csv("725090_2009_solar.csv", stringsAsFactors = FALSE)
MA2010 <- read.csv("725090_2010_solar.csv", stringsAsFactors = FALSE)

MA1 <- rbind(MA2001, MA2002, MA2003, MA2004, MA2005, MA2006, MA2007, MA2008, MA2009, MA2010)

MA1 <- MA1[c(1,2,16)]
names(MA1) <- c("date","time","solar")
MA1 <- subset(MA1, MA1$solar!="NA")
MA1 <- subset(MA1, MA1$solar!=-9900)

MA2 <- MA1
MA1 <- data.frame(MA1, siteID=rep("25-25-42", nrow(MA1)))
MA2 <- data.frame(MA2, siteID=rep("25-9-2006", nrow(MA2)))

# for the two sites located in RI

RI2001 <- read.csv("725070_2001_solar.csv", stringsAsFactors = FALSE)
RI2002 <- read.csv("725070_2002_solar.csv", stringsAsFactors = FALSE)
RI2003 <- read.csv("725070_2003_solar.csv", stringsAsFactors = FALSE)
RI2004 <- read.csv("725070_2004_solar.csv", stringsAsFactors = FALSE)
RI2005 <- read.csv("725070_2005_solar.csv", stringsAsFactors = FALSE)
RI2006 <- read.csv("725070_2006_solar.csv", stringsAsFactors = FALSE)
RI2007 <- read.csv("725070_2007_solar.csv", stringsAsFactors = FALSE)
RI2008 <- read.csv("725070_2008_solar.csv", stringsAsFactors = FALSE)
RI2009 <- read.csv("725070_2009_solar.csv", stringsAsFactors = FALSE)
RI2010 <- read.csv("725070_2010_solar.csv", stringsAsFactors = FALSE)

RI1 <- rbind(RI2001, RI2002, RI2003, RI2004, RI2005, RI2006, RI2007, RI2008, RI2009, RI2010)

RI1 <- RI1[c(1,2,16)]
names(RI1) <- c("date","time","solar")
RI1 <- subset(RI1, RI1$solar!="NA")
RI1 <- subset(RI1, RI1$solar!=-9900)

RI2 <- RI1
RI1 <- data.frame(RI1, siteID=rep("44-3-2", nrow(RI1)))
RI2 <- data.frame(RI2, siteID=rep("44-7-1010", nrow(RI2)))

# for the one site located in Denton TX

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

DEN1 <- DEN1[c(1,2,16)]
names(DEN1) <- c("date","time","solar")
DEN1 <- subset(DEN1, DEN1$solar!="NA")
DEN1 <- subset(DEN1, DEN1$solar!=-9900)

DEN1 <- data.frame(DEN1, siteID=rep("48-121-34", nrow(DEN1)))

# for the two sites located in Houston TX

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

HOU2 <- HOU1
HOU3 <- HOU1

HOU2 <- data.frame(HOU2, siteID=rep("48-201-1039", nrow(HOU2)))
HOU3 <- data.frame(HOU3, siteID=rep("48-201-24", nrow(HOU3)))

# for the one site located in CA

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

CA1 <- data.frame(CA1, siteID=rep("6-37-2", nrow(CA1)))

SR <- rbind(GA1,GA2,CHI1,CHI2,LA1,LA2,LA3,MD1,MA1,MA2,RI1,RI2,DEN1,HOU2,HOU3,CA1)
SR$date <- as.Date(as.character(SR$date), format("%Y-%m-%d"))
class(SR$date)

SR <- SR[order(SR$siteID, SR$date),]

write.csv(SR, "SR.csv")

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

write.csv(SR1, "SR_daily.csv")
