################################################################################################
# This program is to find nearby sites for meteorological data, for sites with incomplete data
# Programmer: Xinyi Zhao
# Date: 11/08/2015
################################################################################################

setwd("H:/AQproject/data/data files/level 1 - raw data/meterological data")

###### Temperature 
##### incomplete sites: 17-31-4201, 25-13-8

temp2000 <- read.csv("daily_TEMP_2000.csv", stringsAsFactors = FALSE)
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
temp2011 <- read.csv("daily_TEMP_2011.csv", stringsAsFactors = FALSE)

temp2000 <- data.frame(temp2000, year=rep(2000, nrow(temp2000)))
temp2001 <- data.frame(temp2001, year=rep(2001, nrow(temp2001)))
temp2002 <- data.frame(temp2002, year=rep(2002, nrow(temp2002)))
temp2003 <- data.frame(temp2003, year=rep(2003, nrow(temp2003)))
temp2004 <- data.frame(temp2004, year=rep(2004, nrow(temp2004)))
temp2005 <- data.frame(temp2005, year=rep(2005, nrow(temp2005)))
temp2006 <- data.frame(temp2006, year=rep(2006, nrow(temp2006)))
temp2007 <- data.frame(temp2007, year=rep(2007, nrow(temp2007)))
temp2008 <- data.frame(temp2008, year=rep(2008, nrow(temp2008)))
temp2009 <- data.frame(temp2009, year=rep(2009, nrow(temp2009)))
temp2010 <- data.frame(temp2010, year=rep(2010, nrow(temp2010)))
temp2011 <- data.frame(temp2011, year=rep(2011, nrow(temp2011)))

temp <- rbind(temp2000, temp2001, temp2002, temp2003, temp2004, temp2005, 
              temp2006, temp2007, temp2008, temp2009, temp2010, temp2011)

temp$location <- paste(temp$State.Code, temp$County.Code, sep = '-')
temp$siteID <- paste(temp$State.Code, temp$County.Code, temp$Site.Num, sep = '-')

temp1 <- subset(temp, temp$Arithmetic.Mean!="NA")
temp1 <- subset(temp1, temp1$Observation.Count>=23)
temp1$quality <- as.numeric(temp1$Observation.Count == 24)

temp1 <- subset(temp1, temp1$location =="17-31" | 
                  temp1$location =="25-13" |
                  temp1$location =="48-113")

tapply(temp1$year, temp1$siteID, table) 

# 17-31-72 complete


###### Relative Humidity 
##### incomplete sites: 17-31-4201, 48-113-69

hmdt2000 <- read.csv("daily_RH_DP_2000.csv", stringsAsFactors = FALSE)
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
hmdt2011 <- read.csv("daily_RH_DP_2011.csv", stringsAsFactors = FALSE)

hmdt2000 <- data.frame(hmdt2000, year=rep(2000, nrow(hmdt2000)))
hmdt2001 <- data.frame(hmdt2001, year=rep(2001, nrow(hmdt2001)))
hmdt2002 <- data.frame(hmdt2002, year=rep(2002, nrow(hmdt2002)))
hmdt2003 <- data.frame(hmdt2003, year=rep(2003, nrow(hmdt2003)))
hmdt2004 <- data.frame(hmdt2004, year=rep(2004, nrow(hmdt2004)))
hmdt2005 <- data.frame(hmdt2005, year=rep(2005, nrow(hmdt2005)))
hmdt2006 <- data.frame(hmdt2006, year=rep(2006, nrow(hmdt2006)))
hmdt2007 <- data.frame(hmdt2007, year=rep(2007, nrow(hmdt2007)))
hmdt2008 <- data.frame(hmdt2008, year=rep(2008, nrow(hmdt2008)))
hmdt2009 <- data.frame(hmdt2009, year=rep(2009, nrow(hmdt2009)))
hmdt2010 <- data.frame(hmdt2010, year=rep(2010, nrow(hmdt2010)))
hmdt2011 <- data.frame(hmdt2011, year=rep(2011, nrow(hmdt2011)))

hmdt <- rbind(hmdt2000, hmdt2001, hmdt2002, hmdt2003, hmdt2004, hmdt2005, 
              hmdt2006, hmdt2007, hmdt2008, hmdt2009, hmdt2010, hmdt2011)

hmdt$location <- paste(hmdt$State.Code, hmdt$County.Code, sep = '-')
hmdt$siteID <- paste(hmdt$State.Code, hmdt$County.Code, hmdt$Site.Num, sep = '-')

hmdt1 <- subset(hmdt, hmdt$Arithmetic.Mean!="NA")
hmdt1 <- subset(hmdt1, hmdt1$Observation.Count>=23)
hmdt1$quality <- as.numeric(hmdt1$Observation.Count == 24)

hmdt1 <- subset(hmdt1, hmdt1$location =="17-31" | 
                  hmdt1$location =="25-13" |
                  hmdt1$location =="48-121")

tapply(hmdt1$year, hmdt1$siteID, table)

# 17-31-72 complete from 2000 to 2010


###### Wind Speed (Resultant) 
##### incomplete sites: many

wind2000 <- read.csv("daily_WIND_2000.csv", stringsAsFactors = FALSE)
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
wind2011 <- read.csv("daily_WIND_2011.csv", stringsAsFactors = FALSE)

wind2000 <- data.frame(wind2000, year=rep(2000, nrow(wind2000)))
wind2001 <- data.frame(wind2001, year=rep(2001, nrow(wind2001)))
wind2002 <- data.frame(wind2002, year=rep(2002, nrow(wind2002)))
wind2003 <- data.frame(wind2003, year=rep(2003, nrow(wind2003)))
wind2004 <- data.frame(wind2004, year=rep(2004, nrow(wind2004)))
wind2005 <- data.frame(wind2005, year=rep(2005, nrow(wind2005)))
wind2006 <- data.frame(wind2006, year=rep(2006, nrow(wind2006)))
wind2007 <- data.frame(wind2007, year=rep(2007, nrow(wind2007)))
wind2008 <- data.frame(wind2008, year=rep(2008, nrow(wind2008)))
wind2009 <- data.frame(wind2009, year=rep(2009, nrow(wind2009)))
wind2010 <- data.frame(wind2010, year=rep(2010, nrow(wind2010)))
wind2011 <- data.frame(wind2011, year=rep(2011, nrow(wind2011)))

wind <- rbind(wind2000, wind2001, wind2002, wind2003, wind2004, wind2005, 
              wind2006, wind2007, wind2008, wind2009, wind2010, wind2011)

wind$location <- paste(wind$State.Code, wind$County.Code, sep = '-')
wind$siteID <- paste(wind$State.Code, wind$County.Code, wind$Site.Num, sep = '-')

wind1 <- subset(wind, wind$Arithmetic.Mean!="NA")
wind1 <- subset(wind1, wind1$Observation.Count>=23)
wind1$quality <- as.numeric(wind1$Observation.Count == 24)

wind1 <- subset(wind1, wind1$location =="13-223" | 
                  wind1$location =="13-89" | 
                  wind1$location =="18-89" |
                  wind1$location =="24-5" | 
                  wind1$location =="25-25" |
                  wind1$location =="25-9" |
                  wind1$location =="44-3" |
                  wind1$location =="44-7")

tapply(wind1$year, wind1$siteID, table)

# no complete site