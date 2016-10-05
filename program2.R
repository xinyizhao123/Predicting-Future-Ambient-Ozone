#############################################################################
# This program is to combine, merge and clean data of each  main parameter
# Programmer: Xinyi Zhao
# Date: 10/28/2015
#############################################################################

setwd("H:/AQproject/data/data files/level 2 - subset data/main parameters")

################################# 
#           VOC Data            #
################################# 

VOC2000 <- read.csv("VOC_2000.csv", stringsAsFactors = FALSE)
VOC2001 <- read.csv("VOC_2001.csv", stringsAsFactors = FALSE)
VOC2002 <- read.csv("VOC_2002.csv", stringsAsFactors = FALSE)
VOC2003 <- read.csv("VOC_2003.csv", stringsAsFactors = FALSE)
VOC2004 <- read.csv("VOC_2004.csv", stringsAsFactors = FALSE)
VOC2005 <- read.csv("VOC_2005.csv", stringsAsFactors = FALSE)
VOC2006 <- read.csv("VOC_2006.csv", stringsAsFactors = FALSE)
VOC2007 <- read.csv("VOC_2007.csv", stringsAsFactors = FALSE)
VOC2008 <- read.csv("VOC_2008.csv", stringsAsFactors = FALSE)
VOC2009 <- read.csv("VOC_2009.csv", stringsAsFactors = FALSE)
VOC2010 <- read.csv("VOC_2010.csv", stringsAsFactors = FALSE)
VOC2011 <- read.csv("VOC_2011.csv", stringsAsFactors = FALSE)

VOC <- rbind(VOC2000, VOC2001, VOC2002, VOC2003, VOC2004, VOC2005, VOC2006, VOC2007, VOC2008, VOC2009, VOC2010, VOC2011)
VOC <- VOC[-c(1, 5, 7, 11, 13)]
names(VOC) <- c("state", "county", "site", "VOC.POC", "VOC.unit", "VOC.method", "date", "VOC.value", "siteID", "year")
VOC$date <- as.Date(as.character(VOC$date), format("%Y%m%d"))
#write.csv(VOC, "VOC.csv")

################################# 
#           NOx Data            #
################################# 

NOx2000 <- read.csv("NOx_2000.csv", stringsAsFactors = FALSE)
NOx2001 <- read.csv("NOx_2001.csv", stringsAsFactors = FALSE)
NOx2002 <- read.csv("NOx_2002.csv", stringsAsFactors = FALSE)
NOx2003 <- read.csv("NOx_2003.csv", stringsAsFactors = FALSE)
NOx2004 <- read.csv("NOx_2004.csv", stringsAsFactors = FALSE)
NOx2005 <- read.csv("NOx_2005.csv", stringsAsFactors = FALSE)
NOx2006 <- read.csv("NOx_2006.csv", stringsAsFactors = FALSE)
NOx2007 <- read.csv("NOx_2007.csv", stringsAsFactors = FALSE)
NOx2008 <- read.csv("NOx_2008.csv", stringsAsFactors = FALSE)
NOx2009 <- read.csv("NOx_2009.csv", stringsAsFactors = FALSE)
NOx2010 <- read.csv("NOx_2010.csv", stringsAsFactors = FALSE)
NOx2011 <- read.csv("NOx_2011.csv", stringsAsFactors = FALSE)

NOx <- rbind(NOx2000, NOx2001, NOx2002, NOx2003, NOx2004, NOx2005, NOx2006, NOx2007, NOx2008, NOx2009, NOx2010, NOx2011)
NOx <- NOx[-c(1, 5, 10, 11, 14, 15, 17, 18, 19, 27)]
names(NOx) <- c("state", "county", "site", "NOx.POC", "latitude", "longitude", "datum", 
                "date", "NOx.unit", "NOx.value", "NOx.method", "site.name", "address", 
                "state.name", "county.name", "city.name", "CBSA.name", "siteID", "year", "NOx.quality")
#write.csv(NOx, "NOx.csv")

################################# 
#           OZone Data          #
################################# 

ozone2000 <- read.csv("ozone_2000.csv", stringsAsFactors = FALSE)
ozone2001 <- read.csv("ozone_2001.csv", stringsAsFactors = FALSE)
ozone2002 <- read.csv("ozone_2002.csv", stringsAsFactors = FALSE)
ozone2003 <- read.csv("ozone_2003.csv", stringsAsFactors = FALSE)
ozone2004 <- read.csv("ozone_2004.csv", stringsAsFactors = FALSE)
ozone2005 <- read.csv("ozone_2005.csv", stringsAsFactors = FALSE)
ozone2006 <- read.csv("ozone_2006.csv", stringsAsFactors = FALSE)
ozone2007 <- read.csv("ozone_2007.csv", stringsAsFactors = FALSE)
ozone2008 <- read.csv("ozone_2008.csv", stringsAsFactors = FALSE)
ozone2009 <- read.csv("ozone_2009.csv", stringsAsFactors = FALSE)
ozone2010 <- read.csv("ozone_2010.csv", stringsAsFactors = FALSE)
ozone2011 <- read.csv("ozone_2011.csv", stringsAsFactors = FALSE)

ozone <- rbind(ozone2000, ozone2001, ozone2002, ozone2003, ozone2004, ozone2005, ozone2006, 
               ozone2007, ozone2008, ozone2009, ozone2010, ozone2011)
ozone <- ozone[-c(1, 5, 10, 11, 14, 15, 17, 18, 19, 27)]
names(ozone) <- c("state", "county", "site", "ozone.POC", "latitude", "longitude", "datum", 
                  "date", "ozone.unit", "ozone.value", "ozone.method", "site.name", "address", 
                  "state.name", "county.name", "city.name", "CBSA.name", "siteID", "year", "ozone.quality")
#write.csv(ozone, "ozone.csv")

##################################################
#             merge and manage data              #
##################################################

VOC <- VOC[order(VOC$siteID, VOC$date),]
NOx <- NOx[order(NOx$siteID, NOx$date),]
ozone <- ozone[order(ozone$siteID, ozone$date),]

temp <- merge(VOC, NOx, by=c("siteID", "date"))
ovn <- merge(temp, ozone, by=c("siteID", "date"))

ovn <- ovn[-c(3, 4, 5, 10, 11, 12, 13, 15, 16, 17, 21:27, 29:31, 38, 44)]
ovn <- ovn[order(ovn$siteID, ovn$date),]
ovn <- ovn[c(1, 23, 2, 3:8, 10, 9, 11, 12, 16, 17, 24, 20:22, 18, 19, 13:15)]
ovn <- ovn[c(1:3, 17:24, 4:16)]
colnames(ovn)[4] <- "state"
colnames(ovn)[5] <- "county"
colnames(ovn)[6] <- "city"
colnames(ovn)[7] <- "site"
colnames(ovn)[8] <- "address"
colnames(ovn)[9] <- "latitude"
colnames(ovn)[10] <- "longitude"
colnames(ovn)[11] <- "datum"

write.csv(ovn, "ovn.csv")

#######################################################################
# screen out sites whose data are not continuously available from 2000 to 2011

ovn <- read.csv("ovn.csv", stringsAsFactors = FALSE)
tapply(ovn$year, ovn$siteID, table)

ovn <- subset(ovn, ovn$siteID =="13-223-3" | 
                ovn$siteID =="13-89-2" |
                ovn$siteID =="17-31-4201" |
                ovn$siteID =="18-89-22" |
                ovn$siteID =="22-33-13" |
                ovn$siteID =="22-33-9" |
                ovn$siteID =="22-47-9" |
                ovn$siteID =="24-5-3001" |
                ovn$siteID =="25-13-8" |
                ovn$siteID =="25-25-42" |
                ovn$siteID =="25-9-2006" |
                ovn$siteID =="44-3-2" |
                ovn$siteID =="44-7-1010" |
                ovn$siteID =="48-113-69" |
                ovn$siteID =="48-121-34" |
                ovn$siteID =="48-201-1039" |
                ovn$siteID =="48-201-24" |
                ovn$siteID =="48-201-29" |
                ovn$siteID =="6-37-2")

ovn <- ovn[-c(1)]

tapply(ovn$year, ovn$siteID, table)
length(unique(ovn$siteID))
length(unique(ovn$city))
length(unique(ovn$county))
length(unique(ovn$state))

write.csv(ovn, "ovn2.csv")

##########################################################
# list for sites
ovn2 <- read.csv("ovn2.csv", stringsAsFactors = FALSE)
site <- ovn2[c(2, 5:12)]
site <- unique(site)
site <- site[c(1, 7:9, 2:6)]
write.csv(site, "site.csv")
