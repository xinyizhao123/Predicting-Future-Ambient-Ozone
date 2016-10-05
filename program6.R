#####################################################################################################
# This program is to find additional monitoring sites in the metropolitan area that are applicable
# Programmer: Xinyi Zhao
# Date: 12/01/2015
#####################################################################################################

setwd("H:/AQproject/Data Collection/Data Files/Level 1 - Raw Data/Main Parameters")

### VOC data ###

voc01 <- read.table("RD_501_PVOC_2001-0.txt", sep = "|", stringsAsFactors = F, fill = T)
voc01 <- voc01[-c(1,2, 14:28)]
names(voc01) <- c("state", "county", "site", "parameter", "POC", "duration", "unit", "method", "date", "time", "value")
voc01$siteid <- paste(voc01$state, voc01$county, voc01$site, sep = '-')
voc01 <- voc01[voc01$parameter == 43102 & voc01$duration == 7,]
voc01 <- subset(voc01, voc01$value!="NA")

voc02 <- read.table("RD_501_PVOC_2002-0.txt", sep = "|", stringsAsFactors = F, fill = T)
voc02 <- voc02[-c(1,2, 14:28)]
names(voc02) <- c("state", "county", "site", "parameter", "POC", "duration", "unit", "method", "date", "time", "value")
voc02$siteid <- paste(voc02$state, voc02$county, voc02$site, sep = '-')
voc02 <- voc02[voc02$parameter == 43102 & voc02$duration == 7,]
voc02 <- subset(voc02, voc02$value!="NA")

voc03 <- read.table("RD_501_PVOC_2003-0.txt", sep = "|", stringsAsFactors = F, fill = T)
voc03 <- voc03[-c(1,2, 14:28)]
names(voc03) <- c("state", "county", "site", "parameter", "POC", "duration", "unit", "method", "date", "time", "value")
voc03$siteid <- paste(voc03$state, voc03$county, voc03$site, sep = '-')
voc03 <- voc03[voc03$parameter == 43102 & voc03$duration == 7,]
voc03 <- subset(voc03, voc03$value!="NA")

voc04 <- read.table("RD_501_PVOC_2004-0.txt", sep = "|", stringsAsFactors = F, fill = T)
voc04 <- voc04[-c(1,2, 14:28)]
names(voc04) <- c("state", "county", "site", "parameter", "POC", "duration", "unit", "method", "date", "time", "value")
voc04$siteid <- paste(voc04$state, voc04$county, voc04$site, sep = '-')
voc04 <- voc04[voc04$parameter == 43102 & voc04$duration == 7,]
voc04 <- subset(voc04, voc04$value!="NA")

voc05 <- read.table("RD_501_PVOC_2005-0.txt", sep = "|", stringsAsFactors = F, fill = T)
voc05 <- voc05[-c(1,2, 14:28)]
names(voc05) <- c("state", "county", "site", "parameter", "POC", "duration", "unit", "method", "date", "time", "value")
voc05$siteid <- paste(voc05$state, voc05$county, voc05$site, sep = '-')
voc05 <- voc05[voc05$parameter == 43102 & voc05$duration == 7,]
voc05 <- subset(voc05, voc05$value!="NA")

voc06 <- read.table("RD_501_PAMSVOC_2006-0.txt", sep = "|", stringsAsFactors = F, fill = T)
voc06 <- voc06[-c(1,2, 14:28)]
names(voc06) <- c("state", "county", "site", "parameter", "POC", "duration", "unit", "method", "date", "time", "value")
voc06$siteid <- paste(voc06$state, voc06$county, voc06$site, sep = '-')
voc06 <- voc06[voc06$parameter == 43102 & voc06$duration == 7,]
voc06 <- subset(voc06, voc06$value!="NA")

voc07 <- read.table("RD_501_PAMSVOC_2007-0.txt", sep = "|", stringsAsFactors = F, fill = T)
voc07 <- voc07[-c(1,2, 14:28)]
names(voc07) <- c("state", "county", "site", "parameter", "POC", "duration", "unit", "method", "date", "time", "value")
voc07$siteid <- paste(voc07$state, voc07$county, voc07$site, sep = '-')
voc07 <- voc07[voc07$parameter == 43102 & voc07$duration == 7,]
voc07 <- subset(voc07, voc07$value!="NA")

voc08 <- read.table("RD_501_PAMSVOC_2008-0.txt", sep = "|", stringsAsFactors = F, fill = T)
voc08 <- voc08[-c(1,2, 14:28)]
names(voc08) <- c("state", "county", "site", "parameter", "POC", "duration", "unit", "method", "date", "time", "value")
voc08$siteid <- paste(voc08$state, voc08$county, voc08$site, sep = '-')
voc08 <- voc08[voc08$parameter == 43102 & voc08$duration == 7,]
voc08 <- subset(voc08, voc08$value!="NA")

voc09 <- read.table("RD_501_PAMSVOC_2009-0.txt", sep = "|", stringsAsFactors = F, fill = T)
voc09 <- voc09[-c(1,2, 14:28)]
names(voc09) <- c("state", "county", "site", "parameter", "POC", "duration", "unit", "method", "date", "time", "value")
voc09$siteid <- paste(voc09$state, voc09$county, voc09$site, sep = '-')
voc09 <- voc09[voc09$parameter == 43102 & voc09$duration == 7,]
voc09 <- subset(voc09, voc09$value!="NA")

voc10 <- read.table("RD_501_PAMSVOC_2010-0.txt", sep = "|", stringsAsFactors = F, fill = T)
voc10 <- voc10[-c(1,2, 14:28)]
names(voc10) <- c("state", "county", "site", "parameter", "POC", "duration", "unit", "method", "date", "time", "value")
voc10$siteid <- paste(voc10$state, voc10$county, voc10$site, sep = '-')
voc10 <- voc10[voc10$parameter == 43102 & voc10$duration == 7,]
voc10 <- subset(voc10, voc10$value!="NA")

voc <- rbind(voc01, voc02, voc03, voc04, voc05, voc06, voc07, voc08, voc09, voc10)

voc$date <- as.Date(as.character(voc$date), format("%Y%m%d"))
voc$year <- format(voc$date, "%Y")
voc$month <- format(voc$date, "%m")

write.csv(voc, "voc_all.csv")

voc <- read.csv("voc_all.csv", stringsAsFactors = FALSE)
unique(voc$siteid)
tapply(voc$year, voc$siteid, table)

voc_new <- subset(voc, voc$siteid == "13-89-2"|
                       voc$siteid =="13-89-3001" |
                       voc$siteid =="13-247-1"|
                       voc$siteid == "17-31-4201"|
                       voc$siteid == "17-31-72"|
                       voc$siteid == "17-97-1007"|
                       voc$siteid == "18-89-22"|
                       voc$siteid == "18-89-23"|
                       voc$siteid == "18-89-28"|
                       voc$siteid == "18-89-29"|
                       voc$siteid == "18-89-30"|
                       voc$siteid == "18-89-1003"|
                       voc$siteid == "18-89-2004"|
                       voc$siteid == "18-89-2008"|
                       voc$siteid == "24-5-3001"|
                       voc$siteid == "24-3-19"|
                       voc$siteid == "24-25-9001"|
                       voc$siteid == "25-9-2006"|
                       voc$siteid == "25-25-41"|
                       voc$siteid == "25-25-42"|
                       voc$siteid == "44-3-2"|
                       voc$siteid == "44-3-11"|
                       voc$siteid == "44-3-12"|
                       voc$siteid == "44-3-13"|
                       voc$siteid == "44-3-14"|
                       voc$siteid == "44-3-15"|
                       voc$siteid == "44-7-22"|
                       voc$siteid == "44-7-24"|
                       voc$siteid == "44-7-25"|
                       voc$siteid == "44-7-26"|
                       voc$siteid == "44-7-29"|
                       voc$siteid == "44-7-1010"|
                       voc$siteid == "48-113-69"|
                       voc$siteid == "48-121-34"|
                       voc$siteid == "48-139-15"|
                       voc$siteid == "48-139-1044"|
                       voc$siteid == "48-257-5"|
                       voc$siteid == "48-439-1002"|
                       voc$siteid == "48-439-3009"|
                       voc$siteid == "48-167-14"|
                       voc$siteid == "48-167-1034"|
                       voc$siteid == "48-201-24"|
                       voc$siteid == "48-201-26"|
                       voc$siteid == "48-201-29"|
                       voc$siteid == "48-201-30"|
                       voc$siteid == "48-201-55"|
                       voc$siteid == "48-201-1035"|
                       voc$siteid == "48-201-1039"|
                       voc$siteid == "48-339-78"|
                       voc$siteid == "6-37-2"|
                       voc$siteid == "6-37-1002"|
                       voc$siteid == "6-37-1103"|
                       voc$siteid == "6-37-1601"|
                       voc$siteid == "6-37-1602"|
                       voc$siteid == "6-37-5001"|
                       voc$siteid == "6-37-5005"|
                       voc$siteid == "6-37-6012"|
                       voc$siteid == "6-65-8001"|
                       voc$siteid == "6-71-1004"|
                       voc$siteid == "18-97-57"|
                       voc$siteid == "18-97-78"|
                       voc$siteid == "18-97-84"|
                       voc$siteid == "18-97-85"|
                       voc$siteid == "19-45-21"|
                       voc$siteid == "19-139-20"|
                       voc$siteid == "19-163-15"|
                       voc$siteid == "48-141-37"|
                       voc$siteid == "48-141-44"|
                       voc$siteid == "48-141-55")

tapply(voc_new$year, voc_new$siteid, table)

### Ozone data ###

ozone01 <- read.csv("daily_44201_2001.csv", stringsAsFactors = FALSE)
ozone02 <- read.csv("daily_44201_2002.csv", stringsAsFactors = FALSE)
ozone03 <- read.csv("daily_44201_2003.csv", stringsAsFactors = FALSE)
ozone04 <- read.csv("daily_44201_2004.csv", stringsAsFactors = FALSE)
ozone05 <- read.csv("daily_44201_2005.csv", stringsAsFactors = FALSE)
ozone06 <- read.csv("daily_44201_2006.csv", stringsAsFactors = FALSE)
ozone07 <- read.csv("daily_44201_2007.csv", stringsAsFactors = FALSE)
ozone08 <- read.csv("daily_44201_2008.csv", stringsAsFactors = FALSE)
ozone09 <- read.csv("daily_44201_2009.csv", stringsAsFactors = FALSE)
ozone10 <- read.csv("daily_44201_2010.csv", stringsAsFactors = FALSE)

ozone <- rbind(ozone01, ozone02, ozone03, ozone04, ozone05, 
               ozone06, ozone07, ozone08, ozone09, ozone10)

ozone$State.Code <- as.numeric(ozone$State.Code)
ozone$siteid <- paste(ozone$State.Code, ozone$County.Code, ozone$Site.Num, sep = '-')

ozone$Date.Local <- as.Date(as.character(ozone$Date.Local), format("%Y-%m-%d"))
ozone$year <- format(ozone$Date.Local, "%Y")
ozone$month <- format(ozone$Date.Local, "%m")

ozone <- ozone[ozone$Parameter.Code == 44201 & ozone$Sample.Duration == "8-HR RUN AVG BEGIN HOUR",]

write.csv(ozone, "ozone_all.csv")

ozone1 <- subset(ozone, ozone$Arithmetic.Mean!="NA")
ozone1 <- subset(ozone1, ozone1$Observation.Count>=23)

ozone1_new <- subset(ozone1, ozone1$siteid == "13-89-2"|
                       ozone1$siteid =="13-89-3001" |
                       ozone1$siteid =="13-247-1"|
                       ozone1$siteid == "17-31-4201"|
                       ozone1$siteid == "17-31-72"|
                       ozone1$siteid == "17-97-1007"|
                       ozone1$siteid == "18-89-22"|
                       ozone1$siteid == "18-89-23"|
                       ozone1$siteid == "18-89-28"|
                       ozone1$siteid == "18-89-29"|
                       ozone1$siteid == "18-89-30"|
                       ozone1$siteid == "18-89-1003"|
                       ozone1$siteid == "18-89-2004"|
                       ozone1$siteid == "18-89-2008"|
                       ozone1$siteid == "24-5-3001"|
                       ozone1$siteid == "24-3-19"|
                       ozone1$siteid == "24-25-9001"|
                       ozone1$siteid == "25-9-2006"|
                       ozone1$siteid == "25-25-41"|
                       ozone1$siteid == "25-25-42"|
                       ozone1$siteid == "44-3-2"|
                       ozone1$siteid == "44-3-11"|
                       ozone1$siteid == "44-3-12"|
                       ozone1$siteid == "44-3-13"|
                       ozone1$siteid == "44-3-14"|
                       ozone1$siteid == "44-3-15"|
                       ozone1$siteid == "44-7-22"|
                       ozone1$siteid == "44-7-24"|
                       ozone1$siteid == "44-7-25"|
                       ozone1$siteid == "44-7-26"|
                       ozone1$siteid == "44-7-29"|
                       ozone1$siteid == "44-7-1010"|
                       ozone1$siteid == "48-113-69"|
                       ozone1$siteid == "48-121-34"|
                       ozone1$siteid == "48-139-15"|
                       ozone1$siteid == "48-139-1044"|
                       ozone1$siteid == "48-257-5"|
                       ozone1$siteid == "48-439-1002"|
                       ozone1$siteid == "48-439-3009"|
                       ozone1$siteid == "48-167-14"|
                       ozone1$siteid == "48-167-1034"|
                       ozone1$siteid == "48-201-24"|
                       ozone1$siteid == "48-201-26"|
                       ozone1$siteid == "48-201-29"|
                       ozone1$siteid == "48-201-30"|
                       ozone1$siteid == "48-201-55"|
                       ozone1$siteid == "48-201-1035"|
                       ozone1$siteid == "48-201-1039"|
                       ozone1$siteid == "48-339-78"|
                       ozone1$siteid == "6-37-2"|
                       ozone1$siteid == "6-37-1002"|
                       ozone1$siteid == "6-37-1103"|
                       ozone1$siteid == "6-37-1601"|
                       ozone1$siteid == "6-37-1602"|
                       ozone1$siteid == "6-37-5001"|
                       ozone1$siteid == "6-37-5005"|
                       ozone1$siteid == "6-37-6012"|
                       ozone1$siteid == "6-65-8001"|
                       ozone1$siteid == "6-71-1004"|
                       ozone1$siteid == "18-97-57"|
                       ozone1$siteid == "18-97-78"|
                       ozone1$siteid == "18-97-84"|
                       ozone1$siteid == "18-97-85"|
                       ozone1$siteid == "19-45-21"|
                       ozone1$siteid == "19-139-20"|
                       ozone1$siteid == "19-163-15"|
                       ozone1$siteid == "48-141-37"|
                       ozone1$siteid == "48-141-44"|
                       ozone1$siteid == "48-141-55")

tapply(ozone1_new$year, ozone1_new$siteid, table)

### NOx data ###

NOx01 <- read.csv("daily_NONOxNOy_2001.csv", stringsAsFactors = FALSE)
NOx02 <- read.csv("daily_NONOxNOy_2002.csv", stringsAsFactors = FALSE)
NOx03 <- read.csv("daily_NONOxNOy_2003.csv", stringsAsFactors = FALSE)
NOx04 <- read.csv("daily_NONOxNOy_2004.csv", stringsAsFactors = FALSE)
NOx05 <- read.csv("daily_NONOxNOy_2005.csv", stringsAsFactors = FALSE)
NOx06 <- read.csv("daily_NONOxNOy_2006.csv", stringsAsFactors = FALSE)
NOx07 <- read.csv("daily_NONOxNOy_2007.csv", stringsAsFactors = FALSE)
NOx08 <- read.csv("daily_NONOxNOy_2008.csv", stringsAsFactors = FALSE)
NOx09 <- read.csv("daily_NONOxNOy_2009.csv", stringsAsFactors = FALSE)
NOx10 <- read.csv("daily_NONOxNOy_2010.csv", stringsAsFactors = FALSE)

NOx <- rbind(NOx01, NOx02, NOx03, NOx04, NOx05, 
             NOx06, NOx07, NOx08, NOx09, NOx10)

NOx$State.Code <- as.numeric(NOx$State.Code)
NOx$siteid <- paste(NOx$State.Code, NOx$County.Code, NOx$Site.Num, sep = '-')

NOx$Date.Local <- as.Date(as.character(NOx$Date.Local), format("%Y-%m-%d"))
NOx$year <- format(NOx$Date.Local, "%Y")
NOx$month <- format(NOx$Date.Local, "%m")

NOx <- NOx[NOx$Parameter.Code == 42603 & NOx$Sample.Duration == "1 HOUR",]

write.csv(NOx, "NOx_all.csv")

NOx1 <- subset(NOx, NOx$Arithmetic.Mean!="NA")
NOx1 <- subset(NOx1, NOx1$Observation.Count>=23)

NOx1_new <- subset(NOx1, NOx1$siteid == "13-89-2"|
                     NOx1$siteid =="13-89-3001" |
                     NOx1$siteid =="13-247-1"|
                     NOx1$siteid == "17-31-4201"|
                     NOx1$siteid == "17-31-72"|
                     NOx1$siteid == "17-97-1007"|
                     NOx1$siteid == "18-89-22"|
                     NOx1$siteid == "18-89-23"|
                     NOx1$siteid == "18-89-28"|
                     NOx1$siteid == "18-89-29"|
                     NOx1$siteid == "18-89-30"|
                     NOx1$siteid == "18-89-1003"|
                     NOx1$siteid == "18-89-2004"|
                     NOx1$siteid == "18-89-2008"|
                     NOx1$siteid == "24-5-3001"|
                     NOx1$siteid == "24-3-19"|
                     NOx1$siteid == "24-25-9001"|
                     NOx1$siteid == "25-9-2006"|
                     NOx1$siteid == "25-25-41"|
                     NOx1$siteid == "25-25-42"|
                     NOx1$siteid == "44-3-2"|
                     NOx1$siteid == "44-3-11"|
                     NOx1$siteid == "44-3-12"|
                     NOx1$siteid == "44-3-13"|
                     NOx1$siteid == "44-3-14"|
                     NOx1$siteid == "44-3-15"|
                     NOx1$siteid == "44-7-22"|
                     NOx1$siteid == "44-7-24"|
                     NOx1$siteid == "44-7-25"|
                     NOx1$siteid == "44-7-26"|
                     NOx1$siteid == "44-7-29"|
                     NOx1$siteid == "44-7-1010"|
                     NOx1$siteid == "48-113-69"|
                     NOx1$siteid == "48-121-34"|
                     NOx1$siteid == "48-139-15"|
                     NOx1$siteid == "48-139-1044"|
                     NOx1$siteid == "48-257-5"|
                     NOx1$siteid == "48-439-1002"|
                     NOx1$siteid == "48-439-3009"|
                     NOx1$siteid == "48-167-14"|
                     NOx1$siteid == "48-167-1034"|
                     NOx1$siteid == "48-201-24"|
                     NOx1$siteid == "48-201-26"|
                     NOx1$siteid == "48-201-29"|
                     NOx1$siteid == "48-201-30"|
                     NOx1$siteid == "48-201-55"|
                     NOx1$siteid == "48-201-1035"|
                     NOx1$siteid == "48-201-1039"|
                     NOx1$siteid == "48-339-78"|
                     NOx1$siteid == "6-37-2"|
                     NOx1$siteid == "6-37-1002"|
                     NOx1$siteid == "6-37-1103"|
                     NOx1$siteid == "6-37-1601"|
                     NOx1$siteid == "6-37-1602"|
                     NOx1$siteid == "6-37-5001"|
                     NOx1$siteid == "6-37-5005"|
                     NOx1$siteid == "6-37-6012"|
                     NOx1$siteid == "6-65-8001"|
                     NOx1$siteid == "6-71-1004"|
                     NOx1$siteid == "18-97-57"|
                     NOx1$siteid == "18-97-78"|
                     NOx1$siteid == "18-97-84"|
                     NOx1$siteid == "18-97-85"|
                     NOx1$siteid == "19-45-21"|
                     NOx1$siteid == "19-139-20"|
                     NOx1$siteid == "19-163-15"|
                     NOx1$siteid == "48-141-37"|
                     NOx1$siteid == "48-141-44"|
                     NOx1$siteid == "48-141-55")

tapply(NOx1_new$year, NOx1_new$siteid, table)
